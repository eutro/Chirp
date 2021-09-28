#include "Codegen.h"

#include "Util.h"
#include "Intrinsics.h"

#include <llvm/ADT/None.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DIBuilder.h>
#include <optional>
#include <sstream>
#include <string>
#include <utility>
#include <variant>
#include <vector>

namespace lir::codegen {
  const std::string GC_METHOD = "shadow-stack";

  void preEmitInsn(Insn &insn, CC &cc, LocalCC &lcc) {
    std::visit(overloaded{
        [&](Insn::DeclareParam &i) -> void {
          Idx index = lcc.paramC++;
          llvm::Argument *arg = lcc.func->getArg(index);
          const auto &gcData = getTy<std::optional<GCData>>(lcc, insn.ty);
          if (insn.span || gcData) {
            llvm::AllocaInst *alloca = lcc.ib.CreateAlloca(arg->getType());
            lcc.ib.CreateStore(arg, alloca);
            if (insn.span) {
              cc.db.insertDeclare(
                  alloca,
                  cc.db.createParameterVariable(
                      lcc.scopes.back(), i.name, index, cc.cu->getFile(),
                      insn.span->lo.line, getTy<llvm::DIType *>(lcc, insn.ty)
                  ),
                  cc.db.createExpression(),
                  locFromSpan(cc, lcc, insn.span->lo),
                  lcc.ib.GetInsertBlock()
              );
            }
            if (gcData) {
              gcRoot(cc, lcc.ib, alloca, gcData->metadata);
            }
            lcc.vals[&insn] = {alloca, arg->getType(), LocalCC::Value::Pointer};
            return;
          }
          lcc.vals[&insn] = {arg, arg->getType(), LocalCC::Value::Direct};
        },
        [&](Insn::DeclareVar &i) -> void {
          auto tyTup = getTyTuple(cc, lcc.inst.types.at(insn.ty));
          llvm::Type *ty = std::get<0>(tyTup);
          const auto &gcData = std::get<2>(tyTup);
          llvm::AllocaInst *alloca = lcc.ib.CreateAlloca(ty);
          if (insn.span) {
            cc.db.insertDeclare(
                alloca,
                cc.db.createAutoVariable(
                    lcc.scopes.back(), i.name, cc.cu->getFile(),
                    insn.span->lo.line, std::get<1>(tyTup)
                ),
                cc.db.createExpression(),
                locFromSpan(cc, lcc, insn.span->lo),
                lcc.ib.GetInsertBlock()
            );
          }
          if (gcData) {
            lcc.ib.CreateStore(llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ty)), alloca);
            gcRoot(cc, lcc.ib, alloca, gcData->metadata);
          }
          lcc.vals[&insn] = {alloca, ty, LocalCC::Value::Pointer};
        },
        [](auto&){}
    }, insn.v);
  }

  void emitInsn(Insn &insn, CC &cc, LocalCC &lcc) {
    if (insn.span) {
      auto &loc = insn.span->lo;
      lcc.ib.SetCurrentDebugLocation(locFromSpan(cc, lcc, loc));
    }
    lcc.vals[&insn] = std::visit(overloaded {
        [&](Insn::DeclareParam &i) -> LocalCC::Value {
          return lcc.vals[&insn];
        },
        [&](Insn::DeclareVar &i) -> LocalCC::Value {
          return lcc.vals[&insn];
        },
        [&](Insn::SetVar &i) -> LocalCC::Value {
          lcc.ib.CreateStore(lcc.load(i.value), lcc.reference(i.var));
          return {}; // ignored
        },
        [&](Insn::GetVar &i) -> LocalCC::Value {
          return lcc.vals.at(i.var);
        },

#define FIELD_GEP \
          auto i32Ty = llvm::Type::getInt32Ty(cc.ctx); \
          auto structTy = adtTy(cc, std::get<type::Ty::ADT>(lcc.inst.types.at(i.obj->ty)->v));\
          auto castPtr = lcc.ib.CreatePointerCast(lcc.load(i.obj), structTy->getPointerTo());\
          auto gep = lcc.ib.CreateInBoundsGEP(castPtr, {\
            llvm::ConstantInt::get(i32Ty, 0),\
            llvm::ConstantInt::get(i32Ty, i.field)\
          });

        [&](Insn::SetField &i) -> LocalCC::Value {
          FIELD_GEP
          lcc.ib.CreateStore(lcc.load(i.value), gep);
          return {};
        },
        [&](Insn::GetField &i) -> LocalCC::Value {
          FIELD_GEP
          llvm::Type *ty = getTy(lcc, insn.ty);
          return {gep, ty, LocalCC::Value::Pointer};
        },

#define MAYBE_TEMP(VALUE)                                               \
          do {                                                          \
            const std::optional<GCData> &gcData = getTy<std::optional<GCData>>(lcc, insn.ty); \
            if (gcData) {                                               \
              auto alloca = addTemporary(lcc, VALUE->getType(), gcData->metadata); \
              lcc.ib.CreateStore(VALUE, alloca);                        \
              return {alloca, VALUE->getType(), LocalCC::Value::Pointer}; \
            }                                                           \
          } while(0)

        [&](Insn::HeapAlloc &i) -> LocalCC::Value {
          auto i32Ty = llvm::IntegerType::getInt32Ty(cc.ctx);
          auto i8PtrTy = llvm::IntegerType::getInt8PtrTy(cc.ctx);
          auto ty = getTy(lcc, insn.ty);
          auto gcAlloc = cc.mod.getOrInsertFunction("gcAlloc", i8PtrTy, i32Ty, i32Ty);
          auto structTy = adtTy(cc, std::get<type::Ty::ADT>(lcc.inst.types.at(insn.ty)->v));
          auto rawSize = llvm::ConstantExpr::getSizeOf(structTy);
          auto rawAlign = llvm::ConstantExpr::getAlignOf(structTy);
          auto size = llvm::ConstantExpr::getTruncOrBitCast(rawSize, i32Ty);
          auto align = llvm::ConstantExpr::getTruncOrBitCast(rawAlign, i32Ty);
          auto call = lcc.ib.CreateCall(gcAlloc, {size, align});
          llvm::Value *cast = lcc.ib.CreatePointerCast(call, ty);
          MAYBE_TEMP(cast);
          return {cast, ty, LocalCC::Value::Direct};
        },

        [&](Insn::CallTrait &i) -> LocalCC::Value {
          TraitImpl::For tFor {
            .ty = lcc.inst.types.at(i.obj->ty),
            .tb = lcc.inst.traits.at(i.trait),
          };
          llvm::Value *value;
          if (std::holds_alternative<Ty::FfiFn>(tFor.ty->v)) {
            // hack for FFI
            std::vector<llvm::Value *> args;
            args.reserve(i.args.size());
            for (auto &arg : i.args) {
              args.push_back(lcc.load(arg));
            }
            llvm::FunctionType *fnTy = ffiFnTy(cc, std::get<Ty::FfiFn>(tFor.ty->v));
            value = lcc.ib.CreateCall(fnTy, lcc.load(i.obj), args);
          } else {
            value = cc.emitCall.at(tFor)(insn, i, cc, lcc);
          }
          if (std::holds_alternative<Jump::Ret>(lcc.bb->end.v) &&
              std::get<Jump::Ret>(lcc.bb->end.v).value == &insn) {
            if (auto call = llvm::dyn_cast_or_null<llvm::CallInst>(value)) {
              if (call->getFunction() == call->getCalledFunction()) {
                // force TCO if it's self-recursive
                call->setTailCallKind(llvm::CallInst::TCK_MustTail);
              } else {
                // otherwise just encourage it
                call->setTailCallKind(llvm::CallInst::TCK_Tail);
              }
            }
          } else {
            MAYBE_TEMP(value);
          }
          return {value, value->getType(), LocalCC::Value::Direct};
        },
        [&](Insn::PhiNode &i) -> LocalCC::Value {
          llvm::Type *ty = getTy(lcc, insn.ty);
          auto phi = lcc.ib.CreatePHI(ty, i.branches.size());
          for (auto &ic : i.branches) {
            llvm::BasicBlock *bb = lcc.bbs.at(ic.block);
            llvm::Instruction *jump = &bb->getInstList().back();
            lcc.ib.SetInsertPoint(jump);
            phi->addIncoming(lcc.load(ic.ref), bb);
          }
          lcc.ib.SetInsertPoint(phi->getParent());
          MAYBE_TEMP(phi);
          return {phi, ty, LocalCC::Value::Direct};
        },
        [&](Insn::NewTuple &i) -> LocalCC::Value {
          {
            for (auto &v : i.values) {
              if (!llvm::isa<llvm::Constant>(lcc.load(v))) {
                goto not_const;
              }
            }
            std::vector<llvm::Constant *> vs;
            vs.reserve(i.values.size());
            for (auto &v : i.values) {
              vs.push_back(llvm::cast<llvm::Constant>(lcc.load(v)));
            }
            auto ty = llvm::cast<llvm::StructType>(getTy(lcc, insn.ty));
            return {llvm::ConstantStruct::get(ty, vs), ty, LocalCC::Value::Direct};
          } not_const: {
            auto ty = getTy(lcc, insn.ty);
            auto alloca = lcc.ib.CreateAlloca(ty);
            Idx idx = 0;
            auto i32Ty = llvm::Type::getInt32Ty(cc.ctx);
            for (auto &v : i.values) {
              auto gep = lcc.ib.CreateInBoundsGEP(alloca, {
                  llvm::ConstantInt::get(i32Ty, 0),
                  llvm::ConstantInt::get(i32Ty, idx++),
                });
              lcc.ib.CreateStore(lcc.load(v), gep);
            }
            return {alloca, ty, LocalCC::Value::Pointer};
          }
        },
        [&](Insn::ForeignRef &i) -> LocalCC::Value {
          auto crpTy = lcc.inst.types.at(insn.ty);
          if (std::holds_alternative<Ty::FfiFn>(crpTy->v)) {
            auto fnTy = ffiFnTy(cc, std::get<Ty::FfiFn>(crpTy->v));
            auto modFn = cc.mod.getOrInsertFunction(i.symbol, fnTy);
            llvm::Value *callee = modFn.getCallee();
            return {callee, callee->getType(), LocalCC::Value::Direct};
          } else {
            auto ty = getTy(cc, crpTy);
            auto global = cc.mod.getOrInsertGlobal(i.symbol, ty);
            return {global, ty, LocalCC::Value::Pointer};
          }
        },
        [&](Insn::LiteralString &i) -> LocalCC::Value {
          auto cTy = lcc.inst.types.at(insn.ty);
          bool nulTerminate = std::get<Ty::String>(cTy->v).nul;
          size_t len = i.value.size() + nulTerminate;
          auto charTy = llvm::Type::getInt8Ty(cc.ctx);
          llvm::ArrayType *charArrayTy = llvm::ArrayType::get(charTy, len);
          std::vector<llvm::Constant *> constantChars;
          constantChars.reserve(len);
          for (auto c : i.value) {
            constantChars.push_back(llvm::ConstantInt::get(cc.ctx, llvm::APInt(8, c)));
          }
          if (nulTerminate) {
            constantChars.push_back(llvm::ConstantInt::get(cc.ctx, llvm::APInt(8, 0)));
          }
          llvm::Constant *charsConstant = llvm::ConstantArray::get(charArrayTy, constantChars);
          auto globalVar = new llvm::GlobalVariable(cc.mod,
                                                    charArrayTy,
                                                    true,
                                                    llvm::GlobalValue::PrivateLinkage,
                                                    charsConstant);
          auto const0 = llvm::ConstantInt::get(cc.ctx, llvm::APInt(32, 0));
          std::array<llvm::Constant *, 2> indices = {const0, const0};
          llvm::Constant *charPtrConst = llvm::ConstantExpr::
            getInBoundsGetElementPtr(charArrayTy, globalVar, indices);
          if (nulTerminate) {
            return {charPtrConst, charTy->getPointerTo(), LocalCC::Value::Direct};
          }
          auto stringStructTy = llvm::cast<llvm::StructType>(getTy(lcc, insn.ty));
          llvm::Constant *lenConst = llvm::ConstantInt::get(cc.ctx, llvm::APInt(64, len));
          llvm::Constant *constant = llvm::ConstantStruct::get(stringStructTy, {lenConst, charPtrConst});
          return {constant, stringStructTy, LocalCC::Value::Direct};
        },
        [&](Insn::LiteralInt &i) -> LocalCC::Value {
          Ty *cTy = lcc.inst.types.at(insn.ty);
          auto ty = getTy(cc, cTy);
          llvm::IntegerType *intTy = llvm::cast<llvm::IntegerType>(ty);
          return {llvm::ConstantInt::get(intTy, i.value, 10), ty, LocalCC::Value::Direct};
        },
        [&](Insn::LiteralFloat &i) -> LocalCC::Value {
          auto ty = getTy(lcc, insn.ty);
          return {llvm::ConstantFP::get(ty, i.value), ty, LocalCC::Value::Direct};
        },
        [&](Insn::LiteralBool &i) -> LocalCC::Value {
          auto ty = getTy(lcc, insn.ty);
          return {llvm::ConstantInt::get(ty, i.value), ty, LocalCC::Value::Direct};
        },
        [&](Insn::BlockStart &i) -> LocalCC::Value {
          loc::SrcLoc &loc = insn.span->lo;
          llvm::DILexicalBlock *block = cc.db.createLexicalBlock(
              lcc.scopes.back(),
              cc.cu->getFile(),
              loc.line, loc.col
          );
          lcc.scopes.push_back(block);
          return {};
        },
        [&](Insn::BlockEnd &i) -> LocalCC::Value {
          lcc.scopes.pop_back();
          return {};
        },
    }, insn.v);
    lcc.ib.SetCurrentDebugLocation(nullptr);
  }

  void emitJump(Jump &j, CC &cc, LocalCC &lcc) {
    std::visit(overloaded {
        [&](Jump::Ret &r) {
          lcc.ib.CreateRet(lcc.load(r.value));
        },
        [&](Jump::Br &r) {
          lcc.ib.CreateBr(lcc.bbs.at(r.target));
        },
        [&](Jump::CondBr &r) {
          lcc.ib.CreateCondBr(lcc.load(r.pred),
                              lcc.bbs.at(r.thenB),
                              lcc.bbs.at(r.elseB));
        },
    }, j.v);
  }

  void emitBB(BasicBlock &bb, CC &cc, LocalCC &lcc) {
    for (auto &i : bb.insns) {
      emitInsn(*i, cc, lcc);
    }
    emitJump(bb.end, cc, lcc);
  }

  void emitBlockList(BlockList &bl, CC &cc, LocalCC &lcc) {
    llvm::BasicBlock *decl = llvm::BasicBlock::Create(cc.ctx, "decl", lcc.func);
    for (auto &bb : bl.blocks) {
      lcc.bbs[bb.get()] = llvm::BasicBlock::Create(cc.ctx, "", lcc.func);
    }
    lcc.ib.SetInsertPoint(decl);
    for (auto &bb : bl.blocks) {
      lcc.bb = bb.get();
      for (auto &i : bb->insns) {
        preEmitInsn(*i, cc, lcc);
      }
    }
    lcc.ib.CreateBr(lcc.bbs.at(bl.blocks.front().get()));
    for (auto &bb : bl.blocks) {
      lcc.ib.SetInsertPoint(lcc.bbs.at(bb.get()));
      lcc.bb = bb.get();
      emitBB(*bb, cc, lcc);
    }
  }

  llvm::Function *getBbFunc(const BlockList &bl, const Instantiation &inst, CC &cc) {
    std::vector<llvm::Type *> argTys;
    std::vector<llvm::Metadata *> diTys;
    const std::string *name = nullptr;
    const std::optional<loc::Span> *span = nullptr;
    Idx retIdx = std::get<Jump::Ret>(bl.blocks.back()->end.v).value->ty;
    auto &retPair = getTyTuple(cc, inst.types.at(retIdx));
    diTys.push_back(std::get<1>(retPair));
    for (auto &insn : bl.blocks.front()->insns) {
      if (std::holds_alternative<Insn::DeclareParam>(insn->v)) {
        auto argPair = getTyTuple(cc, inst.types.at(insn->ty));
        argTys.push_back(std::get<0>(argPair));
        diTys.push_back(std::get<1>(argPair));
        name = &std::get<Insn::DeclareParam>(insn->v).name;
        span = &insn->span;
      }
    }
    llvm::FunctionType *funcTy = llvm::FunctionType::get(std::get<0>(retPair), argTys, false);
    llvm::Function *func = llvm::Function::Create(funcTy,
                                                  llvm::GlobalValue::PrivateLinkage,
                                                  name ? *name : "",
                                                  cc.mod);
    func->setGC(GC_METHOD);
    uint32_t lineNo = span && *span ? (**span).lo.line : 0;
    func->setSubprogram(cc.db.createFunction(
        cc.cu->getFile(), *name, func->getName(), cc.cu->getFile(),
        lineNo, cc.db.createSubroutineType(cc.db.getOrCreateTypeArray(diTys)),
        lineNo, llvm::DINode::FlagPrivate, llvm::DISubprogram::SPFlagDefinition
    ));
    return func;
  }

  CodegenResult generate(type::Tcx &tcx,
                         type::Tbcx &tbcx,
                         Module &mod,
                         const std::string &fileName,
                         const std::string &fileDir) {
    auto llvmCtx = std::make_unique<llvm::LLVMContext>();
    auto llvmMod = std::make_unique<llvm::Module>(fileName, *llvmCtx);
    llvm::DIBuilder db(*llvmMod);
    auto cu = db.createCompileUnit(
        llvm::dwarf::DW_LANG_C,
        db.createFile(fileName, fileDir, llvm::None),
        "Chirp Compiler",
        false, "", 0
    );
    llvmMod->addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
    llvmMod->addModuleFlag(llvm::Module::Warning, "Dwarf Version", llvm::dwarf::DWARF_VERSION);
    CC cc { tcx, tbcx, *llvmMod, *llvmCtx, db, cu };

    addIntrinsics(cc);

    std::vector<std::unique_ptr<std::vector<llvm::Function *>>> traitMethods;

    for (auto &trait : mod.traitImpls) {
      for (auto &entry : trait.instantiations) {
        std::vector<llvm::Function *> &funcs = *traitMethods.emplace_back(std::make_unique<std::vector<llvm::Function*>>());
        funcs.reserve(trait.methods.size());
        for (const BlockList &bl : trait.methods) {
          funcs.push_back(getBbFunc(bl, entry.second, cc));
        }
        cc.emitCall[entry.first] = [&funcs](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc) -> llvm::Value * {
          auto func = funcs.at(ct.method);
          std::vector<llvm::Value *> args;
          args.reserve(ct.args.size() + 1 /* receiver */);
          for (auto &i : ct.args) {
            args.push_back(lcc.load(i));
          }
          args.push_back(lcc.load(ct.obj));
          return lcc.ib.CreateCall(func->getFunctionType(), func, args);
        };
      }
    }

    llvm::IRBuilder<> ib(cc.ctx);

    auto unitTyPair = getTyTuple(cc, tcx.intern(Ty::Tuple{{}}));
    auto unitThunkTy = llvm::FunctionType::get(std::get<0>(unitTyPair), false);
    auto crpMainFunc = llvm::Function::Create(
        unitThunkTy, llvm::GlobalValue::PrivateLinkage, "crpMain", cc.mod);
    {
      crpMainFunc->setGC(GC_METHOD);
      crpMainFunc->setSubprogram(cc.db.createFunction(
          cc.cu->getFile(), "crpMain", "crpMain", cc.cu->getFile(),
          1,
          cc.db.createSubroutineType(cc.db.getOrCreateTypeArray({std::get<1>(unitTyPair)})),
          1,
          llvm::DINode::FlagPrivate, llvm::DISubprogram::SPFlagDefinition
      ));
      LocalCC lcc{
          .cc = cc,
          .ib = ib,
          .func = crpMainFunc,
          .inst = mod.instantiation,
      };
      emitBlockList(mod.topLevel, cc, lcc);
    }

    auto i32Ty = llvm::Type::getInt32Ty(cc.ctx);
    auto voidTy = llvm::Type::getVoidTy(cc.ctx);
    auto mainTy = llvm::FunctionType::get(i32Ty, false);
    auto mainFunc = llvm::Function::Create(mainTy, llvm::GlobalValue::ExternalLinkage, "main", cc.mod);
    {
      llvm::BasicBlock *entry = llvm::BasicBlock::Create(cc.ctx, "entry", mainFunc);
      ib.SetInsertPoint(entry);
      auto gcInit = cc.mod.getOrInsertFunction("gcInit", voidTy);
      ib.CreateCall(gcInit);
      ib.CreateCall(unitThunkTy, crpMainFunc);
      auto gcShutdown = cc.mod.getOrInsertFunction("gcShutdown", voidTy);
      ib.CreateCall(gcShutdown);
      ib.CreateRet(llvm::ConstantInt::get(i32Ty, 0));
    }

    {
      auto tIt = traitMethods.begin();
      for (auto &trait : mod.traitImpls) {
        for (auto &entry : trait.instantiations) {
          std::vector<llvm::Function *> &funcs = **tIt++;
          auto fIt = funcs.begin();
          for (BlockList &bl : trait.methods) {
            LocalCC lcc{
                .cc = cc,
                .ib = ib,
                .func = *fIt++,
                .inst = entry.second,
            };
            emitBlockList(bl, cc, lcc);
          }
        }
      }
    }

    for (auto &deferred : cc.deferred) {
      deferred(cc);
    }

    db.finalize();

    CodegenResult res;
    res.ctx = std::move(llvmCtx);
    res.mod = std::move(llvmMod);
    return res;
  }
}
