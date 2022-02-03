#include "Codegen.h"

#include "Util.h"
#include "Intrinsics.h"

#include <llvm/ADT/None.h>
#include <llvm/Support/Casting.h>
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
#include <string>
#include <utility>
#include <variant>
#include <vector>

namespace lir::codegen {
  template <typename I>
  llvm::Value *fieldGep(LocalCC &lcc, llvm::Value *loaded, I &i) {
    auto structTy = adtTy(lcc.cc, std::get<type::Ty::ADT>(type::uncycle(lcc.inst.loggedTys.at(i.obj->ty))->v));
    auto castPtr = lcc.ib.CreatePointerCast(loaded, structTy->getPointerTo());
    return lcc.ib.CreateStructGEP(structTy, castPtr, i.field);
  }

  bool maybeTemp(LocalCC &lcc, Insn &insn, llvm::Value *value, Value &out) {
    const std::optional<GCData> &gcData = getTy<std::optional<GCData>>(lcc, insn.ty);
    if (gcData) {
      auto alloca = addTemporary(lcc, value->getType(), gcData->metadata);
      lcc.ib.CreateStore(value, alloca);
      out = {alloca, value->getType(), Value::Pointer};
      return true;
    }
    return false;
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::SetVar &i) {
    lcc.ib.CreateStore(lcc.load(i.value), lcc.reference(i.var));
    return {}; // ignored
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::GetVar &i) {
    return lcc.varFor(i.var);
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::SetField &i) {
    auto loaded = lcc.load(i.obj);
    if (!loaded->getType()->isPointerTy()) {
      // setting field of zero-sized type:
      // noop
      return {};
    }
    lcc.ib.CreateStore(lcc.load(i.value), fieldGep(lcc, loaded, i));
    return {};
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::GetField &i) {
    llvm::Type *ty = getTy(lcc, insn.ty);
    llvm::Type *objTy = getTy(lcc, i.obj->ty);
    if (!objTy->isPointerTy()) {
      return {llvm::ConstantExpr::getNullValue(ty), ty, Value::Direct};
    }
    return {
        [&](){return fieldGep(lcc, lcc.load(i.obj), i);},
        ty,
        Value::Pointer
    };
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::HeapAlloc &i) {
    auto ty = getTy(lcc, insn.ty);
    if (!ty->isPointerTy()) {
      // zero-sized type
      return {llvm::ConstantExpr::getNullValue(ty), ty, Value::Direct};
    }
    auto structTy = adtTy(cc, std::get<type::Ty::ADT>(type::uncycle(lcc.inst.loggedTys.at(insn.ty))->v));
    llvm::Value *cast = gcAlloc(lcc, structTy);
    Value ret;
    if (maybeTemp(lcc, insn, cast, ret))return ret;
    return {cast, ty, Value::Direct};
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::CallTrait &i) {
    auto impl = lcc.inst.loggedRefs.at(i.trait);
    llvm::Value *value = cc.emitCall.at(impl.first).at(impl.second)(insn, i, cc, lcc);
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
      Value ret;
      if (maybeTemp(lcc, insn, value, ret)) return ret;
    }
    return {value, value->getType(), Value::Direct};
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::PhiNode &i) {
    Tp outTy = getChirpTy(lcc, insn.ty);
    llvm::Type *ty = getTy(lcc.cc, outTy);
    auto phi = lcc.ib.CreatePHI(ty, i.branches.size());
    for (auto &ic : i.branches) {
      llvm::BasicBlock *bb = lcc.bbs.at(ic.block);
      llvm::Instruction *jump = &bb->getInstList().back();
      lcc.ib.SetInsertPoint(jump);
      Tp inTy = getChirpTy(lcc, ic.ref->ty);
      llvm::Value *inValue;
      if (outTy != inTy) {
        inValue = unionise(lcc, lcc.vals.at(ic.ref), inTy, outTy);
      } else {
        inValue = lcc.load(ic.ref);
      }
      phi->addIncoming(inValue, bb);
    }
    lcc.ib.SetInsertPoint(phi->getParent());
    Value ret;
    if (maybeTemp(lcc, insn, phi, ret)) return ret;
    return {phi, ty, Value::Direct};
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::NewTuple &i) {
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
      return {llvm::ConstantStruct::get(ty, vs), ty, Value::Direct};
    } not_const: {
    auto ty = getTy(lcc, insn.ty);
    auto alloca = lcc.ib.CreateAlloca(ty);
    Idx idx = 0;
    auto i32Ty = llvm::Type::getInt32Ty(cc.ctx);
    for (auto &v : i.values) {
      auto gep = lcc.ib.CreateInBoundsGEP(ty, alloca, {
          llvm::ConstantInt::get(i32Ty, 0),
          llvm::ConstantInt::get(i32Ty, idx++),
      });
      lcc.ib.CreateStore(lcc.load(v), gep);
    }
    return {alloca, ty, Value::Pointer};
  }
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::ForeignRef &i) {
    auto crpTy = lcc.inst.loggedTys.at(insn.ty);
    if (std::holds_alternative<Ty::FfiFn>(crpTy->v)) {
      auto fnTy = ffiFnTy(cc, std::get<Ty::FfiFn>(crpTy->v));
      auto modFn = cc.mod.getOrInsertFunction(i.symbol, fnTy);
      llvm::Value *callee = modFn.getCallee();
      return {callee, callee->getType(), Value::Direct};
    } else {
      auto ty = getTy(cc, crpTy);
      auto global = cc.mod.getOrInsertGlobal(i.symbol, ty);
      return {global, ty, Value::Pointer};
    }
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::LiteralString &i) {
    auto cTy = lcc.inst.loggedTys.at(insn.ty);
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
    globalVar->setName("str");
    auto const0 = llvm::ConstantInt::get(cc.ctx, llvm::APInt(32, 0));
    std::array<llvm::Constant *, 2> indices = {const0, const0};
    llvm::Constant *charPtrConst = llvm::ConstantExpr::
    getInBoundsGetElementPtr(charArrayTy, globalVar, indices);
    if (nulTerminate) {
      return {charPtrConst, charTy->getPointerTo(), Value::Direct};
    }
    auto stringStructTy = llvm::cast<llvm::StructType>(getTy(lcc, insn.ty));
    llvm::Constant *lenConst = llvm::ConstantInt::get(cc.ctx, llvm::APInt(64, len));
    llvm::Constant *constant = llvm::ConstantStruct::get(stringStructTy, {lenConst, charPtrConst});
    return {constant, stringStructTy, Value::Direct};
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::LiteralInt &i) {
    Ty *cTy = lcc.inst.loggedTys.at(insn.ty);
    auto ty = getTy(cc, cTy);
    auto *intTy = llvm::cast<llvm::IntegerType>(ty);
    return {llvm::ConstantInt::get(intTy, i.value, 10), ty, Value::Direct};
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::LiteralFloat &i) {
    auto ty = getTy(lcc, insn.ty);
    return {llvm::ConstantFP::get(ty, i.value), ty, Value::Direct};
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::LiteralBool &i) {
    auto ty = getTy(lcc, insn.ty);
    return {llvm::ConstantInt::get(ty, i.value), ty, Value::Direct};
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::BlockStart &i) {
    loc::SrcLoc &loc = insn.span->lo;
    llvm::DILexicalBlock *block = cc.db.createLexicalBlock(
        lcc.scopes.back(),
        cc.cu->getFile(),
        loc.line, loc.col
    );
    lcc.scopes.push_back(block);
    return {};
  }

  Value emitInsn(Insn &insn, CC &cc, LocalCC &lcc, Insn::BlockEnd &i) {
    lcc.scopes.pop_back();
    return {};
  }

  void emitInsn(Insn &insn, CC &cc, LocalCC &lcc) {
    if (insn.span) {
      auto &loc = insn.span->lo;
      lcc.ib.SetCurrentDebugLocation(locFromSpan(cc, lcc, loc));
    }
    lcc.vals[&insn] = std::visit([&](auto &i) { return emitInsn(insn, cc, lcc, i); }, insn.v);
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

  void emitParam(Idx idx, Decl &i, CC &cc, LocalCC &lcc) {
    Idx index = lcc.paramC++;
    llvm::Argument *arg = lcc.func->getArg(index);
    arg->setName(i.name);
    const auto &gcData = getTy<std::optional<GCData>>(lcc, i.ty);
    if (i.span || gcData) {
      llvm::AllocaInst *alloca = lcc.ib.CreateAlloca(arg->getType(), nullptr, i.name + ".ref");
      lcc.ib.CreateStore(arg, alloca);
      if (i.span) {
        cc.db.insertDeclare(
          alloca,
            cc.db.createParameterVariable(
            lcc.scopes.back(), i.name, index, cc.cu->getFile(),
              i.span->lo.line, getTy<llvm::DIType *>(lcc, i.ty)
            ),
            cc.db.createExpression(),
            locFromSpan(cc, lcc, i.span->lo),
            lcc.ib.GetInsertBlock()
        );
      }
      if (gcData) {
        gcRoot(cc, lcc.ib, alloca, gcData->metadata);
      }
      lcc.vars[idx] = {alloca, arg->getType(), Value::Pointer};
      return;
    }
    lcc.vars[idx] = {arg, arg->getType(), Value::Direct};
  }

  void emitVar(Idx idx, Decl &i, CC &cc, LocalCC &lcc, bool topLevel) {
    auto tyTup = getTyTuple(cc, lcc.inst.loggedTys.at(i.ty));
    llvm::Type *ty = std::get<0>(tyTup);
    const auto &gcData = std::get<2>(tyTup);
    if (topLevel) {
      auto global = new llvm::GlobalVariable(
        cc.mod,
        ty,
        false,
        llvm::GlobalVariable::PrivateLinkage,
        llvm::ConstantExpr::getNullValue(ty)
      );
      global->setName("global." + i.name);
      if (gcData) {
        llvm::AllocaInst *alloca = lcc.ib.CreateAlloca(global->getType(), nullptr, i.name);
        lcc.ib.CreateStore(global, alloca);
        ensureMetaTy(cc);
        auto meta = new llvm::GlobalVariable(
          cc.mod,
          cc.gcMetaTy,
          true,
          llvm::GlobalVariable::PrivateLinkage,
          llvm::ConstantStruct::get(
            cc.gcMetaTy,
            gcData->metadata ?
            gcData->metadata->getInitializer()->getAggregateElement(0U) :
            llvm::ConstantPointerNull::get(cc.metaFnTy->getPointerTo()),
            llvm::ConstantInt::get(cc.ctx, llvm::APInt(8, 1))
          ),
          "global." + i.name + ".meta"
        );
        gcRoot(cc, lcc.ib, alloca, meta);
      }
      
      cc.vars[idx] = {global, ty, Value::Pointer};
      return;
    }
    llvm::AllocaInst *alloca = lcc.ib.CreateAlloca(ty, nullptr, i.name);
    if (i.span) {
      cc.db.insertDeclare(
          alloca,
          cc.db.createAutoVariable(
              lcc.scopes.back(), i.name, cc.cu->getFile(),
              i.span->lo.line, std::get<1>(tyTup)
          ),
          cc.db.createExpression(),
          locFromSpan(cc, lcc, i.span->lo),
          lcc.ib.GetInsertBlock()
      );
    }
    if (gcData) {
      lcc.ib.CreateStore(llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ty)), alloca);
      gcRoot(cc, lcc.ib, alloca, gcData->metadata);
    }
    lcc.vars[idx] = {alloca, ty, Value::Pointer};
  }

  void emitBlockList(BlockList &bl, CC &cc, LocalCC &lcc, bool topLevel) {
    llvm::BasicBlock *decl = llvm::BasicBlock::Create(cc.ctx, "decl", lcc.func);
    for (auto &bb : bl.blocks) {
      lcc.bbs[bb.get()] = llvm::BasicBlock::Create(cc.ctx, "", lcc.func);
    }
    lcc.ib.SetInsertPoint(decl);
    for (auto &e : bl.vars) {
      emitVar(e.first, e.second, cc, lcc, topLevel);
    }
    for (auto &e : bl.params) {
      emitParam(e.first, e.second, cc, lcc);
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
    auto &receiver = bl.params.rbegin()->second;
    const std::string &name = receiver.name;
    const std::optional<loc::Span> &span = receiver.span;
    Idx retIdx = std::get<Jump::Ret>(bl.blocks.back()->end.v).value->ty;
    auto &retPair = getTyTuple(cc, inst.loggedTys.at(retIdx));
    diTys.push_back(std::get<1>(retPair));
    for (const auto &e : bl.params) {
      auto argPair = getTyTuple(cc, inst.loggedTys.at(e.second.ty));
      argTys.push_back(std::get<0>(argPair));
      diTys.push_back(std::get<1>(argPair));
    }
    llvm::FunctionType *funcTy = llvm::FunctionType::get(std::get<0>(retPair), argTys, false);
    llvm::Function *func = llvm::Function::Create(funcTy,
                                                  llvm::GlobalValue::PrivateLinkage,
                                                  name,
                                                  cc.mod);
    func->setGC(GC_METHOD);
    uint32_t lineNo = span ? span->lo.line : 0;
    func->setSubprogram(cc.db.createFunction(
        cc.cu->getFile(), name, func->getName(), cc.cu->getFile(),
        lineNo, cc.db.createSubroutineType(cc.db.getOrCreateTypeArray(diTys)),
        lineNo, llvm::DINode::FlagPrivate, llvm::DISubprogram::SPFlagDefinition
    ));
    return func;
  }

  CodegenResult generate(type::infer::Inst::Set &allInsts,
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
    CC cc { *llvmMod, *llvmCtx, db, cu };

    std::vector<std::unique_ptr<std::vector<llvm::Function *>>> traitMethods;
    // block 0 is main
    addIntrinsics(cc, allInsts);
    for (auto &trait : mod.traitImpls) {
      Idx blockIdx = trait.methods.front().blockIdx;
      if (!allInsts.entities.count(blockIdx)) continue;
      auto &emitCalls = cc.emitCall[blockIdx];
      auto &insts = allInsts.entities.at(blockIdx);
      for (auto &inst : insts) {
        Idx instIdx = inst.first;
        // only one method is actually supported
        // for (auto &method : trait.methods) {
        std::vector<llvm::Function *> &funcs = *traitMethods.emplace_back(std::make_unique<std::vector<llvm::Function*>>());
        for (const BlockList &bl : trait.methods) {
          funcs.push_back(getBbFunc(bl, allInsts[{blockIdx, instIdx}], cc));
        }
        emitCalls.emplace(instIdx, [&funcs](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc) -> llvm::Value * {
          auto func = funcs.at(ct.method);
          std::vector<llvm::Value *> args;
          args.reserve(ct.args.size() + 1 /* receiver */);
          for (auto &i : ct.args) {
            args.push_back(lcc.load(i));
          }
          args.push_back(lcc.load(ct.obj));
          return lcc.ib.CreateCall(func->getFunctionType(), func, args);
        });
      }
    }

    llvm::IRBuilder<> ib(cc.ctx);

    type::Tcx tmpTcx;
    auto unitTyPair = getTyTuple(cc, tmpTcx.intern(Ty::Tuple{{}}));
    auto unitThunkTy = llvm::FunctionType::get(std::get<0>(unitTyPair), false);
    auto crpMainFunc = llvm::Function::Create(
      unitThunkTy,
      llvm::GlobalValue::PrivateLinkage,
      "crpMain",
      cc.mod
    );
    {
      crpMainFunc->setGC(GC_METHOD);
      crpMainFunc->setSubprogram(cc.db.createFunction(
          cc.cu->getFile(), "crpMain", "crpMain", cc.cu->getFile(),
          1,
          cc.db.createSubroutineType(cc.db.getOrCreateTypeArray({std::get<1>(unitTyPair)})),
          1,
          llvm::DINode::FlagPrivate, llvm::DISubprogram::SPFlagDefinition
      ));
      LocalCC lcc(cc, ib, crpMainFunc, allInsts[{0, 0}]);
      emitBlockList(mod.topLevel, cc, lcc, true);
    }

    auto i32Ty = llvm::Type::getInt32Ty(cc.ctx);
    auto voidTy = llvm::Type::getVoidTy(cc.ctx);
    auto mainTy = llvm::FunctionType::get(i32Ty, false);
    auto mainFunc = llvm::Function::Create(mainTy, llvm::GlobalValue::ExternalLinkage, "main", cc.mod);
    {
      llvm::BasicBlock *entry = llvm::BasicBlock::Create(cc.ctx, "entry", mainFunc);
      ib.SetInsertPoint(entry);
      auto gcInit = cc.mod.getOrInsertFunction("chirpGcInit", voidTy);
      ib.CreateCall(gcInit);
      ib.CreateCall(unitThunkTy, crpMainFunc);
      auto gcShutdown = cc.mod.getOrInsertFunction("chirpGcShutdown", voidTy);
      ib.CreateCall(gcShutdown);
      ib.CreateRet(llvm::ConstantInt::get(i32Ty, 0));
    }

    {
      auto tIt = traitMethods.begin();
      for (auto &trait : mod.traitImpls) {
        Idx blockIdx = trait.methods.front().blockIdx;
        if (!allInsts.entities.count(blockIdx)) continue;
        auto &insts = allInsts.entities.at(blockIdx);
        for (auto &inst : insts) {
          std::vector<llvm::Function *> &funcs = **tIt++;
          auto fIt = funcs.begin();
          for (BlockList &bl : trait.methods) {
            LocalCC lcc(cc, ib, *fIt++, inst.second);
            emitBlockList(bl, cc, lcc, false);
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
