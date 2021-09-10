#include "Codegen.h"

#include "../hir/Builtins.h"

#include <array>
#include <functional>
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
#include <string>
#include <utility>
#include <variant>
#include <vector>

namespace lir::codegen {
  using type::Ty;
  using Tp = Ty *;

  const std::string GC_METHOD = "shadow-stack"; // TODO gc

  struct LocalCC;
  struct CC;
  using EmitFn = std::function<llvm::Value*(Insn&, Insn::CallTrait&, CC&, LocalCC&)>;
  using TyPair = std::tuple<llvm::Type *, llvm::DIType *>;

  struct CC {
    arena::InternArena<Ty> &tcx;
    arena::InternArena<type::TraitBound> &tbcx;
    llvm::Module &mod;
    llvm::LLVMContext &ctx;

    llvm::DIBuilder &db;
    llvm::DICompileUnit *cu;

    std::map<TraitImpl::For, EmitFn> emitCall;
    std::map<type::Ty *, TyPair> tyCache;
  };

  struct LocalCC {
    CC &cc;

    llvm::IRBuilder<> &ib;
    llvm::Function *func;
    lir::Instantiation &inst;

    std::map<Insn *, llvm::Value *> vals;
    std::map<BasicBlock *, llvm::BasicBlock *> bbs;
    Idx paramC = 0;

    std::vector<llvm::DILocalScope *> scopes{func->getSubprogram()};
  };

  llvm::FunctionType *ffiFnTy(CC &cc, type::Ty::FfiFn &v);

  const TyPair &getTyPair(CC &cc, type::Ty *ty) {
    auto found = cc.tyCache.find(ty);
    if (found != cc.tyCache.end()) {
      if (std::get<0>(found->second)) {
        return found->second;
      } else {
        throw std::runtime_error("Recursive flat type has infinite size");
      }
    }
    if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
      Ty *uncycled = type::uncycle(cc.tcx, cc.tbcx, ty);
      auto &pair = getTyPair(cc, uncycled);
      return cc.tyCache[ty] = pair;
    } else {
      cc.tyCache[ty] = std::make_tuple(nullptr, nullptr);
    }
    TyPair tPair = std::visit(overloaded {
        [&](Ty::Bool &v) -> TyPair {
          return std::make_tuple(
              llvm::Type::getInt1Ty(cc.ctx),
              cc.db.createBasicType("bool", 1, llvm::dwarf::DW_ATE_boolean)
          );
        },
        [&](Ty::Int &v) -> TyPair {
          Idx bitC = type::bitCount(v.s);
          return std::make_tuple(
              llvm::Type::getIntNTy(cc.ctx, bitC),
              cc.db.createBasicType("i" + std::to_string(bitC), bitC, llvm::dwarf::DW_ATE_signed)
          );
        },
        [&](Ty::UInt &v) -> TyPair {
          Idx bitC = type::bitCount(v.s);
          return std::make_tuple(
              llvm::Type::getIntNTy(cc.ctx, bitC),
              cc.db.createBasicType("u" + std::to_string(bitC), bitC, llvm::dwarf::DW_ATE_unsigned)
          );
        },
        [&](Ty::Float &v) -> TyPair {
          llvm::Type *lt;
          switch (v.s) {
            case type::FloatSize::f16: lt = llvm::Type::getHalfTy(cc.ctx); break;
            case type::FloatSize::f32: lt = llvm::Type::getFloatTy(cc.ctx); break;
            case type::FloatSize::f64: lt = llvm::Type::getDoubleTy(cc.ctx); break;
            default: throw 0;
          }
          Idx bitC = type::bitCount(v.s);
          return std::make_tuple(
              lt,
              cc.db.createBasicType("f" + std::to_string(bitC), bitC, llvm::dwarf::DW_ATE_float)
          );
        },
        [&](Ty::ADT &v) -> TyPair {
          auto sType = llvm::StructType::get(cc.ctx); // opaque
          llvm::PointerType *lt = llvm::PointerType::getUnqual(sType);
          return std::make_tuple(
              lt,
              cc.db.createUnspecifiedType("adt")
          );
        },
        [&](Ty::Tuple &v) -> TyPair {
          std::vector<llvm::Type *> fieldTys;
          std::vector<llvm::Metadata *> fieldDiTys;
          fieldTys.reserve(v.t.size());
          for (Tp fieldTy : v.t) {
            auto &pair = getTyPair(cc, fieldTy);
            fieldTys.push_back(std::get<0>(pair));
            fieldDiTys.push_back(std::get<1>(pair));
          }
          llvm::StructType *lt = llvm::StructType::get(cc.ctx, fieldTys);
          return std::make_tuple(
              lt,
              cc.db.createStructType(
                  cc.cu->getFile(), "tuple", cc.cu->getFile(), 1,
                  0, 1, llvm::DINode::DIFlags::FlagPublic,
                  nullptr, cc.db.getOrCreateArray(fieldDiTys)
              )
          );
        },
        [&](Ty::String &v) -> TyPair {
          return std::make_tuple(
              llvm::StructType::get(cc.ctx, {
                  llvm::Type::getInt64Ty(cc.ctx), // len
                  llvm::Type::getInt8PtrTy(cc.ctx), // bytes_utf8
              }),
              cc.db.createStructType(
                  cc.cu->getFile(), "string", cc.cu->getFile(), 1,
                  128, 64, llvm::DINode::DIFlags::FlagPublic,
                  nullptr, cc.db.getOrCreateArray({
                          cc.db.createBasicType("u64", 64,llvm::dwarf::DW_ATE_signed_fixed),
                          cc.db.createStringType("string", 64)
                      })
              )
          );
        },
        [&](Ty::FfiFn &v) -> TyPair {
          auto &tup = std::get<Ty::Tuple>(v.args->v);
          std::vector<llvm::Metadata *> argTys;
          argTys.reserve(tup.t.size() + 1);
          argTys.push_back(std::get<1>(getTyPair(cc, v.ret)));
          for (auto &t : tup.t) {
            argTys.push_back(std::get<1>(getTyPair(cc, t)));
          }
          return std::make_pair(
              ffiFnTy(cc, v)->getPointerTo(),
              cc.db.createPointerType(
                  cc.db.createSubroutineType(cc.db.getOrCreateTypeArray(argTys)),
                  64
              )
          );
        },
        [](auto&) -> TyPair {
          throw std::runtime_error("Type cannot exist after inference");
        }
    }, ty->v);
    return cc.tyCache[ty] = tPair;
  }

  template <typename T = llvm::Type *>
  T getTy(CC &cc, type::Tp ty) {
    return std::get<T>(getTyPair(cc, ty));
  }

  template <typename T = llvm::Type *>
  T getTy(LocalCC &lcc, Idx i) {
    return getTy<T>(lcc.cc, lcc.inst.types.at(i));
  }

  llvm::Type *adtTy(CC &cc, type::Ty::ADT &v) {
    std::vector<llvm::Type *> fieldTys;
    fieldTys.reserve(v.s.size());
    for (Tp fieldTy : v.s) {
      fieldTys.push_back(getTy(cc, fieldTy));
    }
    return llvm::StructType::get(cc.ctx, fieldTys);
  }

  llvm::FunctionType *ffiFnTy(CC &cc, type::Ty::FfiFn &v) {
    auto &tup = std::get<Ty::Tuple>(v.args->v);
    std::vector<llvm::Type *> argTys;
    argTys.reserve(tup.t.size());
    for (auto &t : tup.t) {
      argTys.push_back(getTy(cc, t));
    }
    return llvm::FunctionType::get(getTy(cc, v.ret), argTys, false);
  }

  llvm::DILocation *locFromSpan(CC &cc, LocalCC &lcc, const loc::SrcLoc &loc) {
    return llvm::DILocation::get(cc.ctx, loc.line, loc.col, lcc.scopes.back());
  }

  void emitInsn(Insn &insn, CC &cc, LocalCC &lcc) {
    if (insn.span) {
      auto &loc = insn.span->lo;
      lcc.ib.SetCurrentDebugLocation(locFromSpan(cc, lcc, loc));
    }
    lcc.vals[&insn] = std::visit(overloaded {
        [&](Insn::DeclareParam &i) -> llvm::Value * {
          Idx index = lcc.paramC++;
          llvm::Argument *arg = lcc.func->getArg(index);
          if (insn.span) {
            llvm::AllocaInst *alloca = lcc.ib.CreateAlloca(arg->getType());
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
            lcc.ib.CreateStore(arg, alloca);
            return alloca;
          }
          return arg;
        },
        [&](Insn::DeclareVar &i) -> llvm::Value * {
          llvm::AllocaInst *alloca = lcc.ib.CreateAlloca(getTy(lcc, insn.ty));
          if (insn.span) {
            cc.db.insertDeclare(
                alloca,
                cc.db.createAutoVariable(
                    lcc.scopes.back(), i.name, cc.cu->getFile(),
                    insn.span->lo.line, getTy<llvm::DIType *>(lcc, insn.ty)
                ),
                cc.db.createExpression(),
                locFromSpan(cc, lcc, insn.span->lo),
                lcc.ib.GetInsertBlock()
            );
          }
          return alloca;
        },
        [&](Insn::HeapAlloc &i) -> llvm::Value * {
          auto i32Ty = llvm::IntegerType::getInt32Ty(cc.ctx);
          auto i8PtrTy = llvm::IntegerType::getInt8PtrTy(cc.ctx);
          auto ty = getTy(lcc, insn.ty);
          auto gcAlloc = cc.mod.getOrInsertFunction("gcAlloc", i8PtrTy, i32Ty);
          auto structTy = adtTy(cc, std::get<type::Ty::ADT>(lcc.inst.types.at(insn.ty)->v));
          auto rawSize = llvm::ConstantExpr::getSizeOf(structTy);
          auto size = llvm::ConstantExpr::getTruncOrBitCast(rawSize, i32Ty);
          auto call = lcc.ib.CreateCall(gcAlloc, size);
          return lcc.ib.CreatePointerCast(call, ty);
        },
        [&](Insn::SetVar &i) -> llvm::Value * {
          lcc.ib.CreateStore(lcc.vals.at(i.value), lcc.vals.at(i.var));
          return nullptr; // ignored
        },
        [&](Insn::GetVar &i) -> llvm::Value * {
          llvm::Value *value = lcc.vals.at(i.var);
          if (llvm::isa<llvm::Argument>(value)) {
            return value;
          } else {
            return lcc.ib.CreateLoad(getTy(lcc, insn.ty), value);
          }
        },
        [&](Insn::SetField &i) -> llvm::Value * {
          auto i32Ty = llvm::Type::getInt32Ty(cc.ctx);
          auto structTy = adtTy(cc, std::get<type::Ty::ADT>(lcc.inst.types.at(i.obj->ty)->v));
          auto castPtr = lcc.ib.CreatePointerCast(lcc.vals.at(i.obj), structTy->getPointerTo());
          auto gep = lcc.ib.CreateInBoundsGEP(castPtr, {
              llvm::ConstantInt::get(i32Ty, 0),
              llvm::ConstantInt::get(i32Ty, i.field)
            });
          lcc.ib.CreateStore(lcc.vals.at(i.value), gep);
          return nullptr;
        },
        [&](Insn::GetField &i) -> llvm::Value * {
          auto i32Ty = llvm::Type::getInt32Ty(cc.ctx);
          auto structTy = adtTy(cc, std::get<type::Ty::ADT>(lcc.inst.types.at(i.obj->ty)->v));
          auto castPtr = lcc.ib.CreatePointerCast(lcc.vals.at(i.obj), structTy->getPointerTo());
          auto gep = lcc.ib.CreateInBoundsGEP(castPtr, {
              llvm::ConstantInt::get(i32Ty, 0),
              llvm::ConstantInt::get(i32Ty, i.field)
            });
          return lcc.ib.CreateLoad(getTy(lcc, insn.ty), gep);
        },
        [&](Insn::CallTrait &i) -> llvm::Value * {
          TraitImpl::For tFor {
            .ty = lcc.inst.types.at(i.obj->ty),
            .tb = lcc.inst.traits.at(i.trait),
          };
          if (std::holds_alternative<Ty::FfiFn>(tFor.ty->v)) {
            // hack for FFI
            std::vector<llvm::Value *> args;
            args.reserve(i.args.size());
            for (auto &arg : i.args) {
              args.push_back(lcc.vals.at(arg));
            }
            llvm::FunctionType *fnTy = ffiFnTy(cc, std::get<Ty::FfiFn>(tFor.ty->v));
            return lcc.ib.CreateCall(fnTy, lcc.vals.at(i.obj), args);
          } else {
            return cc.emitCall.at(tFor)(insn, i, cc, lcc);
          }
        },
        [&](Insn::PhiNode &i) -> llvm::Value * {
          auto phi = lcc.ib.CreatePHI(getTy(lcc, insn.ty), i.branches.size());
          for (auto &ic : i.branches) {
            phi->addIncoming(lcc.vals.at(ic.ref), lcc.bbs.at(ic.block));
          }
          return phi;
        },
        [&](Insn::NewTuple &i) -> llvm::Value * {
          {
            for (auto &v : i.values) {
              if (!llvm::isa<llvm::Constant>(lcc.vals.at(v))) {
                goto notconst;
              }
            }
            std::vector<llvm::Constant *> vs;
            vs.reserve(i.values.size());
            for (auto &v : i.values) {
              vs.push_back(llvm::cast<llvm::Constant>(lcc.vals.at(v)));
            }
            auto ty = llvm::cast<llvm::StructType>(getTy(lcc, insn.ty));
            return llvm::ConstantStruct::get(ty, vs);
          } notconst: {
            auto ty = getTy(lcc, insn.ty);
            auto alloca = lcc.ib.CreateAlloca(ty);
            Idx idx = 0;
            auto i32Ty = llvm::Type::getInt32Ty(cc.ctx);
            for (auto &v : i.values) {
              auto gep = lcc.ib.CreateInBoundsGEP(alloca, {
                  llvm::ConstantInt::get(i32Ty, 0),
                  llvm::ConstantInt::get(i32Ty, idx++),
                });
              lcc.ib.CreateStore(lcc.vals.at(v), gep);
            }
            return lcc.ib.CreateLoad(ty, alloca);
          }
        },
        [&](Insn::ForeignRef &i) -> llvm::Value * {
          auto crpTy = lcc.inst.types.at(insn.ty);
          if (std::holds_alternative<Ty::FfiFn>(crpTy->v)) {
            auto fnTy = ffiFnTy(cc, std::get<Ty::FfiFn>(crpTy->v));
            auto modFn = cc.mod.getOrInsertFunction(i.symbol, fnTy);
            return modFn.getCallee();
          } else {
            auto ty = getTy(cc, crpTy);
            auto global = cc.mod.getOrInsertGlobal(i.symbol, ty);
            return lcc.ib.CreateLoad(ty, global);
          }
        },
        [&](Insn::LiteralString &i) -> llvm::Value * {
          size_t len = i.value.size();
          auto charTy = llvm::Type::getInt8Ty(cc.ctx);
          llvm::ArrayType *charArrayTy = llvm::ArrayType::get(charTy, len);
          std::vector<llvm::Constant *> constantChars;
          constantChars.reserve(len);
          for (auto c : i.value) {
            constantChars.push_back(llvm::ConstantInt::get(cc.ctx, llvm::APInt(8, c)));
          }
          llvm::Constant *charsConstant = llvm::ConstantArray::get(charArrayTy, constantChars);
          auto globalVar = new llvm::GlobalVariable(cc.mod,
                                                    charArrayTy,
                                                    true,
                                                    llvm::GlobalValue::PrivateLinkage,
                                                    charsConstant);
          llvm::Constant *lenConst = llvm::ConstantInt::get(cc.ctx, llvm::APInt(64, len));
          auto const0 = llvm::ConstantInt::get(cc.ctx, llvm::APInt(32, 0));
          llvm::ArrayRef<llvm::Constant *> idces = {const0, const0};
          llvm::Constant *charPtrConst = llvm::ConstantExpr::
            getInBoundsGetElementPtr(charArrayTy, globalVar, idces);
          auto stringStructTy = llvm::cast<llvm::StructType>(getTy(lcc, insn.ty));
          return llvm::ConstantStruct::get(stringStructTy, {lenConst, charPtrConst});
        },
        [&](Insn::LiteralInt &i) -> llvm::Value * {
          return llvm::ConstantInt::get(getTy(lcc, insn.ty), i.value);
        },
        [&](Insn::LiteralFloat &i) -> llvm::Value * {
          return llvm::ConstantFP::get(getTy(lcc, insn.ty), i.value);
        },
        [&](Insn::LiteralBool &i) -> llvm::Value * {
          return llvm::ConstantInt::get(getTy(lcc, insn.ty), i.value);
        },
        [&](Insn::BlockStart &i) -> llvm::Value * {
          loc::SrcLoc &loc = insn.span->lo;
          llvm::DILexicalBlock *block = cc.db.createLexicalBlock(
              lcc.scopes.back(),
              cc.cu->getFile(),
              loc.line, loc.col
          );
          lcc.scopes.push_back(block);
          return nullptr;
        },
        [&](Insn::BlockEnd &i) -> llvm::Value * {
          lcc.scopes.pop_back();
          return nullptr;
        },
    }, insn.v);
    lcc.ib.SetCurrentDebugLocation(nullptr);
  }

  void emitJump(Jump &j, CC &cc, LocalCC &lcc) {
    std::visit(overloaded {
        [&](Jump::Ret &r) {
          lcc.ib.CreateRet(lcc.vals.at(r.value));
        },
        [&](Jump::Br &r) {
          lcc.ib.CreateBr(lcc.bbs.at(r.target));
        },
        [&](Jump::CondBr &r) {
          lcc.ib.CreateCondBr(lcc.vals.at(r.pred),
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
    for (auto &bb : bl.blocks) {
      lcc.bbs[bb.get()] = llvm::BasicBlock::Create(cc.ctx, "", lcc.func);
    }
    for (auto &bb : bl.blocks) {
      lcc.ib.SetInsertPoint(lcc.bbs.at(bb.get()));
      emitBB(*bb, cc, lcc);
    }
  }

  llvm::Function *getBbFunc(const BlockList &bl, const Instantiation &inst, CC &cc) {
    std::vector<llvm::Type *> argTys;
    std::vector<llvm::Metadata *> diTys;
    const std::string *name;
    const std::optional<loc::Span> *span;
    Idx retIdx = std::get<Jump::Ret>(bl.blocks.back()->end.v).value->ty;
    auto &retPair = getTyPair(cc, inst.types.at(retIdx));
    diTys.push_back(std::get<1>(retPair));
    for (auto &insn : bl.blocks.front()->insns) {
      if (std::holds_alternative<Insn::DeclareParam>(insn->v)) {
        auto argPair = getTyPair(cc, inst.types.at(insn->ty));
        argTys.push_back(std::get<0>(argPair));
        diTys.push_back(std::get<1>(argPair));
        name = &std::get<Insn::DeclareParam>(insn->v).name;
        span = &insn->span;
      }
    }
    llvm::FunctionType *funcTy = llvm::FunctionType::get(std::get<0>(retPair), argTys, false);
    llvm::Function *func = llvm::Function::Create(funcTy, llvm::GlobalValue::PrivateLinkage, *name, cc.mod);
    uint32_t lineNo = *span ? (**span).lo.line : 0;
    func->setSubprogram(cc.db.createFunction(
        cc.cu->getFile(), *name, func->getName(), cc.cu->getFile(),
        lineNo, cc.db.createSubroutineType(cc.db.getOrCreateTypeArray(diTys)),
        lineNo, llvm::DINode::FlagPrivate, llvm::DISubprogram::SPFlagDefinition
    ));
    return func;
  }

  template <typename T, std::size_t... Idx, typename C, typename... Args>
  auto implTraitFn(T C::*fn,
                   std::index_sequence<Idx...>,
                   Args&&... args) {
    return [=](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc) -> llvm::Value * {
      return (lcc.ib.*fn)(lcc.vals.at(ct.obj), lcc.vals.at(ct.args.at(Idx))..., args...);
    };
  }

  void implTrait(CC &cc,
                 Tp ty,
                 Idx trait,
                 std::vector<Tp> &&params,
                 EmitFn fn) {
    auto tb = cc.tbcx.intern(type::TraitBound{trait, std::move(params)});
    cc.emitCall[{ty, tb}] = fn;
  }

  template <std::size_t Arity, typename T, typename C, typename... Args>
  void implTrait(CC &cc,
                 Tp ty,
                 Idx trait,
                 std::vector<Tp> &&params,
                 T C::*fn,
                 Args&&... args) {
    implTrait(cc, ty, trait, std::forward<std::vector<Tp>>(params),
              implTraitFn(fn, std::make_index_sequence<Arity>{},
                          std::forward<Args>(args)...));
  }

  void addIntrinsics(CC &cc) {
    for (type::IntSize is = type::IntSize::i8;
         is <= type::IntSize::i128;
         ++(std::underlying_type<type::IntSize>::type&)is) {
      Tp i = cc.tcx.intern(Ty::Int{is});
      Tp u = cc.tcx.intern(Ty::UInt{is});
      for (Tp ty : {i, u}) {
        implTrait<1>(cc, ty, hir::Builtins::Add, {ty}, &llvm::IRBuilder<>::CreateAdd,
                     "", false, false);
        implTrait<1>(cc, ty, hir::Builtins::Sub, {ty}, &llvm::IRBuilder<>::CreateSub,
                     "", false, false);
        implTrait<1>(cc, ty, hir::Builtins::Mul, {ty}, &llvm::IRBuilder<>::CreateMul,
                     "", false, false);
        bool isU = std::holds_alternative<Ty::UInt>(ty->v);
        implTrait<1>(cc, ty, hir::Builtins::Div, {ty},
                     isU ? &llvm::IRBuilder<>::CreateUDiv : &llvm::IRBuilder<>::CreateSDiv,
                     "", false);
        implTrait<1>(cc, ty, hir::Builtins::Rem, {ty},
                     isU ? &llvm::IRBuilder<>::CreateURem : &llvm::IRBuilder<>::CreateSRem,
                     "");

        implTrait(cc, ty, hir::Builtins::Cmp, {ty},
                  [isU](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc)
                  -> llvm::Value * {
                    std::array<llvm::ICmpInst::Predicate, 6> cis{
                      llvm::ICmpInst::ICMP_NE,
                      llvm::ICmpInst::ICMP_EQ,
                      isU ? llvm::ICmpInst::ICMP_ULT : llvm::ICmpInst::ICMP_SLT,
                      isU ? llvm::ICmpInst::ICMP_ULE : llvm::ICmpInst::ICMP_SLE,
                      isU ? llvm::ICmpInst::ICMP_UGT : llvm::ICmpInst::ICMP_SGT,
                      isU ? llvm::ICmpInst::ICMP_UGE : llvm::ICmpInst::ICMP_SGE,
                    };
                    return lcc.ib.CreateICmp(cis[ct.method],
                                             lcc.vals.at(ct.obj),
                                             lcc.vals.at(ct.args.at(0)));
                  });
      }
      implTrait<0>(cc, i, hir::Builtins::Neg, {}, &llvm::IRBuilder<>::CreateNeg, "", false, false);
    }

    // TODO floats
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
        db.createFile(fileName, fileDir),
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
            args.push_back(lcc.vals.at(i));
          }
          args.push_back(lcc.vals.at(ct.obj));
          return lcc.ib.CreateCall(func->getFunctionType(), func, args);
        };
      }
    }

    llvm::IRBuilder<> ib(cc.ctx);

    auto unitTyPair = getTyPair(cc, tcx.intern(Ty::Tuple{{}}));
    auto unitThunkTy = llvm::FunctionType::get(std::get<0>(unitTyPair), false);
    auto crpMainFunc = llvm::Function::Create(
        unitThunkTy, llvm::GlobalValue::PrivateLinkage, "crpMain", cc.mod);
    {
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
    auto mainTy = llvm::FunctionType::get(i32Ty, false);
    auto mainFunc = llvm::Function::Create(mainTy, llvm::GlobalValue::ExternalLinkage, "main", cc.mod);
    {
      llvm::BasicBlock *entry = llvm::BasicBlock::Create(cc.ctx, "entry", mainFunc);
      ib.SetInsertPoint(entry);
      ib.CreateCall(unitThunkTy, crpMainFunc);
      auto gcShutdown = cc.mod.getOrInsertFunction("gcShutdown", llvm::Type::getVoidTy(cc.ctx));
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

    db.finalize();

    CodegenResult res;
    res.ctx = std::move(llvmCtx);
    res.mod = std::move(llvmMod);
    return res;
  }
}
