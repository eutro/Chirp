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

  struct CC {
    arena::InternArena<Ty> &tcx;
    arena::InternArena<type::TraitBound> &tbcx;
    llvm::Module &mod;
    llvm::LLVMContext &ctx;

    std::map<TraitImpl::For, EmitFn> emitCall;
    std::map<type::Ty *, llvm::Type *> tyCache;
  };

  struct LocalCC {
    CC &cc;

    llvm::IRBuilder<> &ib;
    llvm::Function *func;
    lir::Instantiation &inst;

    std::map<Insn *, llvm::Value *> vals;
    std::map<BasicBlock *, llvm::BasicBlock *> bbs;
    Idx paramC = 0;
  };

  llvm::Type *getTy(CC &cc, type::Ty *ty);

  llvm::FunctionType *ffiFnTy(CC &cc, type::Ty::FfiFn &v) {
    auto &tup = std::get<Ty::Tuple>(v.args->v);
    std::vector<llvm::Type *> argTys;
    argTys.reserve(tup.t.size());
    for (auto &t : tup.t) {
      argTys.push_back(getTy(cc, t));
    }
    return llvm::FunctionType::get(getTy(cc, v.ret), argTys, false);
  }

  llvm::Type *getTy(CC &cc, type::Ty *ty) {
    auto found = cc.tyCache.find(ty);
    if (found != cc.tyCache.end()) {
      if (found->second) {
        return found->second;
      } else {
        throw std::runtime_error("Recursive flat type has infinite size");
      }
    }
    if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
      Ty *uncycled = type::uncycle(cc.tcx, cc.tbcx, ty);
      llvm::Type *llvmTy = getTy(cc, uncycled);
      cc.tyCache[ty] = llvmTy;
      return llvmTy;
    } else {
      cc.tyCache[ty] = nullptr;
    }
    llvm::Type *llvmTy = std::visit(overloaded {
        [&](Ty::Bool &v) -> llvm::Type * { return llvm::Type::getInt1Ty(cc.ctx); },
        [&](Ty::Int &v) -> llvm::Type * { return llvm::Type::getIntNTy(cc.ctx, type::bitCount(v.s)); },
        [&](Ty::UInt &v) -> llvm::Type * { return llvm::Type::getIntNTy(cc.ctx, type::bitCount(v.s)); },
        [&](Ty::Float &v) -> llvm::Type * {
          switch (v.s) {
            case type::FloatSize::f16: return llvm::Type::getHalfTy(cc.ctx);
            case type::FloatSize::f32: return llvm::Type::getFloatTy(cc.ctx);
            case type::FloatSize::f64: return llvm::Type::getDoubleTy(cc.ctx);
            default: throw 0;
          }
        },
        [&](Ty::ADT &v) -> llvm::Type * {
          auto sType = llvm::StructType::get(cc.ctx);
          return llvm::PointerType::getUnqual(sType); // opaque
        },
        [&](Ty::Tuple &v) -> llvm::Type * {
          std::vector<llvm::Type *> fieldTys;
          fieldTys.reserve(v.t.size());
          for (Tp fieldTy : v.t) {
            fieldTys.push_back(getTy(cc, fieldTy));
          }
          return llvm::StructType::get(cc.ctx, fieldTys);
        },
        [&](Ty::String &v) -> llvm::Type * {
          return llvm::StructType::get(cc.ctx, {
            llvm::Type::getInt64Ty(cc.ctx), // len
            llvm::Type::getInt8PtrTy(cc.ctx), // bytes_utf8
          });
        },
        [&](Ty::FfiFn &v) -> llvm::Type * {
          return ffiFnTy(cc, v)->getPointerTo();
        },
        [](auto&) -> llvm::Type * {
          throw std::runtime_error("Type cannot exist after inference");
        }
    }, ty->v);
    cc.tyCache[ty] = llvmTy;
    return llvmTy;
  }

  llvm::Type *getTy(LocalCC &lcc, Idx i) {
    return getTy(lcc.cc, lcc.inst.types.at(i));
  }

  llvm::Type *adtTy(CC &cc, type::Ty::ADT &v) {
    std::vector<llvm::Type *> fieldTys;
    fieldTys.reserve(v.s.size());
    for (Tp fieldTy : v.s) {
      fieldTys.push_back(getTy(cc, fieldTy));
    }
    return llvm::StructType::get(cc.ctx, fieldTys);
  }

  void emitInsn(Insn &insn, CC &cc, LocalCC &lcc) {
    lcc.vals[&insn] = std::visit(overloaded {
        [&](Insn::DeclareParam &i) -> llvm::Value * {
          return lcc.func->getArg(lcc.paramC++);
        },
        [&](Insn::DeclareVar &i) -> llvm::Value * {
          return lcc.ib.CreateAlloca(getTy(lcc, insn.ty));
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
          if (std::holds_alternative<Insn::DeclareParam>(i.var->v)) {
            return lcc.vals.at(i.var);
          } else {
            return lcc.ib.CreateLoad(getTy(lcc, insn.ty), lcc.vals.at(i.var));
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
          llvm::ArrayType *charArrayTy = llvm::ArrayType::get(llvm::Type::getInt8Ty(cc.ctx), len);
          std::vector<llvm::Constant *> constantChars;
          constantChars.reserve(len);
          for (auto c : i.value) {
            constantChars.push_back(llvm::ConstantInt::get(cc.ctx, llvm::APInt(8, c)));
          }
          llvm::Constant *charsConstant = llvm::ConstantArray::get(charArrayTy, constantChars);
          return llvm::ConstantStruct::get(llvm::cast<llvm::StructType>(getTy(lcc, insn.ty)), {
              llvm::ConstantInt::get(cc.ctx, llvm::APInt(64, len)),
              new llvm::GlobalVariable(cc.mod, charArrayTy, true, llvm::GlobalValue::PrivateLinkage, charsConstant),
          });
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
        [&](Insn::BlockStart &i) -> llvm::Value * {return nullptr;},
        [&](Insn::BlockEnd &i) -> llvm::Value * {return nullptr;},
    }, insn.v);
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
    llvm::IRBuilder<> ib(cc.ctx);
    for (auto &bb : bl.blocks) {
      lcc.ib.SetInsertPoint(lcc.bbs.at(bb.get()));
      emitBB(*bb, cc, lcc);
    }
  }
  
  llvm::FunctionType *getBbType(const BlockList &bl, const Instantiation &inst, CC &cc) {
    std::vector<llvm::Type *> argTys;
    for (auto &insn : bl.blocks.front()->insns) {
      if (std::holds_alternative<Insn::DeclareParam>(insn->v)) {
        argTys.push_back(getTy(cc, inst.types.at(insn->ty)));
      }
    }
    Idx retIdx = std::get<Jump::Ret>(bl.blocks.back()->end.v).value->ty;
    llvm::Type *retTy = getTy(cc, inst.types.at(retIdx));
    return llvm::FunctionType::get(retTy, argTys, false);
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
                         Module &mod) {
    auto llvmCtx = std::make_unique<llvm::LLVMContext>();
    auto llvmMod = std::make_unique<llvm::Module>("module", *llvmCtx);
    CC cc { tcx, tbcx, *llvmMod, *llvmCtx };

    addIntrinsics(cc);

    std::vector<std::unique_ptr<std::vector<llvm::Function *>>> traitMethods;

    for (auto &trait : mod.traitImpls) {
      for (auto &entry : trait.instantiations) {
        std::vector<llvm::Function *> &funcs = *traitMethods.emplace_back(std::make_unique<std::vector<llvm::Function*>>());
        funcs.reserve(trait.methods.size());
        for (const BlockList &bl : trait.methods) {
          llvm::FunctionType *funcTy = getBbType(bl, entry.second, cc);
          funcs.push_back(llvm::Function::Create(funcTy, llvm::GlobalValue::PrivateLinkage, "", cc.mod));
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

    auto unitTy = getTy(cc, tcx.intern(Ty::Tuple{{}}));
    auto unitThunkTy = llvm::FunctionType::get(unitTy, false);
    auto crpMainFunc = llvm::Function::Create(
        unitThunkTy, llvm::GlobalValue::PrivateLinkage, "crpMain", cc.mod);
    {
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

    CodegenResult res;
    res.ctx = std::move(llvmCtx);
    res.mod = std::move(llvmMod);
    return res;
  }
}
