#include "Codegen.h"

#include "../hir/Builtins.h"

#include <array>
#include <functional>
#include <llvm-10/llvm/IR/Constant.h>
#include <llvm-10/llvm/IR/Constants.h>
#include <llvm-10/llvm/IR/Instructions.h>
#include <llvm-10/llvm/IR/Type.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <utility>
#include <variant>
#include <vector>

namespace lir::codegen {
  using type::Ty;
  using Tp = Ty *;

  const std::string GC_METHOD = "shadow-stack";

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

  llvm::Type *getTy(CC &cc, type::Ty *ty) {
    auto found = cc.tyCache.find(ty);
    if (found != cc.tyCache.end()) {
      if (found->second) {
        return found->second;
      } else {
        throw "Recursive flat type has infinite size";
      }
    }
    return nullptr; // TODO
  }

  llvm::Type *getTy(LocalCC &lcc, Idx i) {
    return getTy(lcc.cc, lcc.inst.types.at(i));
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
          auto rawSize = llvm::ConstantExpr::getSizeOf(ty->getPointerElementType());
          auto size = llvm::ConstantExpr::getTruncOrBitCast(rawSize, i32Ty);
          auto call = lcc.ib.CreateCall(gcAlloc, size);
          return lcc.ib.CreatePointerCast(call, ty);
        },
        [&](Insn::SetVar &i) -> llvm::Value * {
          lcc.ib.CreateStore(lcc.vals.at(i.value), lcc.vals.at(i.var));
          return nullptr; // ignored
        },
        [&](Insn::GetVar &i) -> llvm::Value * {
          return lcc.ib.CreateLoad(lcc.vals.at(i.var));
        },
        [&](Insn::SetField &i) -> llvm::Value * {
          auto i32Ty = llvm::Type::getInt32Ty(cc.ctx);
          auto gep = lcc.ib.CreateInBoundsGEP(lcc.vals.at(i.obj), {
              llvm::ConstantInt::get(i32Ty, 0),
              llvm::ConstantInt::get(i32Ty, i.field)
            });
          lcc.ib.CreateStore(lcc.vals.at(i.value), gep);
          return nullptr;
        },
        [&](Insn::GetField &i) -> llvm::Value * {
          auto i32Ty = llvm::Type::getInt32Ty(cc.ctx);
          auto gep = lcc.ib.CreateInBoundsGEP(lcc.vals.at(i.obj), {
              llvm::ConstantInt::get(i32Ty, 0),
              llvm::ConstantInt::get(i32Ty, i.field)
            });
          return lcc.ib.CreateLoad(gep);
        },
        [&](Insn::CallTrait &i) -> llvm::Value * {
          TraitImpl::For tFor {
            .ty = lcc.inst.types.at(i.obj->ty),
            .tb = lcc.inst.traits.at(i.trait),
          };
          return cc.emitCall.at(tFor)(insn, i, cc, lcc);
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
            auto alloca = lcc.ib.CreateAlloca(getTy(lcc, insn.ty));
            Idx idx = 0;
            auto i32Ty = llvm::Type::getInt32Ty(cc.ctx);
            for (auto &v : i.values) {
              auto gep = lcc.ib.CreateInBoundsGEP(alloca, {
                  llvm::ConstantInt::get(i32Ty, 0),
                  llvm::ConstantInt::get(i32Ty, idx++),
                });
              lcc.ib.CreateStore(lcc.vals.at(v), gep);
            }
            return lcc.ib.CreateLoad(alloca);
          }
        },
        [&](Insn::ForeignRef &i) -> llvm::Value * {
          return cc.mod.getOrInsertGlobal(i.symbol, getTy(lcc, insn.ty));
        },
        [&](Insn::LiteralString &i) -> llvm::Value * {
          return nullptr; // TODO
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
    for (auto &bb : bl.blocks) {
      lcc.ib.SetInsertPoint(lcc.bbs.at(bb.get()));
      emitBB(*bb, cc, lcc);
    }
  }

  template <typename T, std::size_t... Idx, typename... Args>
  auto implTraitFn(T llvm::IRBuilder<>::*fn,
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

  template <std::size_t Arity, typename T, typename... Args>
  void implTrait(CC &cc,
                 Tp ty,
                 Idx trait,
                 std::vector<Tp> &&params,
                 T llvm::IRBuilder<>::*fn,
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

  CodegenResult generate(arena::InternArena<type::Ty> &tcx,
                         arena::InternArena<type::TraitBound> &tbcx,
                         const Module &mod,
                         llvm::LLVMContext &ctx) {
    auto llvmMod = std::make_unique<llvm::Module>("module", ctx);
    CC cc { tcx, tbcx, *llvmMod, ctx };

    addIntrinsics(cc);

    auto voidTy = llvm::Type::getVoidTy(cc.ctx);
    auto voidThunkTy = llvm::FunctionType::get(voidTy, false);

    auto i32Ty = llvm::Type::getInt32Ty(cc.ctx);
    auto mainTy = llvm::FunctionType::get(i32Ty, false);

    CodegenResult res;
    res.mod = std::move(llvmMod);
    return res;
  }
}
