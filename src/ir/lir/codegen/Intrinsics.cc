#include "Intrinsics.h"

#include "../../hir/Builtins.h"

namespace lir::codegen {
  template <typename T, std::size_t... Idx, typename C, typename... Args>
  auto implTraitFn(T C::*fn,
                   std::index_sequence<Idx...>,
                   Args&&... args) {
    return [=](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc) -> llvm::Value * {
      return (lcc.ib.*fn)(lcc.load(ct.obj), lcc.load(ct.args.at(Idx))..., args...);
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
    using BinaryAndTwine = llvm::Value *(llvm::IRBuilder<>::*)(llvm::Value*, llvm::Value*, const llvm::Twine&);
    for (type::IntSize is : type::INT_SIZE_VALUES) {
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

        implTrait(cc, ty, hir::Builtins::Eq, {ty},
                  [](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc)
                      -> llvm::Value * {
                    std::array<llvm::ICmpInst::Predicate, 2> cis{
                        llvm::ICmpInst::ICMP_NE,
                        llvm::ICmpInst::ICMP_EQ,
                    };
                    return lcc.ib.CreateICmp(cis[ct.method],
                                             lcc.load(ct.obj),
                                             lcc.load(ct.args.at(0)));
                  });
        implTrait(cc, ty, hir::Builtins::Cmp, {ty},
                  [isU](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc)
                      -> llvm::Value * {
                    std::array<llvm::ICmpInst::Predicate, 4> cis{
                        isU ? llvm::ICmpInst::ICMP_ULT : llvm::ICmpInst::ICMP_SLT,
                        isU ? llvm::ICmpInst::ICMP_ULE : llvm::ICmpInst::ICMP_SLE,
                        isU ? llvm::ICmpInst::ICMP_UGT : llvm::ICmpInst::ICMP_SGT,
                        isU ? llvm::ICmpInst::ICMP_UGE : llvm::ICmpInst::ICMP_SGE,
                    };
                    return lcc.ib.CreateICmp(cis[ct.method],
                                             lcc.load(ct.obj),
                                             lcc.load(ct.args.at(0)));
                  });
        
        implTrait<1>(cc, ty, hir::Builtins::BitOr, {ty},
                     (BinaryAndTwine)&llvm::IRBuilder<>::CreateOr, "");
        implTrait<1>(cc, ty, hir::Builtins::BitAnd, {ty},
                     (BinaryAndTwine)&llvm::IRBuilder<>::CreateAnd, "");
      }
      implTrait<0>(cc, i, hir::Builtins::Neg, {}, &llvm::IRBuilder<>::CreateNeg, "", false, false);
    }
    Ty *boolTy = cc.tcx.intern(Ty::Bool{});
    implTrait<1>(cc, boolTy, hir::Builtins::BitOr, {boolTy},
                 (BinaryAndTwine)&llvm::IRBuilder<>::CreateOr, "");
    implTrait<1>(cc, boolTy, hir::Builtins::BitAnd, {boolTy},
                 (BinaryAndTwine)&llvm::IRBuilder<>::CreateAnd, "");

    for (type::FloatSize fs : type::FLOAT_SIZE_VALUES) {
      Tp ty = cc.tcx.intern(Ty::Float{fs});
      implTrait<1>(cc, ty, hir::Builtins::Add, {ty}, &llvm::IRBuilder<>::CreateFAdd,"", nullptr);
      implTrait<1>(cc, ty, hir::Builtins::Sub, {ty}, &llvm::IRBuilder<>::CreateFSub,"", nullptr);
      implTrait<1>(cc, ty, hir::Builtins::Mul, {ty}, &llvm::IRBuilder<>::CreateFMul,"", nullptr);
      implTrait<1>(cc, ty, hir::Builtins::Div, {ty}, &llvm::IRBuilder<>::CreateFDiv,"", nullptr);
      implTrait<1>(cc, ty, hir::Builtins::Rem, {ty}, &llvm::IRBuilder<>::CreateFRem,"", nullptr);
      implTrait<0>(cc, ty, hir::Builtins::Neg, {}, &llvm::IRBuilder<>::CreateFNeg, "", nullptr);
      // NaN can and will ruin your life
      implTrait(cc, ty, hir::Builtins::Eq, {ty},
                [](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc)
                    -> llvm::Value * {
                  std::array<llvm::FCmpInst::Predicate, 2> cis{
                      llvm::FCmpInst::FCMP_ONE,
                      llvm::FCmpInst::FCMP_OEQ,
                  };
                  return lcc.ib.CreateFCmp(cis[ct.method],
                                           lcc.load(ct.obj),
                                           lcc.load(ct.args.at(0)));
                });
      implTrait(cc, ty, hir::Builtins::Cmp, {ty},
                [](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc)
                    -> llvm::Value * {
                  std::array<llvm::FCmpInst::Predicate, 4> cis{
                      llvm::FCmpInst::FCMP_OLT,
                      llvm::FCmpInst::FCMP_OLE,
                      llvm::FCmpInst::FCMP_OGT,
                      llvm::FCmpInst::FCMP_OGE,
                  };
                  return lcc.ib.CreateFCmp(cis[ct.method],
                                           lcc.load(ct.obj),
                                           lcc.load(ct.args.at(0)));
                });
    }
  }
}

