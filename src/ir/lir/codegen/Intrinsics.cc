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

  void implTrait(CC &cc, EmitFn fn) {
    auto &m = cc.emitCall[cc.emitCall.size()];
    m[m.size()] = fn;
  }

  template <std::size_t Arity, typename T, typename C, typename... Args>
  void implTrait(CC &cc,
                 T C::*fn,
                 Args&&... args) {
    implTrait(
      cc,
      implTraitFn(fn, std::make_index_sequence<Arity>{},
                  std::forward<Args>(args)...));
  }

  void addIntrinsics(CC &cc, type::infer::Inst::Set &sys) {
    using BinaryAndTwine = llvm::Value *(llvm::IRBuilder<>::*)(llvm::Value*, llvm::Value*, const llvm::Twine&);
    for ([[maybe_unused]] type::IntSize is : type::INT_SIZE_FIXED) {
      for (bool isU : {false, true}) {
        implTrait<1>(cc, /*Add*/ &llvm::IRBuilder<>::CreateAdd, "add", false, false);
        implTrait<1>(cc, /*Sub*/ &llvm::IRBuilder<>::CreateSub, "sub", false, false);
        implTrait<1>(cc, /*Mul*/ &llvm::IRBuilder<>::CreateMul, "mul", false, false);
        implTrait<1>(
          cc, /*Div*/
          isU ?
          &llvm::IRBuilder<>::CreateUDiv :
          &llvm::IRBuilder<>::CreateSDiv,
          "div", false
        );
        implTrait<1>(
          cc, /*Rem*/
          isU ?
          &llvm::IRBuilder<>::CreateURem :
          &llvm::IRBuilder<>::CreateSRem,
          "rem"
        );
        implTrait<1>(cc, /*BitOr*/ (BinaryAndTwine)&llvm::IRBuilder<>::CreateOr, "or");
        implTrait<1>(cc, /*BitAnd*/ (BinaryAndTwine)&llvm::IRBuilder<>::CreateAnd, "and");

        implTrait(
          cc, /*Eq*/
          [](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc)
          -> llvm::Value * {
            std::array<llvm::ICmpInst::Predicate, 2> cis{
              llvm::ICmpInst::ICMP_NE,
              llvm::ICmpInst::ICMP_EQ,
            };
            return lcc.ib.CreateICmp(
              cis[ct.method],
              lcc.load(ct.obj),
              lcc.load(ct.args.at(0)),
              "eq"
            );
          }
        );
        implTrait(
          cc, /*Cmp*/
          [isU](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc)
          -> llvm::Value * {
            std::array<llvm::ICmpInst::Predicate, 4> cis{
              isU ? llvm::ICmpInst::ICMP_ULT : llvm::ICmpInst::ICMP_SLT,
              isU ? llvm::ICmpInst::ICMP_ULE : llvm::ICmpInst::ICMP_SLE,
              isU ? llvm::ICmpInst::ICMP_UGT : llvm::ICmpInst::ICMP_SGT,
              isU ? llvm::ICmpInst::ICMP_UGE : llvm::ICmpInst::ICMP_SGE,
            };
            return lcc.ib.CreateICmp(
              cis[ct.method],
              lcc.load(ct.obj),
              lcc.load(ct.args.at(0)),
              "cmp"
            );
          }
        );
      }
      implTrait<0>(cc, /*Neg*/ &llvm::IRBuilder<>::CreateNeg, "neg", false, false);
    }
    implTrait<1>(cc, /*BitOr*/ (BinaryAndTwine)&llvm::IRBuilder<>::CreateOr, "or");
    implTrait<1>(cc, /*BitAnd*/ (BinaryAndTwine)&llvm::IRBuilder<>::CreateAnd, "and");

    for ([[maybe_unused]] type::FloatSize fs : type::FLOAT_SIZE_VALUES) {
      implTrait<1>(cc, /*Add*/ &llvm::IRBuilder<>::CreateFAdd, "add", nullptr);
      implTrait<1>(cc, /*Sub*/ &llvm::IRBuilder<>::CreateFSub, "sub", nullptr);
      implTrait<1>(cc, /*Mul*/ &llvm::IRBuilder<>::CreateFMul, "mul", nullptr);
      implTrait<1>(cc, /*Div*/ &llvm::IRBuilder<>::CreateFDiv, "div", nullptr);
      implTrait<1>(cc, /*Rem*/ &llvm::IRBuilder<>::CreateFRem, "rem", nullptr);
      implTrait<0>(cc, /*Neg*/ &llvm::IRBuilder<>::CreateFNeg, "neg", nullptr);
      // NaN can and will ruin your life
      implTrait(
        cc, /*Eq*/
        [](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc)
        -> llvm::Value * {
          std::array<llvm::FCmpInst::Predicate, 2> cis{
            llvm::FCmpInst::FCMP_ONE,
            llvm::FCmpInst::FCMP_OEQ,
          };
          return lcc.ib.CreateFCmp(
            cis[ct.method],
            lcc.load(ct.obj),
            lcc.load(ct.args.at(0)),
            "eq"
          );
        }
      );
      implTrait(
        cc, /*Cmp*/
        [](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc)
        -> llvm::Value * {
          std::array<llvm::FCmpInst::Predicate, 4> cis{
            llvm::FCmpInst::FCMP_OLT,
            llvm::FCmpInst::FCMP_OLE,
            llvm::FCmpInst::FCMP_OGT,
            llvm::FCmpInst::FCMP_OGE,
          };
          return lcc.ib.CreateFCmp(
            cis[ct.method],
            lcc.load(ct.obj),
            lcc.load(ct.args.at(0)),
            "cmp"
          );
        }
      );
    }

    auto &ffiInsts = sys.entities.at(1); // :)
    auto &ffiCalls = cc.emitCall[1];
    for ([[maybe_unused]] const auto &inst : ffiInsts) {
      ffiCalls.emplace(inst.first, [](Insn &insn, Insn::CallTrait &ct, CC &cc, LocalCC &lcc)
                       -> llvm::Value * {
        std::vector<llvm::Value *> args;
        args.reserve(ct.args.size());
        for (auto &arg : ct.args) {
          args.push_back(lcc.load(arg));
        }
        Tp objTy = lcc.inst.loggedTys.at(ct.obj->ty);
        llvm::FunctionType *fnTy = ffiFnTy(cc, std::get<Ty::FfiFn>(objTy->v));
        return lcc.ib.CreateCall(fnTy, lcc.load(ct.obj), args);
      });
    }
  }
}

