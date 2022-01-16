#include "Infer.h"

#include "Builtins.h"
#include "../../type/TypePrint.h"
#include "RecurVisitor.h"

#include <variant>

namespace hir::infer {
  using namespace type::infer;

#define RET_T VarRef
#define ARGS(TYPE) (TYPE &e, InsnList &ig) // NOLINT(bugprone-macro-parentheses)
#define PASS_ARGS , ig

  class InferenceVisitor :
    public ExprVisitor<VarRef, InsnList&>,
    public ProgramVisitor<InferResult> {
  public:
    type::Tcx &tcx;

    InferenceVisitor(type::TTcx &ttcx):
      tcx(ttcx.tcx)
    {}

    Program *program = nullptr;
    std::map<DefIdx, VarRef> varNodes;

    InferResult visitProgram(Program &p) override {
      InferResult res;
      visitRootBlock(p.topLevel, res.insnLists.emplace_back());
      for (auto &traitImpl : p.traitImpls) {
        InsnList &ig = res.insnLists.emplace_back();
        for (auto &block : traitImpl.methods) {
          visitRootBlock(block, ig);
        }
      }
      return res;
    }

    class DefinitionVisitor : public RecurVisitor<std::monostate, std::vector<Idx>> {
    public:
      void visitBlock(Block &it, std::vector<Idx> &vars) override {
        for (auto b : it.bindings) {
          vars.push_back(b);
        }
        RecurVisitor::visitBlock(it, vars);
      }
    };

    VarRef visitRootBlock(Block &block, InsnList &ig) {
      std::vector<DefIdx> vars;
      DefinitionVisitor().visitBlock(block, vars);
      for (Idx var : vars) {
        ig.insns.push_back(Insn(TrapInsn::key(), {}, {}, {"unvisited var"})); // to modify later
        varNodes.insert({var, ig.lastInsn()});
      }
      VarRef ret({}, 0);
      for (Eptr &expr : block.body) {
        ret = visitExpr(*expr, ig);
      }
      return ret;
    }

    VarRef visitExpr ARGS(Expr) override {
      // TODO type parsing
      return ExprVisitor::visitExpr(e PASS_ARGS);
    }
    RET_T visitBlockExpr ARGS(BlockExpr) override {
      VarRef ret({}, 0);
      for (Eptr &expr : e.block.body) {
        ret = visitExpr(*expr, ig);
      }
      return ret;
    }
    RET_T visitVarExpr ARGS(VarExpr) override {
      return varNodes.at(e.ref);
    }
    RET_T visitCondExpr ARGS(CondExpr) override {
      VarRef predTy = visitExpr(*e.predE PASS_ARGS);
      ig.insns.push_back(Insn(DeConstructInsn::key(), {}, {predTy}, {tcx.intern(Ty::Bool{})}));
      VarRef thenTy = visitExpr(*e.thenE PASS_ARGS);
      VarRef elseTy = visitExpr(*e.elseE PASS_ARGS);
      ig.insns.push_back(Insn(UnionInsn::key(), {}, {thenTy, elseTy}, {}));
      return ig.lastInsn();
    }
    RET_T visitVoidExpr ARGS(VoidExpr) override {
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, {}, {tcx.intern(Ty::Tuple{})}));
      return ig.lastInsn();
    }
    RET_T visitLiteralExpr ARGS(LiteralExpr) override {
      auto &hints = e.hints;
      Tp ty = ([&]() {
        switch (e.type) {
          case LiteralExpr::Type::Int:
            for (auto &h : hints) {
              if (h.base &&
                  *h.base >= Builtins::I8 &&
                  *h.base <= Builtins::I128) {
                return tcx.intern(Ty::Int{(type::IntSize)(*h.base - (Idx)Builtins::I8)});
              }
              if (h.base &&
                  *h.base >= Builtins::U8 &&
                  *h.base <= Builtins::U128) {
                return tcx.intern(Ty::UInt{(type::IntSize)(*h.base - (Idx)Builtins::U8)});
              }
            }
            return tcx.intern(Ty::Int{type::IntSize::i64});
          case LiteralExpr::Type::Float:
            for (auto &h : hints) {
              if (h.base &&
                  *h.base >= Builtins::F16 &&
                  *h.base <= Builtins::F64) {
                return tcx.intern(Ty::Float{(type::FloatSize)(*h.base - (Idx)Builtins::F16)});
              }
            }
            return tcx.intern(Ty::Float{type::FloatSize::f64});
          case LiteralExpr::Type::String:
            for (auto &h : hints) {
              if (h.base && *h.base == Builtins::NULSTRING) {
                return tcx.intern(Ty::String{true});
              }
            }
            return tcx.intern(Ty::String{false});
        }
        throw std::runtime_error("unreachable");
      })();
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, {}, {ty}));
      return ig.lastInsn();
    }
    RET_T visitBoolExpr ARGS(BoolExpr) override {
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, {}, {tcx.intern(Ty::Bool{})}));
      return ig.lastInsn();
    }
    RET_T visitBinExpr ARGS(BinExpr) override {
      VarRef lhsNode = visitExpr(*e.lhs PASS_ARGS);
      VarRef rhsNode = visitExpr(*e.rhs PASS_ARGS);
      Idx trait;
      switch (e.op) {
        case BinExpr::BitOr: trait = BitOr; break;
        case BinExpr::BitAnd: trait = BitAnd; break;
        case BinExpr::Add: trait = Add; break;
        case BinExpr::Sub: trait = Sub; break;
        case BinExpr::Mul: trait = Mul; break;
        case BinExpr::Div: trait = Div; break;
        case BinExpr::Rem: trait = Rem; break;
        default: throw std::runtime_error("Invalid binary expression type");
      }
      ig.insns.push_back(Insn(TraitInsn::key(), {trait}, {lhsNode, rhsNode}, {}));
      return ig.lastInsn(0);
    }
    RET_T visitCmpExpr ARGS(CmpExpr) override {
      VarRef lhsTy = visitExpr(*e.lhs PASS_ARGS);
      VarRef rhsTy = visitExpr(*e.rhs PASS_ARGS);
      ig.insns.push_back(Insn(TraitInsn::key(), {Cmp}, {lhsTy, rhsTy}, {}));
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, {}, {tcx.intern(Ty::Bool{})}));
      return ig.lastInsn();
    }
    RET_T visitNegExpr ARGS(NegExpr) override {
      VarRef valTy = visitExpr(*e.value PASS_ARGS);
      ig.insns.push_back(Insn(TraitInsn::key(), {Neg}, {valTy}, {}));
      return ig.lastInsn(0);
    }
    RET_T visitCallExpr ARGS(CallExpr) override {
      std::vector<VarRef> allTys;
      VarRef fnTy = visitExpr(*e.func PASS_ARGS);
      allTys.push_back(fnTy);
      for (Eptr &arg : e.args) {
        allTys.push_back(visitExpr(*arg PASS_ARGS));
      }
      ig.insns.push_back(Insn(TraitInsn::key(), {Fn}, std::move(allTys), {}));
      return ig.lastInsn(0);
    }
    RET_T visitDefineExpr ARGS(DefineExpr) override {
      VarRef varTy = visitExpr(*e.value PASS_ARGS);
      VarRef &var = varNodes.at(e.idx);
      ig.insns.at(*var.insn) = Insn(IdentityInsn::key(), {}, {varTy}, {});
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, {}, {tcx.intern(Ty::Tuple{})}));
      return ig.lastInsn();
    }
    RET_T visitNewExpr ARGS(NewExpr) override {
      std::vector<VarRef> args;
      std::vector<Tp> argTys;
      args.reserve(e.values.size());
      argTys.reserve(e.values.size());
      Idx i = 0;
      for (const auto &se : e.values) {
        args.push_back(visitExpr(*se PASS_ARGS));
        argTys.push_back(tcx.intern(Ty::Placeholder{i++}));
      }
      Tp tyTemplate = tcx.intern(Ty::ADT{e.adt, {e.variant}, argTys});
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, std::move(args), {tyTemplate}));
      return ig.lastInsn();
    }
    RET_T visitGetExpr ARGS(GetExpr) override {
      VarRef objTy = visitExpr(*e.value PASS_ARGS);
      auto &adt = std::get<DefType::ADT>(program->bindings.at(e.adt).defType.v);
      size_t fieldCount = adt.variants.front().values.size();
      std::vector<Tp> params;
      for (Idx i = 0; i < fieldCount; ++i) {
        params.push_back(tcx.intern(Ty::Placeholder{i}));
      }
      Tp tyTemplate = tcx.intern(Ty::ADT{e.adt, {e.variant}, std::move(params)});
      ig.insns.push_back(Insn(DeConstructInsn::key(), {}, {objTy}, {tyTemplate}));
      return ig.lastInsn(e.field);
    }
    RET_T visitForeignExpr ARGS(ForeignExpr) override {
      // TODO type parsing
      ig.insns.push_back(Insn(TrapInsn::key(), {}, {}, {"unimplemented foreign"}));
      return ig.lastInsn();
    }
    RET_T visitDummyExpr ARGS(DummyExpr) override {
      ig.insns.push_back(Insn(TrapInsn::key(), {}, {}, {"dummy expr"}));
      return ig.lastInsn();
    }
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor(type::TTcx &ttcx) {
    return std::make_unique<InferenceVisitor>(ttcx);
  }
}
