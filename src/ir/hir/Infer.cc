#include "Infer.h"

#include "Builtins.h"
#include "../../type/TypePrint.h"
#include "../../type/InferenceGraph.h"

#include <sstream>
#include <cstddef>
#include <deque>

namespace hir::infer {
  using namespace type::infer;

#define RET_T TVar *
#define ARGS(TYPE) (TYPE &e, InferenceGraph &ig, Idx &counter)
#define PASS_ARGS , ig, counter

  class InferenceVisitor :
    public ExprVisitor<TVar*, InferenceGraph&, Idx&>,
    public ProgramVisitor<InferResult> {
  public:
    type::Tcx tcx;
    type::Tbcx tbcx;

    InferenceVisitor() {
    }

    Program *program = nullptr;
    std::map<DefIdx, TVar*> varNodes;

    InferResult visitProgram(Program &p) override {
      program = &p;
      InferResult res;
      Idx counter = 0;
      initBlock(p.topLevel, res, counter);
      visitBlock(p.topLevel, res.graphs.front(), counter);
      res.tcx = std::move(tcx);
      res.tbcx = std::move(tbcx);
      return res;
    }

    void initBlock(Block &block, InferResult &res, Idx &counter) {
      InferenceGraph &graph = res.graphs.emplace_back();
      std::set<Idx> defs;
      DefinitionVisitor().visitBlock(block, defs);
      for (Idx def : defs) {
        varNodes[def] = &graph.add(tcx, counter);
      }
    }

    class DefinitionVisitor : public ExprVisitor<std::monostate, std::set<Idx>&> {
#define DEFINITION_VISIT(TY) std::monostate visit##TY([[maybe_unused]] TY &it, std::set<Idx> &vars) override
    public:
      void visitBlock(Block &it, std::set<Idx> &vars) {
        for (auto b : it.bindings) {
          vars.insert(b);
        }
        for (auto &e : it.body) {
          visitExpr(*e, vars);
        }
      }
      DEFINITION_VISIT(BlockExpr) {
        visitBlock(it.block, vars);
        return {};
      }
      DEFINITION_VISIT(VarExpr) {
        return {};
      }
      DEFINITION_VISIT(CondExpr) {
        visitExpr(*it.predE, vars);
        visitExpr(*it.thenE, vars);
        visitExpr(*it.elseE, vars);
        return {};
      }
      DEFINITION_VISIT(VoidExpr) {
        return {}; // noop
      }
      DEFINITION_VISIT(LiteralExpr) {
        return {}; // noop
      }
      DEFINITION_VISIT(BoolExpr) {
        return {}; // noop
      }
      DEFINITION_VISIT(BinExpr) {
        visitExpr(*it.lhs, vars);
        visitExpr(*it.rhs, vars);
        return {};
      }
      DEFINITION_VISIT(CmpExpr) {
        visitExpr(*it.lhs, vars);
        visitExpr(*it.rhs, vars);
        return {};
      }
      DEFINITION_VISIT(NegExpr) {
        visitExpr(*it.value, vars);
        return {};
      }
      DEFINITION_VISIT(CallExpr) {
        visitExpr(*it.func, vars);
        for (auto &a : it.args) {
          visitExpr(*a, vars);
        }
        return {};
      }
      DEFINITION_VISIT(DefineExpr) {
        visitExpr(*it.value, vars);
        return {};
      }
      DEFINITION_VISIT(NewExpr) {
        for (auto &v : it.values) {
          visitExpr(*v, vars);
        }
        return {};
      }
      DEFINITION_VISIT(GetExpr) {
        visitExpr(*it.value, vars);
        return {};
      }
      DEFINITION_VISIT(ForeignExpr) {
        return {}; // noop
      }
      DEFINITION_VISIT(DummyExpr) {
        return {}; // noop
      }
    };

    Idx paramC = 0;
    Tp unitType() { return tcx.intern(Ty::Tuple{{}}); }
    Tp boolType() { return tcx.intern(Ty::Bool{}); }

    RET_T visitBlock ARGS(Block) {
      for (auto it = e.body.begin();;) {
        TVar &n = *visitExpr(**it PASS_ARGS);
        if (++it == e.body.end()) {
          return &n;
        }
      }
    }

    RET_T visitExpr ARGS(Expr) override {
      TVar &n = *ExprVisitor::visitExpr(e PASS_ARGS);
      n.desc.maybeSpan(e.span, "type of this expression");
      // TODO add hints
      return &n;
    }
    RET_T visitBlockExpr ARGS(BlockExpr) override {
      return visitBlock(e.block PASS_ARGS);
    }
    RET_T visitVarExpr ARGS(VarExpr) override {
      TVar &retNode = ig.add(tcx, counter);
      TVar &varNode = *varNodes.at(e.ref);
      {
        auto &cnstr = ig.add<Constraint::Assigned>();
        cnstr.toTy = retNode.ty;
        cnstr.fromTy = varNode.ty;
        cnstr.desc.maybeSpan(e.span, "variable is referenced here");
        varNode.connectTo(cnstr);
        cnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitCondExpr ARGS(CondExpr) override {
      TVar &predENode = *visitExpr(*e.predE PASS_ARGS);
      TVar &thenNode = *visitExpr(*e.thenE PASS_ARGS);
      TVar &elseNode = *visitExpr(*e.elseE PASS_ARGS);
      TVar &retNode = ig.add(tcx, counter);
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.tyA = boolType();
        cnstr.tyB = predENode.ty;
        cnstr.desc.maybeSpan(e.predE->span, "predicate of conditional must be boolean");
        cnstr.connectTo(predENode);
      }
      for (auto p : std::vector<std::pair<TVar&, Expr&>>{
          {thenNode, *e.thenE},
          {elseNode, *e.elseE},
        }) {
        auto &cnstr = ig.add<Constraint::Assigned>();
        cnstr.fromTy = p.first.ty;
        cnstr.toTy = retNode.ty;
        cnstr.desc.maybeSpan(p.second.span, "is a conditional branch");
        p.first.connectTo(cnstr);
        cnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitVoidExpr ARGS(VoidExpr) override {
      TVar &retNode = ig.add(tcx, counter);
      auto &cnstr = ig.add<Constraint::Concrete>();
      cnstr.tyA = unitType();
      cnstr.tyB = retNode.ty;
      cnstr.desc.maybeSpan(e.span, "is void");
      cnstr.connectTo(retNode);
      return &retNode;
    }
    RET_T visitLiteralExpr ARGS(LiteralExpr) override {
      auto &hints = dynamic_cast<Expr&>(e).type;
      auto ty = ([&]() {
        switch (e.type) {
        case LiteralExpr::Int:
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
        case LiteralExpr::Float:
          for (auto &h : hints) {
            if (h.base &&
                *h.base >= Builtins::F16 &&
                *h.base <= Builtins::F64) {
              return tcx.intern(Ty::Float{(type::FloatSize)(*h.base - (Idx)Builtins::F16)});
            }
          }
          return tcx.intern(Ty::Float{type::FloatSize::f64});
        case LiteralExpr::String:
          for (auto &h : hints) {
            if (h.base && *h.base == Builtins::NULSTRING) {
              return tcx.intern(Ty::String{true});
            }
          }
          return tcx.intern(Ty::String{false});
        default: throw 0;
        }
      })();
      TVar &retNode = ig.add(tcx, counter);
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.tyA = ty;
        cnstr.tyB = retNode.ty;
        cnstr.desc.maybeSpan(e.span, "from this expression");
        cnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitBoolExpr ARGS(BoolExpr) override {
      TVar &retNode = ig.add(tcx, counter);
      auto &cnstr = ig.add<Constraint::Concrete>();
      cnstr.tyA = boolType();
      cnstr.tyB = retNode.ty;
      cnstr.desc.maybeSpan(e.span, "is a boolean");
      cnstr.connectTo(retNode);
      return &retNode;
    }
    RET_T visitBinExpr ARGS(BinExpr) override {
      TVar &lhsNode = *visitExpr(*e.lhs PASS_ARGS);
      TVar &rhsNode = *visitExpr(*e.rhs PASS_ARGS);
      TVar &retNode = ig.add(tcx, counter);
      Idx trait;
      switch (e.op) {
      case BinExpr::BitOr: trait = BitOr; break;
      case BinExpr::BitAnd: trait = BitAnd; break;
      case BinExpr::Add: trait = Add; break;
      case BinExpr::Sub: trait = Sub; break;
      case BinExpr::Mul: trait = Mul; break;
      case BinExpr::Div: trait = Div; break;
      case BinExpr::Rem: trait = Rem; break;
      default: throw 0;
      }
      TraitBound *traitBound = tbcx.intern(TraitBound{trait, {rhsNode.ty}});
      auto &tCnstr = ig.add<Constraint::Trait>();
      {
        tCnstr.ty = lhsNode.ty;
        tCnstr.tb = traitBound;
        tCnstr.desc.maybeSpan(e.span, "from expression here");
        lhsNode.connectTo(tCnstr);
        rhsNode.connectTo(tCnstr);
      }
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.tyA = tcx.intern(Ty::TraitRef{lhsNode.ty, traitBound, 0});
        cnstr.tyB = retNode.ty;
        cnstr.desc.maybeSpan(e.span, "from expression here");
        tCnstr.connectTo(cnstr);
        cnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitCmpExpr ARGS(CmpExpr) override {
      TVar &lhsNode = *visitExpr(*e.lhs PASS_ARGS);
      TVar &rhsNode = *visitExpr(*e.rhs PASS_ARGS);
      TVar &retNode = ig.add(tcx, counter);
      Idx trait = e.op <= CmpExpr::Eq ? Eq : Cmp;
      TraitBound *traitBound = tbcx.intern(TraitBound{trait, {rhsNode.ty}});
      {
        auto &cnstr = ig.add<Constraint::Trait>();
        cnstr.ty = lhsNode.ty;
        cnstr.tb = traitBound;
        cnstr.desc.maybeSpan(e.span, "compared here");
        lhsNode.connectTo(cnstr);
        rhsNode.connectTo(cnstr);
      }
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.tyA = boolType();
        cnstr.tyB = retNode.ty;
        cnstr.desc.maybeSpan(e.span, "result of comparison");
        cnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitNegExpr ARGS(NegExpr) override {
      TVar &exprNode = *visitExpr(*e.value PASS_ARGS);
      TVar &retNode = ig.add(tcx, counter);
      TraitBound *traitBound = tbcx.intern(TraitBound{Neg});
      auto &tCnstr = ig.add<Constraint::Trait>();
      {
        tCnstr.ty = exprNode.ty;
        tCnstr.tb = traitBound;
        tCnstr.desc.maybeSpan(e.span, "negated here");
        exprNode.connectTo(tCnstr);
      }
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.tyA = tcx.intern(Ty::TraitRef{exprNode.ty, traitBound, 0});
        cnstr.tyB = retNode.ty;
        cnstr.desc.maybeSpan(e.span, "result of negation");
        tCnstr.connectTo(cnstr);
        cnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitCallExpr ARGS(CallExpr) override {
      TVar &funcNode = *visitExpr(*e.func PASS_ARGS);
      TVar &retNode = ig.add(tcx, counter);
      auto &tCnstr = ig.add<Constraint::Trait>();
      std::vector<Tp> argTys;
      argTys.reserve(e.args.size());
      for (Eptr &e : e.args) {
        TVar &argNode = *visitExpr(*e PASS_ARGS);
        argTys.push_back(argNode.ty);
        argNode.connectTo(tCnstr);
      }
      Tp argsTy = tcx.intern(Ty::Tuple{argTys});
      TraitBound *traitBound = tbcx.intern(TraitBound{Fn, {argsTy, retNode.ty}});
      {
        tCnstr.ty = funcNode.ty;
        tCnstr.tb = traitBound;
        tCnstr.desc.maybeSpan(e.span, "called here");
        funcNode.connectTo(tCnstr);
        tCnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitDefineExpr ARGS(DefineExpr) override {
      TVar &retNode = *visitExpr(*e.value PASS_ARGS);
      TVar &varNode = *varNodes.at(e.idx);
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.desc.maybeSpan(e.span, "defined here");
        cnstr.tyA = varNode.ty;
        cnstr.tyB = retNode.ty;
        varNode.connectTo(cnstr);
        cnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitNewExpr ARGS(NewExpr) override {
      TVar &retNode = ig.add(tcx, counter);
      std::vector<Tp> argTys;
      argTys.reserve(e.values.size());
      auto &cnstr = ig.add<Constraint::Concrete>();
      for (Eptr &sE : e.values) {
        TVar &argNode = *visitExpr(*sE PASS_ARGS);
        argTys.push_back(argNode.ty);
        argNode.connectTo(cnstr);
      }
      {
        cnstr.tyA = tcx.intern(Ty::ADT{e.adt, {e.variant}, argTys});
        cnstr.tyB = retNode.ty;
        cnstr.desc.maybeSpan(e.span, "constructed here");
        cnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitGetExpr ARGS(GetExpr) override {
      TVar &objTy = *visitExpr(*e.value PASS_ARGS);
      auto &cnstr = ig.add<Constraint::Assigned>();
      objTy.connectTo(cnstr);
      std::vector<Tp> argTys;
      auto &adt = std::get<DefType::ADT>(program->bindings.at(e.adt).defType.v);
      Idx size = 0;
      std::optional<Idx> fieldTyIdx;
      for (Idx v = 0; v < adt.variants.size(); ++v) {
        if (v == e.variant) {
          fieldTyIdx = size + e.field;
        }
        size += adt.variants[v].values.size();
      }
      argTys.reserve(size);
      TVar *retNode = nullptr;
      for (Idx i = 0; i < size; ++i) {
        auto &argNode = ig.add(tcx, counter);
        argTys.push_back(argNode.ty);
        cnstr.connectTo(argNode);
        if (i == *fieldTyIdx) {
          retNode = &argNode;
        }
      }
      TVar &gottenTy = ig.add(tcx, counter);
      {
        cnstr.fromTy = objTy.ty;
        cnstr.toTy = gottenTy.ty;
        cnstr.desc.maybeSpan(e.span, "field taken here");
      }
      {
        auto &cCnstr = ig.add<Constraint::Concrete>();
        cCnstr.tyA = tcx.intern(Ty::ADT{e.adt, {e.variant}, argTys});
        cCnstr.tyB = gottenTy.ty;
        cCnstr.desc.maybeSpan(e.span, "field is taken");
        cCnstr.connectTo(cnstr);
      }
      return &*retNode;
    }
    RET_T visitForeignExpr ARGS(ForeignExpr) override {
      return &ig.add(tcx, counter);
    }
    RET_T visitDummyExpr ARGS(DummyExpr) override {
      TVar &n = ig.add(tcx, counter); // propagate error
      auto &cnstr = ig.add<Constraint::Concrete>();
      cnstr.tyA = tcx.intern(Ty::Err{});
      cnstr.tyB = n.ty;
      cnstr.connectTo(n);
      return &n;
    }
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor() {
    return std::make_unique<InferenceVisitor>();
  }
}
