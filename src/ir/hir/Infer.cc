#include "Infer.h"

#include "Builtins.h"
#include "../../type/TypePrint.h"
#include "../../type/infer/InferenceGraph.h"
#include "RecurVisitor.h"

#include <sstream>
#include <cstddef>
#include <deque>
#include <variant>

namespace hir::infer {
  using namespace type::infer;

#define RET_T TVar *
#define ARGS(TYPE) (TYPE &e, InferenceGraph &ig, Idx &counter, std::vector<TVar *>::iterator &tvi)
#define PASS_ARGS , ig, counter, tvi

  class InferenceVisitor :
    public ExprVisitor<TVar*, InferenceGraph&, Idx&, std::vector<TVar *>::iterator&>,
    public ProgramVisitor<InferResult> {
  public:
    type::Tcx &tcx;
    type::Tbcx &tbcx;

    InferenceVisitor(type::TTcx &ttcx):
      tcx(ttcx.tcx),
      tbcx(ttcx.tbcx)
    {}

    Program *program = nullptr;
    std::map<DefIdx, TVar*> varNodes;

    InferResult visitProgram(Program &p) override {
      program = &p;
      InferResult res;
      Idx counter = 0;

      std::vector<InferenceGraph> graphs;
      std::vector<std::vector<TVar *>> tvs;

      initBlock(p.topLevel, graphs, counter, tvs.emplace_back());
      for (auto &ti : p.traitImpls) {
        for (auto &m : ti.methods) {
          initBlock(m, graphs, counter, tvs.emplace_back());
        }
      }

      Idx graphI = 0;
      std::vector<TVar*>::iterator tvIter;
      {
        tvIter = tvs.at(graphI).begin();
        visitBlock(p.topLevel, graphs.at(graphI), counter, tvIter);
        graphI++;
        for (auto &ti : p.traitImpls) {
          for (auto &m : ti.methods) {
            tvIter = tvs.at(graphI).begin();
            visitBlock(m, graphs.at(graphI), counter, tvIter);
            graphI++;
          }
        }
      }
      res.top = graphs.front();
      return res;
    }

    void initBlock(
      Block &block,
      std::vector<InferenceGraph> &graphs,
      Idx &counter,
      std::vector<TVar *> &tvs
    ) {
      Idx index = graphs.size();
      InferenceGraph &graph = graphs.emplace_back();
      TVarVisitor(graph, tcx, counter, tvs).visitBlock(block);
      graph.index = index;
      std::set<Idx> defs;
      DefinitionVisitor().visitBlock(block, defs);
      for (Idx def : defs) {
        varNodes[def] = &graph.add(tcx, counter);
      }
    }

    class TVarVisitor : public RecurVisitor<std::monostate> {
    public:
      InferenceGraph &ig;
      type::Tcx &tcx;
      Idx &counter;
      std::vector<TVar *> &tvs;
      TVarVisitor(
        decltype(ig) ig,
        decltype(tcx) tcx,
        decltype(counter) counter,
        decltype(tvs) tvs
      ): ig(ig), tcx(tcx), counter(counter), tvs(tvs) {}

      std::monostate visitExpr(Expr &it) override {
        tvs.push_back(&ig.add(tcx, counter));
        RecurVisitor::visitExpr(it);
        return {};
      }
    };

    class DefinitionVisitor : public RecurVisitor<std::monostate, std::set<Idx>> {
    public:
      void visitBlock(Block &it, std::set<Idx> &vars) override {
        for (auto b : it.bindings) {
          vars.insert(b);
        }
        RecurVisitor::visitBlock(it, vars);
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
      TVar &retNode = **tvi++;
      TVar &ty = *visitBlock(e.block PASS_ARGS);
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.tyA = ty.ty;
        cnstr.tyB = retNode.ty;
        ty.connectTo(cnstr);
        cnstr.connectTo(retNode);
        cnstr.desc.maybeSpan(e.span, "last expression of block");
      }
      return &retNode;
    }
    RET_T visitVarExpr ARGS(VarExpr) override {
      TVar &retNode = **tvi++;
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
      TVar &retNode = **tvi++;
      TVar &predENode = *visitExpr(*e.predE PASS_ARGS);
      TVar &thenNode = *visitExpr(*e.thenE PASS_ARGS);
      TVar &elseNode = *visitExpr(*e.elseE PASS_ARGS);
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
      TVar &retNode = **tvi++;
      auto &cnstr = ig.add<Constraint::Concrete>();
      cnstr.tyA = unitType();
      cnstr.tyB = retNode.ty;
      cnstr.desc.maybeSpan(e.span, "is void");
      cnstr.connectTo(retNode);
      return &retNode;
    }
    RET_T visitLiteralExpr ARGS(LiteralExpr) override {
      TVar &retNode = **tvi++;
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
      TVar &retNode = **tvi++;
      auto &cnstr = ig.add<Constraint::Concrete>();
      cnstr.tyA = boolType();
      cnstr.tyB = retNode.ty;
      cnstr.desc.maybeSpan(e.span, "is a boolean");
      cnstr.connectTo(retNode);
      return &retNode;
    }
    RET_T visitBinExpr ARGS(BinExpr) override {
      TVar &retNode = **tvi++;
      TVar &lhsNode = *visitExpr(*e.lhs PASS_ARGS);
      TVar &rhsNode = *visitExpr(*e.rhs PASS_ARGS);
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
      TVar &retNode = **tvi++;
      TVar &lhsNode = *visitExpr(*e.lhs PASS_ARGS);
      TVar &rhsNode = *visitExpr(*e.rhs PASS_ARGS);
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
      TVar &retNode = **tvi++;
      TVar &exprNode = *visitExpr(*e.value PASS_ARGS);
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
      TVar &retNode = **tvi++;
      TVar &funcNode = *visitExpr(*e.func PASS_ARGS);
      auto &tCnstr = ig.add<Constraint::Trait>();
      std::vector<Tp> argTys;
      argTys.reserve(e.args.size());
      for (Eptr &e : e.args) {
        TVar &argNode = *visitExpr(*e PASS_ARGS);
        argTys.push_back(argNode.ty);
        argNode.connectTo(tCnstr);
      }
      Tp argsTy = tcx.intern(Ty::Tuple{argTys});
      TraitBound *traitBound = tbcx.intern(TraitBound{Fn, {argsTy}});
      {
        tCnstr.ty = funcNode.ty;
        tCnstr.tb = traitBound;
        tCnstr.desc.maybeSpan(e.span, "called here");
        funcNode.connectTo(tCnstr);
        tCnstr.connectTo(retNode);
      }
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.tyA = tcx.intern(Ty::TraitRef{funcNode.ty, traitBound, 0});
        cnstr.tyB = retNode.ty;
        cnstr.desc.maybeSpan(e.span, "result of function call");
        tCnstr.connectTo(cnstr);
        cnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitDefineExpr ARGS(DefineExpr) override {
      TVar &retNode = **tvi++;
      TVar &exprNode = *visitExpr(*e.value PASS_ARGS);
      TVar &varNode = *varNodes.at(e.idx);
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.desc.maybeSpan(e.span, "defined here");
        cnstr.tyA = varNode.ty;
        cnstr.tyB = exprNode.ty;
        exprNode.connectTo(cnstr);
        cnstr.connectTo(varNode);
      }
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.desc.maybeSpan(e.span, "is definition");
        cnstr.tyA = unitType();
        cnstr.tyB = retNode.ty;
        cnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitNewExpr ARGS(NewExpr) override {
      TVar &retNode = **tvi++;
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
      TVar &retNode = **tvi++;
      TVar &objTy = *visitExpr(*e.value PASS_ARGS);
      auto &cnstr = ig.add<Constraint::Assigned>();
      objTy.connectTo(cnstr);
      cnstr.connectTo(retNode);
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
      for (Idx i = 0; i < size; ++i) {
        Tp ty;
        if (i == *fieldTyIdx) {
          ty = retNode.ty;
        } else {
          ty = tcx.intern(Ty::Placeholder{counter++});
        }
        argTys.push_back(ty);
      }
      Tp gottenTy = tcx.intern(Ty::Placeholder{counter++});
      {
        cnstr.fromTy = objTy.ty;
        cnstr.toTy = gottenTy;
        cnstr.desc.maybeSpan(e.span, "field taken here");
      }
      {
        auto &cCnstr = ig.add<Constraint::Concrete>();
        cCnstr.tyA = tcx.intern(Ty::ADT{e.adt, {e.variant}, argTys});
        cCnstr.tyB = gottenTy;
        cCnstr.desc.maybeSpan(e.span, "field is taken");
        cCnstr.connectTo(cnstr);
      }
      return &retNode;
    }
    RET_T visitForeignExpr ARGS(ForeignExpr) override {
      return &**tvi++; // should be never type
    }
    RET_T visitDummyExpr ARGS(DummyExpr) override {
      TVar &n = **tvi++; // propagate error
      auto &cnstr = ig.add<Constraint::Concrete>();
      cnstr.tyA = tcx.intern(Ty::Err{});
      cnstr.tyB = n.ty;
      cnstr.connectTo(n);
      return &n;
    }
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor(type::TTcx &ttcx) {
    return std::make_unique<InferenceVisitor>(ttcx);
  }
}
