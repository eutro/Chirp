#include "Infer.h"

#include "Builtins.h"
#include "../../type/TypePrint.h"
#include "../../type/infer/InferenceGraph.h"
#include "RecurVisitor.h"

#include <limits>
#include <sstream>
#include <cstddef>
#include <deque>
#include <variant>

namespace hir::infer {
  using namespace type::infer;

#define RET_T TVar *
#define ARGS(TYPE) (TYPE &e, InferenceGraph &ig, std::vector<TVar *>::iterator &tvi)
#define PASS_ARGS , ig, tvi

  class InferenceVisitor :
    public ExprVisitor<TVar*, InferenceGraph&, std::vector<TVar *>::iterator&>,
    public ProgramVisitor<InferResult> {
  public:
    type::Tcx &tcx;
    type::Tbcx &tbcx;
    type::TTcx &ttcx;

    InferenceVisitor(type::TTcx &ttcx):
      tcx(ttcx.tcx),
      tbcx(ttcx.tbcx),
      ttcx(ttcx)
    {}

    Program *program = nullptr;
    std::map<DefIdx, TVar*> varNodes;
    std::map<DefIdx, Tp> definedTys;
    Idx counter = 0;

    void addBuiltins(System &sys) {
      auto implBinOp = [&](Tp ty, Idx trait) {
        AbstractTraitImpl ati;
        ati.blockIdx = sys.seqs.size();
        sys.seqs.emplace_back();
        ati.inputs = {ty, ty};
        ati.outputs = {ty};
        sys.traits[trait].insert(ttcx, {ty}, std::move(ati));
      };
      auto implNeg = [&](Tp ty) {
        AbstractTraitImpl ati;
        ati.blockIdx = sys.seqs.size();
        sys.seqs.emplace_back();
        ati.inputs = {ty};
        ati.outputs = {ty};
        sys.traits[Neg].insert(ttcx, {ty}, std::move(ati));
      };
      auto implEq = [&](Tp ty) {
        AbstractTraitImpl ati;
        ati.blockIdx = sys.seqs.size();
        sys.seqs.emplace_back();
        ati.inputs = {ty, ty};
        sys.traits[Eq].insert(ttcx, {ty}, std::move(ati));
      };
      auto implCmp = [&](Tp ty) {
        AbstractTraitImpl ati;
        ati.blockIdx = sys.seqs.size();
        sys.seqs.emplace_back();
        ati.inputs = {ty, ty};
        sys.traits[Cmp].insert(ttcx, {ty}, std::move(ati));
      };

      for (type::IntSize is : type::INT_SIZE_FIXED) {
        Tp i = tcx.intern(Ty::Int{is});
        Tp u = tcx.intern(Ty::UInt{is});
        for (Tp ty : {i, u}) {
          for (Builtins bt : {Add, Sub, Mul, Div, Rem, BitOr, BitAnd}) {
            implBinOp(ty, bt);
          }
          implEq(ty);
          implCmp(ty);
        }
        implNeg(i);
      }
      Tp boolTy = boolType();
      implBinOp(boolTy, BitOr);
      implBinOp(boolTy, BitAnd);

      for (type::FloatSize fs : type::FLOAT_SIZE_VALUES) {
        Tp ty = tcx.intern(Ty::Float{fs});
        for (Builtins bt : {Add, Sub, Mul, Div, Rem}) {
          implBinOp(ty, bt);
        }
        implNeg(ty);
        implEq(ty);
        implCmp(ty);
      }

      {
        AbstractTraitImpl ati;
        ati.blockIdx = sys.seqs.size();
        sys.seqs.emplace_back();
        auto args = tcx.intern(Ty::Placeholder{counter++});
        auto ret = tcx.intern(Ty::Placeholder{counter++});
        auto fnTy = tcx.intern(Ty::FfiFn{args, ret});
        ati.inputs = {fnTy, args};
        ati.outputs = {ret};
        sys.traits[Fn].insert(ttcx, {fnTy}, std::move(ati));
      }
    }

    InferResult visitProgram(Program &p) override {
      InferResult res;
      program = &p;

      std::vector<InferenceGraph> graphs;
      std::vector<std::vector<TVar *>> tvs;

      initBlock(p.topLevel, graphs, tvs.emplace_back());
      for (auto &ti : p.traitImpls) {
        for (auto &m : ti.methods) {
          initBlock(m, graphs, tvs.emplace_back());
        }
      }

      Idx graphI = 0;
      std::vector<TVar*>::iterator tvIter;
      {
        tvIter = tvs.at(graphI).begin();
        visitBlock(p.topLevel, graphs.at(graphI), tvIter);
        auto &topBlock = res.sys.seqs.emplace_back();
        topBlock.seq = graphs.at(graphI);
        graphI++;
        for (auto &ti : p.traitImpls) {
          auto &map = res.sys.traits[*ti.trait.base];
          auto &ig = graphs.at(graphI);
          AbstractTraitImpl ati;
          ati.blockIdx = res.sys.seqs.size();
          auto &block = res.sys.seqs.emplace_back();
          for (Idx param : ti.params) {
            definedTys[param] = ig.add(tcx, counter).ty;
          }
          std::vector<Tp> &tys = ati.inputs;
          tys.push_back(parseTy(ig, ti.type));
          {
            auto tp = parseTyParams(ig, ti.trait.params);
            std::copy(tp.begin(), tp.end(), std::back_inserter(tys));
          }
          ati.outputs = parseTyParams(ig, ti.types);
          for (auto &m : ti.methods) {
            tvIter = tvs.at(graphI).begin();
            visitBlock(m, ig, tvIter);
            graphI++;
          }
          block.seq = ig;
          map.insert(ttcx, {tys.front()}, std::move(ati));
        }
      }
      addBuiltins(res.sys);
      return res;
    }

    void initBlock(
      Block &block,
      std::vector<InferenceGraph> &graphs,
      std::vector<TVar *> &tvs
    ) {
      Idx index = graphs.size();
      InferenceGraph &graph = graphs.emplace_back();
      graph.index = index;
      TVarVisitor(graph, tcx, counter, tvs).visitBlock(block);
      std::vector<Idx> defs;
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

    class DefinitionVisitor : public RecurVisitor<std::monostate, std::vector<Idx>> {
    public:
      void visitBlock(Block &it, std::vector<Idx> &vars) override {
        for (auto b : it.bindings) {
          vars.push_back(b);
        }
        RecurVisitor::visitBlock(it, vars);
      }
    };

    Tp unitType() { return tcx.intern(Ty::Tuple{{}}); }
    Tp boolType() { return tcx.intern(Ty::Bool{}); }

    // TODO proper parsing -_-

    Tp getDefinedType(InferenceGraph &ig, DefIdx id) {
      auto found = definedTys.find(id);
      if (found != definedTys.end()) {
        return found->second;
      }
      return definedTys[id] = ig.add(tcx, counter).ty;
    }

    std::optional<Tp> maybeParseTy(InferenceGraph &ig, Type &ty) {
      DefIdx idx = *ty.base;

      // builtins
      switch (idx) {
      case BOOL:
        return boolType();
      case I8: case I16: case I32: case I64: case I128:
        return tcx.intern(Ty::Int{(type::IntSize) (idx - I8)});
      case U8: case U16: case U32: case U64: case U128:
        return tcx.intern(Ty::UInt{(type::IntSize) (idx - U8)});
      case F16: case F32: case F64:
        return tcx.intern(Ty::Float{(type::FloatSize) (idx - F16)});
      case TUPLE:
        return tcx.intern(Ty::Tuple{parseTyParams(ig, ty.params)});
      case STRING:
        return tcx.intern(Ty::String{false});
      case NULSTRING:
        return tcx.intern(Ty::String{true});
      case FFIFN: {
        auto tys = parseTyParams(ig, ty.params);
        return tcx.intern(Ty::FfiFn{tys.at(0), tys.at(1)});
      }
      }

      auto &def = program->bindings.at(idx);
      if (std::holds_alternative<DefType::Type>(def.defType.v)) {
        return getDefinedType(ig, idx);
      } if (std::holds_alternative<DefType::ADT>(def.defType.v)) {
        auto &adt = std::get<DefType::ADT>(def.defType.v);
        type::Variants variants;
        for (Idx i = 0; i < adt.variants.size(); ++i) {
          variants.insert(i);
        }
        return tcx.intern(Ty::ADT{idx, variants, parseTyParams(ig, ty.params)});
      }

      return std::nullopt;
    }

    void parseTypeHint(InferenceGraph &ig, TVar &target, Type &hint) {
      if (!hint.base) {
        return; // explicit placeholder hint
      }

      auto type = maybeParseTy(ig, hint);
      if (type) {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.tyA = target.ty;
        cnstr.tyB = *type;
        if (hint.source) {
          cnstr.desc.maybeSpan(hint.source, "type hint");
        } else {
          cnstr.desc.msg("implicit type hint");
        }
        if (hint.informative) {
          cnstr.connectTo(target);
        } else {
          target.connectTo(cnstr);
        }
        return;
      }

      auto found = program->bindings.find(*hint.base);
      if (found != program->bindings.end() &&
          std::holds_alternative<DefType::Trait>(found->second.defType.v)) {
        auto &cnstr = ig.add<Constraint::Trait>();
        cnstr.ty = target.ty;
        cnstr.tb = parseTb(ig, hint);
        cnstr.idx = std::numeric_limits<Idx>::max();
        cnstr.desc.maybeSpan(hint.source, "from trait hint");
        target.connectTo(cnstr);
      } else {
        throw std::runtime_error("ICE: index is not a type or trait");
      }
    }

    Tp parseTy(InferenceGraph &ig, Type &ty) {
      if (ty.base) {
        auto type = maybeParseTy(ig, ty);
        if (type) {
          return *type;
        }
        throw std::runtime_error("ICE: Undefined type");
      }
      return tcx.intern(Ty::Placeholder{counter++});
    }

    std::vector<Tp> parseTyParams(InferenceGraph &ig, std::vector<Type> params) {
      std::vector<Tp> ret;
      ret.reserve(params.size());
      for (Type &ty : params) {
        ret.push_back(parseTy(ig, ty));
      }
      return ret;
    }

    TraitBound *parseTb(InferenceGraph &ig, Type &ty) {
      return tbcx.intern(TraitBound{ty.base.value(), parseTyParams(ig, ty.params)});
    }

    RET_T visitBlock ARGS(Block) {
      for (Idx var : e.bindings) {
        auto &varDef = std::get<DefType::Variable>(program->bindings.at(var).defType.v);
        TVar &n = *varNodes.at(var);
        for (auto &hint : varDef.hints) {
          parseTypeHint(ig, n, hint);
        }
      }
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
      for (auto &hint : e.hints) {
        parseTypeHint(ig, n, hint);
      }
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
        cnstr.fromTy.insert(varNode.ty);
        cnstr.desc.maybeSpan(e.span, "variable reference");
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
        cnstr.desc.maybeSpan(e.predE->span, "predicate of a conditional");
        cnstr.connectTo(predENode);
      }
      {
        auto &cnstr = ig.add<Constraint::Assigned>();
        cnstr.toTy = retNode.ty;
        cnstr.connectTo(retNode);
        for (auto p : std::vector<std::pair<TVar&, Expr&>>{
            {thenNode, *e.thenE},
            {elseNode, *e.elseE},
        }) {
          cnstr.fromTy.insert(p.first.ty);
          cnstr.desc.maybeSpan(p.second.span, "conditional branch");
          p.first.connectTo(cnstr);
        }
      }
      return &retNode;
    }
    RET_T visitVoidExpr ARGS(VoidExpr) override {
      TVar &retNode = **tvi++;
      auto &cnstr = ig.add<Constraint::Concrete>();
      cnstr.tyA = unitType();
      cnstr.tyB = retNode.ty;
      cnstr.desc.maybeSpan(e.span, "void value");
      cnstr.connectTo(retNode);
      return &retNode;
    }
    RET_T visitLiteralExpr ARGS(LiteralExpr) override {
      TVar &retNode = **tvi++;
      auto &hints = e.hints;
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
        cnstr.desc.maybeSpan(e.span, "literal value");
        cnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitBoolExpr ARGS(BoolExpr) override {
      TVar &retNode = **tvi++;
      auto &cnstr = ig.add<Constraint::Concrete>();
      cnstr.tyA = boolType();
      cnstr.tyB = retNode.ty;
      cnstr.desc.maybeSpan(e.span, "literal boolean");
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
        tCnstr.idx = retNode.ref.index;
        tCnstr.desc.maybeSpan(e.span, "binary expression here");
        lhsNode.connectTo(tCnstr);
        rhsNode.connectTo(tCnstr);
      }
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.tyA = tcx.intern(Ty::TraitRef{lhsNode.ty, traitBound, 0});
        cnstr.tyB = retNode.ty;
        cnstr.desc.maybeSpan(e.span, "binary expression here");
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
        cnstr.idx = retNode.ref.index;
        cnstr.desc.maybeSpan(e.span, "comparison here");
        lhsNode.connectTo(cnstr);
        rhsNode.connectTo(cnstr);
      }
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.tyA = boolType();
        cnstr.tyB = retNode.ty;
        cnstr.desc.maybeSpan(e.span, "result of this comparison");
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
        tCnstr.idx = retNode.ref.index;
        tCnstr.desc.maybeSpan(e.span, "negation here");
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
      tCnstr.idx = retNode.ref.index;
      funcNode.connectTo(tCnstr);
      std::vector<Tp> argTys;
      argTys.reserve(e.args.size());
      for (Eptr &argE : e.args) {
        TVar &argNode = *visitExpr(*argE PASS_ARGS);
        argTys.push_back(argNode.ty);
        argNode.connectTo(tCnstr);
      }
      Tp argsTy = tcx.intern(Ty::Tuple{argTys});
      TraitBound *traitBound = tbcx.intern(TraitBound{Fn, {argsTy}});
      {
        tCnstr.ty = funcNode.ty;
        tCnstr.tb = traitBound;
        tCnstr.desc.maybeSpan(e.span, "call here");
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
        cnstr.desc.maybeSpan(e.span, "definition here");
        cnstr.tyA = varNode.ty;
        cnstr.tyB = exprNode.ty;
        exprNode.connectTo(cnstr);
        cnstr.connectTo(varNode);
      }
      {
        auto &cnstr = ig.add<Constraint::Concrete>();
        cnstr.desc.maybeSpan(e.span, "being a definition");
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
        cnstr.desc.maybeSpan(e.span, "construction here");
        cnstr.connectTo(retNode);
      }
      return &retNode;
    }
    RET_T visitGetExpr ARGS(GetExpr) override {
      TVar &retNode = **tvi++;
      TVar &objTy = *visitExpr(*e.value PASS_ARGS);
      auto &cnstr = ig.add<Constraint::Assigned>();
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
          cnstr.connectTo(retNode);
          ty = retNode.ty;
        } else {
          TVar &argNode = ig.add(tcx, counter);
          cnstr.connectTo(argNode);
          ty = argNode.ty;
        }
        argTys.push_back(ty);
      }
      TVar &gottenNode = ig.add(tcx, counter);
      {
        cnstr.fromTy.insert(objTy.ty);
        cnstr.toTy = gottenNode.ty;
        objTy.connectTo(gottenNode);
        gottenNode.connectTo(cnstr);
        cnstr.desc.maybeSpan(e.span, "field taken here");
      }
      {
        auto &cCnstr = ig.add<Constraint::Concrete>();
        cCnstr.tyA = tcx.intern(Ty::ADT{e.adt, {e.variant}, argTys});
        cCnstr.tyB = gottenNode.ty;
        cCnstr.desc.maybeSpan(e.span, "field being taken");
        cCnstr.connectTo(gottenNode);
      }
      return &retNode;
    }
    RET_T visitForeignExpr ARGS(ForeignExpr) override {
      TVar &retNode = **tvi++;
      return &retNode;
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
