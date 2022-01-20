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
    std::map<DefIdx, VarRef> tyNodes;

    void addBuiltins(LookupTable &sys) {
      auto implBinOp = [&](Tp ty, Idx trait) {
        sys.insertFn(TraitInsn::key(), {trait}, {ty, ty}, ConstInsn({ty}));
      };
      auto implNeg = [&](Tp ty) {
        sys.insertFn(TraitInsn::key(), {(Idx)Neg}, {ty}, ConstInsn({ty}));
      };
      auto implEq = [&](Tp ty) {
        sys.insertFn(TraitInsn::key(), {(Idx)Eq}, {ty, ty}, ConstInsn({}));
      };
      auto implCmp = [&](Tp ty) {
        sys.insertFn(TraitInsn::key(), {(Idx)Cmp}, {ty, ty}, ConstInsn({}));
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
      Tp boolTy = tcx.intern(Ty::Bool{});
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
      sys.insertFn(TraitInsn::key(), {(Idx)Builtins::Fn}, 
                   {tcx.intern(Ty::FfiFn{tcx.intern(Ty::Placeholder{0}),
                                         tcx.intern(Ty::Placeholder{1})}),
                    tcx.intern(Ty::Placeholder{0})},
                   [](const std::vector<Tp> &tys, const auto&) -> std::vector<Tp> {
        return {std::get<Ty::FfiFn>(tys.at(0)->v).ret};
      });
    }

    static void doSorting(const std::vector<Insn*> &insns) {
      if (insns.size() > 1) {
        // throw std::runtime_error("Uh oh! Cycles!");
      }
    }

    InferResult visitProgram(Program &p) override {
      program = &p;
      InferResult res;
      addBuiltins(*res.table);
      visitRootBlock(p.topLevel, res.insnLists.emplace_back(), false);
      res.insnLists.back().topSort(doSorting);
      for (auto &traitImpl : p.traitImpls) {
        InsnList &ig = res.insnLists.emplace_back();
        std::vector<Tp> allParams;
        {
          allParams.reserve(traitImpl.trait.params.size() + 1);
          {
            VarRef vr({}, 0);
            allParams.push_back(parseTyHint(traitImpl.type, vr, ig));
          }
          {
            Idx i = 1;
            for (auto &ty : traitImpl.trait.params) {
              VarRef vr({}, i++);
              allParams.push_back(parseTyHint(ty, vr, ig));
            }
          }
        }
        for (auto &block : traitImpl.methods) {
          visitRootBlock(block, ig, true);
        }
        std::vector<VarRef> retTypes;
        for (auto &rt : traitImpl.types) {
          retTypes.push_back(parseCompleteTy(rt, ig));
        }
        ig.insns.push_back(Insn(IdentityInsn::key(), {}, std::move(retTypes), {}));
        ig.retInsn = *ig.lastInsn().insn;
        ig.topSort(doSorting);
        res.table->insertFn(TraitInsn::key(), {(Idx)Builtins::Fn}, allParams, InstWrapper<std::monostate>(ig));
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

    VarRef visitRootBlock(Block &block, InsnList &ig, bool isFunction) {
      std::vector<DefIdx> vars;
      DefinitionVisitor().visitBlock(block, vars);
      if (isFunction) {
        std::vector<Tp> placeholders;
        placeholders.reserve(vars.size());
        for (Idx i = 0; i < vars.size(); ++i) {
          placeholders.push_back(tcx.intern(Ty::Placeholder{i}));
          varNodes.insert({vars[i], VarRef(ig.insns.size(), i)});
        }
        ig.insns.push_back(Insn(DeConstructInsn::key(), {}, {VarRef({}, 1)}, {}));
      } else {
        for (Idx var : vars) {
          ig.insns.push_back(Insn(TrapInsn::key(), {}, {}, {"unvisited var"})); // to modify later
          varNodes.insert({var, ig.lastInsn()});
        }
      }
      for (Idx var : vars) {
        VarRef &node = varNodes.at(var);
        Definition &def = program->bindings.at(var);
        auto &varDef = std::get<DefType::Variable>(def.defType.v);
        for (auto &hint : varDef.hints) {
          parseTyHint(hint, node, ig);
        }
      }
      VarRef ret({}, 0);
      for (Eptr &expr : block.body) {
        ret = visitExpr(*expr, ig);
      }
      return ret;
    }
    
    Tp parseTyTemplate(Type &ty, std::vector<std::optional<DefIdx>> &outIdcs) {
      Idx counter = 0;
      std::map<DefIdx, Tp> placeholders;
      std::function<Tp(Type&)> visitTy;
      auto visitParams = [&](Type &ty) {
        std::vector<Tp> params;
        for (Type &tp : ty.params) {
          params.push_back(visitTy(tp));
        }
        return params;
      };
      visitTy = [&](Type &ty) {
        if (!ty.base) {
          outIdcs.emplace_back();
          return tcx.intern(Ty::Placeholder{counter++});
        }
        DefIdx base = *ty.base;
        if (base < Builtins::LAST_BUILTIN_) {
          switch (base) {
            case Fn: case Add: case Sub: case Mul: case Div: case Rem:
            case BitOr: case BitAnd: case Eq: case Cmp: case Neg:
              throw std::runtime_error("Trait type hints unimplemented");
            case BOOL:
              return tcx.intern(Ty::Bool{});
            case I8: case I16: case I32: case I64: case I128:
              return tcx.intern(Ty::Int{(type::IntSize)(base - I8)});
            case U8: case U16: case U32: case U64: case U128:
              return tcx.intern(Ty::UInt{(type::IntSize)(base - U8)});
            case F16: case F32: case F64:
              return tcx.intern(Ty::Float{(type::FloatSize)(base - F16)});
            case TUPLE:
              return tcx.intern(Ty::Tuple{visitParams(ty)});
            case FFIFN: {
              auto params = visitParams(ty);
              return tcx.intern(Ty::FfiFn{params.at(0), params.at(1)});
            }
            case STRING: case NULSTRING:
              return tcx.intern(Ty::String{base == NULSTRING});
            default:
              throw std::runtime_error("ICE: Unreachable reached");
          }
        } else {
          auto &def = program->bindings.at(base);
          return std::visit(overloaded{
              [&](DefType::Type &) -> Tp {
                if (!placeholders[base]) {
                  outIdcs.emplace_back(base);
                  placeholders[base] = tcx.intern(Ty::Placeholder{counter++});
                }
                return placeholders[base];
              },
              [&](DefType::ADT &) -> Tp {
                return tcx.intern(Ty::ADT{base, {0}, visitParams(ty)});
              },
              [](DefType::Trait &) -> Tp {
                throw std::runtime_error("Trait type hints unimplemented");
              },
              [](DefType::Variable &) -> Tp {
                throw std::runtime_error("ICE: Unexpected definition kind while parsing type");
              }
          }, def.defType.v);
        }
      };
      return visitTy(ty);
    }

    VarRef parseCompleteTy(Type &ty, InsnList &ig) {
      std::vector<std::optional<DefIdx>> idcs;
      Tp tmpl = parseTyTemplate(ty, idcs);
      std::vector<VarRef> inputs;
      inputs.reserve(idcs.size());
      for (auto &e : idcs) {
        if (!e || !tyNodes.count(*e)) {
          throw std::runtime_error("ICE: Incomplete type");
        }
        inputs.push_back(tyNodes.at(*e));
      }
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, std::move(inputs), {tmpl}, "from type", ty.source));
      return ig.lastInsn();
    }

    Tp parseTyHint(Type &ty, VarRef &node, InsnList &ig) {
      std::vector<std::optional<DefIdx>> idcs;
      Tp tmpl = parseTyTemplate(ty, idcs);
      bool doDeconstruct = false;
      for (auto &entry : idcs) {
        if (!entry || !tyNodes.count(*entry)) {
          doDeconstruct = true;
          break;
        }
      }
      if (doDeconstruct) {
        ig.insns.push_back(Insn(DeConstructInsn::key(), {}, {node}, {tmpl}, "deconstructed from type hint", ty.source));
      }
      std::vector<VarRef> constructed;
      constructed.reserve(idcs.size());
      Idx i = 0;
      for (auto &entry : idcs) {
        if (entry) {
          auto found = tyNodes.find(*entry);
          if (found != tyNodes.end()) {
            constructed.push_back(found->second);
            goto cont;
          } else {
            if (!doDeconstruct) throw std::runtime_error("ICE: Incredibly shocked and confused");
            tyNodes.insert({*entry, ig.lastInsn(i)});
          }
        }
        if (!doDeconstruct) throw std::runtime_error("ICE: Incredibly shocked and confused");
        constructed.push_back(ig.lastInsn(i));

       cont:
        i++;
      }
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, std::move(constructed), {tmpl}, "constructed from type hint", ty.source));
      VarRef reconstructed = ig.lastInsn();
      ig.insns.push_back(Insn(CheckInsn::key(), {}, {node, reconstructed}, {}, "check from type hint", ty.source));
      node = reconstructed;
      return tmpl;
    }

    bool foregoHints = false;
    VarRef visitExpr ARGS(Expr) override {
      VarRef node = ExprVisitor::visitExpr(e PASS_ARGS);
      if (foregoHints) {
        foregoHints = false;
      } else {
        for (auto &hint : e.hints) {
          parseTyHint(hint, node, ig);
        }
      }
      auto logFn = [](std::monostate &ms, const std::vector<Tp> &tys) {
        std::cout << tys.at(0);
      };
      InstWrapper<std::monostate>::LogInsn<decltype(logFn)> log(std::move(logFn));
      ig.insns.push_back(Insn(DynInsn::key(), {}, {node}, {(type::infer::Fn) log}, {}, e.span));
      return node;
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
      ig.insns.push_back(Insn(TraitInsn::key(), {trait}, {lhsNode, rhsNode}, {}, "result of operation", e.span));
      return ig.lastInsn(0);
    }
    RET_T visitCmpExpr ARGS(CmpExpr) override {
      VarRef lhsTy = visitExpr(*e.lhs PASS_ARGS);
      VarRef rhsTy = visitExpr(*e.rhs PASS_ARGS);
      ig.insns.push_back(Insn(TraitInsn::key(), {Cmp}, {lhsTy, rhsTy}, {}, "comparison made", e.span));
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, {}, {tcx.intern(Ty::Bool{})}, "result of comparison", e.span));
      return ig.lastInsn();
    }
    RET_T visitNegExpr ARGS(NegExpr) override {
      VarRef valTy = visitExpr(*e.value PASS_ARGS);
      ig.insns.push_back(Insn(TraitInsn::key(), {Neg}, {valTy}, {}, "result of negation", e.span));
      return ig.lastInsn(0);
    }
    RET_T visitCallExpr ARGS(CallExpr) override {
      VarRef fnTy = visitExpr(*e.func PASS_ARGS);
      std::vector<VarRef> argTys;
      std::vector<Tp> placeholders;
      Idx i = 0;
      for (Eptr &arg : e.args) {
        argTys.push_back(visitExpr(*arg PASS_ARGS));
        placeholders.push_back(tcx.intern(Ty::Placeholder{i++}));
      }
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, std::move(argTys), {tcx.intern(Ty::Tuple{std::move(placeholders)})}));
      VarRef tupleTy = ig.lastInsn();
      ig.insns.push_back(Insn(TraitInsn::key(), {(Idx)Builtins::Fn}, {fnTy, tupleTy}, {}, "function called", e.span));
      return ig.lastInsn(0);
    }
    RET_T visitDefineExpr ARGS(DefineExpr) override {
      VarRef varTy = visitExpr(*e.value PASS_ARGS);
      VarRef &var = varNodes.at(e.idx);
      ig.insns.at(*var.insn) = Insn(IdentityInsn::key(), {}, {varTy}, {}, "definition", e.span);
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
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, std::move(args), {tyTemplate}, "construction", e.span));
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
      ig.insns.push_back(Insn(DeConstructInsn::key(), {}, {objTy}, {tyTemplate}, "field taken", e.span));
      return ig.lastInsn(e.field);
    }
    RET_T visitForeignExpr ARGS(ForeignExpr) override {
      ig.insns.push_back(Insn(TrapInsn::key(), {}, {}, {"unimplemented foreign"}, "unimplemented foreign", e.span));
      VarRef origNode = ig.lastInsn();
      VarRef updatedNode = origNode;
      for (auto &hint : e.hints) {
        parseTyHint(hint, updatedNode, ig);
      }
      ig.insns.at(*origNode.insn) = Insn(IdentityInsn::key(), {}, {updatedNode}, {}, "foreign value", e.span);
      foregoHints = true;
      return updatedNode;
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
