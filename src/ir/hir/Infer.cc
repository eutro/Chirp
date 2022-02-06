#include "Infer.h"

#include "Builtins.h"
#include "IdxCounter.h"
#include "../../common/Logging.h"

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

    InferenceVisitor(type::Tcx &ttcx):
      tcx(ttcx)
    {
      instSet->tcx = &tcx;
    }

    err::ErrorContext ecx;
    Program *program = nullptr;
    std::map<DefIdx, VarRef> varNodes;
    std::map<DefIdx, VarRef> tyNodes;

    std::shared_ptr<type::infer::Inst::ConstructingSet> instSet =
      std::make_shared<decltype(instSet)::element_type>();

    void addBuiltins(LookupTable &sys) {
      Idx igIdx = BUILTIN_BLOCKS_START;
      sys.insertFn(TraitInsn::key(), {(Idx) Builtins::Fn},
                   {tcx.intern(Ty::FfiFn{tcx.intern(Ty::Placeholder{0}),
                                         tcx.intern(Ty::Placeholder{1})}),
                    tcx.intern(Ty::Placeholder{0})},
                   InstWrapper(
                       [](const std::vector<Tp> &tys, const auto &) -> std::vector<Tp> {
                         LogInsn log;
                         log({tys.at(0)}, {(Idx)0});
                         auto &ffifn = std::get<Ty::FfiFn>(tys.at(0)->v);
                         CheckInsn check;
                         check({tys.at(1), ffifn.args}, {});
                         return {ffifn.ret};
                       },
                       1, igIdx++, instSet)
      );

      auto constFn = [&](const std::vector<Tp> &i) {
        return InstWrapper(ConstInsn(i), 0, igIdx++, instSet);
      };
      auto implBinOp = [&](Tp ty, Idx trait) {
        sys.insertFn(TraitInsn::key(), {trait}, {ty, ty}, constFn({ty}));
      };
      auto implNeg = [&](Tp ty) {
        sys.insertFn(TraitInsn::key(), {(Idx)Neg}, {ty}, constFn({ty}));
      };
      auto implEq = [&](Tp ty) {
        sys.insertFn(TraitInsn::key(), {(Idx)Eq}, {ty, ty}, constFn({}));
      };
      auto implCmp = [&](Tp ty) {
        sys.insertFn(TraitInsn::key(), {(Idx)Cmp}, {ty, ty}, constFn({}));
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

      sys.insertFn(LookupKey::intern("union-dispatch"), {},{},
                   InstWrapper(
                       [](const std::vector<Tp> &tys, const std::vector<Constant> &cs) -> std::vector<Tp> {
                         Idx splitUnion = constant_cast<Idx>(cs.at(1));
                         auto &lookup = *constant_cast<std::function<type::infer::Fn(const std::vector<Tp> &)>*>(cs.at(2));
                         std::vector<Tp> callTys(tys);
                         Tp dispatchingTy = type::uncycle(callTys.at(splitUnion));
                         auto &_tcx = *dispatchingTy->tcx;
                         auto &uTy = std::get<Ty::Union>(dispatchingTy->v);
                         std::vector<std::vector<Tp>> rets;
                         {
                           Idx i = 0;
                           for (Tp ty : uTy.tys) {
                             callTys[splitUnion] = ty;
                             rets.push_back(lookup(callTys)(callTys, {i++}));
                           }
                         }
                         std::vector<std::vector<Tp>> transpose(rets.front().size(), std::vector<Tp>(rets.size()));
                         for (Idx x = 0; x < rets.size(); ++x) {
                           for (Idx y = 0; y < rets[x].size(); ++y) {
                             transpose[y][x] = rets[x][y];
                           }
                         }
                         std::vector<Tp> unionedRets;
                         unionedRets.reserve(transpose.size());
                         for (auto &t : transpose) {
                           unionedRets.push_back(type::unionOf(_tcx, t));
                         }
                         std::vector<Constant> logIdcs;
                         std::vector<Tp> logVals;
                         {
                           Idx i = 0;
                           for (Tp ty : tys) {
                             logIdcs.emplace_back(i++);
                             logVals.push_back(ty);
                           }
                           logIdcs.emplace_back(i++);
                           logVals.push_back(_tcx.intern(Ty::Placeholder{splitUnion}));
                           for (Tp retTy : unionedRets) {
                             logIdcs.emplace_back(i++);
                             logVals.push_back(retTy);
                           }
                           logIdcs.emplace_back(i++);
                           logVals.push_back(_tcx.intern(Ty::Placeholder{0}));
                           for (auto &retSet : transpose) {
                             logIdcs.emplace_back(i++);
                             logVals.push_back(_tcx.intern(Ty::Tuple{retSet}));
                           }
                         }
                         LogInsn log;
                         // [arg0, ..., argN, Placeholder{dispatchedUnion}, ret0, ..., retM, Placeholder{0}, fRet0, ... fRetM]
                         log(logVals, logIdcs);
                         return unionedRets;
                       },
                       // return count set to 1; at the time of writing, this is correct in all cases, but undesirable
                       1,
                       igIdx++, instSet)
      );
    }

    void doSorting(InsnList &il, const std::vector<Insn *> &insns) {
      Insn *frontInsn = insns.front();
      if (insns.size() > 1 ||
          std::any_of(frontInsn->inputs.begin(), frontInsn->inputs.end(),
                      [&](VarRef &vr) { return vr.insn && &il.insns.at(*vr.insn) == frontInsn; })) {
        std::map<Tp, VarRef> externs;
        std::map<VarRef, Tp> externsRev;
        std::map<Insn *, std::vector<Tp>> tys;
        std::map<Tp, Tp> values;
        for (Insn *insn : insns) tys[insn];
        Idx counter = 0;
        auto setOutputs = [&](Insn *insn, const std::vector<Tp> &outs) {
          std::vector<Tp> &vars = tys[insn];
          vars.reserve(outs.size());
          while (vars.size() < outs.size()) {
            vars.push_back(tcx.intern(Ty::Placeholder{counter++}));
          }
          for (Idx i = 0; i < vars.size(); ++i) {
            values[vars[i]] = outs[i];
          }
        };
        auto getVar = [&](VarRef vr) {
          if (vr.insn) {
            auto found = tys.find(&il.insns.at(*vr.insn));
            if (found != tys.end()) {
              while (found->second.size() <= vr.retIdx) {
                found->second.push_back(tcx.intern(Ty::Placeholder{counter++}));
              }
              return found->second.at(vr.retIdx);
            }
          }
          if (externsRev.count(vr)) {
            return externsRev.at(vr);
          } else {
            Tp tp = tcx.intern(Ty::Placeholder{counter++});
            externs.insert({tp, vr});
            return externsRev[vr] = tp;
          }
        };
        LookupKey::P idKey = IdentityInsn::key();
        LookupKey::P constructKey = ConstructInsn::key();
        for (Insn *insn : insns) {
          std::vector<Tp> inputs;
          inputs.reserve(insn->inputs.size());
          for (auto in : insn->inputs) {
            inputs.push_back(getVar(in));
          }
          if (insn->key == idKey) {
            setOutputs(insn, inputs);
          } else if (insn->key == constructKey) {
            ConstructInsn construct;
            setOutputs(insn, construct(inputs, insn->constArgs));
          } else {
            logging::CHIRP.debug("InsnList:\n", il, "\n");
            err::Location loc;
            loc.maybeSpan(insn->src, "Suggestion: add type hints here");
            loc.msg("Instructions in cycle:");
            std::map<Insn *, Idx> idcs;
            Idx idx = insns.size();
            for (Insn *i : insns) idcs[i] = idx--;
            for (auto iit = insns.rbegin(); iit != insns.rend(); ++iit) {
              auto i = *iit;
              std::stringstream ss;
              if (i->src && i->reason) {
                ss << idcs[i] << ". " << *i->reason << " ('" << i->key->value << "' instruction" ;
                if (i->key != constructKey && i->key != idKey) {
                  ss << ", not allowed";
                }
                ss << ")";
              } else {
                ss << " " << idcs[i] << ". '" << i->key->value << "' instruction" ;
                if (!i->constArgs.empty()) {
                  ss << " with constants";
                  for (auto &c : i->constArgs) {
                    ss << " " << c;
                  }
                }
                if (i->key != constructKey && i->key != idKey) {
                  ss << "; not allowed";
                }
              }
              std::vector<Idx> deps;
              for (auto &input : i->inputs) {
                if (input.insn) {
                  Insn *depInsn = &il.insns.at(*input.insn);
                  if (idcs.count(depInsn)) {
                    deps.push_back(idcs.at(depInsn));
                  }
                }
              }
              if (!deps.empty()) {
                ss << "; depends on: ";
                for (auto it = deps.begin(); it != deps.end();) {
                  ss << *it;
                  if (++it != deps.end()) {
                    ss << ", ";
                  }
                }
              }
              loc.maybeSpan(i->src, ss.str());
            }
            throw err::LocationError(util::toStr("Bad instruction in cycle: '", insn->key->value, "'"), {loc});
          }
        }

        for (auto &e : tys) {
          std::vector<VarRef> externalInputs;
          for (auto &t : e.second) {
            bool cyclic = false;
            Idx cycleDepth = 0;
            std::function<Tp(Tp)> doReplace;
            auto replaceFn = overloaded {
              [&](Tp ty, type::PreWalk) {
                if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
                  cycleDepth++;
                }
                return ty;
              },
              [&](Tp ty, type::PostWalk) {
                if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
                  cycleDepth--;
                }
                return ty;
              },
              [&](Tp tp) {
                if (std::holds_alternative<Ty::Placeholder>(tp->v)) {
                  if (externs.count(tp)) {
                    externalInputs.push_back(externs.at(tp));
                    return tcx.intern(Ty::Placeholder{(Idx) externalInputs.size() - 1});
                  } else if (tp == t) {
                    cyclic = true;
                    return tcx.intern(Ty::CyclicRef{cycleDepth});
                  } else {
                    return doReplace(values.at(tp));
                  }
                }
                return tp;
              }
            };
            doReplace = [&](Tp ty) { return type::replaceTy(tcx, ty, replaceFn); };
            Tp replaced = doReplace(values.at(t));
            if (cyclic) {
              replaced = tcx.intern(Ty::Cyclic{replaced});
            }
            t = replaced;
          }
          e.first->key = ConstructInsn::key();
          e.first->inputs = std::move(externalInputs);
          e.first->constArgs.clear();
          std::copy(e.second.begin(), e.second.end(), std::back_inserter(e.first->constArgs));
        }
      }
    }

    void addTypeNames() {
      for (auto &b : program->bindings) {
        if (std::holds_alternative<DefType::ADT>(b.second.defType.v)) {
          tcx.addName(b.first, b.second.name);
        }
      }
    }

    InferResult visitProgram(Program &p) override {
      program = &p;
      InferResult res;
      addTypeNames();
      res.insts = instSet;
      visitTopLevel(p.topLevel, res);
      addBuiltins(*res.table);
      for (auto &traitImpl : p.traitImpls) {
        visitTrait(traitImpl, res);
      }
      res.errors = std::move(ecx);
      return res;
    }

    void topSort(InsnList &ig) {
      auto sortFn = [this](auto &&...arg){ doSorting(arg...); };
      try {
        ig.optIdCons();
        ig.topSort(sortFn);
        ig.opt();
      } catch (err::LocationError &le) {
        le.addToContext(ecx);
      }
    }

    void visitTopLevel(Block &block, InferResult &res) {
      InsnList ig;
      visitRootBlock(block, res, ig, false);
      ig.insns.push_back(Insn(IdentityInsn::key(), {}, {}, {}));
      ig.retInsn = *ig.lastInsn().insn;
      topSort(ig);
      InstWrapper wrapper(ig, 0, *block.idx, instSet);
      res.root = [wrapper = std::move(wrapper), instSet = instSet](const auto &tys, const auto &cs) {
        auto ret = wrapper(tys, cs);
        // garbage collect unreferenced instantiations
        std::set<Inst::Ref> live;
        std::function<void(Inst::Ref)> mark = [&](Inst::Ref ref) {
          if (!live.insert(ref).second) return;
          for (auto &e : (*instSet)[ref].loggedRefs) {
            mark(e.second);
          }
        };
        auto &rootCalls = instSet->entities.at(wrapper.entityIdx);
        for (auto &e : rootCalls) {
          mark({wrapper.entityIdx, e.first});
        }
        for (auto &e : instSet->entities) {
          for (auto it = e.second.begin(); it != e.second.end();) {
            if (!live.count({e.first, it->first})) {
              it = e.second.erase(it);
            } else {
              ++it;
            }
          }
        }
        return ret;
      };
      tyNodes.clear();
    }

    void visitTrait(TraitImpl &ti, InferResult &res) {
      logging::CHIRP.trace("Visiting trait at ", ti.source, "\n");
      InsnList ig;
      std::vector<Tp> allParams;
      {
        allParams.reserve(ti.trait.params.size() + 1);
        {
          VarRef vr({}, 0);
          allParams.push_back(parseTyHint(ti.type, vr, ig));
        }
        {
          Idx i = 1;
          for (auto &ty : ti.trait.params) {
            VarRef vr({}, i++);
            allParams.push_back(parseTyHint(ty, vr, ig));
          }
        }
      }
      for (auto &block : ti.methods) {
        visitRootBlock(block, res, ig, true);
      }
      std::vector<VarRef> retTypes;
      for (auto &rt : ti.types) {
        retTypes.push_back(parseCompleteTy(rt, ig));
      }
      ig.insns.push_back(Insn(IdentityInsn::key(), {}, std::move(retTypes), {}));
      ig.retInsn = *ig.lastInsn().insn;
      topSort(ig);
      res.table->insertFn(
        TraitInsn::key(),
        {(Idx)Builtins::Fn},
        allParams,
        InstWrapper(ig, ti.types.size(), *ti.methods.front().idx, instSet)
      );
      tyNodes.clear();
    }

    std::map<Expr*, Idx> exprTys;
    std::map<Idx, Idx> defTys;
    void visitRootBlock(Block &block, InferResult &res, InsnList &ig, bool isFunction) {
      Idx counter = 0;
      {
        TyCounter tc;
        exprTys.clear();
        tc.visitBlock(block, exprTys, counter);
      }
      {
        DefCounter dc;
        defTys.clear();
        dc.visitBlock(block, defTys, counter);
      }
      visitBlock(block, ig, isFunction);
      varNodes.clear();
    }

    VarRef visitBlock(Block &block, InsnList &ig, bool isFunction) {
      if (isFunction) {
        std::vector<Tp> placeholders;
        placeholders.reserve(block.bindings.size() - 1);
        for (Idx i = 0; i < block.bindings.size() - 1; ++i) {
          placeholders.push_back(tcx.intern(Ty::Placeholder{i}));
        }
        ig.insns.push_back(Insn(DeConstructInsn::key(), {}, {VarRef({}, 1)}, {tcx.intern(Ty::Tuple{placeholders})}));
        for (Idx i = 0; i < block.bindings.size() - 1; ++i) {
          varNodes.insert({block.bindings[i], ig.lastInsn(i)});
        }
        varNodes.insert({block.bindings.back(), VarRef({}, 0)});
      } else {
        for (Idx var : block.bindings) {
          ig.insns.push_back(Insn(TrapInsn::key(), {}, {}, {"unvisited var"})); // to modify later
          varNodes.insert({var, ig.lastInsn()});
        }
      }
      for (Idx var : block.bindings) {
        VarRef &node = varNodes.at(var);
        Definition &def = program->bindings.at(var);
        auto &varDef = std::get<DefType::Variable>(def.defType.v);
        for (auto &hint : varDef.hints) {
          parseTyHint(hint, node, ig);
        }
        ig.insns.push_back(Insn(LogInsn::key(), {}, {node}, {defTys.at(var)}));
      }
      for (Idx ty : block.typeBindings) {
        auto &tyDef = std::get<DefType::Type>(program->bindings.at(ty).defType.v);
        if (tyDef.value && tyDef.value->base) {
          ig.insns.push_back(Insn(TrapInsn::key(), {}, {}, {"unvisited var"})); // to modify later
          tyNodes.insert({ty, ig.lastInsn()});
        }
      }
      for (Idx ty : block.typeBindings) {
        auto &tyDef = std::get<DefType::Type>(program->bindings.at(ty).defType.v);
        if (tyDef.value && tyDef.value->base) {
          VarRef outTyNode = parseCompleteTy(*tyDef.value, ig);
          VarRef inTyNode = tyNodes.at(ty);
          ig.insns.at(*inTyNode.insn) = Insn(IdentityInsn::key(), {}, {outTyNode}, {});
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
      std::set<DefIdx> stackedRefSet;
      std::vector<DefIdx> stackedRefs;
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
        auto doIt = [&]() {
          if (base < Builtins::LAST_BUILTIN_) {
            switch (base) {
              case Fn:
              case Add:
              case Sub:
              case Mul:
              case Div:
              case Rem:
              case BitOr:
              case BitAnd:
              case Eq:
              case Cmp:
              case Neg:
                throw std::runtime_error("Trait type hints unimplemented");
              case BOOL:
                return tcx.intern(Ty::Bool{});
              case I8:
              case I16:
              case I32:
              case I64:
              case I128:
                return tcx.intern(Ty::Int{(type::IntSize) (base - I8)});
              case U8:
              case U16:
              case U32:
              case U64:
              case U128:
                return tcx.intern(Ty::UInt{(type::IntSize) (base - U8)});
              case F16:
              case F32:
              case F64:
                return tcx.intern(Ty::Float{(type::FloatSize) (base - F16)});
              case TUPLE:
                return tcx.intern(Ty::Tuple{visitParams(ty)});
              case UNION:
                return type::unionOf(tcx, visitParams(ty));
              case FFIFN: {
                auto params = visitParams(ty);
                return tcx.intern(Ty::FfiFn{params.at(0), params.at(1)});
              }
              case STRING:
              case NULSTRING:
                return tcx.intern(Ty::String{base == NULSTRING});
              case TYPETOKEN: {
                auto params = visitParams(ty);
                return tcx.intern(Ty::TypeToken{params.at(0)});
              }
              default:
                throw util::Unreachable();
            }
          } else {
            auto &def = program->bindings.at(base);
            return std::visit(overloaded{
                [&](DefType::Type &t) -> Tp {
                  if (stackedRefSet.count(base)) {
                    auto found = std::find(stackedRefs.rbegin(), stackedRefs.rend(), base);
                    Idx depth = std::distance(stackedRefs.rbegin(), found);
                    return tcx.intern(Ty::CyclicRef{depth});
                  }
                  if (!placeholders.count(base)) {
                    outIdcs.emplace_back(base);
                    placeholders[base] = tcx.intern(Ty::Placeholder{counter++});
                  }
                  return placeholders[base];
                },
                [&](DefType::ADT &a) -> Tp {
                  std::vector<Tp> params = visitParams(ty);
                  std::vector<Tp> fields;
                  for (auto &t : a.fields) {
                    fields.push_back(visitTy(t));
                  }
                  return tcx.intern(Ty::ADT{base, std::move(params), std::move(fields)});
                },
                [](DefType::Trait &) -> Tp {
                  throw std::runtime_error("Trait type hints unimplemented");
                },
                [](DefType::Variable &) -> Tp {
                  throw util::ICE("Unexpected definition kind while parsing type");
                }
            }, def.defType.v);
          }
        };
        if (ty.self) {
          stackedRefs.push_back(*ty.self);
          stackedRefSet.insert(*ty.self);
        }
        Tp ret = doIt();
        if (ty.self) {
          ret = tcx.intern(Ty::Cyclic{ret});
          stackedRefSet.erase(*ty.self);
          stackedRefs.pop_back();
        }
        return ret;
      };
      return visitTy(ty);
    }

    VarRef parseCompleteTy(Type &ty, InsnList &ig) {
      logging::CHIRP.trace("Parsing type at: ", ty.source, "\n");
      std::vector<std::optional<DefIdx>> idcs;
      Tp tmpl = parseTyTemplate(ty, idcs);
      std::vector<VarRef> inputs;
      inputs.reserve(idcs.size());
      for (auto &e : idcs) {
        if (!e || !tyNodes.count(*e)) {
          Definition &def = program->bindings.at(*e);
          if (std::holds_alternative<DefType::Type>(def.defType.v)) { // should be always
            if (auto val = std::get<DefType::Type>(def.defType.v).value) {
              tyNodes.insert({*e, parseCompleteTy(*val, ig)}); // FIXME this is uh very fragile
              goto notFail;
            }
          }
          throw util::ICE("Incomplete type");
        }
       notFail:
        inputs.push_back(tyNodes.at(*e));
      }
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, std::move(inputs), {tmpl}, "from type", ty.source));
      return ig.lastInsn();
    }

    Tp parseTyHint(Type &ty, VarRef &node, InsnList &ig) {
      logging::CHIRP.trace("Parsing type hint at: ", ty.source, "\n");
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
            if (!doDeconstruct) throw util::ICE("Incredibly shocked and confused");
            tyNodes.insert({*entry, ig.lastInsn(i)});
          }
        }
        if (!doDeconstruct) throw util::ICE("Incredibly shocked and confused");
        constructed.push_back(ig.lastInsn(i));

       cont:
        i++;
      }
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, std::move(constructed), {tmpl}, "constructed from type hint", ty.source));
      VarRef reconstructed = ig.lastInsn();
      ig.insns.push_back(Insn(CheckInsn::key(), {}, {reconstructed, node}, {}, "check from type hint", ty.source));
      node = reconstructed;
      logging::CHIRP.trace("Parsed hint\n", ig, "\n");
      return tmpl;
    }

    bool foregoHints = false;
    VarRef visitExpr ARGS(Expr) override {
      Idx id = exprTys.at(&e);
      VarRef node = ExprVisitor::visitExpr(e PASS_ARGS);
      if (foregoHints) {
        foregoHints = false;
      } else {
        for (auto &hint : e.hints) {
          parseTyHint(hint, node, ig);
        }
      }
      ig.insns.push_back(Insn(LogInsn::key(), {}, {node}, {id}, {}, e.span));
      return node;
    }
    RET_T visitBlockExpr ARGS(BlockExpr) override {
      return visitBlock(e.block, ig, false);
    }
    RET_T visitVarExpr ARGS(VarExpr) override {
      if (std::holds_alternative<DefType::Type>(program->bindings.at(e.ref).defType.v)) {
        Tp tyTemplate = tcx.intern(Ty::TypeToken{tcx.intern(Ty::Placeholder{0})});
        Type ty;
        ty.source = e.span;
        ty.base = e.ref;
        ig.insns.push_back(Insn(ConstructInsn::key(), {}, {parseCompleteTy(ty, ig)},
                                {tyTemplate}, "type referenced", e.span));
        return ig.lastInsn();
      }
      return varNodes.at(e.ref);
    }
    RET_T visitCondExpr ARGS(CondExpr) override {
      VarRef predTy = visitExpr(*e.predE PASS_ARGS);
      ig.insns.push_back(Insn(CheckInsn::key(), {}, {predTy}, {tcx.intern(Ty::Bool{})}));
      VarRef thenTy = visitExpr(*e.thenE PASS_ARGS);
      VarRef elseTy = visitExpr(*e.elseE PASS_ARGS);
      if (e.pos == Pos::Stmt) {
        ig.insns.push_back(Insn(ConstructInsn::key(), {}, {}, {tcx.intern(Ty::Tuple{})}));
      } else {
        ig.insns.push_back(Insn(UnionInsn::key(), {}, {thenTy, elseTy}, {}));
      }
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
        throw util::Unreachable();
      })();
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, {}, {ty}));
      return ig.lastInsn();
    }
    RET_T visitBoolExpr ARGS(BoolExpr) override {
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, {}, {tcx.intern(Ty::Bool{})}));
      return ig.lastInsn();
    }
    RET_T visitBinExpr ARGS(BinExpr) override {
      Idx id = exprTys.at(&e);
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
        default: throw util::ICE("Invalid binary expression type");
      }
      ig.insns.push_back(Insn(TraitInsn::key(), {trait}, {lhsNode, rhsNode}, {id}, "result of operation", e.span));
      return ig.lastInsn(0);
    }
    RET_T visitCmpExpr ARGS(CmpExpr) override {
      Idx id = exprTys.at(&e);
      VarRef lhsTy = visitExpr(*e.lhs PASS_ARGS);
      VarRef rhsTy = visitExpr(*e.rhs PASS_ARGS);
      ig.insns.push_back(Insn(TraitInsn::key(), {e.op <= CmpExpr::Eq ? Eq : Cmp}, {lhsTy, rhsTy}, {id}, "comparison made", e.span));
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, {}, {tcx.intern(Ty::Bool{})}, "result of comparison", e.span));
      return ig.lastInsn();
    }
    RET_T visitNegExpr ARGS(NegExpr) override {
      Idx id = exprTys.at(&e);
      VarRef valTy = visitExpr(*e.value PASS_ARGS);
      ig.insns.push_back(Insn(TraitInsn::key(), {Neg}, {valTy}, {id}, "result of negation", e.span));
      return ig.lastInsn(0);
    }
    RET_T visitCallExpr ARGS(CallExpr) override {
      Idx id = exprTys.at(&e);
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
      ig.insns.push_back(Insn(TraitInsn::key(), {(Idx)Builtins::Fn}, {fnTy, tupleTy}, {id}, "function called", e.span));
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
      args.reserve(e.types.size() + e.values.size());
      std::vector<Tp> fieldTys;
      fieldTys.reserve(e.values.size());
      std::vector<Tp> paramTys;
      paramTys.reserve(e.types.size());
      Idx i = 0;
      for (const auto &se : e.values) {
        args.push_back(visitExpr(*se PASS_ARGS));
        fieldTys.push_back(tcx.intern(Ty::Placeholder{i++}));
      }
      for (auto &t : e.types) {
        args.push_back(parseCompleteTy(t, ig));
        paramTys.push_back(tcx.intern(Ty::Placeholder{i++}));
      }
      Tp tyTemplate = tcx.intern(Ty::ADT{e.adt, paramTys, fieldTys});
      ig.insns.push_back(Insn(ConstructInsn::key(), {}, std::move(args), {tyTemplate}, "construction", e.span));
      return ig.lastInsn();
    }
    RET_T visitGetExpr ARGS(GetExpr) override {
      VarRef objTy = visitExpr(*e.value PASS_ARGS);
      auto &adt = std::get<DefType::ADT>(program->bindings.at(e.adt).defType.v);
      std::vector<Tp> params, fields;
      size_t paramCount = adt.params.size();
      size_t fieldCount = adt.fields.size();
      Idx i = 0;
      for (; i < paramCount; ++i) {
        params.push_back(tcx.intern(Ty::Placeholder{i}));
      }
      for (; i < paramCount + fieldCount; ++i) {
        fields.push_back(tcx.intern(Ty::Placeholder{i}));
      }
      Tp tyTemplate = tcx.intern(Ty::ADT{e.adt, std::move(params), std::move(fields)});
      ig.insns.push_back(Insn(DeConstructInsn::key(), {}, {objTy}, {tyTemplate}, "field taken", e.span));
      return ig.lastInsn(paramCount + e.field);
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

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor(type::Tcx &ttcx) {
    return std::make_unique<InferenceVisitor>(ttcx);
  }
}
