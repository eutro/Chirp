#include "Infer.h"

#include "Builtins.h"

#include <sstream>
#include <cstddef>
#include <deque>

namespace hir::infer {
  struct Constraints {
    arena::Arena<std::map<Tp, Tp>> instantiations;
    std::map<Tp, std::set<TraitBound *>> traitBounds;
    std::vector<std::pair<Tp, Tp>> unions;
  };

  struct AbstractTraitImpl {
    Tp ty;
    TraitBound *bound;
    Constraints constraints;
    std::vector<Tp> refs;
  };

  struct TyTB { // concrete trait impl if you will
    Tp ty;
    TraitBound *tb;
    bool operator<(const TyTB &o) const {
      return std::make_tuple(ty, tb) < std::make_tuple(o.ty, o.tb);
    }
  };

  using TypeReplacer = std::function<Tp(Tp)>;

  class InferenceVisitor :
    public ExprVisitor<Tp>,
    public ProgramVisitor<InferResult> {
  public:
    HashInternArena<Ty> tcx;
    HashInternArena<type::TraitBound> tbcx;
    arena::Arena<AbstractTraitImpl> aticx;

    std::map<DefIdx, Tp> definedTys;

    std::map<Tp, err::Location> typeSources;

    err::ErrorContext ecx;

    std::map<DefIdx, Tp> varTypes;
    std::map<Expr *, Tp> exprTypes;
    std::map<Expr *, TraitBound *> traitBoundTypes;
    std::map<Block *, std::set<Expr *>> blockExprs;
    std::map<Block *, std::set<Idx>> blockVars;

    std::map<std::pair<Idx/*trait*/, Tp/*ty*/>, AbstractTraitImpl*> directTraits;
    std::map<std::pair<Idx/*trait*/, DefIdx/*adt*/>, AbstractTraitImpl*> adtTraits;

    InferenceVisitor() {
      auto implBinOp = [this](Tp ty, Idx trait) {
        auto &ati = *(directTraits[{trait, ty}] = aticx.add());
        ati.ty = ty;
        ati.refs = {ty};
        ati.bound = tbcx.intern(TraitBound{trait, {ty}});
      };
      auto implNegCmp = [this](Tp ty) {
        auto &cmp = *(directTraits[{Cmp, ty}] = aticx.add());
        cmp.ty = ty;
        cmp.bound = tbcx.intern(TraitBound{Cmp, {ty}});

        auto &neg = *(directTraits[{Neg, ty}] = aticx.add());
        neg.ty = ty;
        neg.refs = {ty};
        neg.bound = tbcx.intern(TraitBound{Neg});
      };

      for (type::IntSize is = type::IntSize::i8;
           is <= type::IntSize::i128;
           ++(std::underlying_type<type::IntSize>::type&)is) {
        Tp i = tcx.intern(Ty::Int{is});
        Tp u = tcx.intern(Ty::UInt{is});
        for (Tp ty : {i, u}) {
          for (Builtins bt : {Add, Sub, Mul, Div, Rem, BitOr, BitAnd}) {
            implBinOp(ty, bt);
          }
          implNegCmp(ty);
        }
      }
      for (type::FloatSize fs = type::FloatSize::f16;
           fs <= type::FloatSize::f64;
           ++(std::underlying_type<type::FloatSize>::type&)fs) {
        Tp ty = tcx.intern(Ty::Float{fs});
        for (Builtins bt : {Add, Sub, Mul, Div, Rem}) {
          implBinOp(ty, bt);
        }
        implNegCmp(ty);
      }
      implBinOp(boolType(), BitAnd);
      implBinOp(boolType(), BitOr);
    }

    Program *program = nullptr;

    InferResult visitProgram(Program &p) {
      program = &p;

      std::vector<std::pair<Block *, Constraints *>> traitConstraints;

      for (TraitImpl &ti : p.traitImpls) {
        // all of these are currently assumed to be for Fn,
        // and with an ADT type
        AbstractTraitImpl &ati = *aticx.add();
        Idx startC = paramC;
        for (DefIdx param : ti.params) {
          definedTys[param] = freshType(maybeLoc(p.bindings.at(param).source, "from here"));
        }
        for (Block &m : ti.methods) {
          // assumed to be only one
          ati.constraints = visitRootBlock(m);
          traitConstraints.push_back({&m, &ati.constraints});
        }
        ati.ty = parseTy(ti.type);
        ati.bound = parseTb(ti.trait);
        ati.refs.reserve(ti.types.size());
        for (Type &ref : ti.types) {
          ati.refs.push_back(parseTy(ref));
        }
        adtTraits[{Fn, std::get<Ty::ADT>(ati.ty->v).i}] = &ati;
      }

      Constraints mainConstraints = visitRootBlock(p.topLevel);
      auto traitImplTypes = inferTypes(mainConstraints);

      InferResult res;
      auto completeBlock = [&](Block &block, Constraints &constraints) {
        if (constraints.instantiations.ptrs.empty()) {
          return;
        }

        auto &blockInsts = res.insts[&block];

        std::map<Tp, Idx> typeIdx;
        std::map<TraitBound *, Idx> traitIdx;
        Idx typeCount = 0;
        Idx traitCount = 0;
        {
          auto getIdx = [](auto &map, auto &ty, Idx &count) -> Idx {
            auto found = map.find(ty);
            if (found == map.end()) {
              found = map.insert({ty, count++}).first;
            }
            return found->second;
          };
          for (auto &e : blockExprs.at(&block)) {
            blockInsts.exprTypes[e] = getIdx(typeIdx, exprTypes[e], typeCount);
            if (traitBoundTypes.count(e)) {
              blockInsts.traitTypes[e] = getIdx(traitIdx, traitBoundTypes[e], traitCount);
            }
          }
          for (auto &v : blockVars.at(&block)) {
            blockInsts.varTypes[v] = getIdx(typeIdx, varTypes[v], typeCount);
          }
        }

        struct Insts {
          std::vector<Tp> tys;
          std::vector<TraitBound *> tbs;
          bool operator<(const Insts &o) const {
            return std::make_pair(tys, tbs) < std::make_pair(o.tys, o.tbs);
          }
        };

        std::set<Insts> instSet;
        for (auto &complete : constraints.instantiations.ptrs) {
          TypeReplacer replacer = [&](Tp ty) -> Tp {
            if (std::holds_alternative<Ty::TraitRef>(ty->v)) {
              Ty::TraitRef &trf = std::get<Ty::TraitRef>(ty->v);
              if (!isComplete(trf.ty) || !isComplete(trf.trait)) {
                return tcx.intern(Ty::Err{});
              }
              auto &idces = traitImplTypes.at({trf.ty, trf.trait});
              if (trf.ref >= idces.size()) {
                // propagated
                return tcx.intern(Ty::Err{});
              }
              return idces[trf.ref];
            } else if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
              return complete->at(ty);
            }
            return ty;
          };

          Insts thisInst = Insts {
            .tys = std::vector<Tp>(typeCount, nullptr),
            .tbs = std::vector<TraitBound *>(traitCount, nullptr),
          };
          auto putIdx = [&](auto &instTypes, auto &genericTypes,
                            auto &thisInst,
                            auto &v) {
            Idx i = instTypes.at(v);
            auto &thisTy = thisInst.at(i);
            if (!thisTy) {
              thisTy = replaceTy(genericTypes[v], replacer);
            }
          };
          for (Expr *e : blockExprs[&block]) {
            putIdx(blockInsts.exprTypes, exprTypes, thisInst.tys, e);
            if (traitBoundTypes.count(e)) {
              putIdx(blockInsts.traitTypes, traitBoundTypes, thisInst.tbs, e);
            }
          }
          for (Idx v : blockVars[&block]) {
            putIdx(blockInsts.varTypes, varTypes, thisInst.tys, v);
          }
          instSet.insert(std::move(thisInst));
        }
        blockInsts.types.reserve(instSet.size());
        for (auto &inst : instSet) {
          blockInsts.types.push_back(inst.tys);
          blockInsts.traitBounds.push_back(inst.tbs);
        }
      };
      completeBlock(p.topLevel, mainConstraints);
      for (auto &pair : traitConstraints) {
        completeBlock(*pair.first, *pair.second);
      }
      res.errors = std::move(ecx);
      res.tcx = std::move(tcx);
      return res;
    }

    AbstractTraitImpl *lookupAti(Idx trait, Tp ty) {
      ty = uncycle(ty);
      auto directFound = directTraits.find({trait, ty});
      if (directFound != directTraits.end()) {
        return directFound->second;
      }
      if (std::holds_alternative<Ty::ADT>(ty->v)) {
        DefIdx adtI = std::get<Ty::ADT>(ty->v).i;
        auto adtFound = adtTraits.find({trait, adtI});
        if (adtFound != adtTraits.end()) {
          return adtFound->second;
        }
      }
      return nullptr;
    }

    std::optional<err::Location> maybeLoc(std::optional<loc::Span> maybeSpan,
                                          const std::string &msg) {
      if (maybeSpan) {
        return std::move(err::Location().span(*maybeSpan, msg));
      } else {
        return std::nullopt;
      }
    }

    void mergeSources(Tp target, Tp from) {
      auto foundSource = typeSources.find(from);
      if (foundSource != typeSources.end()) {
        err::Location *loc;
        if (typeSources.count(target)) {
          loc = &typeSources[target];
          loc->msg("which is the same as");
        } else {
          loc = &typeSources[target];
        }
        loc->chain(foundSource->second);
      }
    }

    std::vector<Tp> parseTyParams(std::vector<Type> params) {
      std::vector<Tp> ret;
      ret.reserve(params.size());
      for (Type &ty : params) {
        ret.push_back(parseTy(ty));
      }
      return ret;
    }

    Tp getDefinedType(DefIdx id) {
      auto found = definedTys.find(id);
      if (found != definedTys.end()) {
        return found->second;
      }
      auto &def = program->bindings.at(id);
      if (!std::holds_alternative<DefType::Type>(def.defType.v)) {
        throw std::runtime_error("ICE: Not a type");
      }
      return definedTys[id] = freshType(maybeLoc(def.source, "here"));
    }

    std::optional<Tp> maybeParseTy(Type &ty) {
      DefIdx idx = *ty.base;

      // builtins
      switch (idx) {
      case BOOL:
        return boolType();
      case I8: case I16: case I32: case I64:
        return tcx.intern(Ty::Int{(type::IntSize) (idx - U8)});
      case U8: case U16: case U32: case U64:
        return tcx.intern(Ty::UInt{(type::IntSize) (idx - U8)});
      case F16: case F32: case F64:
        return tcx.intern(Ty::Float{(type::FloatSize) (idx - F16)});
        // no FN,
      case TUPLE:
        return tcx.intern(Ty::Tuple{parseTyParams(ty.params)});
      }

      auto &def = program->bindings.at(idx);
      if (std::holds_alternative<DefType::Type>(def.defType.v)) {
        return getDefinedType(idx);
      } if (std::holds_alternative<DefType::ADT>(def.defType.v)) {
        auto &adt = std::get<DefType::ADT>(def.defType.v);
        type::Variants variants;
        for (Idx i = 0; i < adt.variants.size(); ++i) {
          variants.insert(i);
        }
        return tcx.intern(Ty::ADT{idx, variants, parseTyParams(ty.params)});
      }

      return std::nullopt;
    }

    void parseTypeHint(Tp target, Type &hint) {
      if (!hint.base) {
        return; // explicit placeholder hint
      }

      auto type = maybeParseTy(hint);
      if (type) {
        constrainUnite(target, *type);
        return;
      }

      auto found = program->bindings.find(*hint.base);
      if (found != program->bindings.end() &&
          std::holds_alternative<DefType::Trait>(found->second.defType.v)) {
        constrainTrait(target, parseTb(hint));
      } else {
        throw std::runtime_error("ICE: index is not a type or trait");
      }
    }

    Tp parseTy(Type &ty) {
      if (ty.base) {
        auto type = maybeParseTy(ty);
        if (type) {
          return *type;
        }
        ecx.err().maybeSpan(ty.source, "Undefined type");
      }
      return freshType(maybeLoc(ty.source, "here"));
    }

    TraitBound *parseTb(Type &ty) {
      return tbcx.intern(TraitBound{ty.base.value(), parseTyParams(ty.params)});
    }

    Tp uncycle(Tp ty) {
      if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
        Idx depth = 0;
        auto uncycler = overloaded {
          [&](Tp rt) {
            if (std::holds_alternative<Ty::CyclicRef>(rt->v)) {
              Ty::CyclicRef &ref = std::get<Ty::CyclicRef>(rt->v);
              if (ref.depth == depth) {
                return ty;
              }
            }
            return rt;
          },
          [&](Tp ty, PreWalk) {
            if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
              ++depth;
            }
            return ty;
          },
        };
        return replaceTy(std::get<Ty::Cyclic>(ty->v).ty, uncycler);
      }
      return ty;
    }

    template <typename T>
    bool isComplete(T ty) {
      bool isComplete = true;
      auto checker = overloaded {
        [&](Tp ty) -> Tp {
          if (!std::holds_alternative<Ty::CyclicRef>(ty->v)) {
            isComplete = false;
          }
          return ty;
        },
        [&](Tp ty, PostWalk) -> Tp {
          if (std::holds_alternative<Ty::Err>(ty->v)) {
            isComplete = false;
          }
          return ty;
        },
      };
      replaceTy<true>(ty, checker);
      return isComplete;
    }

    template <typename T>
    bool isReasonable(T ty) {
      bool isReasonable = true;
      auto checker = [&](Tp ty) -> Tp {
        if (std::holds_alternative<Ty::TraitRef>(ty->v)) {
          isReasonable = false;
        }
        return ty;
      };
      replaceTy<true>(ty, checker);
      return isReasonable;
    }

    std::map<TyTB, std::vector<Tp>> inferTypes(Constraints &cnstr) {
      std::vector<std::map<Tp, Tp>*> allInsts;

      auto &topInsts = *cnstr.instantiations.add();
      allInsts.push_back(&topInsts);
      auto visitCnstr = [this](Constraints &cnstr, auto &f) {
        for (auto &tb : cnstr.traitBounds) {
          replaceTy<true>(tb.first, f);
          replaceTy<true>(tb.second, f);
        }
        for (auto &u : cnstr.unions) {
          replaceTy<true>(u.first, f);
          replaceTy<true>(u.second, f);
        }
      };
      {
        auto freeChecker = [&](Tp ty) -> Tp {
          if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
            topInsts[ty] = ty;
          }
          return ty;
        };
        visitCnstr(cnstr, freeChecker);
      }

      Idx tParamC = paramC;
      auto newFreshTy = [&]() -> Tp {
        return tcx.intern(Ty::Placeholder{tParamC++});
      };

      std::deque<std::pair<Tp, Tp>> unions;
      std::copy(cnstr.unions.begin(), cnstr.unions.end(), std::back_inserter(unions));

      std::set<TyTB> traitBounds;
      std::map<TyTB, std::vector<Tp>> implementedTraitBounds;
      for (auto &pair : cnstr.traitBounds) {
        for (auto &tb : pair.second) {
          traitBounds.insert({pair.first, tb});
        }
      }

      std::map<Tp, Tp> replacements;
      auto applyReplacements = [&](auto ty) {
        TypeReplacer recurse;
        auto replacer = overloaded {
          [&](Tp ty) -> Tp {
            if (std::holds_alternative<Ty::TraitRef>(ty->v)) {
              Ty::TraitRef &trf = std::get<Ty::TraitRef>(ty->v);
              auto found = implementedTraitBounds.find({trf.ty, trf.trait});
              if (found != implementedTraitBounds.end()) {
                auto &idces = found->second;
                if (trf.ref >= idces.size()) {
                  // propagated
                  return tcx.intern(Ty::Err{});
                }
                auto &refd = idces[trf.ref];
                return refd = replaceTy(refd, recurse);
              }
            } else if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
              auto found = replacements.find(ty);
              if (found != replacements.end()) {
                return found->second = replaceTy(found->second, recurse);
              }
            }
            return ty;
          },
        };        
        recurse = [&](Tp ty) -> Tp {
          return replacer(ty);
        };
        return replaceTy(ty, replacer);
      };

      auto refreshTyBSet = overloaded {
        [&](std::set<TyTB> &set) {
          std::set<TyTB> newSet;
          for (const TyTB &tb : set) {
            newSet.insert({applyReplacements(tb.ty), applyReplacements(tb.tb)});
          }
          std::swap(newSet, set);
        },
        [&](std::map<TyTB, std::vector<Tp>> &set) {
          std::map<TyTB, std::vector<Tp>> newSet;
          for (auto &tb : set) {
            newSet.insert({
                {
                  applyReplacements(tb.first.ty),
                  replaceTy(tb.first.tb, applyReplacements)
                },
                replaceTy(tb.second, applyReplacements)
              });
          }
          std::swap(newSet, set);
        },
      };

      auto setPlaceholder = [&](Tp ph, Tp ty) -> void {
        bool isFree = false;
        Idx depth = 0;
        auto checkFree = overloaded {
          [&](Tp ty) {
            if (ty == ph) {
              isFree = true;
              return tcx.intern(Ty::CyclicRef{depth});
            }
            return ty;
          },
          [&](Tp ty, PreWalk) {
            if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
              ++depth;
            }
            return ty;
          },
        };
        Tp setTy = replaceTy(ty, checkFree);
        if (isFree) {
          setTy = tcx.intern(Ty::Cyclic{setTy});
        }
        replacements[ph] = setTy;
        mergeSources(setTy, ph);
        // std::cerr << ph << " set to " << setTy << std::endl;
      };

      std::vector<std::pair<Tp, Tp>> unresolved;

      bool isFixed;
      do {
        isFixed = true; // repeat until fixed point
        std::set<std::pair<Tp, Tp>> seen; // ensures that unifying cycles will terminate
        while (!unions.empty()) {
          Tp lhs, rhs;
          std::tie(lhs, rhs) = unions.back();
          unions.pop_back();
          lhs = applyReplacements(lhs);
          rhs = applyReplacements(rhs);
          lhs = uncycle(lhs);
          rhs = uncycle(rhs);
          if (*rhs < *lhs) std::swap(lhs, rhs);
          // std::cerr << "unifying " << lhs << " and " << rhs << std::endl;
          if (std::holds_alternative<Ty::TraitRef>(lhs->v) ||
              std::holds_alternative<Ty::TraitRef>(rhs->v)) {
            unresolved.emplace_back(lhs, rhs);
          }
          if (lhs == rhs || seen.count({lhs, rhs})) {
            continue;
          }
          seen.insert({lhs, rhs});
          isFixed = false;
          // lhs and rhs are guaranteed different
          std::visit(overloaded {
              [&](Ty::Err&, auto&) {},
                [&](Ty::Err&, Ty::Placeholder&) { setPlaceholder(rhs, lhs); },
                [&](Ty::Placeholder&, Ty::Placeholder&) { setPlaceholder(lhs, rhs); },
                [&](Ty::Placeholder&, auto&) { setPlaceholder(lhs, rhs); },
                [&](auto&, Ty::Placeholder&) { setPlaceholder(rhs, lhs); },
                [](Ty::Bool&, Ty::Bool&) {},
                [&](Ty::Int&l, Ty::Int&r) { 
                  ecx.err().msg("Type error").msg("Differing int sizes");
                },
                [&](Ty::UInt&l, Ty::UInt&r) { 
                  ecx.err().msg("Type error").msg("Differing int sizes");
                },
                [&](Ty::Float&l, Ty::Float&r) { 
                  ecx.err().msg("Type error").msg("Differing float sizes");
                },
                [&](Ty::ADT&l, Ty::ADT&r) {
                  if (l.i != r.i) {
                    ecx.err().msg("Type error").msg("Differing base types");
                  } else if (l.v != r.v) {
                    ecx.err().msg("Type error").msg("Differing variant sets");
                  } else if (l.s.size() != r.s.size()) {
                    ecx.err().msg("Type error").msg("Differing arities");
                  } else {
                    for (size_t i = 0; i < l.s.size(); ++i) {
                      unions.emplace_back(l.s[i], r.s[i]);
                    }
                    return;
                  }
                },
                // Dyn is a myth
                [&](Ty::Tuple&l, Ty::Tuple&r) {
                  if (l.t.size() != r.t.size()) {
                    ecx.err().msg("Type error").msg("Differing arities");
                  } else {
                    for (size_t i = 0; i < l.t.size(); ++i) {
                      unions.emplace_back(l.t[i], r.t[i]);
                    }
                  }
                },
                [](Ty::String&, Ty::String&) {},
                [&](auto&, auto&) {
                  ecx.err().msg("Type error").msg("Incompatible types");
                },
                }, lhs->v, rhs->v);
        }

        refreshTyBSet(implementedTraitBounds);
        refreshTyBSet(traitBounds);

        std::vector<TyTB> newTraitBounds;

        for (auto it = traitBounds.begin(); it != traitBounds.end();) {
          // std::cerr << "does " << it->ty << " impl " << it->tb << "?" << std::endl;
          if (implementedTraitBounds.count(*it)) {
            it = traitBounds.erase(it);
          } else if (isComplete(it->ty) && isReasonable(it->tb)) {
            // std::cerr << "YES" << std::endl;
            AbstractTraitImpl *ati = lookupAti(it->tb->i, it->ty);
            if (ati) {
              std::map<Tp, Tp> &atiSubs = *ati->constraints.instantiations.add();
              allInsts.push_back(&atiSubs);
              auto atiReplacer = [&](Tp ty) -> Tp {
                if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
                  auto found =  atiSubs.find(ty);
                  if (found == atiSubs.end()) {
                    found = atiSubs.insert({ty, newFreshTy()}).first;
                  }
                  return found->second;
                }
                return ty;
              };
              auto atiReplace = [&](auto ty) { return replaceTy(ty, atiReplacer); };
              unions.emplace_back(it->ty, atiReplace(ati->ty));
              Idx i = 0;
              for (Tp tty : atiReplace(ati->bound)->s) {
                unions.emplace_back(it->tb->s[i++], tty);
              }
              for (auto &pair : ati->constraints.traitBounds) {
                for (auto &tb : pair.second) {
                  TyTB newTb = {atiReplace(pair.first), atiReplace(tb)};
                  if (!implementedTraitBounds.count(newTb)) {
                    newTraitBounds.push_back(newTb);
                  }
                }
              }
              for (auto &u : ati->constraints.unions) {
                unions.emplace_back(atiReplace(u.first), atiReplace(u.second));
              }
              implementedTraitBounds[*it] = atiReplace(ati->refs);              
            } else {
              ecx.err().msg("Trait not implemented");
              implementedTraitBounds[*it] = {};
            }
            isFixed = false;
            it = traitBounds.erase(it);
          } else {
            ++it;
          }
        }

        while (!newTraitBounds.empty()) {
          traitBounds.insert(newTraitBounds.back());
          newTraitBounds.pop_back();
        }

        for (auto &u : unresolved) {
          unions.emplace_back(applyReplacements(u.first), applyReplacements(u.second));
        }
        unresolved = {};
      } while (!isFixed);

      refreshTyBSet(implementedTraitBounds);

      for (auto map : allInsts) {
        for (auto &kv : *map) {
          Tp replaced = applyReplacements(kv.second);
          if (!isComplete(replaced)) {
            auto &err = ecx.err()
              .msg("Not enough information to infer type");
            auto foundSource = typeSources.find(replaced);
            if (foundSource == typeSources.end()) {
              err.msg("of unknown source");
            } else {
              err.chain(foundSource->second);
            }
            replaced = tcx.intern(Ty::Err{});
          }
          kv.second = replaced;
        }
      }

      return implementedTraitBounds;
    }

    Constraints *cCnstr = nullptr;
    Block *rootBlock = nullptr;
    Constraints visitRootBlock(Block &block, Tp *ret = nullptr) {
      Constraints data;
      cCnstr = &data;
      rootBlock = &block;
      Tp ty = visitBlock(block);
      if (ret) *ret = ty;
      blockVars[rootBlock]; // ensure init
      cCnstr = nullptr;
      return data;
    }

    struct PreWalk {};
    struct PostWalk {};

    template <bool IGNORED = false, typename TR>
    TraitBound *replaceTy(TraitBound *tb, TR &tr) {
      if constexpr (IGNORED) {
        replaceTy<IGNORED>(tb->s, tr);
        return tb;
      }
      return tbcx.intern(TraitBound{tb->i, replaceTy(tb->s, tr)});
    }

    template <bool IGNORED = false, typename TR>
    Tp replaceTy(Tp ty, TR &tr) {
      if constexpr (std::is_invocable<TR, Tp, PreWalk>::value) {
        ty = tr(ty, PreWalk{});
      }
      switch (ty->v.index()) {
      case 5: // Placeholder
      case 12: // CyclicRef
        ty = tr(ty);
        break;
      case 9: { // TraitRef
        auto &trf = std::get<9>(ty->v);
        auto uret = Ty::TraitRef{replaceTy<IGNORED>(trf.ty, tr),
                                 replaceTy<IGNORED>(trf.trait, tr),
                                 trf.ref};
        if constexpr (!IGNORED) ty = tr(tcx.intern(std::move(uret)));
        break;
      }
      case 6: { // ADT
        auto &adt = std::get<6>(ty->v);
        auto uret = Ty::ADT{adt.i, adt.v, replaceTy<IGNORED>(adt.s, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        break;
      }
      case 7: { // Dyn
        auto &dyn = std::get<7>(ty->v);
        auto uret = Ty::Dyn{replaceTy<IGNORED>(dyn.t, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        break;
      }
      case 8: { // Tuple
        auto &tup = std::get<8>(ty->v);
        auto uret = Ty::Tuple{replaceTy<IGNORED>(tup.t, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        break;
      }
      case 11: { // Cyclic
        auto &clc = std::get<11>(ty->v);
        auto uret = Ty::Cyclic{replaceTy<IGNORED>(clc.ty, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        break;
      }
      case 0: // Err
      case 1: // Bool
      case 2: // Int
      case 3: // UInt
      case 4: // Float
      case 10: // String
      default:
        break; // noop
      }
      if constexpr (std::is_invocable<TR, Tp, PostWalk>::value) {
        ty = tr(ty, PostWalk{});
      }      
      return ty;
    }

    template <bool IGNORED = false, typename T, typename TR>
    std::vector<T> replaceTy(const std::vector<T> &tys, TR &tr) {
      if constexpr (IGNORED) {
        for (auto &s : tys) replaceTy<true>(s, tr);
        return {};
      }
      auto ret = tys;
      for (auto &s : ret) s = replaceTy(s, tr);
      return ret;
    }

    template <bool IGNORED = false, typename T, typename TR>
    std::set<T> replaceTy(const std::set<T> &tys, TR &tr) {
      if constexpr (IGNORED) {
        for (auto &s : tys) replaceTy<true>(s, tr);
        return {};
      }
      std::set<T> set;
      for (auto &s : tys) set.insert(replaceTy(s, tr));
      return set;
    }

    Tp visitBlock(Block &block) {
      for (auto &b : block.bindings) {
        blockVars[rootBlock].insert(b);
        Tp ty = varTypes[b] =
          freshType(maybeLoc(program->bindings.at(b).source, "type of this"));
        for (auto &hint :
               std::get<DefType::Variable>(program->bindings.at(b).defType.v).hints) {
          parseTypeHint(ty, hint);
        }
      }
      Tp last = unitType();
      for (auto &e : block.body) {
        last = visitExpr(*e);
      }
      return last;
    }

    Idx paramC = 0;
    Tp freshType(std::optional<err::Location> source = std::nullopt) {
      Tp ty = tcx.intern(Ty::Placeholder{paramC++});
      if (source) {
        typeSources[ty] = *source;
      }
      return ty;
    }
    Tp unitType() { return tcx.intern(Ty::Tuple{{}}); }
    Tp boolType() { return tcx.intern(Ty::Bool{}); }

    void constrainTrait(Tp param, TraitBound *bound) {
      cCnstr->traitBounds[param].insert(bound);
    }

    void constrainUnite(Tp type, Tp other) {
      cCnstr->unions.emplace_back(type, other);
    }

    Tp visitExpr(Expr &it) {
      blockExprs[rootBlock].insert(&it);
      Tp ty = exprTypes[&it] = ExprVisitor::visitExpr(it);
      for (auto &hint : it.type) {
        parseTypeHint(ty, hint);
      }
      return ty;
    }

    Tp visitBlockExpr(BlockExpr &it) {
      return visitBlock(it.block);
    }
    Tp visitVarExpr(VarExpr &it) {
      return varTypes.at(it.ref);
    }
    Tp visitCondExpr(CondExpr &it) {
      Tp predT = visitExpr(*it.predE);
      Tp thenT = visitExpr(*it.thenE);
      Tp elseT = visitExpr(*it.elseE);
      constrainUnite(boolType(), predT);
      if (it.pos == Pos::Expr) {
        Tp retT = freshType(maybeLoc(it.span, "here"));
        constrainUnite(retT, thenT);
        constrainUnite(retT, elseT);
        return retT;
      } else {
        return unitType();
      }
    }
    Tp visitVoidExpr(VoidExpr &it) {
      return unitType();
    }
    Tp visitLiteralExpr(LiteralExpr &it) {
      switch (it.type) {
      case LiteralExpr::Int:
        return tcx.intern(Ty::Int{type::IntSize::i64});
      case LiteralExpr::Float:
        return tcx.intern(Ty::Float{type::FloatSize::f64});
      case LiteralExpr::String:
        return tcx.intern(Ty::String{});
      default: throw 0;
      }
    }
    Tp visitBoolExpr(BoolExpr &it) {
      return boolType();
    }
    Tp visitBinExpr(BinExpr &it) {
      Tp lhsType = visitExpr(*it.lhs);
      Tp rhsType = visitExpr(*it.rhs);
      Idx trait;
      switch (it.op) {
      case BinExpr::BitOr: trait = BitOr; break;
      case BinExpr::BitAnd: trait = BitAnd; break;
      case BinExpr::Add: trait = Add; break;
      case BinExpr::Sub: trait = Sub; break;
      case BinExpr::Mul: trait = Mul; break;
      case BinExpr::Div: trait = Div; break;
      case BinExpr::Rem: trait = Rem; break;
      default: throw 0;
      }
      TraitBound *traitBound = tbcx.intern(TraitBound{trait, {rhsType}});
      constrainTrait(lhsType, traitBound);
      traitBoundTypes[&it] = traitBound;
      return tcx.intern(Ty::TraitRef{lhsType, traitBound, 0});
    }
    Tp visitCmpExpr(CmpExpr &it) {
      Tp lhsType = visitExpr(*it.lhs);
      Tp rhsType = visitExpr(*it.rhs);
      TraitBound *traitBound = tbcx.intern(TraitBound{Cmp, {rhsType}});
      traitBoundTypes[&it] = traitBound;
      constrainTrait(lhsType, traitBound);
      return boolType();
    }
    Tp visitNegExpr(NegExpr &it) {
      Tp exprType = visitExpr(*it.value);
      TraitBound *traitBound = tbcx.intern(TraitBound{Neg});
      traitBoundTypes[&it] = traitBound;
      constrainTrait(exprType, traitBound);
      return tcx.intern(Ty::TraitRef{exprType, traitBound, 0});
    }
    Tp visitCallExpr(CallExpr &it) {
      Tp funcTy = visitExpr(*it.func);
      Tp retTy = freshType(maybeLoc(it.span, "type of this"));
      std::vector<Tp> argTys;
      argTys.reserve(it.args.size());
      for (Eptr &e : it.args) {
        argTys.push_back(visitExpr(*e));
      }
      Tp argsTy = tcx.intern(Ty::Tuple{argTys});
      TraitBound *traitBound = tbcx.intern(TraitBound{Fn, {argsTy, retTy}});
      traitBoundTypes[&it] = traitBound;
      constrainTrait(funcTy, traitBound);
      return retTy;
    }
    Tp visitDefineExpr(DefineExpr &it) {
      Tp definedTy = visitExpr(*it.value);
      constrainUnite(varTypes.at(it.idx), definedTy);
      return unitType();
    }
    Tp visitNewExpr(NewExpr &it) {
      std::vector<Tp> argTys;
      argTys.reserve(it.values.size());
      for (Eptr &e : it.values) {
        argTys.push_back(visitExpr(*e));
      }
      return tcx.intern(Ty::ADT{it.adt, {it.variant}, argTys});
    }
    Tp visitGetExpr(GetExpr &it) {
      std::vector<Tp> argTys;
      auto &adt = std::get<DefType::ADT>(program->bindings.at(it.adt).defType.v);
      Idx size = 0;
      std::optional<Idx> fieldTyIdx;
      for (Idx v = 0; v < adt.variants.size(); ++v) {
        if (v == it.variant) {
          fieldTyIdx = size + it.field;
        }
        size += adt.variants[v].values.size();
      }
      argTys.reserve(size);
      for (Idx i = 0; i < size; ++i) argTys.push_back(freshType());
      Tp adtTy = tcx.intern(Ty::ADT{it.adt, {it.variant}, argTys});
      constrainUnite(adtTy, visitExpr(*it.value));
      return argTys.at(fieldTyIdx.value());
    }
    Tp visitForeignExpr(ForeignExpr &it) {
      return freshType(maybeLoc(it.span, "this value"));
    }
    Tp visitDummyExpr(DummyExpr &it) {
      // suppressed
      return tcx.intern(Ty::Err{});
    }
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor() {
    return std::make_unique<InferenceVisitor>();
  }
}
