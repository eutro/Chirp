#include "Infer.h"

#include "../../type/TypePrint.h"
#include <sstream>

#include <algorithm>
#include <array>
#include <cstddef>
#include <deque>
#include <functional>
#include <iostream>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>

namespace hir::infer {
  enum BuiltinTraits {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitOr,
    BitAnd,
    Cmp,
    Neg,
  };

  enum BuiltinTypes {    
    BOOL,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F16, F32, F64,
    FN,
    TUPLE,
  };

  struct Constraints {
    Idx start, end;
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
    arena::InternArena<Ty> tcx;
    arena::InternArena<type::TraitBound> tbcx;
    arena::Arena<AbstractTraitImpl> aticx;

    std::map<DefIdx, Tp> varTypes;

    std::map<DefIdx, ADT*> adts;
    std::map<DefIdx, Tp> definedTys;

    err::ErrorContext ecx;

    std::map<Expr *, Tp> exprTypes;
    std::map<Block *, std::set<Expr *>> blockExprs;

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
          for (BuiltinTraits bt : {Add, Sub, Mul, Div, Rem, BitOr, BitAnd}) {
            implBinOp(ty, bt);
          }
          implNegCmp(ty);
        }
      }
      for (type::FloatSize fs = type::FloatSize::f16;
           fs <= type::FloatSize::f64;
           ++(std::underlying_type<type::FloatSize>::type&)fs) {
        Tp ty = tcx.intern(Ty::Float{fs});
        for (BuiltinTraits bt : {Add, Sub, Mul, Div, Rem}) {
          implBinOp(ty, bt);
        }
        implNegCmp(ty);
      }
      implBinOp(boolType(), BitAnd);
      implBinOp(boolType(), BitOr);
    }

    Program *program = nullptr;

    InferResult visitProgram(Program &p) {
      for (ADT &adt : p.types) {
        adts[adt.id] = &adt;
      }

      program = &p;

      std::vector<std::pair<Block *, Constraints *>> traitConstraints;

      for (TraitImpl &ti : p.traitImpls) {
        // all of these are currently assumed to be for Fn,
        // and with an ADT type
        AbstractTraitImpl &ati = *aticx.add();
        Idx startC = paramC;
        for (DefIdx param : ti.params) {
          definedTys[param] = freshType();
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
        ati.constraints.start = startC;
        ati.constraints.end = paramC;
        adtTraits[{FN, std::get<Ty::ADT>(ati.ty->v).i}] = &ati;
      }

      Constraints mainConstraints = visitRootBlock(p.topLevel);
      auto traitImplTypes = inferTypes(mainConstraints);

      InferResult res;
      auto completeBlock = [&](Block &block, Constraints &constraints) {
        auto &instTypes = res.insts[&block];
        for (auto &complete : constraints.instantiations.ptrs) {
          std::map<Expr *, Tp> thisInst;
          for (Expr *e : blockExprs[&block]) {
            TypeReplacer replacer = [&](Tp ty) -> Tp {
              if (std::holds_alternative<Ty::TraitRef>(ty->v)) {
                Ty::TraitRef &trf = std::get<Ty::TraitRef>(ty->v);
                auto &idces = traitImplTypes.at({trf.ty, trf.trait});
                if (trf.ref >= idces.size()) {
                  // propagated
                  return tcx.intern(Ty::Err{});
                }
                return idces[trf.ref];;
              } else if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
                return complete->at(ty);
              }
              return ty;
            };
            thisInst[e] = replaceTy(exprTypes[e], replacer);
          }
          instTypes.exprTypes.insert(std::move(thisInst));
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

    std::vector<Tp> parseTyParams(std::vector<Type> params) {
      std::vector<Tp> ret;
      ret.reserve(params.size());
      for (Type &ty : params) {
        ret.push_back(parseTy(ty));
      }
      return ret;
    }

    Tp parseTy(Type &ty) {
      if (ty.base) {
        auto foundAdt = adts.find(*ty.base);
        if (foundAdt != adts.end()) {
          ADT &adt = *foundAdt->second;
          type::Variants variants;
          for (Idx i = 0; i < adt.variants.size(); ++i) {
            variants.insert(i);
          }
          return tcx.intern(Ty::ADT{adt.id, variants, parseTyParams(ty.params)});
        }
        switch (*ty.base) {
        case BOOL:
          return boolType();
        case I8: case I16: case I32: case I64:
          return tcx.intern(Ty::Int{(type::IntSize) (*ty.base - U8)});
        case U8: case U16: case U32: case U64:
          return tcx.intern(Ty::UInt{(type::IntSize) (*ty.base - U8)});
        case F16: case F32: case F64:
          return tcx.intern(Ty::Float{(type::FloatSize) (*ty.base - F16)});
          // no FN,
        case TUPLE:
          return tcx.intern(Ty::Tuple{parseTyParams(ty.params)});
        }
        auto foundConcrete = definedTys.find(*ty.base);
        if (foundConcrete != definedTys.end()) {
          return foundConcrete->second;
        }
        ecx.err().span(ty.source, "Undefined type");
      }
      return freshType();
    }

    TraitBound *parseTb(Type &ty) {
      if (ty.base && *ty.base == FN /* spoiler: it is */) {
        return tbcx.intern(TraitBound{FN, parseTyParams(ty.params)});
      }
      ecx.err().span(ty.source, "Trait parsing not yet implemented");
      return nullptr;
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
          [&](Tp ty, auto) {
            if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
              ++depth;
            }
          },
        };
        return replaceTy(std::get<Ty::Cyclic>(ty->v).ty, uncycler);
      }
      return ty;
    }

    bool isComplete(Tp ty) {
      bool isComplete = true;
      auto checker = [&](Tp ty) -> Tp {
        if (!std::holds_alternative<Ty::CyclicRef>(ty->v)) {
          isComplete = false;
        }
        return ty;
      };
      replaceTy(ty, checker);
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
      replaceTy(ty, checker);
      return isReasonable;
    }

    std::map<TyTB, std::vector<Tp>> inferTypes(Constraints &cnstr) {
      std::vector<std::map<Tp, Tp>*> allInsts;

      auto &topInsts = *cnstr.instantiations.add();
      allInsts.push_back(&topInsts);
      for (Idx i = cnstr.start; i < cnstr.end; ++i) {
        Tp param = tcx.intern(Ty::Placeholder{i});
        topInsts[param] = param;
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
        TypeReplacer replacer = [&](Tp ty) -> Tp {
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
              return refd = replaceTy(refd, replacer);
            }
          } else if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
            auto found = replacements.find(ty);
            if (found != replacements.end()) {
              return found->second = replaceTy(found->second, replacer);
            }
          }
          return ty;
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
            newSet.insert({{applyReplacements(tb.first.ty), replaceTy(tb.first.tb, applyReplacements)},
                replaceTy(tb.second, applyReplacements)});
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
          [&](Tp ty, auto) {
            if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
              ++depth;
            }
          },
        };
        Tp setTy = replaceTy(ty, checkFree);
        if (isFree) {
          setTy = tcx.intern(Ty::Cyclic{setTy});
        }
        replacements[ph] = setTy;
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
              for (Idx i = ati->constraints.start; i < ati->constraints.end; ++i) {
                atiSubs[tcx.intern(Ty::Placeholder{i})] = newFreshTy();
              }
              auto atiReplacer = [&](Tp ty) -> Tp {
                if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
                  return atiSubs.at(ty);
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
            ecx.err().msg("Not enough information to infer type");
            replaced = tcx.intern(Ty::Err{});
          }
          kv.second = replaced;
        }
      }

      return implementedTraitBounds;
    }

    Constraints *cnstr = nullptr;
    Block *currentBlock = nullptr;
    Constraints visitRootBlock(Block &block, Tp *ret = nullptr) {
      Constraints data;
      data.start = paramC;
      cnstr = &data;
      currentBlock = &block;
      Tp ty = visitBlock(block);
      if (ret) *ret = ty;
      data.end = paramC;
      return data;
    }

    template <typename TR>
    TraitBound *replaceTy(TraitBound *tb, TR &tr) {
      return tbcx.intern(TraitBound{tb->i, replaceTy(tb->s, tr)});
    }

    struct ReplaceTag {};

    template <typename TR>
    Tp replaceTy(Tp ty, TR &tr) {
      if constexpr (std::is_invocable<TR, Tp, ReplaceTag>::value) {
        tr(ty, ReplaceTag{});
      }
      switch (ty->v.index()) {
      case 5: // Placeholder
      case 12: // CyclicRef
        return tr(ty);
      case 9: { // TraitRef
        auto &trf = std::get<9>(ty->v);
        return tr(tcx.intern(Ty::TraitRef{replaceTy(trf.ty, tr),
                                          replaceTy(trf.trait, tr),
                                          trf.ref}));
      }
      case 6: { // ADT
        auto &adt = std::get<6>(ty->v);
        return tcx.intern(Ty::ADT{adt.i, adt.v, replaceTy(adt.s, tr)});
      }
      case 7: { // Dyn
        auto &dyn = std::get<7>(ty->v);
        return tcx.intern(Ty::Dyn{replaceTy(dyn.t, tr)});
      }
      case 8: { // Tuple
        auto &tup = std::get<8>(ty->v);
        return tcx.intern(Ty::Tuple{replaceTy(tup.t, tr)});
      }
      case 11: { // Cyclic
        auto &clc = std::get<11>(ty->v);
        return tcx.intern(Ty::Cyclic{replaceTy(clc.ty, tr)});
      }
      case 0: // Err
      case 1: // Bool
      case 2: // Int
      case 3: // UInt
      case 4: // Float
      case 10: // String
      default:
        break;
      }
      return ty;
    }

    template <typename T, typename TR>
    std::vector<T> replaceTy(std::vector<T> tys, TR &tr) {
      for (auto &s : tys) s = replaceTy(s, tr);
      return tys;
    }

    template <typename T, typename TR>
    std::set<T> replaceTy(std::set<T> &tys, TR &tr) {
      std::set<T> set;
      for (auto &s : tys) set.insert(replaceTy(s, tr));
      return set;
    }

    Tp visitBlock(Block &block) {
      for (auto &b : block.typeBindings) {
        definedTys[b] = freshType();
      }
      for (auto &b : block.bindings) {
        Tp ty = varTypes[b] = freshType();
        for (auto &hint : program->bindings.at(b).type) {
          constrainUnite(ty, parseTy(hint));
        }
      }
      Tp last = unitType();
      for (auto &e : block.body) {
        last = visitExpr(*e);
      }
      return last;
    }

    Idx paramC = 0;
    Tp freshType() { return tcx.intern(Ty::Placeholder{paramC++}); }
    Tp unitType() { return tcx.intern(Ty::Tuple{{}}); }
    Tp boolType() { return tcx.intern(Ty::Bool{}); }

    void constrainTrait(Tp param, TraitBound *bound) {
      cnstr->traitBounds[param].insert(bound);
    }

    void constrainUnite(Tp type, Tp other) {
      cnstr->unions.emplace_back(type, other);
    }

    Tp visitExpr(Expr &it) {
      blockExprs[currentBlock].insert(&it);
      Tp ty = exprTypes[&it] = ExprVisitor::visitExpr(it);
      for (auto &hint : it.type) {
        constrainUnite(ty, parseTy(hint));
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
        Tp retT = freshType();
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
      }
    }
    Tp visitBoolExpr(BoolExpr &it) {
      return boolType();
    }
    Tp visitBinExpr(BinExpr &it) {
      Tp lhsType = visitExpr(*it.lhs);
      Tp rhsType = visitExpr(*it.rhs);
      BuiltinTraits trait;
      switch (it.op) {
      case BinExpr::BitOr: trait = BuiltinTraits::BitOr; break;
      case BinExpr::BitAnd: trait = BuiltinTraits::BitAnd; break;
      case BinExpr::Add: trait = BuiltinTraits::Add; break;
      case BinExpr::Sub: trait = BuiltinTraits::Sub; break;
      case BinExpr::Mul: trait = BuiltinTraits::Mul; break;
      case BinExpr::Div: trait = BuiltinTraits::Div; break;
      case BinExpr::Rem: trait = BuiltinTraits::Rem; break;
      }
      TraitBound *traitBound = tbcx.intern(TraitBound{trait, {rhsType}});
      constrainTrait(lhsType, traitBound);
      return tcx.intern(Ty::TraitRef{lhsType, traitBound, 0});
    }
    Tp visitCmpExpr(CmpExpr &it) {
      Tp lhsType = visitExpr(*it.lhs);
      Tp rhsType = visitExpr(*it.rhs);
      TraitBound *traitBound = tbcx.intern(TraitBound{BuiltinTraits::Cmp, {rhsType}});
      constrainTrait(lhsType, traitBound);
      return boolType();
    }
    Tp visitNegExpr(NegExpr &it) {
      Tp exprType = visitExpr(*it.value);
      TraitBound *traitBound = tbcx.intern(TraitBound{BuiltinTraits::Neg});
      constrainTrait(exprType, traitBound);
      return tcx.intern(Ty::TraitRef{exprType, traitBound, 0});
    }
    Tp visitCallExpr(CallExpr &it) {
      Tp funcTy = visitExpr(*it.func);
      Tp retTy = freshType();
      std::vector<Tp> argTys;
      argTys.reserve(it.args.size());
      for (Eptr &e : it.args) {
        argTys.push_back(visitExpr(*e));
      }
      Tp argsTy = tcx.intern(Ty::Tuple{argTys});
      TraitBound *traitBound = tbcx.intern(TraitBound{FN, {argsTy, retTy}});

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
      ADT &adt = *adts.at(it.adt);
      auto size = adt.variants.at(it.variant).values.size();
      argTys.reserve(size);
      for (Idx i = 0; i < size; ++i) argTys.push_back(freshType());
      Tp adtTy = tcx.intern(Ty::ADT{it.adt, {it.variant}, argTys});
      constrainUnite(adtTy, visitExpr(*it.value));
      Tp retTy = argTys.at(it.field);
      return retTy;
    }
    Tp visitForeignExpr(ForeignExpr &it) {
      return freshType();
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
