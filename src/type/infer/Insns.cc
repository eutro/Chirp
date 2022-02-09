#include "Insns.h"
#include "../../common/Logging.h"

#include <utility>

namespace type::infer {
  logging::Marker CONSTRUCT_TRACE("CONSTRUCT_TRACE", false);
  std::vector<Tp> IdentityInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    return tys;
  }
  std::vector<Tp> ConstructInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cArgs) const {
    logging::CHIRP.log(CONSTRUCT_TRACE, "Constructing ", cArgs, " from ", tys, "\n");
    std::vector<Tp> ret;
    ret.reserve(cArgs.size());
    for (const Constant &constant : cArgs) {
      if (tys.empty()) {
        ret.push_back(constant_cast<Tp>(constant));
      } else {
        TyTemplate tyTemplate = constant_cast<Tp>(constant);
        ret.push_back(tyTemplate.construct(tys));
      }
    }
    logging::CHIRP.log(CONSTRUCT_TRACE, "got ", ret, "\n");
    return ret;
  }
  std::vector<Tp> DeConstructInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cArgs) const {
    TyTemplate tyTemplate = constant_cast<Tp>(cArgs.front());
    return tyTemplate.deconstruct(tys.front());
  }

  Tp TyTemplate::construct(const std::vector<Tp> &tys) const {
    std::map<Tp, Tp> replacements;
    for (Idx i = 0; i < tys.size(); ++i) {
      replacements.insert({targetTy->tcx->intern(Ty::Placeholder{i}), tys[i]});
    }
    return replaceTy(targetTy, replacements);
  }
  std::vector<Tp> TyTemplate::deconstruct(Tp ty) const {
    logging::CHIRP.log(CONSTRUCT_TRACE, "Deconstructing ", targetTy, " from ", ty, "\n");
    Idx rets = targetTy->free.size();
    std::vector<Tp> out(rets, ty->tcx->intern(Ty::Err{}));
    std::function<void(Tp, Tp)> doDeconstruct = [&doDeconstruct, &out](Tp tmplTy, Tp ty) {
      if (std::holds_alternative<Ty::Placeholder>(tmplTy->v)) {
        out.at(std::get<Ty::Placeholder>(tmplTy->v).i) = ty;
      } else {
        if (tmplTy == ty) {
          return;
        }
        ty = type::uncycle(ty);
        if (tmplTy->v.index() != ty->v.index()) {
          throw err::LocationError(util::toStr("Base type mismatch in ", tmplTy, " and ", ty));
        } else {
          std::visit(
              overloaded{
                  [&](const Ty::Tuple &lt, const Ty::Tuple &rt) {
                    if (lt.t.size() != rt.t.size()) {
                      throw err::LocationError(util::toStr("Tuple size mismatch in ", tmplTy, " and ", ty));
                    }
                    auto ltIt = lt.t.begin();
                    auto rtIt = rt.t.begin();
                    for (; ltIt != lt.t.end(); ++ltIt, ++rtIt) {
                      doDeconstruct(*ltIt, *rtIt);
                    }
                  },
                  [&](const Ty::ADT &lt, const Ty::ADT &rt) {
                    if (lt.i != rt.i || lt.s.size() != rt.s.size() || lt.fieldTys.size() != rt.fieldTys.size()) {
                      throw err::LocationError(util::toStr("Base type mismatch when deconstructing in,\n ", tmplTy, "\n and ", ty));
                    }
                    {
                      auto ltIt = lt.s.begin();
                      auto rtIt = rt.s.begin();
                      for (; ltIt != lt.s.end(); ++ltIt, ++rtIt) {
                        doDeconstruct(*ltIt, *rtIt);
                      }
                    }
                    {
                      auto lftIt = lt.fieldTys.begin();
                      auto rftIt = rt.fieldTys.begin();
                      for (; lftIt != lt.fieldTys.end(); ++lftIt, ++rftIt) {
                        doDeconstruct(*lftIt, *rftIt);
                      }
                    }
                  },
                  [&](const Ty::FfiFn &lt, const Ty::FfiFn &rt) {
                    doDeconstruct(lt.args, rt.args);
                    doDeconstruct(lt.ret, rt.ret);
                  },
                  [&](const Ty::TypeToken &lt, const Ty::TypeToken &rt) {
                    doDeconstruct(lt.ty, rt.ty);
                  },
                  [&](const auto &, const auto &) {
                    throw err::LocationError(util::toStr("Mismatched types: ", tmplTy, " and ", ty));
                  }
              },
              tmplTy->v, ty->v);
        }
      }
    };
    try {
      doDeconstruct(targetTy, ty);
      for (Tp tp : out) {
        if (std::holds_alternative<Ty::Err>(tp->v)) {
          throw err::LocationError("Not all results in output");
        }
      }
    } catch (err::LocationError &e) {
      err::Location loc;
      loc.msg(util::toStr(" to: ", targetTy));
      loc.msg(util::toStr(" from: ", ty));
      loc.msg(e.what());
      for (auto &l : e.locations) {
        loc.chain(l);
      }
      throw err::LocationError("Failed deconstruction", {std::move(loc)});
    }
    logging::CHIRP.log(CONSTRUCT_TRACE, "got ", out, "\n");
    return out;
  }

  std::vector<Tp> OutputInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &constArgs) const {
    constant_cast<std::vector<Tp>*>(constArgs.at(0))->at(constant_cast<size_t>(constArgs.at(1))) = tys.front();
    return std::vector<Tp>();
  }

  std::vector<Tp> TrapInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    throw util::ICE("Trap in type inference");
  }

  bool tryUnify(Tp lhs, Tp rhs);
  bool tryUnify(const std::vector<Tp> &lhs, const std::vector<Tp> &rhs) {
    if (lhs.size() != rhs.size()) return false;
    for (size_t i = 0; i < lhs.size(); ++i) {
      if (!tryUnify(type::uncycle(lhs[i]), type::uncycle(rhs[i]))) {
        return false;
      }
    }
    return true;
  }

  bool tryUnify(Tp lhs, Tp rhs) {
    if (lhs == rhs) return true;
    if (std::holds_alternative<Ty::Undetermined>(lhs->v) ||
        std::holds_alternative<Ty::Undetermined>(rhs->v)) {
      return true; // it's fine
    }
    bool leftUnion;
    if ((leftUnion = std::holds_alternative<Ty::Union>(lhs->v)) ||
        std::holds_alternative<Ty::Union>(rhs->v)) {
      auto &u = std::get<Ty::Union>((leftUnion ? lhs : rhs)->v);
      for (auto &t : u.tys) {
        if (std::holds_alternative<Ty::Undetermined>(t->v)) return true;
      }
    }
    if (lhs->v.index() == rhs->v.index()) {
      if (std::visit(
          overloaded{
              [](const Ty::Int &l, const Ty::Int &r) {return l.s == r.s;},
              [](const Ty::UInt &l, const Ty::UInt &r) {return l.s == r.s;},
              [](const Ty::Float &l, const Ty::Float &r) {return l.s == r.s;},
              [](const Ty::ADT &l, const Ty::ADT &r) {return l.i == r.i && tryUnify(l.s, r.s) && tryUnify(l.fieldTys, r.fieldTys);},
              [](const Ty::Undetermined &l, const Ty::Undetermined &r) {return false;},
              [](const Ty::Union &l, const Ty::Union &r) {return false;},
              [](const Ty::Tuple &l, const Ty::Tuple &r) {return tryUnify(l.t, r.t);},
              [](const Ty::TypeToken &l, const Ty::TypeToken &r) {return tryUnify(l.ty, r.ty);},
              [](const Ty::String &l, const Ty::String &r) {return l.nul == r.nul;},
              [](const Ty::FfiFn &l, const Ty::FfiFn &r) {return tryUnify({l.args, l.ret}, {r.args, r.ret});},
              [](const auto &, const auto &) {
                // Err, Bool
                return true;
              }
          },
          lhs->v, rhs->v
      )) return true;
    }
    return false;
  }

  std::vector<Tp> CheckInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    Tp ty = type::uncycle(tys.front());
    for (auto it = tys.begin() + 1; it != tys.end(); ++it) {
      if (!tryUnify(ty, type::uncycle(*it))) {
        err::Location loc;
        loc.msg(util::toStr(" expected: ", ty));
        loc.msg(util::toStr(" got: ", *it));
        throw err::LocationError("Mismatched types", {loc});
      }
    }
    return std::vector<Tp>();
  }

  std::vector<Tp> UnionInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    return {unionOf(*tys.at(0)->tcx, tys)};
  }

  ConstInsn::ConstInsn(std::vector<Tp> ret) : ret(std::move(ret)) {}
  std::vector<Tp> ConstInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    return ret;
  }

  std::vector<Tp> InstWrapper::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cs) const {
    bool isRetry = false;
    std::vector<Tp> *ret;
    std::optional<std::vector<Tp>> retSafe;
    Inst::Ref ref;
    Tcx &tcx = *insts->tcx;

    do {
      bool hasMemo;
      ref = insts->initCall(entityIdx, tys, ret, hasMemo);
      if (hasMemo) {
        retSafe = *ret;
        break;
      } else {
        std::vector<Tp> recurTys;
        if (retSafe) {
          *ret = *retSafe;
        } else {
          // to allow stuff like:
          // defn cons(x, y) = λifpair,ifnull.ifpair(x, y)
          // defn nil = λifpair,ifnull.ifnull()
          // defn rangeR(n) = if n == 0 then nil else cons(n, rangeR(n - 1))
          *ret = std::vector<Tp>();
          ret->reserve(returnCount);
          for (Idx i = 0; i < returnCount; ++i) {
            ret->push_back(tcx.intern(Ty::Undetermined{{ref.first, ref.second, i}}));
          }
          recurTys = *ret;
        }
        auto oldRef = CURRENT_REF;
        auto oldInst = CURRENT_INST;
        CURRENT_REF = &ref;
        CURRENT_INST = &(*insts)[ref];
        retSafe = *ret = fn(tys, cs);
        CURRENT_REF = oldRef;
        CURRENT_INST = oldInst;

        isRetry = insts->finishCall(ref, isRetry);
        if (isRetry) {
          for (Idx i = 0; i < ret->size(); ++i) {
            Tp udt = recurTys.at(i);
            Tp &udtVal = retSafe->at(i);
            if (udtVal == udt) {
              udtVal = tcx.intern(Ty::Union{}); // never type
              continue;
            }
            udtVal = type::uncycle(udtVal);
            if (std::holds_alternative<Ty::Union>(udtVal->v)) {
              auto &u = std::get<Ty::Union>(udtVal->v);
              if (std::binary_search(u.tys.begin(), u.tys.end(), udt)) {
                std::vector<Tp> nU;
                for (Tp tp : u.tys) {
                  if (tp != udt) nU.push_back(tp);
                }
                udtVal = type::unionOf(tcx, nU);
              }
            }
          }
          for (Idx i = 0; i < recurTys.size(); ++i) {
            Tp udt = recurTys.at(i);
            Tp &udtVal = retSafe->at(i);
            udtVal = maybeCycle(udtVal, udt);
            // TODO with multiple returns the return values can theoretically be *mutually* recursive
          }
        }
      }
    } while (isRetry);

    if (CURRENT_REF) {
      Idx refIdx = constant_cast<Idx>(cs.at(0));
      Inst::Ref invokingRef = *CURRENT_REF;
      (*insts)[ref].invokingRefs.insert(invokingRef);
      (*insts)[invokingRef].loggedRefs.insert({refIdx, ref});
    }
    return std::move(*retSafe);
  }
  thread_local Inst::Val *InstWrapper::CURRENT_INST = nullptr;
  thread_local Inst::Ref *InstWrapper::CURRENT_REF = nullptr;

  std::vector<Tp> LogInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cs) const {
    if (tys.size() != cs.size()) {
      throw util::ICE("Type and index list size mismatch");
    }
    for (Idx i = 0; i < tys.size(); ++i) {
      InstWrapper::CURRENT_INST->loggedTys[constant_cast<Idx>(cs.at(i))] = tys.at(i);
    }
    return {};
  }
}
