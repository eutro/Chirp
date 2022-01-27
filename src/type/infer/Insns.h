#pragma once

#include "Fn.h"
#include "LookupTable.h"
#include "Inst.h"

namespace type::infer {
  struct IdentityInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey *key() { return LookupKey::intern("id"); }
  };
  static_assert(std::is_assignable_v<Fn, IdentityInsn>);

  struct TyTemplate {
    Tp targetTy;
    TyTemplate(Tp ty): targetTy(ty) {}
    [[nodiscard]] Tp construct(const std::vector<Tp> &tys) const;
    std::vector<Tp> deconstruct(Tp ty) const;
  };

  struct ConstructInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey *key() { return LookupKey::intern("construct"); }
  };
  static_assert(std::is_assignable_v<Fn, ConstructInsn>);

  struct DeConstructInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey *key() { return LookupKey::intern("deconstruct"); }
  };
  static_assert(std::is_assignable_v<Fn, DeConstructInsn>);

  struct OutputInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey *key() { return LookupKey::intern("output"); }
  };
  static_assert(std::is_assignable_v<Fn, OutputInsn>);

  struct TrapInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey *key() { return LookupKey::intern("trap"); }
  };
  static_assert(std::is_assignable_v<Fn, TrapInsn>);

  struct UnionInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey *key() { return LookupKey::intern("union"); }
  };
  static_assert(std::is_assignable_v<Fn, UnionInsn>);

  struct CheckInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey *key() { return LookupKey::intern("check"); }
  };
  static_assert(std::is_assignable_v<Fn, CheckInsn>);

  struct TraitInsn {
    static LookupKey *key() { return LookupKey::intern("trait"); }
  };

  struct InstWrapper {
    Fn fn;
    Idx returnCount;
    Inst::EntityIdx entityIdx;
    std::shared_ptr<Inst::ConstructingSet> insts;
    InstWrapper(Fn fn, Idx returnCount, decltype(entityIdx) entityIdx, decltype(insts) insts)
      : fn(fn), returnCount(returnCount), entityIdx(entityIdx), insts(insts) {}
    static thread_local Inst::Val *CURRENT_INST;
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cs) const {
      decltype(Inst::Val::loggedRefs) *loggedRefs = nullptr;
      Idx refIdx;
      if (CURRENT_INST) {
        loggedRefs = &CURRENT_INST->loggedRefs;
        refIdx = constant_cast<Idx>(cs.at(0));
      }
      auto &memo = insts->memo[entityIdx];
      if (memo.count(tys)) {
        Inst::Ref ref = memo.at(tys);
        if (loggedRefs) loggedRefs->emplace(refIdx, ref);
        return insts->refRets.at(ref);
      }
      auto &set = insts->entities[entityIdx];
      Inst::Ref ref{entityIdx, (Idx)set.size()};
      memo.emplace(tys, ref);
      // TODO invalidate any memos created referencing this
      std::vector<Tp> &memoRets = insts->refRets.emplace(
        ref,
        std::vector<Tp>(returnCount, insts->tcx->intern(Ty::Union{}))
      ).first->second;

      Inst::Val ci;
      Inst::Val *oldInst = CURRENT_INST;
      CURRENT_INST = &ci;
      auto ret = fn(tys, cs);
      CURRENT_INST = oldInst;

      set.emplace(ref.second, std::move(ci));
      memoRets = ret;
      if (loggedRefs) loggedRefs->emplace(refIdx, ref);
      return ret;
    }
  };

  struct LogInsn {
    static LookupKey *key() { return LookupKey::intern("log"); }
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cs) const {
      InstWrapper::CURRENT_INST->loggedTys[constant_cast<Idx>(cs.at(1))] = tys.at(0);
      return tys;
    }
  };
  static_assert(std::is_assignable_v<Fn, LogInsn>);

  struct ConstInsn {
    std::vector<Tp> ret;
    ConstInsn(std::vector<Tp> ret);
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
  };

  template <typename... T>
  void addSimple(LookupTable &lt) {
    (lt.insertFallback(T::key(), {}, T{}), ...);
  }

  static inline void addInsns(LookupTable &lt) {
    addSimple<IdentityInsn,
              ConstructInsn,
              DeConstructInsn,
              OutputInsn,
              TrapInsn,
              UnionInsn,
              CheckInsn,
              LogInsn>(lt);
  }
}
