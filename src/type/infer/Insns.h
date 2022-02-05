#pragma once

#include "Fn.h"
#include "LookupTable.h"
#include "Inst.h"

namespace type::infer {
  struct IdentityInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey::P key() { return LookupKey::intern("id"); }
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
    static LookupKey::P key() { return LookupKey::intern("construct"); }
  };
  static_assert(std::is_assignable_v<Fn, ConstructInsn>);

  struct DeConstructInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey::P key() { return LookupKey::intern("deconstruct"); }
  };
  static_assert(std::is_assignable_v<Fn, DeConstructInsn>);

  struct OutputInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey::P key() { return LookupKey::intern("output"); }
  };
  static_assert(std::is_assignable_v<Fn, OutputInsn>);

  struct TrapInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey::P key() { return LookupKey::intern("trap"); }
  };
  static_assert(std::is_assignable_v<Fn, TrapInsn>);

  struct UnionInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey::P key() { return LookupKey::intern("union"); }
  };
  static_assert(std::is_assignable_v<Fn, UnionInsn>);

  struct CheckInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const;
    static LookupKey::P key() { return LookupKey::intern("check"); }
  };
  static_assert(std::is_assignable_v<Fn, CheckInsn>);

  struct TraitInsn {
    static LookupKey::P key() { return LookupKey::intern("trait"); }
  };

  struct InstWrapper {
    Fn fn;
    Idx returnCount;
    Inst::EntityIdx entityIdx;
    std::shared_ptr<Inst::ConstructingSet> insts;

    static thread_local Inst::Val *CURRENT_INST;
    static thread_local Inst::Ref *CURRENT_REF;

    InstWrapper(Fn fn, Idx returnCount, decltype(entityIdx) entityIdx, decltype(insts) insts)
      : fn(fn), returnCount(returnCount), entityIdx(entityIdx), insts(insts) {}
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cs) const;
  };
  static_assert(std::is_assignable_v<Fn, InstWrapper>);

  struct LogInsn {
    static LookupKey::P key() { return LookupKey::intern("log"); }
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cs) const;
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
