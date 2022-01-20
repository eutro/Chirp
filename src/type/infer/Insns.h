#pragma once

#include "Fn.h"
#include "LookupTable.h"

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

  struct DynInsn {
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cs);
    static LookupKey *key() { return LookupKey::intern("dyn"); }
  };

  template <typename T>
  struct InstWrapper {
    Fn fn;
    std::shared_ptr<std::vector<T>> insts = std::make_shared<std::vector<T>>();
    InstWrapper(Fn &&fn) : fn(std::forward<Fn>(fn)) {}
    static thread_local T *CURRENT_INST;
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cs) const {
      T ci;
      T *oldInst = CURRENT_INST;
      CURRENT_INST = &ci;
      auto ret = fn(tys, cs);
      CURRENT_INST = oldInst;
      insts->push_back(std::move(ci));
      return ret;
    }

    template <typename F>
    struct LogInsn {
      F f;
      LogInsn(F &&f) : f(std::forward<F>(f)) {}
      static LogInsn of(F &&f) {
        return LogInsn(std::forward<F>(f));
      }
      std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
        f(*CURRENT_INST, tys);
        return tys;
      }
    };
  };
  template <typename T> thread_local T *InstWrapper<T>::CURRENT_INST{};

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
    addSimple<IdentityInsn, ConstructInsn, DeConstructInsn, OutputInsn, TrapInsn, UnionInsn, CheckInsn, DynInsn>(lt);
  }
}
