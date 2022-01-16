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

  struct TraitInsn {
    static LookupKey *key() { return LookupKey::intern("trait"); }
  };
}
