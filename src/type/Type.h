#pragma once

#include "../common/Err.h"
#include "../common/Util.h"

#include <cstdint>
#include <set>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

namespace type {
  using Idx = std::uint32_t;

  enum class IntSize {
    i8,
    i16,
    i32,
    i64,
    i128,
  };

  enum class FloatSize {
    f16,
    f32,
    f64,
  };

  class Ty;

  using Substs = std::vector<Ty*>;

#define BIN_OPS(TYPE)                            \
  bool operator<(const TYPE &o) const;           \
  bool operator==(const TYPE &o) const;

  struct TraitBound {
    Idx i;
    Substs s;
    BIN_OPS(TraitBound)
  };

  using Variants = std::set<Idx>;

  using TraitBounds = std::vector<TraitBound*>;

  class Ty {
  public:
    struct Err {
      BIN_OPS(Err)
    };
    struct Bool {
      BIN_OPS(Bool)
    };
    struct Int {
      IntSize s;
      BIN_OPS(Int)
    };
    struct UInt {
      IntSize s;
      BIN_OPS(UInt)
    };
    struct Float {
      FloatSize s;
      BIN_OPS(Float)
    };
    struct Placeholder {
      Idx i;
      BIN_OPS(Placeholder)
    };
    struct ADT {
      Idx i;
      Variants v;
      Substs s;
      BIN_OPS(ADT)
    };
    struct Dyn {
      TraitBounds t;
      BIN_OPS(Dyn)
    };
    struct Tuple {
      std::vector<Ty*> t;
      BIN_OPS(Tuple)
    };
    struct TraitRef {
      Ty *ty;
      TraitBound *trait;
      Idx ref;
      BIN_OPS(TraitRef)
    };
    struct String {
      BIN_OPS(String)
    };
    struct Cyclic {
      Ty *ty;
      BIN_OPS(Cyclic)
    };
    struct CyclicRef {
      Idx depth;
      BIN_OPS(CyclicRef)
    };
    std::variant<
      Err,
      Bool,
      Int,
      UInt,
      Float,
      Placeholder,
      ADT,
      Dyn,
      Tuple,
      TraitRef,
      String,
      Cyclic,
      CyclicRef> v;

    template <typename ... Arg>
    Ty(Arg &&... arg): v(std::forward<Arg>(arg)...) {}

    BIN_OPS(Ty)
  };
}

#define ITER_HASH(TYPE)                             \
  namespace std { template <> struct hash<TYPE> {   \
      std::size_t operator()(const TYPE &o) {       \
        return util::hashIterable(o); } }; }

ITER_HASH(type::Substs);
ITER_HASH(type::TraitBounds);
ITER_HASH(std::set<type::Idx>)

#define IMPL_OPS(TYPE, LHSA, RHSA)                               \
  namespace std { template <> struct hash<type::TYPE> {          \
      std::size_t operator()(const type::TYPE &o) {              \
        return util::hashMulti RHSA; } }; }
#define IMPL_SINGLETON(TYPE)                                            \
  namespace std { template <> struct hash<type::TYPE> {                 \
    std::size_t operator()(const type::TYPE &o) { return 1; } }; }
#include "TypeImpl.h"
#undef IMPL_OPS
#undef IMPL_SINGLETON
