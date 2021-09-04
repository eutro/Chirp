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

  struct TraitBound {
    Idx i;
    Substs s;
    bool operator<(const TraitBound &o) const {
      return std::make_tuple(i, s) < std::make_tuple(o.i, o.s);
    }
  };

  using Variants = std::set<Idx>;

  using TraitBounds = std::vector<TraitBound*>;

  class Ty {
  public:
    struct Err {
      bool operator<(const Err &o) const { return false; }
    };
    struct Bool {
      bool operator<(const Bool &o) const { return false; }
    };
    struct Int {
      IntSize s;
      bool operator<(const Int &o) const { return s < o.s; }
    };
    struct UInt {
      IntSize s;
      bool operator<(const UInt &o) const { return s < o.s; }
    };
    struct Float {
      FloatSize s;
      bool operator<(const Float &o) const { return s < o.s; }
    };
    struct Placeholder {
      Idx i;
      bool operator<(const Placeholder &o) const { return i < o.i; }
    };
    struct ADT {
      Idx i;
      Variants v;
      Substs s;
      bool operator<(const ADT &o) const {
        return std::make_tuple(i, v, s) < std::make_tuple(o.i, o.v, o.s);
      }
    };
    struct Dyn {
      TraitBounds t;
      bool operator<(const Dyn &o) const { return t < o.t; }
    };
    struct Tuple {
      std::vector<Ty*> t;
      bool operator<(const Tuple &o) const { return t < o.t; }
    };
    struct TraitRef {
      Ty *ty;
      TraitBound *trait;
      Idx ref;
      bool operator<(const TraitRef &o) const {
        return std::make_tuple(ty, trait, ref) < std::make_tuple(o.ty, o.trait, o.ref);
      }
    };
    struct String {
      bool operator<(const String &o) const { return false; }
    };
    struct Cyclic {
      Ty *ty;
      bool operator<(const Cyclic &o) const { return ty < o.ty; }
    };
    struct CyclicRef {
      Idx depth;
      bool operator<(const CyclicRef &o) const { return depth < o.depth; }
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

    bool operator<(const Ty &o) const { return v < o.v; }
  };
}
