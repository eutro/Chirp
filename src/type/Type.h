#pragma once

#include "../common/Idx.h"
#include "../common/Err.h"
#include "../common/Util.h"
#include "../common/Arena.h"

#include <cstdint>
#include <set>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

namespace type {
  class Ty;
  class TraitBound;
  using Tcx = arena::InternArena<Ty>;
  using Tbcx = arena::InternArena<TraitBound>;
  using Tp = Ty *;

  enum class IntSize {
    i8,
    i16,
    i32,
    i64,
    i128,
  };

  Idx bitCount(IntSize);
  static const std::array<IntSize, 5> INT_SIZE_VALUES {
    IntSize::i8,
    IntSize::i16,
    IntSize::i32,
    IntSize::i64,
    IntSize::i128,
  };

  enum class FloatSize {
    f16,
    f32,
    f64,
  };

  Idx bitCount(FloatSize);
  static const std::array<FloatSize, 5> FLOAT_SIZE_VALUES {
      FloatSize::f16,
      FloatSize::f32,
      FloatSize::f64,
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
    struct FfiFn {
      Tp args;
      Tp ret;
      BIN_OPS(FfiFn)
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
      CyclicRef,
      FfiFn> v;

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
ITER_HASH(std::set<Idx>)

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

namespace type {
  Ty *uncycle(Tcx &, Tbcx &, Ty *);

  struct PreWalk {};
  struct PostWalk {};

  template <bool IGNORED = false, typename TR>
  TraitBound *replaceTy(Tcx &tcx, Tbcx &tbcx, TraitBound *tb, TR &tr) {
    if constexpr (IGNORED) {
      replaceTy<IGNORED>(tcx, tbcx, tb->s, tr);
      return tb;
    }
    return tbcx.intern(TraitBound{tb->i, replaceTy(tcx, tbcx, tb->s, tr)});
  }

  template <bool IGNORED = false, typename TR>
  Tp replaceTy(Tcx &tcx, Tbcx &tbcx, Tp ty, TR &tr) {
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
        auto uret = Ty::TraitRef{replaceTy<IGNORED>(tcx, tbcx, trf.ty, tr),
                                 replaceTy<IGNORED>(tcx, tbcx, trf.trait, tr),
                                 trf.ref};
        if constexpr (!IGNORED) ty = tr(tcx.intern(std::move(uret)));
        else tr(ty);
        break;
      }
      case 6: { // ADT
        auto &adt = std::get<6>(ty->v);
        auto uret = Ty::ADT{adt.i, adt.v, replaceTy<IGNORED>(tcx, tbcx, adt.s, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        break;
      }
      case 7: { // Dyn
        auto &dyn = std::get<7>(ty->v);
        auto uret = Ty::Dyn{replaceTy<IGNORED>(tcx, tbcx, dyn.t, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        break;
      }
      case 8: { // Tuple
        auto &tup = std::get<8>(ty->v);
        auto uret = Ty::Tuple{replaceTy<IGNORED>(tcx, tbcx, tup.t, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        break;
      }
      case 11: { // Cyclic
        auto &clc = std::get<11>(ty->v);
        auto uret = Ty::Cyclic{replaceTy<IGNORED>(tcx, tbcx, clc.ty, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        break;
      }
      case 13: { // FfiFn
        auto &ffifn = std::get<13>(ty->v);
        auto uret = Ty::FfiFn{replaceTy<IGNORED>(tcx, tbcx, ffifn.args, tr),
                              replaceTy<IGNORED>(tcx, tbcx, ffifn.ret, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        break;
      }
      case 0: // Err
      case 1: // Bool
      case 2: // Int
      case 3: // UInt
      case 4: // Float
      case 10: // String
      default: break; // noop
    }
    if constexpr (std::is_invocable<TR, Tp, PostWalk>::value) {
      ty = tr(ty, PostWalk{});
    }
    return ty;
  }

  template <bool IGNORED = false, typename T, typename TR>
  std::vector<T> replaceTy(Tcx &tcx, Tbcx &tbcx, const std::vector<T> &tys, TR &tr) {
    if constexpr (IGNORED) {
      for (auto &s : tys) replaceTy<true>(tcx, tbcx, s, tr);
      return {};
    }
    auto ret = tys;
    for (auto &s : ret) s = replaceTy(tcx, tbcx, s, tr);
    return ret;
  }

  template <bool IGNORED = false, typename T, typename TR>
  std::set<T> replaceTy(Tcx &tcx, Tbcx &tbcx, const std::set<T> &tys, TR &tr) {
    if constexpr (IGNORED) {
      for (auto &s : tys) replaceTy<true>(tcx, tbcx, s, tr);
      return {};
    }
    std::set<T> set;
    for (auto &s : tys) set.insert(replaceTy(tcx, tbcx, s, tr));
    return set;
  }
}
