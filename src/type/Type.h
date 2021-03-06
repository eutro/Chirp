#pragma once

#include "../common/Idx.h"
#include "../common/Err.h"
#include "../common/Util.h"
#include "../common/Arena.h"

#include <map>
#include <cstdint>
#include <set>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>
#include <array>

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

namespace type {
  class Ty;

  class Tcx : public arena::InternArena<Ty> {
  public:
    std::map<Idx, std::string> names;
    void addName(Idx i, std::string name);
  };

  using Tp = const Ty *;

  enum class IntSize {
    i8,
    i16,
    i32,
    i64,
    i128,
  };

  Idx bitCount(IntSize);
  static const std::array<IntSize, 5> INT_SIZE_FIXED {
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
  static const std::array<FloatSize, 3> FLOAT_SIZE_VALUES {
      FloatSize::f16,
      FloatSize::f32,
      FloatSize::f64,
  };

  class Ty;

  using Substs = std::vector<Tp>;

#define BIN_OPS(TYPE)                            \
  bool operator<(const TYPE &o) const;           \
  bool operator==(const TYPE &o) const;

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
      Substs s;
      BIN_OPS(ADT)
    };
    struct Union {
      std::vector<Tp> tys; // must be sorted
      BIN_OPS(Union)
    };
    struct Tuple {
      std::vector<Tp> t;
      BIN_OPS(Tuple)
    };
    struct String {
      bool nul;
      BIN_OPS(String)
    };
    struct Cyclic {
      Tp ty;
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
    struct Undetermined {
      std::vector<Idx> ref;
      BIN_OPS(Undetermined)
    };
    std::variant<
      Err,
      Bool,
      Int,
      UInt,
      Float,
      Placeholder,
      ADT,
      Union,
      Tuple,
      String,
      Cyclic,
      CyclicRef,
      FfiFn,
      Undetermined> v;
    Tcx *tcx;

    template <typename ... Arg>
    Ty(Arg &&... arg): v(std::forward<Arg>(arg)...) {}

    BIN_OPS(Ty)
  };
}

template <>
struct arena::InternHook<type::Ty> {
  inline void postIntern(InternArena<type::Ty> &tcx, type::Ty *ty) {
    ty->tcx = static_cast<type::Tcx *>(&tcx);
  }
};

#define ITER_HASH(TYPE)                             \
  namespace std { template <> struct hash<TYPE> {   \
      std::size_t operator()(const TYPE &o) {       \
        return util::hashIterable(o); } }; }

ITER_HASH(type::Substs)
ITER_HASH(std::set<Idx>)
ITER_HASH(std::vector<Idx>)

#define IMPL_OPS(TYPE, LHSA, RHSA)                               \
  namespace std { template <> struct hash<type::TYPE> {          \
      std::size_t operator()(const type::TYPE &o) {              \
        return util::hashMulti RHSA; } }; }
#define IMPL_SINGLETON(TYPE)                                            \
  namespace std { template <> struct hash<type::TYPE> {                 \
    std::size_t operator()(const type::TYPE &) { return 1; } }; }
#include "TypeImpl.h"
#undef IMPL_OPS
#undef IMPL_SINGLETON

namespace type {
  Tp uncycle(Tp );

  struct PreWalk {};
  struct PostWalk {};

  Tp unionOf(Tcx &tcx, const std::vector<Tp> &tys);

  template <bool IGNORED = false, typename TR>
  Tp replaceTy(Tcx &tcx, Tp ty, TR &tr) {
    if constexpr (std::is_invocable<TR, Tp, PreWalk>::value) {
      ty = tr(ty, PreWalk{});
    }
    using VTy = decltype(ty->v);
    switch (ty->v.index()) {
      case util::index_of_type_v<Ty::Placeholder, VTy>:
      case util::index_of_type_v<Ty::CyclicRef, VTy>:
      case util::index_of_type_v<Ty::Undetermined, VTy>:
        ty = tr(ty);
        break;
      case util::index_of_type_v<Ty::ADT, VTy>: {
        auto &adt = std::get<Ty::ADT>(ty->v);
        auto uret = Ty::ADT{adt.i, replaceTy<IGNORED>(tcx, adt.s, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        else (void)uret;
        break;
      }
      case util::index_of_type_v<Ty::Tuple, VTy>: {
        auto &tup = std::get<Ty::Tuple>(ty->v);
        auto uret = Ty::Tuple{replaceTy<IGNORED>(tcx, tup.t, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        else (void)uret;
        break;
      }
      case util::index_of_type_v<Ty::Cyclic, VTy>: {
        auto &clc = std::get<Ty::Cyclic>(ty->v);
        auto uret = Ty::Cyclic{replaceTy<IGNORED>(tcx, clc.ty, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        else (void)uret;
        break;
      }
      case util::index_of_type_v<Ty::FfiFn, VTy>: {
        auto &ffifn = std::get<Ty::FfiFn>(ty->v);
        auto uret = Ty::FfiFn{replaceTy<IGNORED>(tcx, ffifn.args, tr),
                              replaceTy<IGNORED>(tcx, ffifn.ret, tr)};
        if constexpr (!IGNORED) ty = tcx.intern(uret);
        else (void)uret;
        break;
      }
      case util::index_of_type_v<Ty::Union, VTy>: {
        auto &unty = std::get<Ty::Union>(ty->v);
        auto uret = replaceTy<IGNORED>(tcx, unty.tys, tr);
        if constexpr(!IGNORED) ty = unionOf(tcx, uret);
        else (void)uret;
        break;
      }
      case util::index_of_type_v<Ty::Err, VTy>:
      case util::index_of_type_v<Ty::Bool, VTy>:
      case util::index_of_type_v<Ty::Int, VTy>:
      case util::index_of_type_v<Ty::UInt, VTy>:
      case util::index_of_type_v<Ty::Float, VTy>:
      case util::index_of_type_v<Ty::String, VTy>:
      default: break; // noop
    }
    if constexpr (std::is_invocable<TR, Tp, PostWalk>::value) {
      ty = tr(ty, PostWalk{});
    }
    return ty;
  }

  template <bool IGNORED = false, typename T, typename TR>
  std::vector<T> replaceTy(Tcx &tcx, const std::vector<T> &tys, TR &tr) {
    if constexpr (IGNORED) {
      for (auto &s : tys) replaceTy<true>(tcx, s, tr);
      return {};
    }
    auto ret = tys;
    for (auto &s : ret) s = replaceTy(tcx, s, tr);
    return ret;
  }

  template <bool IGNORED = false, typename T, typename TR>
  std::set<T> replaceTy(Tcx &tcx, const std::set<T> &tys, TR &tr) {
    if constexpr (IGNORED) {
      for (auto &s : tys) replaceTy<true>(tcx, s, tr);
      return {};
    }
    std::set<T> set;
    for (auto &s : tys) set.insert(replaceTy(tcx, s, tr));
    return set;
  }
}

std::ostream &operator<<(std::ostream &os, type::Tp ty);

namespace type::print {
  void printTy(type::Tp t);
}
