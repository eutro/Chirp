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

  struct Path {
    using V = std::shared_ptr<Path>;
    Idx car;
    V cdr;
    static V cons(Idx car, V cdr) {
      return std::make_shared<Path>(Path{car, std::move(cdr)});
    }
    static std::vector<V> cons(Idx car, const std::vector<V> &cdrs) {
      std::vector<V> ret;
      ret.reserve(cdrs.size());
      for (const V &cdr : cdrs) {
        ret.push_back(cons(car, cdr));
      }
      return ret;
    }
  };

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
      Substs fieldTys;
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
    struct TypeToken {
      Tp ty;
      BIN_OPS(TypeToken)
    };
    std::variant<
      /* 0  */ Err,
      /* 1  */ Bool,
      /* 2  */ Int,
      /* 3  */ UInt,
      /* 4  */ Float,
      /* 5  */ Placeholder,
      /* 6  */ ADT,
      /* 7  */ Union,
      /* 8  */ Tuple,
      /* 9  */ String,
      /* 10 */ Cyclic,
      /* 11 */ CyclicRef,
      /* 12 */ FfiFn,
      /* 13 */ Undetermined,
      /* 14 */ TypeToken
      > v;
    Tcx *tcx;
    std::map<Tp, std::vector<Path::V>> free;
    std::map<Idx, std::vector<Path::V>> cycleRefs;

    size_t countFree(Tp ty) const {
      auto found = free.find(ty);
      if (found != free.end()) {
        return found->second.size();
      }
      return 0;
    }

    template <typename ... Arg>
    Ty(Arg &&... arg): v(std::forward<Arg>(arg)...) {}

    BIN_OPS(Ty)
  };
  using VTy = decltype(Ty::v);
}

template <>
struct arena::InternHook<type::Ty> {
  template <typename M>
  static void addPathsTo(M &m, Idx i, const M &om) {
    for (auto &e : om) {
      auto &rf = m[e.first];
      auto vs = type::Path::cons(i, e.second);
      rf.insert(rf.end(), vs.begin(), vs.end());
    }
  }
  static void addPaths(type::Ty *ptr, Idx i, type::Tp ty) {
    using namespace type;
    addPathsTo(ptr->free, i, ty->free);
    addPathsTo(ptr->cycleRefs, i, ty->cycleRefs);
  }
  inline void postIntern(InternArena<type::Ty> &tcx, type::Ty *ty) {
    ty->tcx = static_cast<type::Tcx *>(&tcx);
    using namespace type;
    switch (ty->v.index()) {
      case util::index_of_type_v<Ty::Placeholder, VTy>:
      case util::index_of_type_v<Ty::Undetermined, VTy>:
        ty->free.insert({ty, {nullptr}});
        break;
      case util::index_of_type_v<Ty::CyclicRef, VTy>:
        ty->cycleRefs.insert({std::get<Ty::CyclicRef>(ty->v).depth + 1, {nullptr}});
        break;
      case util::index_of_type_v<Ty::ADT, VTy>: {
        auto &adt = std::get<Ty::ADT>(ty->v);
        for (Idx i = 0; i < adt.s.size(); ++i) addPaths(ty, i, adt.s[i]);
        for (Idx i = 0; i < adt.fieldTys.size(); ++i) addPaths(ty, adt.s.size() + i, adt.fieldTys[i]);
        break;
      }
      case util::index_of_type_v<Ty::Tuple, VTy>: {
        auto &tup = std::get<Ty::Tuple>(ty->v);
        for (Idx i = 0; i < tup.t.size(); ++i) addPaths(ty, i, tup.t[i]);
        break;
      }
      case util::index_of_type_v<Ty::Cyclic, VTy>: {
        auto &clc = std::get<Ty::Cyclic>(ty->v);
        addPathsTo(ty->free, 0, clc.ty->free);
        for (auto it = clc.ty->cycleRefs.upper_bound(0); it != clc.ty->cycleRefs.end(); ++it) {
          ty->cycleRefs.insert({it->first - 1, Path::cons(0, it->second)});
        }
        break;
      }
      case util::index_of_type_v<Ty::FfiFn, VTy>: {
        auto &ffifn = std::get<Ty::FfiFn>(ty->v);
        addPaths(ty, 0, ffifn.args);
        addPaths(ty, 1, ffifn.ret);
        break;
      }
      case util::index_of_type_v<Ty::Union, VTy>: {
        auto &unty = std::get<Ty::Union>(ty->v);
        for (Idx i = 0; i < unty.tys.size(); ++i) addPaths(ty, i, unty.tys[i]);
        break;
      }
      case util::index_of_type_v<Ty::TypeToken, VTy>: {
        auto &tyt = std::get<Ty::TypeToken>(ty->v);
        addPaths(ty, 0, tyt.ty);
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
  Tp uncycle(Tp);

  Tp unionOf(Tcx &tcx, const std::vector<Tp> &tys);

  Tp getAt(Tp ty, Idx i);

  Tp getPath(Tp ty, const Path::V &pathIn);

  Tp replacePath(Tp ty, const Path::V &path, Tp with);

  Tp maybeCycle(Tp ty, Tp tyVar);

  Tp replaceTy(Tp replaceIn, Tp toReplace, Tp replaceWith);

  Tp replaceTy(Tp replaceIn, const std::map<Tp, Tp> &replacements);
}

std::ostream &operator<<(std::ostream &os, type::Tp ty);

namespace type::print {
  void printTy(type::Tp t);
}
