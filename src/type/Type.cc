#include "Type.h"

#define IMPL_OPS(TYPE, LHSA, RHSA)                               \
  bool type::TYPE::operator<(const type::TYPE &o) const {        \
    return std::make_tuple LHSA < std::make_tuple RHSA; }        \
  bool type::TYPE::operator==(const type::TYPE &o) const {       \
    return std::make_tuple LHSA == std::make_tuple RHSA; }
#define IMPL_SINGLETON(TYPE)                                            \
  bool type::TYPE::operator<(const type::TYPE &) const { return false; } \
  bool type::TYPE::operator==(const type::TYPE &) const { return true; }
#include "TypeImpl.h"

namespace type {
  Idx bitCount(IntSize i) {
    switch (i) {
      case IntSize::i8: return 8;
      case IntSize::i16: return 16;
      case IntSize::i32: return 32;
      case IntSize::i64: return 64;
      case IntSize::i128: return 128;
      default: throw std::runtime_error("unreachable");
    }
  }
  Idx bitCount(FloatSize f) {
    switch (f) {
      case FloatSize::f16: return 16;
      case FloatSize::f32: return 32;
      case FloatSize::f64: return 64;
      default: throw std::runtime_error("unreachable");
    }
  }

  Ty *uncycle(Tcx &tcx, Ty *ty) {
    if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
      Idx depth = 0;
      auto uncycler = overloaded {
          [&](Tp rt) {
            if (std::holds_alternative<Ty::CyclicRef>(rt->v)) {
              auto &ref = std::get<Ty::CyclicRef>(rt->v);
              if (ref.depth == depth) {
                return ty;
              }
            }
            return rt;
          },
          [&](Tp ty, PreWalk) {
            if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
              ++depth;
            }
            return ty;
          },
          [&](Tp ty, PostWalk) {
            if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
              --depth;
            }
            return ty;
          },
      };
      return replaceTy(tcx, std::get<Ty::Cyclic>(ty->v).ty, uncycler);
    }
    return ty;
  }
  Ty *uncycle(Ty *ty) {
    return uncycle(*ty->tcx, ty);
  }

  Tp unionOf(Tcx &tcx, std::vector<Tp> &tys) {
    if (tys.empty()) {
      return tcx.intern(Ty::Union{{}});
    }
    std::set<Tp> tySet;
    for (Tp ty : tys) {
      if (std::holds_alternative<Ty::Union>(ty->v)) {
        auto &unioned = std::get<Ty::Union>(ty->v);
        std::copy(unioned.tys.begin(), unioned.tys.end(), std::inserter(tySet, tySet.end()));
      } else {
        tySet.insert(ty);
      }
    }
    if (tySet.empty()) {
      return tcx.intern(Ty::Union{});
    } else if (tySet.size() == 1) {
      return *tySet.begin();
    } else {
      tys.clear();
      std::copy(tySet.begin(), tySet.end(), std::back_inserter(tys));
      return tcx.intern(Ty::Union{tys});
    }
  }
}
