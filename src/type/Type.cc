#include "Type.h"

#define IMPL_OPS(TYPE, LHSA, RHSA)                               \
  bool type::TYPE::operator<(const type::TYPE &o) const {        \
    return std::make_tuple LHSA < std::make_tuple RHSA; }        \
  bool type::TYPE::operator==(const type::TYPE &o) const {       \
    return std::make_tuple LHSA == std::make_tuple RHSA; }
#define IMPL_SINGLETON(TYPE)                                            \
  bool type::TYPE::operator<(const type::TYPE &o) const { return false; } \
  bool type::TYPE::operator==(const type::TYPE &o) const { return true; }
#include "TypeImpl.h"

namespace type {
  Idx bitCount(IntSize i) {
    switch (i) {
      case IntSize::i8: return 8;
      case IntSize::i16: return 16;
      case IntSize::i32: return 32;
      case IntSize::i64: return 64;
      case IntSize::i128: return 128;
      default: throw 0;
    }
  }
  Idx bitCount(FloatSize f) {
    switch (f) {
      case FloatSize::f16: return 16;
      case FloatSize::f32: return 32;
      case FloatSize::f64: return 64;
      default: throw 0;
    }
  }

  Ty *uncycle(Tcx &tcx, Tbcx &tbcx, Ty *ty) {
    if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
      Idx depth = 0;
      auto uncycler = overloaded {
          [&](Tp rt) {
            if (std::holds_alternative<Ty::CyclicRef>(rt->v)) {
              Ty::CyclicRef &ref = std::get<Ty::CyclicRef>(rt->v);
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
      return replaceTy(tcx, tbcx, std::get<Ty::Cyclic>(ty->v).ty, uncycler);
    }
    return ty;
  }
}
