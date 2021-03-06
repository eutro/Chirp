#include "Type.h"

#define IMPL_OPS(TYPE, LHSA, RHSA)                               \
  bool type::TYPE::operator<(const type::TYPE &o) const {        \
    return std::tie LHSA < std::tie RHSA; }        \
  bool type::TYPE::operator==(const type::TYPE &o) const {       \
    return std::tie LHSA == std::tie RHSA; }
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
      default: throw util::Unreachable();
    }
  }
  Idx bitCount(FloatSize f) {
    switch (f) {
      case FloatSize::f16: return 16;
      case FloatSize::f32: return 32;
      case FloatSize::f64: return 64;
      default: throw util::Unreachable();
    }
  }

  Tp uncycle(Tcx &tcx, Tp ty) {
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
  Tp uncycle(Tp ty) {
    return uncycle(*ty->tcx, ty);
  }

  Tp unionOf(Tcx &tcx, const std::vector<Tp> &tys) {
    if (tys.empty()) {
      return tcx.intern(Ty::Union{{}});
    }
    std::set<Tp> tySet;
    for (Tp ty : tys) {
      if (std::holds_alternative<Ty::Union>(ty->v) ||
          (std::holds_alternative<Ty::Cyclic>(ty->v) &&
              std::holds_alternative<Ty::Union>(std::get<Ty::Cyclic>(ty->v).ty->v) &&
                  (ty = uncycle(ty), true))) {
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
      return tcx.intern(Ty::Union{std::vector<Tp>(tySet.begin(), tySet.end())});
    }
  }

  void Tcx::addName(Idx i, std::string name) {
    if (!name.empty()) {
      names[i] = std::move(name);
    }
  }
}
