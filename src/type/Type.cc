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

  Tp uncycle(Tp ty) {
    if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
      Tp ret = std::get<Ty::Cyclic>(ty->v).ty;
      for (auto &c : ty->cycleRefs.at(0)) {
        ret = replacePath(ret, c->cdr, ty);
      }
      return ret;
    }
    return ty;
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

  Tp getAt(Tp ty, Idx i) {
    switch (ty->v.index()) {
      case util::index_of_type_v<Ty::ADT, VTy>: {
        auto &adt = std::get<Ty::ADT>(ty->v);
        if (i < adt.s.size()) {
          ty = adt.s[i];
        } else {
          ty = adt.fieldTys.at(i - adt.s.size());
        }
        break;
      }
      case util::index_of_type_v<Ty::Tuple, VTy>: {
        auto &tup = std::get<Ty::Tuple>(ty->v);
        ty = tup.t.at(i);
        break;
      }
      case util::index_of_type_v<Ty::Cyclic, VTy>: {
        auto &clc = std::get<Ty::Cyclic>(ty->v);
        ty = clc.ty;
        break;
      }
      case util::index_of_type_v<Ty::FfiFn, VTy>: {
        auto &ffifn = std::get<Ty::FfiFn>(ty->v);
        ty = i == 0 ? ffifn.args : ffifn.ret;
        break;
      }
      case util::index_of_type_v<Ty::Union, VTy>: {
        auto &unty = std::get<Ty::Union>(ty->v);
        ty = unty.tys.at(i);
        break;
      }
      case util::index_of_type_v<Ty::TypeToken, VTy>: {
        auto &tyt = std::get<Ty::TypeToken>(ty->v);
        ty = tyt.ty;
        break;
      }
      case util::index_of_type_v<Ty::Err, VTy>:
      case util::index_of_type_v<Ty::Bool, VTy>:
      case util::index_of_type_v<Ty::Int, VTy>:
      case util::index_of_type_v<Ty::UInt, VTy>:
      case util::index_of_type_v<Ty::Float, VTy>:
      case util::index_of_type_v<Ty::String, VTy>:
      case util::index_of_type_v<Ty::Placeholder, VTy>:
      case util::index_of_type_v<Ty::Undetermined, VTy>:
      case util::index_of_type_v<Ty::CyclicRef, VTy>:
      default:
        throw util::ICE("Path does not exist");
    }
    return ty;
  }

  Tp getPath(Tp ty, const Path::V &pathIn) {
    const Path::V *pathPtr = &pathIn;
    for (; *pathPtr; pathPtr = &(*pathPtr)->cdr) {
      Idx car = (*pathPtr)->car;
      ty = getAt(ty, car);
    }
    return ty;
  }

  Tp replacePath(Tp ty, const Path::V &path, Tp with) {
    if (!path) return with;
    Idx car = path->car;
    switch (ty->v.index()) {
      case util::index_of_type_v<Ty::ADT, VTy>: {
        auto adt = std::get<Ty::ADT>(ty->v);
        if (car < adt.s.size()) {
          adt.s[car] = replacePath(adt.s[car], path->cdr, with);
        } else {
          Idx i = car - adt.s.size();
          adt.fieldTys[i] = replacePath(adt.fieldTys[i], path->cdr, with);
        }
        return ty->tcx->intern(adt);
      }
      case util::index_of_type_v<Ty::Tuple, VTy>: {
        auto tup = std::get<Ty::Tuple>(ty->v);
        tup.t[car] = replacePath(tup.t.at(car), path->cdr, with);
        return ty->tcx->intern(tup);
      }
      case util::index_of_type_v<Ty::Cyclic, VTy>: {
        auto clc = std::get<Ty::Cyclic>(ty->v);
        clc.ty = replacePath(clc.ty, path->cdr, with);
        return ty->tcx->intern(clc);
      }
      case util::index_of_type_v<Ty::FfiFn, VTy>: {
        auto ffifn = std::get<Ty::FfiFn>(ty->v);
        if (car == 0) {
          ffifn.args = replacePath(ffifn.args, path->cdr, with);
        } else {
          ffifn.ret = replacePath(ffifn.ret, path->cdr, with);
        }
        return ty->tcx->intern(ffifn);
      }
      case util::index_of_type_v<Ty::Union, VTy>: {
        auto unty = std::get<Ty::Union>(ty->v);
        unty.tys[car] = replacePath(unty.tys.at(car), path->cdr, with);
        return ty->tcx->intern(unty);
      }
      case util::index_of_type_v<Ty::TypeToken, VTy>: {
        auto tyt = std::get<Ty::TypeToken>(ty->v);
        tyt.ty = replacePath(tyt.ty, path->cdr, with);
        return ty->tcx->intern(tyt);
      }
      case util::index_of_type_v<Ty::Err, VTy>:
      case util::index_of_type_v<Ty::Bool, VTy>:
      case util::index_of_type_v<Ty::Int, VTy>:
      case util::index_of_type_v<Ty::UInt, VTy>:
      case util::index_of_type_v<Ty::Float, VTy>:
      case util::index_of_type_v<Ty::String, VTy>:
      case util::index_of_type_v<Ty::Placeholder, VTy>:
      case util::index_of_type_v<Ty::Undetermined, VTy>:
      case util::index_of_type_v<Ty::CyclicRef, VTy>:
      default:
        throw util::ICE("Path does not exist");
    }
  }

  Tp maybeCycle(Tp ty, Tp tyVar) {
    if (!ty->countFree(tyVar)) return ty;
    for (auto &p : ty->free.at(tyVar)) {
      Idx depth = 0;
      Tp pTy = ty;
      for (const Path::V *pp = &p; *pp; pp = &(*pp)->cdr) {
        if (std::holds_alternative<Ty::Cyclic>(pTy->v)) depth++;
        pTy = getAt(pTy, (*pp)->car);
      }
      ty = replacePath(ty, p, ty->tcx->intern(Ty::CyclicRef{depth}));
    }
    return ty->tcx->intern(Ty::Cyclic{ty});
  }

  Tp replaceTy(Tp replaceIn, Tp toReplace, Tp replaceWith) {
    if (!replaceIn->countFree(toReplace) || toReplace == replaceWith) return replaceIn;
    for (auto &p : replaceIn->free.at(toReplace)) {
      replaceIn = replacePath(replaceIn, p, replaceWith);
    }
    return replaceIn;
  }

  Tp replaceTy(Tp replaceIn, const std::map<Tp, Tp> &replacements) {
    if (replaceIn->free.size() < replacements.size()) {
      for (const auto &fv : replaceIn->free) {
        auto found = replacements.find(fv.first);
        if (found != replacements.end() && fv.first != found->second) {
          for (const auto &p : fv.second) {
            replaceIn = replacePath(replaceIn, p, found->second);
          }
        }
      }
    } else {
      for (const auto &e : replacements) {
        replaceIn = replaceTy(replaceIn, e.first, e.second);
      }
    }
    return replaceIn;
  }

  void Tcx::addName(Idx i, std::string name) {
    if (!name.empty()) {
      names[i] = std::move(name);
    }
  }
}
