#include "UnifyMap.h"

namespace type::infer {
  bool IndexAndArityCmp::operator()(
    const std::vector<Tp> &lhs,
    const std::vector<Tp> &rhs
  ) const {
    return std::lexicographical_compare(
      lhs.begin(), lhs.end(),
      rhs.begin(), rhs.end(),
      IndexAndArityCmp()
    );
  }
  bool IndexAndArityCmp::operator()(Tp lhs, Tp rhs) const {
    size_t li = lhs->v.index();
    size_t ri = rhs->v.index();
    if (li != ri) return li < ri;
    using VTy = decltype(rhs->v);
    switch (li) {
    case util::index_of_type_v<Ty::Err, VTy>:
    case util::index_of_type_v<Ty::Bool, VTy>:
    case util::index_of_type_v<Ty::String, VTy>:
    case util::index_of_type_v<Ty::FfiFn, VTy>:
    case util::index_of_type_v<Ty::Never, VTy>:
      // all of these have the same arity and base type
      return false;
    case util::index_of_type_v<Ty::Int, VTy>:
    case util::index_of_type_v<Ty::UInt, VTy>:
    case util::index_of_type_v<Ty::Float, VTy>:
      // some of these may be distinct base types,
      // but otherwise have the same arity
      return *lhs < *rhs;
    case util::index_of_type_v<Ty::Cyclic, VTy>:
    case util::index_of_type_v<Ty::CyclicRef, VTy>:
    case util::index_of_type_v<Ty::Placeholder, VTy>:
    case util::index_of_type_v<Ty::TraitRef, VTy>:
      // all of these are illegal at this time
      throw 0;
    case util::index_of_type_v<Ty::ADT, VTy>: {
      // TODO variant support
      auto &lv = std::get<Ty::ADT>(lhs->v);
      auto &rv = std::get<Ty::ADT>(rhs->v);
      // base type comparison
      if (lv.i != rv.i) return lv.i < rv.i;
      // arity comparison
      return lv.s.size() < rv.s.size();
    }
    case util::index_of_type_v<Ty::Tuple, VTy>:
      // same base type, differing arities
      return std::get<Ty::Tuple>(lhs->v).t.size() < std::get<Ty::Tuple>(rhs->v).t.size();
    default: throw 0;
    }
  }

  void addChildren(std::vector<Tp> &c, Tp ty) {
    using VTy = decltype(ty->v);
    switch (ty->v.index()) {
    case util::index_of_type_v<Ty::Err, VTy>:
    case util::index_of_type_v<Ty::Bool, VTy>:
    case util::index_of_type_v<Ty::String, VTy>:
    case util::index_of_type_v<Ty::Int, VTy>:
    case util::index_of_type_v<Ty::UInt, VTy>:
    case util::index_of_type_v<Ty::Float, VTy>:
    case util::index_of_type_v<Ty::Never, VTy>:
      // 0 arity
      return;
    case util::index_of_type_v<Ty::Cyclic, VTy>:
    case util::index_of_type_v<Ty::CyclicRef, VTy>:
    case util::index_of_type_v<Ty::Placeholder, VTy>:
    case util::index_of_type_v<Ty::TraitRef, VTy>:
      // all of these are illegal at this time
      throw 0;
    case util::index_of_type_v<Ty::FfiFn, VTy>: {
      // arity 2
      auto &v = std::get<Ty::FfiFn>(ty->v);
      c.push_back(v.args);
      c.push_back(v.ret);
      return;
    }
    case util::index_of_type_v<Ty::ADT, VTy>: {
      // variadic
      auto &v = std::get<Ty::ADT>(ty->v);
      for (Tp sTy : v.s) {
        c.push_back(sTy);
      }
      return;
    }
    case util::index_of_type_v<Ty::Tuple, VTy>:
      auto &v = std::get<Ty::Tuple>(ty->v);
      std::copy(v.t.begin(), v.t.end(), std::back_inserter(c));
      return;
    }
  }

  std::vector<Tp> childrenOf(const std::vector<Tp> &tys, size_t hint) {
    std::vector<Tp> c;
    c.reserve(hint);
    for (auto &ty : tys) {
      addChildren(c, ty);
    }
    return c;
  }
}
