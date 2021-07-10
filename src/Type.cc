#include "Type.h"
#include <sstream>

namespace type {
  TypeError::TypeError(const TPtr &leafA, const TPtr &leafB,
                       const std::string &reason,
                       const TPtr &rootA, const TPtr &rootB)
      : leafA(leafA), leafB(leafB), reason(reason), rootA(rootA), rootB(rootB) {}
  std::ostream &operator<<(std::ostream &os, const TypeError &error) {
    os << "Could not unify\n  "
       << *error.leafA
       << "\n and\n  "
       << *error.leafB << "\n"
       << " because " << error.reason;
    if (error.rootA != error.leafA ||
        error.rootB != error.leafB) {
      os << "\n while unifying\n    "
         << *error.rootA
         << "\n and\n    "
         << *error.rootB;
    }
    return os;
  }

  Name::Name(size_t index) : index(index) {}
  bool Name::operator<(const Name &o) const { return index < o.index; }
  bool Name::operator==(const Name &o) const { return index == o.index; }
  bool Name::operator!=(const Name &o) const { return index != o.index; }
  std::ostream &operator<<(std::ostream &os, const Name &n) {
    size_t i = n.index;
    while (true) {
      os << ((char) ('a' + (i % 26)));
      i /= 26;
      if (i == 0)
        return os;
    }
  }

  BaseType::BaseType(const std::string &name) : name(name) {}
  std::ostream &operator<<(std::ostream &os, const BaseType &b) {
    os << b.name;
    return os;
  }

  Type::Aggregate::Aggregate(std::shared_ptr<BaseType> &base, std::vector<TPtr> &&values)
      : base(base), values(values) {}

  Type::Type(std::variant<Named, Aggregate> &&value) : value(value) {}
  Type::Type(Name &&name)
      : value((Named) {
      .name = name,
      .size = 1,
      .parent = nullptr,
      .weakParent = nullptr,
  }) {}

  Type Type::named(Name &&name) {
    return Type(std::forward<Name>(name));
  }
  Type Type::aggregate(std::shared_ptr<BaseType> &base, std::vector<TPtr> &&params) {
    return Type(std::variant<Named, Aggregate>
                    (std::in_place_type<Aggregate>,
                     base,
                     std::forward<std::vector<TPtr >>(params)));
  }

  void Type::getFree(TPtr self, const std::function<void(const TPtr &)> &acc) {
    switch (self->value.index()) {
      case 0:
        acc(self);
        break;
      case 1:
        for (TPtr t : std::get<1>(self->value).values) {
          Type::getFree(Type::get(t), acc);
        }
        break;
    }
  }

  std::ostream &operator<<(std::ostream &os, const Type &t) {
    switch (t.value.index()) {
      case 0: {
        const Type::Named &n = std::get<0>(t.value);
        os << n.name;
        if (!n.traits.empty()) {
          os << ":";
          for (const auto &trait : n.traits) {
            os << " " << *trait;
          }
        }
        break;
      }
      case 1: {
        const auto &aggr = std::get<1>(t.value);
        os << *aggr.base;
        if (!aggr.values.empty()) {
          os << "<";
          for (auto it = aggr.values.begin(); it != aggr.values.end();) {
            os << *Type::get(*it);
            if (++it != aggr.values.end()) {
              os << ", ";
            }
          }
          os << ">";
        }
        break;
      }
    }
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const PolyType &t) {
    if (!t.bound.empty()) {
      os << "<";
      for (auto it = t.bound.begin(); it != t.bound.end();) {
        os << **it;
        if (++it != t.bound.end()) os << ", ";
      }
      os << "> ";
    }
    os << *t.type;
    return os;
  }

  TPtr Type::weakGet(TPtr self) {
    switch (self->value.index()) {
      case 0: {
        Type::Named &named = std::get<0>(self->value);
        if (named.parent == nullptr) return self;
        return named.parent = Type::get(named.parent);
      }
      default:
        return self;
    }
  }

  TPtr Type::get(TPtr self) {
    switch (self->value.index()) {
      case 0: {
        Type::Named &named = std::get<0>(self->value);
        if (named.parent == nullptr) {
          if (named.weakParent == nullptr) return self;
          return Type::get(named.weakParent);
        }
        return Type::get(named.parent = Type::weakGet(named.parent));
      }
      default:
        return self;
    }
  }

  void unify(TypeContext &ctx, TPtr t, TPtr o,
             TPtr &rootA, TPtr &rootB) {
    if (t == o) {
      return;
    }
    if (t->value.index() == 0) {
      Type::Named &tn = std::get<0>(t->value);
      if (o->value.index() == 0) {
        Type::Named &on = std::get<0>(o->value);
        TPtr *min, *max;
        if (tn.size < on.size) {
          min = &t;
          max = &o;
        } else {
          max = &t;
          min = &o;
        }
        Type::Named &mxv = std::get<0>((*max)->value);
        Type::Named &mnv = std::get<0>((*min)->value);
        mxv.size += mnv.size;
        mnv.parent = *max;
        mxv.traits.insert(mnv.traits.begin(), mnv.traits.end());
      } else {
        std::set<TPtr> free;
        Type::getFree(o, [&free](TPtr v) { free.insert(v); });
        if (free.count(t)) {
          ctx.errors.emplace_back(t, o, "it would create a recursive type", rootA, rootB);
          return;
        }
        Type::Aggregate aggr = std::get<1>(o->value);
        for (const auto &trait : tn.traits) {
          if (!aggr.base->impls.count(trait)) {
            ctx.errors.emplace_back(t, o, "trait '" + trait->name + "' is not implemented", rootA, rootB);
            return;
          }
        }
        tn.parent = o;
      }
    } else if (o->value.index() == 0) {
      unify(ctx, o, t, o, t);
    } else {
      Type::Aggregate &ta = std::get<1>(t->value);
      Type::Aggregate &oa = std::get<1>(o->value);
      if (ta.base != oa.base) {
        ctx.errors.emplace_back(t, o, "the base types are different", rootA, rootB);
        return;
      } else {
        if (ta.values.size() != oa.values.size()) {
          ctx.errors.emplace_back(t, o, "the number of parameters is different", rootA, rootB);
          return;
        }
        auto ti = ta.values.begin();
        auto oi = oa.values.begin();
        for (; ti != ta.values.end(); ++ti, ++oi) {
          unify(ctx, Type::get(*ti), Type::get(*oi), rootA, rootB);
        }
      }
    }
  }

  void Type::unify(TypeContext &ctx, TPtr t, TPtr o) {
    ::type::unify(ctx, t, o, t, o);
  }

  void Type::getAndUnify(TypeContext &ctx, TPtr t, TPtr o) {
    unify(ctx, get(t), get(o));
  }

  PolyType::PolyType(TPtr type) : type(type) {}

  void PolyType::getFree(const std::function<void(TPtr)> &acc) {
    Type::getFree(Type::get(type), [&acc, this](const TPtr &v) {
      if (!bound.count(v)) {
        acc(v);
      }
    });
  }

  TPtr TypeContext::push(Type &&type) {
    return std::make_shared<Type>(type);
  }

  TPtr TypeContext::fresh() {
    return push(Type::named(counter++));
  }

  TPtr TypeContext::inst(const PolyType &poly) {
    std::map<TPtr, TPtr> subs;
    return inst(poly, subs);
  }
  TPtr TypeContext::inst(const PolyType &poly, std::map<TPtr, TPtr> &subs) {
    for (TPtr b : poly.bound) {
      subs[Type::get(b)] = Type::get(fresh());
    }
    return Type::replace(Type::get(poly.type), *this, subs);
  }

  PolyType TypeContext::gen(TPtr type) {
    TPtr gotten = Type::get(type);
    PolyType pType(gotten);
    std::set<TPtr> free;
    for (auto &b : bound) {
      b->getFree([&free](TPtr v) { free.insert(v); });
    }
    Type::getFree(gotten, [&pType, &free](const TPtr &v) {
      if (!free.count(v)) {
        pType.bound.insert(v);
      }
    });
    return pType;
  }

  TPtr Type::replace(TPtr self, TypeContext &ctx, std::map<TPtr, TPtr> &subs) {
    switch (self->value.index()) {
      case 0: {
        auto found = subs.find(self);
        if (found == subs.end()) {
          return self;
        } else {
          return found->second;
        }
      }
      default: {
        Type::Aggregate &aggr = std::get<Type::Aggregate>(self->value);
        std::vector<TPtr> newValues(aggr.values.size());
        size_t i = 0;
        for (TPtr v : aggr.values) {
          newValues[i++] = replace(Type::get(v), ctx, subs);
        }
        if (newValues == aggr.values) {
          return self;
        } else {
          return ctx.push(Type::aggregate(aggr.base, std::move(newValues)));
        }
      }
    }
  }

  bool CompareType::operator()(TPtr a, TPtr b) const {
    return *Type::get(a) < *Type::get(b);
  }

  bool Type::operator<(const Type &rhs) const {
    return value < rhs.value;
  }

  bool Type::Aggregate::operator<(const Type::Aggregate &rhs) const {
    if (base < rhs.base) return true;
    if (rhs.base < base) return false;
    if (values.size() < rhs.values.size()) return true;
    if (rhs.values.size() < values.size()) return false;
    return std::lexicographical_compare(values.begin(), values.end(),
                                        rhs.values.begin(), rhs.values.end(),
                                        CompareType());
  }

  bool Type::Named::operator<(const Type::Named &rhs) const {
    return name < rhs.name;
  }

  Trait::Trait(const std::string &name) : name(name) {
  }

  std::ostream &operator<<(std::ostream &os, const Trait &t) {
    os << t.name;
    return os;
  }
}
