#include "Type.h"
#include <sstream>

namespace type {
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
      case 0:
        os << std::get<0>(t.value).name;
        break;
      case 1:
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

  TypeError::TypeError(const std::string &message) :
      runtime_error(message) {}

  TypeError typeError(Type &base, Type &other,
                      const std::string &reason) {
    std::stringstream buf;
    buf << "Could not unify\n    "
        << base
        << "\n   and\n    "
        << other << "\n"
        << "   because " << reason;
    return TypeError(buf.str());
  }

  void Type::unify(TPtr t, TPtr o) {
    if (t == o) {
      return;
    }
    if (t->value.index() == 0) {
      Named &tn = std::get<0>(t->value);
      if (o->value.index() == 0) {
        Named &on = std::get<0>(o->value);
        TPtr *min, *max;
        if (tn.size < on.size) {
          min = &t;
          max = &o;
        } else {
          max = &t;
          min = &o;
        }
        std::get<0>((*max)->value).size += std::get<0>((*min)->value).size;
        std::get<0>((*min)->value).parent = *max;
      } else {
        std::set<TPtr> free;
        Type::getFree(o, [&free](TPtr v) { free.insert(v); });
        if (free.count(t)) {
          throw typeError(*o, *t, "it would create a recursive type");
        }
        tn.parent = o;
      }
    } else if (o->value.index() == 0) {
      Type::unify(o, t);
    } else {
      Aggregate &ta = std::get<1>(t->value);
      Aggregate &oa = std::get<1>(o->value);
      if (ta.base != oa.base) {
        throw typeError(*o, *t, "the base types are different");
      } else {
        if (ta.values.size() != oa.values.size()) {
          throw typeError(*o, *t, "the number of parameters is different");
        }
        auto ti = ta.values.begin();
        auto oi = oa.values.begin();
        for (; ti != ta.values.end(); ++ti, ++oi) {
          getAndUnify(*ti, *oi);
        }
      }
    }
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

  void Type::getAndUnify(TPtr t, TPtr o) {
    unify(get(t), get(o));
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
}
