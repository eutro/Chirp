#include "Type.h"

namespace compiler {
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

  BaseType::BaseType(size_t index) : index(index) {}
  bool BaseType::operator<(const BaseType &o) const { return index < o.index; }
  bool BaseType::operator==(const BaseType &o) const { return index == o.index; }
  bool BaseType::operator!=(const BaseType &o) const { return index != o.index; }
  std::ostream &operator<<(std::ostream &os, const BaseType &b) {
    os << "fn";
    return os;
  }

  Type::Aggregate::Aggregate(BaseType &&base, std::vector<Type *> &&values)
      : base(base), values(values) {}

  Type::Type(std::variant<Named, Aggregate> &&value) : value(value) {}
  Type::Type(Name &&name)
      : value((Named) {
      .name = name,
      .size = 1,
      .parent = this,
  }) {}

  Type Type::named(Name &&name) {
    return Type(std::forward<Name>(name));
  }
  Type Type::aggregate(BaseType &&base, std::vector<Type *> &&params) {
    return Type(std::variant<Named, Aggregate>
                    (std::in_place_type<Aggregate>,
                     std::forward<BaseType>(base),
                     std::forward<std::vector<Type *>>(params)));
  }

  void Type::getFree(const std::function<void(Type *)> &acc) {
    switch (value.index()) {
      case 0:
        acc(this);
        break;
      case 1:
        for (Type *t : std::get<1>(value).values) {
          t->get().getFree(acc);
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
        os << aggr.base << "<";
        for (auto it = aggr.values.begin(); it != aggr.values.end();) {
          os << (**it).get();
          if (++it != aggr.values.end()) {
            os << ", ";
          }
        }
        os << ">";
        break;
    }
    return os;
  }

  Type &Type::get() {
    switch (value.index()) {
      case 0: {
        Type::Named &named = std::get<0>(value);
        if (this != named.parent) {
          named.parent = &named.parent->get();
        }
        return *named.parent;
      }
      default:
        return *this;
    }
  }

  void Type::unify(Type &o) {
    if (this == &o)
      return;
    if (value.index() == 0) {
      Named &tn = std::get<0>(value);
      if (o.value.index() == 0) {
        Named &on = std::get<0>(o.value);
        Type *min, *max;
        if (tn.size < on.size) {
          min = this;
          max = &o;
        } else {
          max = this;
          min = &o;
        }
        std::get<0>(max->value).size += std::get<0>(min->value).size;
        std::get<0>(min->value).parent = max;
      } else {
        std::set<Type *> free;
        o.getFree([&free](Type *v) { free.insert(v); });
        if (free.count(this)) {
          throw std::runtime_error("Recursive type");
        }
        tn.parent = &o;
      }
    } else if (o.value.index() == 0) {
      o.unify(*this);
    } else {
      Aggregate &ta = std::get<1>(value);
      Aggregate &oa = std::get<1>(o.value);
      if (ta.base != oa.base) {
        throw std::runtime_error("Differing primitive type");
      } else {
        if (ta.values.size() != oa.values.size()) {
          throw std::runtime_error("Differing arities");
        }
        auto ti = ta.values.begin();
        auto oi = oa.values.begin();
        for (; ti != ta.values.end(); ++ti, ++oi) {
          (**ti).get().unify((**oi).get());
        }
      }
    }
  }

  PolyType::PolyType(Type *type) : type(type) {}

  void PolyType::getFree(const std::function<void(Type *)> &acc) {
    type->get().getFree([&acc, this](Type *v) {
      if (!bound.count(v)) {
        acc(v);
      }
    });
  }

  Type *TypeContext::push(Type &&type) {
    types.push_back(std::make_unique<Type>(type));
    return &*types.back();
  }

  Type *TypeContext::fresh() {
    return push(Type::named(counter++));
  }

  Type *TypeContext::inst(const PolyType &poly) {
    std::map<Type *, Type *> subs;
    for (Type *b : poly.bound) {
      subs[&b->get()] = &fresh()->get();
    }
    return poly.type->get().replace(*this, subs);
  }

  PolyType TypeContext::gen(Type *type) {
    Type &gotten = type->get();
    PolyType pType(&gotten);
    std::set<Type *> free;
    for (PolyType *b : bound) {
      b->getFree([&free](Type *v) { free.insert(v); });
    }
    gotten.getFree([&pType, &free](Type *v) {
      if (!free.count(v)) {
        pType.bound.insert(v);
      }
    });
    return pType;
  }

  Type *Type::replace(TypeContext &ctx, std::map<Type *, Type *> &subs) {
    switch (value.index()) {
      case 0: {
        auto found = subs.find(this);
        if (found == subs.end()) {
          return this;
        } else {
          return found->second;
        }
      }
      default: {
        Type::Aggregate &aggr = std::get<Type::Aggregate>(value);
        std::vector<Type *> newValues(aggr.values.size());
        size_t i = 0;
        for (Type *v : aggr.values) {
          newValues[i++] = v->get().replace(ctx, subs);
        }
        if (newValues == aggr.values) {
          return this;
        } else {
          return ctx.push(Type::aggregate(BaseType(aggr.base), std::move(newValues)));
        }
      }
    }
  }
}
