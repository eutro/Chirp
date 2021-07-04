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

  Type::Aggregate::Aggregate(std::shared_ptr<BaseType> &base, std::vector<Type *> &&values)
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
  Type Type::aggregate(std::shared_ptr<BaseType> &base, std::vector<Type *> &&params) {
    return Type(std::variant<Named, Aggregate>
                    (std::in_place_type<Aggregate>,
                     base,
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
        os << *aggr.base;
        if (!aggr.values.empty()) {
          os << "<";
          for (auto it = aggr.values.begin(); it != aggr.values.end();) {
            os << (**it).get();
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

  Type &Type::weakGet() {
    switch (value.index()) {
      case 0: {
        Type::Named &named = std::get<0>(value);
        if (named.parent == nullptr) return *this;
        return *(named.parent = &named.parent->get());
      }
      default:
        return *this;
    }
  }

  Type &Type::get() {
    switch (value.index()) {
      case 0: {
        Type::Named &named = std::get<0>(value);
        if (named.parent == nullptr) {
          if (named.weakParent == nullptr) return *this;
          return named.weakParent->get();
        }
        return (named.parent = &named.parent->weakGet())->get();
      }
      default:
        return *this;
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
          throw typeError(o, *this, "it would create a recursive type");
        }
        tn.parent = &o;
      }
    } else if (o.value.index() == 0) {
      o.unify(*this);
    } else {
      Aggregate &ta = std::get<1>(value);
      Aggregate &oa = std::get<1>(o.value);
      if (ta.base != oa.base) {
        throw typeError(o, *this, "the base types are different");
      } else {
        if (ta.values.size() != oa.values.size()) {
          throw typeError(o, *this, "the number of parameters is different");
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
    return inst(poly, subs);
  }
  Type *TypeContext::inst(const PolyType &poly, std::map<Type *, Type *> &subs) {
    for (Type *b : poly.bound) {
      subs[&b->get()] = &fresh()->get();
    }
    return poly.type->get().replace(*this, subs);
  }

  PolyType TypeContext::gen(Type *type) {
    Type &gotten = type->get();
    PolyType pType(&gotten);
    std::set<Type *> free;
    for (auto &b : bound) {
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
          return ctx.push(Type::aggregate(aggr.base, std::move(newValues)));
        }
      }
    }
  }

  bool CompareType::operator()(Type *a, Type *b) const {
    return a->get() < b->get();
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
}
