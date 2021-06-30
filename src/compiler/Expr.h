#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <stdexcept>
#include <utility>
#include <variant>
#include <vector>

namespace compiler {
  class Name {
  public:
    size_t index;

    Name(size_t index) : index(index) {}

    bool operator<(const Name &o) const { return index < o.index; }

    bool operator==(const Name &o) const { return index == o.index; }

    bool operator!=(const Name &o) const { return index != o.index; }

    friend std::ostream &operator<<(std::ostream &os, const Name &n) {
      size_t i = n.index;
      while (true) {
        os << ((char)('a' + (i % 26)));
        i /= 26;
        if (i == 0)
          return os;
      }
    }
  };

  class Base {
  public:
    size_t index;

    Base(size_t index) : index(index) {}

    friend std::ostream &operator<<(std::ostream &os, const Base &b) {
      os << "fn";
      return os;
    }

    bool operator<(const Base &o) const { return index < o.index; }

    bool operator==(const Base &o) const { return index == o.index; }

    bool operator!=(const Base &o) const { return index != o.index; }
  };

  class Context;

  class Type {
  public:
    struct Named {
      Name name;
      size_t size;
      Type *parent;
    };
    class Aggregate {
    public:
      Base base;
      std::vector<Type *> values;

      Aggregate(Base &&base, std::vector<Type *> &&values)
        : base(base), values(values) {}
    };

    std::variant<Named, Aggregate> value;

  private:
    Type(std::variant<Named, Aggregate> &&value): value(value) {}

    Type(Name &&name)
        : value((Named){
            .name = name,
            .size = 1,
            .parent = this,
          }) {}

  public:
    static Type named(Name &&name) {
      return Type(std::forward<Name>(name));
    }

    static Type aggregate(Base &&base, std::vector<Type*> &&params) {
      return Type(std::variant<Named, Aggregate>(std::in_place_type<Aggregate>,
                                                 std::forward<Base>(base),
                                                 std::forward<std::vector<Type*>>(params)));
    }

    void getFree(std::set<Type *> &set) {
      switch (value.index()) {
      case 0:
        set.insert(this);
        break;
      case 1:
        for (Type *t : std::get<1>(value).values) {
          t->get().getFree(set);
        }
        break;
      }
    }

    friend std::ostream &operator<<(std::ostream &os, const Type &t) {
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

    Type &get() {
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

    Type *replace(Context &ctx, std::map<Type *, Type *> &subs);

    void unify(Type &o) {
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
          o.getFree(free);
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
  };

  class PolyType {
  public:
    std::set<Type *> bound;
    Type *type;

    PolyType(Type *type) : type(type) {}

    void getFree(std::set<Type *> &set) {
      type->get().getFree(set);
      for (Type *b : bound) {
        set.erase(&b->get());
      }
    }
  };

  class Context {
  public:
    std::vector<Type *> bound;
    std::vector<std::unique_ptr<Type>> types;
    size_t counter;

    Type *push(Type &&type) {
      types.push_back(std::make_unique<Type>(type));
      return &*types.back();
    }

    Type *fresh() {
      return push(Type::named(counter++));
    }

    Type *inst(const PolyType &poly) {
      std::map<Type *, Type *> subs;
      for (Type *b : poly.bound) {
        subs[&b->get()] = &fresh()->get();
      }
      return poly.type->get().replace(*this, subs);
    }

    PolyType gen(Type *type) {
      Type &gotten = type->get();
      PolyType pType(&gotten);
      gotten.getFree(pType.bound);
      std::set<Type*> free;
      for (Type *b : bound) {
        b->get().getFree(free);
      }
      for (Type *f : free) {
        pType.bound.erase(f);
      }
      return pType;
    }
  };

  Type *Type::replace(Context &ctx, std::map<Type *, Type *> &subs) {
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
        return ctx.push(Type::aggregate(Base(aggr.base), std::move(newValues)));
      }
    }
    }
  }

  class Expr {
    Type *type;

  public:
    virtual ~Expr() = default;

  protected:
    virtual Type *infer(Context &ctx) = 0;

  public:
    Type *inferType(Context &ctx) { return type = infer(ctx); }

    Type &getType() {
      return type->get();
    }
  };

  class Var {
  public:
    PolyType type;
  };

  class FnExpr : public Expr {
  public:
    std::vector<Var *> params;
    std::unique_ptr<Expr> body;

    Type *infer(Context &ctx) override {
      std::vector<Type *> values;
      size_t oldSize = ctx.bound.size();
      for (Var *param : params) {
        Type *pType;
        param->type = pType = ctx.fresh();
        values.push_back(pType);
        ctx.bound.push_back(pType);
      }
      Type *bodyType = body->inferType(ctx);
      ctx.bound.resize(oldSize);
      values.push_back(bodyType);
      return ctx.push(Type::aggregate(0, std::move(values)));
    }
  };

  class AppExpr : public Expr {
  public:
    std::unique_ptr<Expr> func;
    std::vector<std::unique_ptr<Expr>> args;

    Type *infer(Context &ctx) override {
      std::vector<Type *> values;
      for (std::unique_ptr<Expr> &arg : args) {
        values.push_back(arg->inferType(ctx));
      }
      Type *type = ctx.fresh();
      values.push_back(type);
      Type *funcType = ctx.push(Type::aggregate(0, std::move(values)));
      funcType->unify(*func->inferType(ctx));
      return type;
    }
  };

  class LetExpr : public Expr {
  public:
    std::vector<std::pair<Var, std::unique_ptr<Expr>>> bindings;
    std::unique_ptr<Expr> body;

    Type *infer(Context &ctx) override {
      for (auto &b : bindings) {
        b.first.type = ctx.gen(b.second->inferType(ctx));
      }
      return body->inferType(ctx);
    }
  };

  class VarExpr : public Expr {
  public:
    Var &var;

    VarExpr(Var &var) : var(var) {}

    Type *infer(Context &ctx) override { return ctx.inst(var.type); }
  };
}
