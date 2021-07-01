#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <stdexcept>
#include <utility>
#include <variant>
#include <vector>
#include <functional>

namespace compiler {
  class Name {
  public:
    size_t index;
    Name(size_t index);
    bool operator<(const Name &o) const;
    bool operator==(const Name &o) const;
    bool operator!=(const Name &o) const;
    friend std::ostream &operator<<(std::ostream &os, const Name &n);
  };

  class BaseType {
  public:
    size_t index;
    BaseType(size_t index);
    bool operator<(const BaseType &o) const;
    bool operator==(const BaseType &o) const;
    bool operator!=(const BaseType &o) const;
    friend std::ostream &operator<<(std::ostream &os, const BaseType &b);
  };

  class TypeContext;

  class Type {
  public:
    struct Named {
      Name name;
      size_t size;
      Type *parent;
    };

    class Aggregate {
    public:
      BaseType base;
      std::vector<Type *> values;

      Aggregate(BaseType &&base, std::vector<Type *> &&values);
    };

    std::variant<Named, Aggregate> value;
  private:
    Type(std::variant<Named, Aggregate> &&value);
    Type(Name &&name);
  public:
    static Type named(Name &&name);
    static Type aggregate(BaseType &&base, std::vector<Type *> &&params);
    void getFree(const std::function<void(Type *)> &acc);
    friend std::ostream &operator<<(std::ostream &os, const Type &t);
    Type &get();
    Type *replace(TypeContext &ctx, std::map<Type *, Type *> &subs);
    void unify(Type &o);
  };

  class PolyType {
  public:
    std::set<Type *> bound;
    Type *type;
    PolyType(Type *type);
    void getFree(const std::function<void(Type *)> &acc);
  };

  class TypeContext {
  public:
    std::vector<PolyType *> bound;
    std::vector<std::unique_ptr<Type>> types;
    size_t counter;

    Type *push(Type &&type);
    Type *fresh();
    Type *inst(const PolyType &poly);
    PolyType gen(Type *type);
  };
}
