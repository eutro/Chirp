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

namespace type {
  class TypeError : public std::runtime_error {
  public:
    TypeError(const std::string &message);
  };

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
    std::string name;
    BaseType(const std::string &name);
    friend std::ostream &operator<<(std::ostream &os, const BaseType &b);
  };

  class TypeContext;

  class Type {
  public:
    struct Named {
      Name name;
      size_t size;
      /**
       * Compressed path
       */
      Type *parent;
      /**
       * Uncompressed path for temporary replacements
       */
      Type *weakParent;

      bool operator<(const Named &rhs) const;
    };

    class Aggregate {
    public:
      std::shared_ptr<BaseType> base;
      std::vector<Type *> values;

      Aggregate(std::shared_ptr<BaseType> &base, std::vector<Type *> &&values);

      bool operator<(const Aggregate &rhs) const;
    };

    std::variant<Named, Aggregate> value;
  private:
    Type(std::variant<Named, Aggregate> &&value);
    Type(Name &&name);
  public:
    static Type named(Name &&name);
    static Type aggregate(std::shared_ptr<BaseType> &base, std::vector<Type *> &&params);
    void getFree(const std::function<void(Type *)> &acc);
    friend std::ostream &operator<<(std::ostream &os, const Type &t);
    Type &weakGet();
    Type &get();
    Type *replace(TypeContext &ctx, std::map<Type *, Type *> &subs);
    void unify(Type &o);

    bool operator<(const Type &rhs) const;
  };

  struct CompareType {
    bool operator()(Type *a, Type *b) const;
  };

  class PolyType {
  public:
    std::set<Type *, CompareType> bound;
    Type *type;
    PolyType(Type *type);
    void getFree(const std::function<void(Type *)> &acc);
    friend std::ostream &operator<<(std::ostream &os, const PolyType &t);
  };

  class TypeContext {
  public:
    std::vector<std::shared_ptr<PolyType>> bound;
    std::vector<std::unique_ptr<Type>> types;
    size_t counter = 0;

    Type *push(Type &&type);
    Type *fresh();
    Type *inst(const PolyType &poly);
    Type *inst(const PolyType &poly, std::map<Type *, Type *> &subs);
    PolyType gen(Type *type);
  };
}