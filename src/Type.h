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

  class Type;

  using TPtr = std::shared_ptr<Type>;

  class Type {
  public:
    struct Named {
      Name name;
      size_t size;
      /**
       * Compressed path
       */
      TPtr parent;
      /**
       * Uncompressed path for temporary replacements
       */
      TPtr weakParent;

      bool operator<(const Named &rhs) const;
    };

    class Aggregate {
    public:
      std::shared_ptr<BaseType> base;
      std::vector<TPtr> values;

      Aggregate(std::shared_ptr<BaseType> &base, std::vector<TPtr> &&values);

      bool operator<(const Aggregate &rhs) const;
    };

    std::variant<Named, Aggregate> value;
  private:
    Type(std::variant<Named, Aggregate> &&value);
    Type(Name &&name);
  public:
    static Type named(Name &&name);
    static Type aggregate(std::shared_ptr<BaseType> &base, std::vector<TPtr> &&params);
    friend std::ostream &operator<<(std::ostream &os, const Type &t);

    static void getFree(TPtr self, const std::function<void(const TPtr &)> &acc);
    static TPtr weakGet(TPtr self);
    static TPtr get(TPtr self);
    static TPtr replace(TPtr self, TypeContext &ctx, std::map<TPtr, TPtr> &subs);
    static void unify(TPtr t, TPtr o);
    static void getAndUnify(TPtr t, TPtr o);

    bool operator<(const Type &rhs) const;
  };

  struct CompareType {
    bool operator()(TPtr a, TPtr b) const;
  };

  class PolyType {
  public:
    std::set<TPtr, CompareType> bound;
    TPtr type;
    PolyType(TPtr type);
    void getFree(const std::function<void(TPtr)> &acc);
    friend std::ostream &operator<<(std::ostream &os, const PolyType &t);
  };

  class TypeContext {
  public:
    std::vector<std::shared_ptr<PolyType>> bound;
    size_t counter = 0;

    TPtr push(Type &&type);
    TPtr fresh();
    TPtr inst(const PolyType &poly);
    TPtr inst(const PolyType &poly, std::map<TPtr, TPtr> &subs);
    PolyType gen(TPtr type);
  };
}
