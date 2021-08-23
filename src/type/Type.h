#pragma once

#include "../common/Err.h"
#include "../common/Util.h"

#include <sstream>

namespace type {
  class Type;
  class Trait;
  class BaseType;

  struct TypeRef {
    err::Location loc;
    Type **raw;
    TypeRef(Type **&&raw): raw(raw) {}
    Type &operator*() { return **raw; }
    Type *operator->() { return &**this; }
    void set(const TypeRef &o) { raw = o.raw; }
  };

  class TraitBoundary {
  public:
    Trait *trait;
    std::vector<TypeRef> params;
  };

  class ConcreteType {
  public:
    BaseType *base;
    std::vector<TypeRef> params;
  };

  using TIdx = std::int32_t;
  using Idx = std::uint32_t;

  class Type {
  public:
    std::optional<ConcreteType> concrete;
    std::vector<TraitBoundary> bounds;
  };

  class TypeTemplate {
  public:
    Idx preVariadic;
    std::optional<Idx> postVariadic;
  };

  class TypeMapping {
  public:
    std::vector<Idx> idces;

    std::vector<Idx> map(const TypeTemplate &tt, Idx arity) {
      std::vector<Idx> mapping;
      for (Idx i : idces) {
        if (i < tt.preVariadic) {
          mapping.push_back(i);
        } else if (i > tt.preVariadic) {
          mapping.push_back(arity - *tt.postVariadic - tt.preVariadic + i);
        } else {
          for (Idx nI = tt.preVariadic;
               nI < arity - *tt.postVariadic;
               ++nI) {
            mapping.push_back(nI);
          }
        }
      }
      return mapping;
    }
  };

  class Trait {
  public:
    TypeTemplate types;
    std::vector<TypeMapping> methods;
  };

  class BaseType {
  public:
    TypeTemplate types;
    struct TraitImpl {
      Trait *trait;
      std::vector<TypeMapping> methodImpls;
    };
    std::vector<TraitImpl> traitImpls;
  };

  class Unification {
  public:
    TypeRef target, dart;
    err::ErrorContext &ec;

    Unification(err::ErrorContext &ec,
                TypeRef target, TypeRef dart):
      ec(ec),
      target(target),
      dart(dart) {}

    Unification(const Unification &o,
                TypeRef target, TypeRef dart):
      ec(o.ec),
      target(target),
      dart(dart) {}

    bool unifyBounds(ConcreteType &ct,
                     std::vector<TraitBoundary> &traitBounds,
                     bool targetConcrete) {
      for (TraitBoundary &tb : traitBounds) {
        Trait *trait = tb.trait;
        for (BaseType::TraitImpl &ti : ct.base->traitImpls) {
          if (ti.trait != trait) continue;
          for (Idx method = 0; method < trait->methods.size(); ++method) {
            auto tBMapping = trait->methods[method].map(trait->types, tb.params.size());
            auto cTMapping = ti.methodImpls[method].map(ct.base->types, ct.params.size());
            for (Idx param = 0; param < tBMapping.size(); ++param) {
              TypeRef bP = tb.params[tBMapping[param]];
              TypeRef iP = ct.params[cTMapping[param]];
              if (!(targetConcrete ?
                    Unification(*this, iP, bP) :
                    Unification(*this, bP, iP)).run()) {
                return false;
              }
            }
          }
        }
      }
      return true;
    }

    bool run() {
      if (!dart->concrete && dart->bounds.empty()) {
        dart.set(target);
      } else if (!target->concrete && target->bounds.empty()) {
        target.set(dart);
      }
      if (!dart->concrete) {
        if (!target->concrete) {
          std::copy(dart->bounds.begin(),
                    dart->bounds.end(), 
                    std::back_inserter(target->bounds));
        } else {
          if (!unifyBounds(*target->concrete, dart->bounds, true)) {
            return false;
          }
        }
        dart.set(target);
      } else if (!target->concrete) {
        if (!unifyBounds(*dart->concrete, target->bounds, false)) {
          return false;
        }
        target.set(dart);
      } else {
        if (target->concrete->base !=
            dart->concrete->base) {
          ec.err()
            .msg("Could not unify types:")
            .msg("Differing base types");
          return false;
        }
        if (target->concrete->params.size() !=
            dart->concrete->params.size()) {
          ec.err()
            .msg("Could not unify types:")
            .msg("Differing parameter arities");
          return false;
        }
        auto &tP = target->concrete->params;
        auto &dP = dart->concrete->params;
        auto tIt = tP.begin();
        auto dIt = dP.begin();
        Idx paramIdx = 0;
        for (; tIt != tP.end(); ++tIt, ++dIt, ++paramIdx) {
          Unification unif(*this, *tIt, *dIt);
          std::stringstream ss;
          ss << "Parameter " << paramIdx << " of:";
          std::string paramMsg = ss.str();
          if (!unif.run()) {
            return false;
          }
        }
      }
      return true;
    }
  };
}
