#include "LookupTable.h"

#include "../TypePrint.h"

#include <memory>
#include <map>
#include <sstream>
#include <variant>

namespace type::infer {
  struct OverloadLookup {
    std::vector<std::pair<std::vector<Tp>, Fn>> overloads;
    std::optional<Fn> fallback;
    std::vector<Fn*> lookup(const std::vector<Tp> &args) {
      std::vector<Fn*> candidates;
      for (auto &overload : overloads) {
        if (tryMatch(overload.first, args)) {
          candidates.push_back(&overload.second);
        }
      }
      if (candidates.empty() && fallback) {
        candidates.push_back(&*fallback);
      }
      return candidates;
    }
    void insert(const std::vector<Tp> &params, Fn &&fnv) {
      overloads.emplace_back(params, std::forward<Fn>(fnv));
    }
    void insertFallback(Fn &&fnv) {
      fallback = std::forward<Fn>(fnv);
    }
    static bool tryMatch(const std::vector<Tp> &to, const std::vector<Tp> &from) {
      if (to.size() != from.size()) {
        return false;
      }
      auto tI = to.begin();
      auto fI = from.begin();
      for (; tI != to.end(); ++tI, ++fI) {
        if (!tryMatchTy(*tI, *fI)) return false;
      }
      return true;
    }
    static bool tryMatchTy(Tp to, Tp from) {
      if (to == from) return true;
      if (std::holds_alternative<Ty::Placeholder>(to->v)) return true;
      if (to->v.index() != from->v.index()) return false;
      return std::visit(
        overloaded {
          [](Ty::ADT &lhs, Ty::ADT &rhs){
            if (lhs.i != rhs.i) return false;
            if (lhs.v != rhs.v) return false;
            return tryMatch(lhs.s, rhs.s);
          },
          [](Ty::Tuple &lhs, Ty::Tuple &rhs) {
            return tryMatch(lhs.t, rhs.t);
          },
          [](Ty::FfiFn &lhs, Ty::FfiFn &rhs) {
            return tryMatchTy(lhs.args, rhs.args) && tryMatchTy(lhs.ret, rhs.ret);
          },
          [](const auto&,const auto&){return false;},
        },
        uncycle(to)->v,
        uncycle(from)->v);
    }
  };

  struct LookupTableImpl : public LookupTable {
    std::map<std::pair<LookupKey*, std::vector<Constant>>, OverloadLookup> fns;
    Fn *lookupFn(
      LookupKey *fn, 
      const std::vector<Constant> &constants,
      const std::vector<Tp> &args
    ) override {
      auto found = fns.find({fn, constants});
      if (found == fns.end()) {
        std::stringstream s;
        s << "Undefined function: " << fn->value;
        if (!constants.empty()) {
          s << "\n with constants:";
          for (const auto &c : constants) {
            s << " " << c;
          }
        }
        throw std::runtime_error(s.str());
      }
      OverloadLookup &overloads = found->second;
      const std::vector<Fn *> &lookedUp = overloads.lookup(args);
      if (lookedUp.empty()) {
        std::stringstream s;
        s << "Undefined function: " << fn->value;
        if (!constants.empty()) {
          s << "\n with constants:";
          for (const auto &c : constants) {
            s << " " << c;
          }
        }
        s << "\n with types:";
        for (const auto &t : args) {
          s << " " << t;
        }
        throw std::runtime_error(s.str());
      }
      return lookedUp.at(0);
    }
    void insertFn(
      LookupKey *fn,
      const std::vector<Constant> &constants,
      const std::vector<Tp> &params,
      Fn &&fnv
    ) override {
      fns[{fn, constants}].insert(params, std::forward<Fn>(fnv));
    }
    void insertFallback(
        LookupKey *fn,
        const std::vector<Constant> &constants,
        Fn &&fnv
    ) override {
      fns[{fn, constants}].insertFallback(std::forward<Fn>(fnv));
    }
  };

  static arena::InternArena<LookupKey> keys;
  LookupKey *LookupKey::intern(const std::string &value) {
    return keys.intern(std::string(value));
  }

  std::unique_ptr<LookupTable> LookupTable::create() {
    return std::make_unique<LookupTableImpl>();
  }
}
