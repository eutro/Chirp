#pragma once

#include <algorithm>
#include <optional>
#include <set>
#include <tuple>
#include <variant>

namespace util {
  template <typename Data = std::monostate>
  struct DSU {
    DSU *parent = this;
    size_t size = 0;
    Data data;

    template <typename... Arg>
    DSU(Arg... arg): data(std::forward<Arg>(arg)...) {}

    DSU *find() {
      if (parent != this) {
        parent = parent->find();
      }
      return parent;
    }

    void unite(DSU *o) {
      DSU *tRoot = find();
      DSU *oRoot = o->find();
      if (tRoot == oRoot) return;
      DSU *minRoot, *maxRoot;
      std::tie(minRoot, maxRoot) = std::minmax(minRoot, maxRoot, [](DSU *a, DSU *b) {
        return a->size < b->size;
      });
      minRoot->parent = maxRoot;
      maxRoot->size += minRoot->size;
    }
  };

  template <typename Compare = std::less<>>
  struct DerefCmp {
    template <typename LHS, typename RHS>
    bool operator()(const LHS &lhs, const RHS &rhs) const {
      Compare cmp;
      return cmp(*lhs, *rhs);
    }
  };
}
