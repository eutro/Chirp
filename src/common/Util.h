#pragma once

#include <algorithm>
#include <optional>
#include <set>
#include <tuple>
#include <variant>

namespace util {
  template<typename Data = std::monostate>
  struct DSU {
    DSU *parent = this;
    size_t size = 0;
    Data data;

    template<typename... Arg>
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
      std::tie(minRoot, maxRoot) = std::minmax(tRoot, oRoot, [](DSU *a, DSU *b) {
        return a->size < b->size;
      });
      minRoot->parent = maxRoot;
      maxRoot->size += minRoot->size;
    }
  };

  template<typename Compare = std::less<>>
  struct DerefCmp {
    template<typename LHS, typename RHS>
    bool operator()(const LHS &lhs, const RHS &rhs) const {
      Compare cmp;
      return cmp(*lhs, *rhs);
    }
  };

  template<typename T, typename Hash = std::hash<T>>
  struct DerefHash {
    std::size_t operator()(const T &v) const {
      Hash hash;
      return hash(v);
    }
  };

  template<typename Iterable>
  std::size_t hashIterable(const Iterable &iterable) {
    std::size_t result = 1;
    for (auto x : iterable) {
      result = 32 * result + std::hash<decltype(x)>()(x);
    }
    return result;
  }

  template<typename... Arg>
  std::size_t hashMulti(const Arg &...t) {
    std::size_t result = 1;
    ((result = 32 * result + std::hash<Arg>()(t)), ...);
    return result;
  }

  template<typename T, typename X>
  struct index_of_type {};

  template<typename T, typename... V>
  struct index_of_type<T, std::variant<V...>> : index_of_type<T, std::tuple<V...>> {};

  template<typename T, typename... V>
  struct index_of_type<T, std::tuple<T, V...>> {
    static const size_t value = 0;
  };

  template<typename T, typename K, typename... V>
  struct index_of_type<T, std::tuple<K, V...>> {
    static const size_t value = 1 + index_of_type<T, std::tuple<V...>>::value;
  };

  template<typename T, typename X>
  inline constexpr size_t index_of_type_v = index_of_type<T, X>::value;

  template<typename... Alts, typename... Ts>
  constexpr bool holds_any_of(std::variant<Ts...> const& v) noexcept {
    return (std::holds_alternative<Alts>(v) || ...);
  }
}
