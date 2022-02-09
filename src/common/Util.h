#pragma once

#include <algorithm>
#include <optional>
#include <set>
#include <tuple>
#include <variant>
#include <vector>
#include <sstream>

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

  struct left_shift {
    template<class L, class R>
    constexpr auto operator()(L &&l, R &&r) const
    noexcept(noexcept(std::forward<L>(l) << std::forward<R>(r)))
    -> decltype(std::forward<L>(l) << std::forward<R>(r)) {
      return std::forward<L>(l) << std::forward<R>(r);
    }
  };

  struct right_shift {
    template<class L, class R>
    constexpr auto operator()(L &&l, R &&r) const
    noexcept(noexcept(std::forward<L>(l) >> std::forward<R>(r)))
    -> decltype(std::forward<L>(l) >> std::forward<R>(r)) {
      return std::forward<L>(l) >> std::forward<R>(r);
    }
  };

  template<class X, class Y, class Op>
  struct op_valid {
    template<class U, class L, class R>
    static auto test(int) -> decltype(std::declval<U>()(std::declval<L>(), std::declval<R>()),void(), std::true_type()) {return std::true_type{};}
    template<class U, class L, class R>
    static auto test(...) -> std::false_type {return std::false_type{};}
    using type = decltype(test<Op, X, Y>(0));
  };

  template<class X, class Y, class Op> using op_valid_t = typename op_valid<X, Y, Op>::type;

  template <typename... T>
  std::string toStr(const T &...x) {
    std::stringstream ss;
    ((ss << x), ...);
    return ss.str();
  }

  class ICE : public std::runtime_error {
  public:
    ICE(const std::string &msg): std::runtime_error("ICE: " + msg) {}
  };

  class Unreachable : public ICE {
  public:
    Unreachable(): ICE("Unreachable reached") {}
  };
}

template <typename T>
std::ostream &operator<<(std::ostream &os, const std::vector<T> &v) {
  os << "[";
  for (auto it = v.begin(); it != v.end();) {
    os << *it;
    if (++it != v.end()) os << ", ";
  }
  os << "]";
  return os;
}
