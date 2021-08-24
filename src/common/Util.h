#pragma once

#include <optional>

namespace util {
  template <typename T>
  class Range {
  public:
    std::optional<T> min /*inclusive*/, max /*exclusive*/;
    bool includes(const T &x) {
      if (min && x < min) return false;
      if (max && x >= max) return false;
      return true;
    }
  };

  template <typename T>
  class LinearFunc {
  public:
    T gradient, intercept;
    LinearFunc(T &&gradient, T &&intercept):
      gradient(gradient),
      intercept(intercept) {}
    T operator()(const T &x) { return gradient * x + intercept; }
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
