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
}
