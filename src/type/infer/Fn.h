#pragma once

#include "../Type.h"

#include <functional>
#include <stdexcept>

namespace type::infer {
  struct Fn {
    Idx inputArity, outputArity;
    std::function<std::vector<Tp>(const std::vector<Tp>&)> invoke;
    Fn(Idx inputArity, Idx outputArity, decltype(invoke) &&invoke);
    std::vector<Tp> operator()(const std::vector<Tp> &args) {
      if (args.size() != inputArity) {
        throw std::runtime_error("ICE: Bad input arity during type inference.");
      }
      auto ret = invoke(args);
      if (args.size() != inputArity) {
        throw std::runtime_error("ICE: Bad output arity during type inference.");
      }
      return ret;
    }
  };
}
