#include "Fn.h"

namespace type::infer {
  Fn::Fn(Idx inputArity, Idx outputArity, decltype(invoke) &&invoke):
    inputArity(inputArity),
    outputArity(outputArity),
    invoke(std::forward<decltype(invoke)>(invoke))
  {}
}
