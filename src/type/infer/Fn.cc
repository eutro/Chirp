#include "Fn.h"

namespace type::infer {
  Constant::Constant(const Constant &o):
    size(o.size),
    raw(new char[size])
  {
    std::memcpy(raw, o.raw, size);
  }
}
