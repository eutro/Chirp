#include "Fn.h"

namespace type::infer {
  Constant::Constant(const Constant &o):
    ty(o.ty),
    size(o.size),
    raw(new char[size])
  {
    std::memcpy(raw, o.raw, size);
  }
}
