#include "Fn.h"

namespace type::infer {
  Constant::Constant(const Constant &o):
    ty(o.ty),
    size(o.size),
    raw(new char[size])
  {
    std::memcpy(raw, o.raw, size);
  }

  Constant &Constant::operator=(const Constant &o) {
    ty = o.ty;
    size = o.size;
    delete[] raw;
    raw = new char[size];
    std::memcpy(raw, o.raw, size);
    return *this;
  }
}
