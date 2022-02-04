#pragma once

#include "Type.h"

#include <ostream>

std::ostream &operator<<(std::ostream &u, type::Ty *t);

namespace type::print {
  void printTy(type::Ty *t);
}
