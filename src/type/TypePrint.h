#pragma once

#include "Type.h"

#include <ostream>

std::ostream &operator<<(std::ostream &os, type::Ty *t);
std::ostream &operator<<(std::ostream &os, type::TraitBound *tb);

namespace type::print {
  void printTy(type::Ty *t);
};
