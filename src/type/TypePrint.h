#pragma once

#include "Type.h"

#include <ostream>

namespace {};

std::ostream &operator<<(std::ostream &os, type::Ty *t);
std::ostream &operator<<(std::ostream &os, type::TraitBound *tb);
