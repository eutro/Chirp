#pragma once

#include "Lir.h"

namespace lir::disas {
  void disassemble(const Module &mod, std::ostream &os);
}
