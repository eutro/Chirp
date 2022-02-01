#pragma once

#include "Util.h"

namespace lir::codegen {
  void addIntrinsics(CC &cc, type::infer::Inst::Set &sys);
}
