#pragma once

#include "Util.h"

namespace lir::codegen {
  Idx addIntrinsics(CC &cc, type::infer::Inst::Set &sys);
}
