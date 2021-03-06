#pragma once

#include "Hir.h"
#include "../lir/Lir.h"

namespace hir::lower {
  struct LowerResult {
    lir::Module module;
  };

  class AbstractLoweringVisitor : public ProgramVisitor<LowerResult> {
  };

  std::unique_ptr<AbstractLoweringVisitor> loweringVisitor();
}
