#pragma once

#include "Infer.h"
#include "../lir/Lir.h"

namespace hir::lower {
  struct LowerResult {
    lir::Module module;
  };

  class AbstractLoweringVisitor : public ProgramVisitor<LowerResult> {
  };

  std::unique_ptr<AbstractLoweringVisitor> loweringVisitor(infer::InferResult &inferResult);
}
