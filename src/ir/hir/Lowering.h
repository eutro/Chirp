#pragma once

#include "Infer.h"
#include "../lir/Lir.h"

namespace hir::lower {
  class AbstractLoweringVisitor {
  public:
    virtual lir::BlockList visitRootBlock(Block &block) = 0;
    virtual ~AbstractLoweringVisitor() = default;
  };

  std::unique_ptr<AbstractLoweringVisitor> loweringVisitor(infer::InferResult &inferResult);
}
