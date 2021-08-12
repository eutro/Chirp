#pragma once

#include "Ast.h"
#include "../hir/Hir.h"
#include "../../common/Err.h"

namespace ast::lower {
  class LowerResult {
  public:
    err::ErrorContext errors;
    hir::Program program;
  };

  std::unique_ptr<ProgramVisitor<LowerResult>> lowerVisitor();
}
