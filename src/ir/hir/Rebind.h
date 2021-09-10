#pragma once

#include "Hir.h"

#include <functional>

namespace hir::rebind {
  using Rebinder = std::function<std::unique_ptr<Expr>(VarExpr &old)>;

  class RebindVisitor : public ExprVisitor<std::monostate, Eptr*> {
  };

  std::unique_ptr<RebindVisitor> rebindVisitor(Rebinder &&rb);
}
