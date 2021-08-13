#pragma once

#include "Hir.h"

#include <functional>

namespace hir::rebind {
  using Rebinder =
      std::function<std::unique_ptr<Expr>(VarExpr &old, Idx depth)>;

  class RebindVisitor : public ExprVisitor<std::monostate, Eptr*, Idx> {
  };

  std::unique_ptr<RebindVisitor> rebindVisitor(Rebinder &&rb);
}
