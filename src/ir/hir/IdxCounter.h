#pragma once

#include "Hir.h"
#include "RecurVisitor.h"

namespace hir {

  class TyCounter : public RecurVisitor<std::monostate, std::map<Expr*, Idx>, Idx> {
    std::monostate visitExpr(Expr &it, std::map<Expr*, Idx> &exprTys, Idx &idx) override;
  };

  class DefCounter : public RecurVisitor<std::monostate, std::map<Idx, Idx>, Idx> {
  public:
    void visitBlock(Block &it, std::map<Idx, Idx> &defTys, Idx &idx) override;
  };

}
