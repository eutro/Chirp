#include "IdxCounter.h"

namespace hir {
  std::monostate TyCounter::visitExpr(Expr &it, std::map<Expr*, Idx> &exprTys, Idx &idx) {
    exprTys[&it] = idx++;
    return RecurVisitor::visitExpr(it, exprTys, idx);
  }

  void DefCounter::visitBlock(Block &it, std::map<Idx, Idx> &defTys, Idx &idx) {
    for (auto &b : it.bindings) {
      defTys[b] = idx++;
    }
    RecurVisitor::visitBlock(it, defTys, idx);
  }
}
