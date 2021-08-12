#include "Hir.h"

namespace hir {
  Expr::~Expr() = default;

  _acceptImpl(Expr, BlockExpr);
  _acceptImpl(Expr, VarExpr);
  _acceptImpl(Expr, CondExpr);
  _acceptImpl(Expr, VoidExpr);
  _acceptImpl(Expr, LiteralExpr);
  _acceptImpl(Expr, BinExpr);
  _acceptImpl(Expr, NegExpr);
  _acceptImpl(Expr, CallExpr);
  _acceptImpl(Expr, DefunExpr);
  _acceptImpl(Expr, DefvalExpr);
  _acceptImpl(Expr, DummyExpr);
}
