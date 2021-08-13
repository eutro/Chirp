#include "Hir.h"

namespace hir {
  Expr::~Expr() = default;

  _acceptImpl(Expr, BlockExpr);
  _acceptImpl(Expr, VarExpr);
  _acceptImpl(Expr, CondExpr);
  _acceptImpl(Expr, VoidExpr);
  _acceptImpl(Expr, LiteralExpr);
  _acceptImpl(Expr, BinExpr);
  _acceptImpl(Expr, CmpExpr);
  _acceptImpl(Expr, NegExpr);
  _acceptImpl(Expr, CallExpr);
  _acceptImpl(Expr, DefineExpr);
  _acceptImpl(Expr, NewExpr);
  _acceptImpl(Expr, GetExpr);
  _acceptImpl(Expr, ForeignExpr);
  _acceptImpl(Expr, DummyExpr);
}
