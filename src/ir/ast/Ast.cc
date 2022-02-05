#include "Ast.h"

namespace ast {
  // the non-inline definitions here ensure that the compiler
  // appropriately puts the vtable in this compulation unit
  Type::~Type() = default;
  Statement::~Statement() = default;

  _acceptImpl(Type, NamedType)
  _acceptImpl(Type, PlaceholderType)
  _acceptImpl(Type, TupleType)
  _acceptImpl(Type, UnionType)
  _acceptImpl(Type, TypeDefn)

  _acceptImpl(Statement, Defn)
  _acceptImpl(Statement, Expr)
  _acceptImpl(Statement, TypeDefn)
  _acceptImpl(Statement, TraitImpl)

  _acceptImpl(Expr, HintedExpr)
  _acceptImpl(Expr, FunCallExpr)
  _acceptImpl(Expr, PrefixExpr)
  _acceptImpl(Expr, BinaryExpr)
  _acceptImpl(Expr, VarExpr)
  _acceptImpl(Expr, LiteralExpr)
  _acceptImpl(Expr, ColonExpr)
  _acceptImpl(Expr, BracketExpr)
  _acceptImpl(Expr, BlockExpr)
  _acceptImpl(Expr, LambdaExpr)
  _acceptImpl(Expr, FnExpr)
  _acceptImpl(Expr, LetExpr)
  _acceptImpl(Expr, IfExpr)
}
