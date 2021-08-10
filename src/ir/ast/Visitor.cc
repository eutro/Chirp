#include "Ast.h"

namespace ast {
  void Visitor::visitProgram(Program &it) {
    for (auto &stmt : it.statements) {
      visitStatement(*stmt);
    }
  }

  void Visitor::visitType(Type &it) { it.acceptType(*this); }
  void Visitor::visitPlaceholderType(PlaceholderType &it) {}
  void Visitor::visitNamedType(NamedType &it) {}

  void Visitor::visitBinding(Binding &it) {
    visitExpr(*it.value, it.arguments ? Position::Tail : Position::Expr);
  }

  void Visitor::visitStatement(Statement &it) {
    it.acceptStatement(*this);
  }
  void Visitor::visitDefn(Defn &it) {
    visitBinding(it.binding);
  }

  void Visitor::visitExpr(Expr &it, Position pos) {
    it.acceptExpr(*this, pos);
  }
  void Visitor::visitIfExpr(IfExpr &it, Position pos) {
    Position branchPos = it.elseClause ? pos : Position::Statement;
    visitExpr(*it.predExpr, Position::Expr);
    visitExpr(*it.thenExpr, branchPos);
    for (auto &clause : it.elseIfClauses) {
      visitExpr(*clause.predExpr, Position::Expr);
      visitExpr(*clause.thenExpr, branchPos);
    }
    if (it.elseClause) {
      visitExpr(*it.elseClause->thenExpr, branchPos);
    }
  }
  void Visitor::visitLetExpr(LetExpr &it, Position pos) {
    for (auto &b : it.bindings) {
      visitBinding(b);
    }
    visitExpr(*it.body, pos);
  }
  void Visitor::visitFnExpr(FnExpr &it, Position pos) {
    visitExpr(*it.body, Position::Tail);
  }
  void Visitor::visitLambdaExpr(LambdaExpr &it, Position pos) {
    visitExpr(*it.body, Position::Tail);
  }
  void Visitor::visitBlockExpr(BlockExpr &it, Position pos) {
    for (auto &stmt : it.statements) {
      visitStatement(*stmt.statement);
    }
    if (it.value) visitExpr(*it.value, pos);
  }
  void Visitor::visitBracketExpr(BracketExpr &it, Position pos) {
    visitExpr(*it.value, Position::Expr);
  }
  void Visitor::visitColonExpr(ColonExpr &it, Position pos) {
    visitExpr(*it.value, pos);
  }
  void Visitor::visitLiteralExpr(LiteralExpr &it, Position pos) {}
  void Visitor::visitVarExpr(VarExpr &it, Position pos) {}
  void Visitor::visitBinaryExpr(BinaryExpr &it, Position pos) {
    visitExpr(*it.lhs, Position::Expr);
    for (auto &term : it.terms) {
      visitExpr(*term.expr, Position::Expr);
    }
  }
  void Visitor::visitPrefixExpr(PrefixExpr &it, Position pos) {
    visitExpr(*it.expr, Position::Expr);
  }
  void Visitor::visitFunCallExpr(FunCallExpr &it, Position pos) {
    visitExpr(*it.function, Position::Expr);
    for (auto &arg : it.arguments) {
      visitExpr(*arg, Position::Expr);
    }
  }
  void Visitor::visitHintedExpr(HintedExpr &it, Position pos) {
    visitExpr(*it.expr, pos);
  }

  void NamedType::acceptType(Visitor &v) { v.visitNamedType(*this); }
  void PlaceholderType::acceptType(Visitor &v) { v.visitPlaceholderType(*this); }

  void Defn::acceptStatement(Visitor &v) { v.visitDefn(*this); }
  void Expr::acceptStatement(Visitor &v) { v.visitExpr(*this, Position::Statement); }

  void HintedExpr::acceptExpr(Visitor &v, Position pos) { v.visitHintedExpr(*this, pos); }
  void FunCallExpr::acceptExpr(Visitor &v, Position pos) { v.visitFunCallExpr(*this, pos); }
  void PrefixExpr::acceptExpr(Visitor &v, Position pos) { v.visitPrefixExpr(*this, pos); }
  void BinaryExpr::acceptExpr(Visitor &v, Position pos) { v.visitBinaryExpr(*this, pos); }
  void VarExpr::acceptExpr(Visitor &v, Position pos) { v.visitVarExpr(*this, pos); }
  void LiteralExpr::acceptExpr(Visitor &v, Position pos) { v.visitLiteralExpr(*this, pos); }
  void ColonExpr::acceptExpr(Visitor &v, Position pos) { v.visitColonExpr(*this, pos); }
  void BracketExpr::acceptExpr(Visitor &v, Position pos) { v.visitBracketExpr(*this, pos); }
  void BlockExpr::acceptExpr(Visitor &v, Position pos) { v.visitBlockExpr(*this, pos); }
  void LambdaExpr::acceptExpr(Visitor &v, Position pos) { v.visitLambdaExpr(*this, pos); }
  void FnExpr::acceptExpr(Visitor &v, Position pos) { v.visitFnExpr(*this, pos); }
  void LetExpr::acceptExpr(Visitor &v, Position pos) { v.visitLetExpr(*this, pos); }
  void IfExpr::acceptExpr(Visitor &v, Position pos) { v.visitIfExpr(*this, pos); }
}
