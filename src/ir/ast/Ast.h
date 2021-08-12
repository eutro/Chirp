#pragma once

#include "../Visitor.h"
#include "../../fsm/Lexer.h"
#include "../../common/Tokens.h"

#include <variant>
#include <memory>
#include <tuple>

namespace ast {
  using Tok = tok::Tok;
  using Token = lexer::Token<Tok>;

  class Identifier {
  public:
    Token ident;
  };

  class ErasedTypeVisitor;

  class Type {
  public:
    loc::Span span;

    virtual ~Type();

    virtual _acceptDef(Type) = 0;
  };

  class PlaceholderType : public Type {
  public:
    Token placeholder;

    _acceptDef(Type) override;
  };

  class NamedType : public Type {
  public:
    Identifier raw;

    struct TypeParameters {
      Token openToken;
      std::vector<std::unique_ptr<Type>> types;
      std::vector<Token> commas;
      Token closeToken;
    };

    std::optional<TypeParameters> parameters;

    _acceptDef(Type) override;
  };

  class TypeHint {
  public:
    Token colon;
    std::unique_ptr<Type> type;
  };

  class RawBinding {
  public:
    Identifier name;
    std::optional<TypeHint> typeHint;
  };

  class Expr;

  class Binding {
  public:
    loc::Span span;

    Identifier name;

    struct TypeArguments {
      Token openToken;
      std::vector<Identifier> idents;
      std::vector<Token> commas;
      Token closeToken;
    };

    struct Arguments {
      std::optional<TypeArguments> typeArguments;
      Token openToken;
      std::vector<RawBinding> bindings;
      std::vector<Token> commas;
      Token closeToken;
    };

    std::optional<Arguments> arguments;
    std::optional<TypeHint> typeHint;
    Token eqToken;

    std::unique_ptr<Expr> value;
    std::optional<Token> foreignToken;
  };

  class ErasedStatementVisitor;

  class Statement {
  public:
    loc::Span span;

    virtual ~Statement();

    virtual _acceptDef(Statement) = 0;
  };

  class Defn : public Statement {
  public:
    Token defnToken;
    Binding binding;

    _acceptDef(Statement) override;
  };

  class ErasedExprVisitor;

  class Expr : public Statement {
  public:
    _acceptDef(Statement) override;
    virtual _acceptDef(Expr) = 0;
  };

  class Program {
  public:
    std::vector<std::unique_ptr<Statement>> statements;
    std::vector<Token> delimiters;
  };

  class IfExpr : public Expr {
  public:
    Token ifToken;
    std::unique_ptr<Expr> predExpr;
    std::unique_ptr<Expr> thenExpr;

    struct ElseIf {
      Token elseToken, ifToken;
      std::unique_ptr<Expr> predExpr;
      std::unique_ptr<Expr> thenExpr;
    };

    struct Else {
      Token elseToken;
      std::unique_ptr<Expr> thenExpr;
    };

    std::vector<ElseIf> elseIfClauses;
    std::optional<Else> elseClause;

    _acceptDef(Expr) override;
  };

  class LetExpr : public Expr {
  public:
    Token letToken;
    std::vector<Binding> bindings;
    std::vector<Token> commas;
    Token inToken;
    std::optional<Identifier> name;
    std::unique_ptr<Expr> body;

    _acceptDef(Expr) override;
  };

  class FnExpr : public Expr {
  public:
    Token fnToken;
    std::optional<Identifier> name;
    Binding::Arguments arguments;
    std::optional<TypeHint> typeHint;
    Token eqToken;

    std::unique_ptr<Expr> body;

    _acceptDef(Expr) override;
  };

  class LambdaExpr : public Expr {
  public:
    Token lambdaToken;
    std::vector<RawBinding> arguments;
    std::vector<Token> commas;
    Token dotToken;
    std::unique_ptr<Expr> body;

    _acceptDef(Expr) override;
  };

  class BlockExpr : public Expr {
  public:
    Token openToken;

    struct Stmt {
      std::unique_ptr<Statement> statement;
      Token delimiter;
    };

    std::vector<Stmt> statements;
    std::unique_ptr<Expr> value;
    Token closeToken;

    _acceptDef(Expr) override;
  };

  class BracketExpr : public Expr {
  public:
    Token openToken;
    std::unique_ptr<Expr> value;
    Token closeToken;

    _acceptDef(Expr) override;
  };

  class ColonExpr : public Expr {
  public:
    Token colonToken;
    std::unique_ptr<Expr> value;

    _acceptDef(Expr) override;
  };

  class LiteralExpr : public Expr {
  public:
    Token value;

    _acceptDef(Expr) override;
  };

  class VarExpr : public Expr {
  public:
    Identifier name;

    _acceptDef(Expr) override;
  };

  class BinaryExpr : public Expr {
  public:
    size_t precedence;
    std::unique_ptr<Expr> lhs;

    struct Rhs {
      Token operatorToken;
      std::unique_ptr<Expr> expr;
    };

    std::vector<Rhs> terms;

    _acceptDef(Expr) override;
  };

  class PrefixExpr : public Expr {
  public:
    std::vector<Token> prefixes;
    std::unique_ptr<Expr> expr;

    _acceptDef(Expr) override;
  };

  class FunCallExpr : public Expr {
  public:
    std::unique_ptr<Expr> function;
    Token openToken;
    std::vector<std::unique_ptr<Expr>> arguments;
    std::vector<Token> commas;
    Token closeToken;

    _acceptDef(Expr) override;
  };

  class HintedExpr : public Expr {
  public:
    std::unique_ptr<Expr> expr;
    TypeHint hint;

    _acceptDef(Expr) override;
  };

  class ErasedTypeVisitor {
  public:
    _EvisitVirtual(Type) _EvisitVirtual(NamedType) _EvisitVirtual(PlaceholderType)
  };

  template <typename Ret=std::monostate, typename ...Arg>
  class TypeVisitor : public ErasedTypeVisitor {
  public:
    _EvisitImpl(Type) _EvisitImpl(NamedType) _EvisitImpl(PlaceholderType)

    virtual _typedRoot(Type);
    virtual _typedVisit(NamedType)  = 0;
    virtual _typedVisit(PlaceholderType) = 0;
  };

  class ErasedExprVisitor {
  public:
    _EvisitVirtual(Expr) _EvisitVirtual(IfExpr) _EvisitVirtual(LetExpr)
    _EvisitVirtual(FnExpr) _EvisitVirtual(LambdaExpr) _EvisitVirtual(BlockExpr)
    _EvisitVirtual(BracketExpr) _EvisitVirtual(ColonExpr) _EvisitVirtual(LiteralExpr)
    _EvisitVirtual(VarExpr) _EvisitVirtual(BinaryExpr) _EvisitVirtual(PrefixExpr)
    _EvisitVirtual(FunCallExpr) _EvisitVirtual(HintedExpr)
  };

  template <typename Ret=std::monostate, typename ...Arg>
  class ExprVisitor : public ErasedExprVisitor {
  public:
    _EvisitImpl(Expr) _EvisitImpl(IfExpr) _EvisitImpl(LetExpr) _EvisitImpl(FnExpr)
    _EvisitImpl(LambdaExpr) _EvisitImpl(BlockExpr) _EvisitImpl(BracketExpr)
    _EvisitImpl(ColonExpr) _EvisitImpl(LiteralExpr) _EvisitImpl(VarExpr)
    _EvisitImpl(BinaryExpr) _EvisitImpl(PrefixExpr) _EvisitImpl(FunCallExpr)
    _EvisitImpl(HintedExpr)

    virtual ~ExprVisitor() = default;
    virtual _typedRoot(Expr);
    virtual _typedVisit(IfExpr) = 0;
    virtual _typedVisit(LetExpr) = 0;
    virtual _typedVisit(FnExpr) = 0;
    virtual _typedVisit(LambdaExpr) = 0;
    virtual _typedVisit(BlockExpr) = 0;
    virtual _typedVisit(BracketExpr) = 0;
    virtual _typedVisit(ColonExpr) = 0;
    virtual _typedVisit(LiteralExpr) = 0;
    virtual _typedVisit(VarExpr) = 0;
    virtual _typedVisit(BinaryExpr) = 0;
    virtual _typedVisit(PrefixExpr) = 0;
    virtual _typedVisit(FunCallExpr) = 0;
    virtual _typedVisit(HintedExpr) = 0;
  };

  class ErasedStatementVisitor {
  public:
    _EvisitVirtual(Statement) _EvisitVirtual(Expr) _EvisitVirtual(Defn)
  };

  template <typename Ret=std::monostate, typename ...Arg>
  class StatementVisitor : public ErasedStatementVisitor {
  public:
    _EvisitImpl(Statement) _EvisitImpl(Expr) _EvisitImpl(Defn)

    virtual ~StatementVisitor() = default;
    virtual _typedRoot(Statement);
    virtual _typedVisit(Expr) = 0;
    virtual _typedVisit(Defn) = 0;
  };

  template <typename Ret=std::monostate, typename ...Arg>
  class ProgramVisitor {
  public:
    virtual ~ProgramVisitor() = default;
    virtual Ret visitProgram(Program &it, Arg...) = 0;
  };
}
