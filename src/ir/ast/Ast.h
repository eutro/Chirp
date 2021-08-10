#pragma once

#include "../../fsm/Lexer.h"
#include "../../common/Tokens.h"

#include <memory>

namespace ast {
  using Token = lexer::Token<Tok>;

  class Visitor;

  class Statement {
  public:
    loc::Span span;

    virtual ~Statement() = default;
    virtual void acceptStatement(Visitor &v) = 0;
  };

  class Identifier {
  public:
    Token ident;
  };

  class Type {
  public:
    loc::Span span;

    virtual ~Type() = default;
    virtual void acceptType(Visitor &v) = 0;
  };

  class PlaceholderType : public Type {
  public:
    Token placeholder;

    void acceptType(Visitor &v) override;
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

    void acceptType(Visitor &v) override;
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

  enum class Position {
    Statement,
    Expr,
    Tail,
  };

  class Expr : public Statement {
  public:
    void acceptStatement(Visitor &v) override;
    virtual void acceptExpr(Visitor &v, Position pos) = 0;
  };

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

  class Defn : public Statement {
  public:
    Token defnToken;
    Binding binding;

    void acceptStatement(Visitor &v) override;
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

    void acceptExpr(Visitor &v, Position pos) override;
  };

  class LetExpr : public Expr {
  public:
    Token letToken;
    std::vector<Binding> bindings;
    std::vector<Token> commas;
    Token inToken;
    std::optional<Identifier> name;
    std::unique_ptr<Expr> body;

    void acceptExpr(Visitor &v, Position pos) override;
  };

  class FnExpr : public Expr {
  public:
    Token fnToken;
    std::optional<Identifier> name;
    Binding::Arguments arguments;
    std::optional<TypeHint> typeHint;
    Token eqToken;

    std::unique_ptr<Expr> body;

    void acceptExpr(Visitor &v, Position pos) override;
  };

  class LambdaExpr : public Expr {
  public:
    Token lambdaToken;
    std::vector<RawBinding> arguments;
    std::vector<Token> commas;
    Token dotToken;
    std::unique_ptr<Expr> body;

    void acceptExpr(Visitor &v, Position pos) override;
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

    void acceptExpr(Visitor &v, Position pos) override;
  };

  class BracketExpr : public Expr {
  public:
    Token openToken;
    std::unique_ptr<Expr> value;
    Token closeToken;

    void acceptExpr(Visitor &v, Position pos) override;
  };

  class ColonExpr : public Expr {
  public:
    Token colonToken;
    std::unique_ptr<Expr> value;

    void acceptExpr(Visitor &v, Position pos) override;
  };

  class LiteralExpr : public Expr {
  public:
    Token value;

    void acceptExpr(Visitor &v, Position pos) override;
  };

  class VarExpr : public Expr {
  public:
    Identifier name;

    void acceptExpr(Visitor &v, Position pos) override;
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

    void acceptExpr(Visitor &v, Position pos) override;
  };

  class PrefixExpr : public Expr {
  public:
    std::vector<Token> prefixes;
    std::unique_ptr<Expr> expr;

    void acceptExpr(Visitor &v, Position pos) override;
  };

  class FunCallExpr : public Expr {
  public:
    std::unique_ptr<Expr> function;
    Token openToken;
    std::vector<std::unique_ptr<Expr>> arguments;
    std::vector<Token> commas;
    Token closeToken;

    void acceptExpr(Visitor &v, Position pos) override;
  };

  class HintedExpr : public Expr {
  public:
    std::unique_ptr<Expr> expr;
    TypeHint hint;

    void acceptExpr(Visitor &v, Position pos) override;
  };

  class Visitor {
  public:
    virtual void visitProgram(Program &it);

    virtual void visitType(Type &it);
    virtual void visitPlaceholderType(PlaceholderType &it);
    virtual void visitNamedType(NamedType &it);

    virtual void visitBinding(Binding &it);

    virtual void visitStatement(Statement &it);
    virtual void visitDefn(Defn &it);

    virtual void visitExpr(Expr &it, Position pos);
    virtual void visitIfExpr(IfExpr &it, Position pos);
    virtual void visitLetExpr(LetExpr &it, Position pos);
    virtual void visitFnExpr(FnExpr &it, Position pos);
    virtual void visitLambdaExpr(LambdaExpr &it, Position pos);
    virtual void visitBlockExpr(BlockExpr &it, Position pos);
    virtual void visitBracketExpr(BracketExpr &it, Position pos);
    virtual void visitColonExpr(ColonExpr &it, Position pos);
    virtual void visitLiteralExpr(LiteralExpr &it, Position pos);
    virtual void visitVarExpr(VarExpr &it, Position pos);
    virtual void visitBinaryExpr(BinaryExpr &it, Position pos);
    virtual void visitPrefixExpr(PrefixExpr &it, Position pos);
    virtual void visitFunCallExpr(FunCallExpr &it, Position pos);
    virtual void visitHintedExpr(HintedExpr &it, Position pos);
  };
}
