#pragma once

#include "Lexer.h"
#include "Type.h"
#include "Tokens.h"

#include <deque>
#include <vector>
#include <optional>
#include <memory>
#include <variant>
#include <ostream>

namespace ast {
  using Token = lexer::Token<Tok>;
  using CType = type::Type;
  using PType = type::PolyType;

  class Var {
  public:
    std::shared_ptr<PType> type;

    Var(PType &&type);
  };

  class Scope {
  public:
    std::map<std::string, std::shared_ptr<Var>> bindings;
  };

  class ParseContext {
  public:
    type::TypeContext &tc;
    std::shared_ptr<type::BaseType> funcType;
    std::shared_ptr<type::BaseType> unitType;
    std::shared_ptr<type::BaseType> intType;
    std::shared_ptr<type::BaseType> floatType;
    std::shared_ptr<type::BaseType> boolType;
    std::shared_ptr<type::BaseType> stringType;
    std::deque<Scope> scopes;

    std::shared_ptr<Var> &introduce(const std::string &name, PType &&type);
    std::shared_ptr<Var> &lookup(const std::string &name);

    ParseContext(type::TypeContext &tc);
  };

  std::ostream &operator<<(std::ostream &os, const Token &token);

  class Statement {
  public:
    virtual ~Statement() = default;

    virtual void inferTypes(ParseContext &ctx) = 0;
    virtual void print(std::ostream &os) const = 0;
    friend std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Statement> &statement);
  };

  class Identifier {
  public:
    Token ident;

    friend std::ostream &operator<<(std::ostream &os, const Identifier &identifier);
  };

  class Type {
  public:
    virtual ~Type() = default;

    virtual void print(std::ostream &os) const = 0;
    friend std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Type> &statement);
  };

  class PlaceholderType : public Type {
  public:
    Token placeholder;

    void print(std::ostream &os) const override;
  };

  class NamedType : public Type {
  public:
    Identifier raw;

    struct TypeParameters {
      Token openToken;
      std::vector<std::unique_ptr<Type>> types;
      std::vector<Token> commas;
      Token closeToken;

      friend std::ostream &operator<<(std::ostream &os, const TypeParameters &parameters);
    };

    std::optional<TypeParameters> parameters;

    void print(std::ostream &os) const override;
  };

  class TypeHint {
  public:
    Token colon;
    std::unique_ptr<Type> type;

    friend std::ostream &operator<<(std::ostream &os, const TypeHint &hint);
  };

  class RawBinding {
  public:
    Identifier name;
    std::optional<TypeHint> typeHint;

    friend std::ostream &operator<<(std::ostream &os, const RawBinding &binding);
  };

  class Expr : public Statement {
  protected:
    CType *type = nullptr;
    virtual CType *inferType(ParseContext &ctx) = 0;
  public:
    void inferTypes(ParseContext &ctx) override;
    CType *infer(ParseContext &ctx);
    friend std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Expr> &statement);
  };

  class Binding {
  public:
    std::shared_ptr<PType> type = nullptr;
    Identifier name;

    struct Arguments {
      Token openToken;
      std::vector<RawBinding> bindings;
      std::vector<Token> commas;
      Token closeToken;

      friend std::ostream &operator<<(std::ostream &os, const Arguments &arguments);
    };

    std::optional<Arguments> arguments;
    std::optional<TypeHint> typeHint;
    Token eqToken;
    std::unique_ptr<Expr> value;

    friend std::ostream &operator<<(std::ostream &os, const Binding &binding);

    std::shared_ptr<PType> &inferType(ParseContext &ctx);
  };

  class Defn : public Statement {
  public:
    Token defnToken;
    Binding binding;

    void print(std::ostream &os) const override;
    void inferTypes(ParseContext &ctx) override;
  };

  class Program {
  public:
    std::vector<std::unique_ptr<Statement>> statements;

    friend std::ostream &operator<<(std::ostream &os, const Program &program);
    void inferTypes(ParseContext &ctx);
  };

  class PrimaryExpr : public Expr {
  };

  class DelimitedExpr : public PrimaryExpr {
  public:
    friend std::ostream &operator<<(std::ostream &os, const std::unique_ptr<DelimitedExpr> &statement);
  };

  class IfExpr : public Expr {
  public:
    Token ifToken;
    std::unique_ptr<Expr> predExpr;
    std::unique_ptr<DelimitedExpr> thenExpr;

    struct ElseIf {
      Token elseToken, ifToken;
      std::unique_ptr<Expr> predExpr;
      std::unique_ptr<DelimitedExpr> thenExpr;

      friend std::ostream &operator<<(std::ostream &os, const ElseIf &anIf);
    };

    struct Else {
      Token elseToken;
      std::unique_ptr<DelimitedExpr> thenExpr;

      friend std::ostream &operator<<(std::ostream &os, const Else &anElse);
    };

    std::vector<ElseIf> elseIfClauses;
    std::optional<Else> elseClause;

    void print(std::ostream &os) const override;
    void inferTypes(ParseContext &ctx) override;
    CType *inferType(ParseContext &ctx) override;
  };

  class LetExpr : public Expr {
  public:
    Token letToken;
    std::vector<Binding> bindings;
    std::vector<Token> commas;
    Token inToken;
    std::optional<Identifier> name;
    std::unique_ptr<DelimitedExpr> body;

    void print(std::ostream &os) const override;
    CType *inferType(ParseContext &ctx) override;
  };

  class FnExpr : public Expr {
  public:
    Token fnToken;
    std::optional<Identifier> name;
    Binding::Arguments arguments;
    Token eqToken;
    std::unique_ptr<Expr> body;

    void print(std::ostream &os) const override;
    CType *inferType(ParseContext &ctx) override;
  };
  class LambdaExpr : public Expr {
  public:
    Token lambdaToken;
    std::vector<RawBinding> arguments;
    std::vector<Token> commas;
    Token dotToken;
    std::unique_ptr<Expr> body;

    void print(std::ostream &os) const override;
    CType *inferType(ParseContext &ctx) override;
  };

  class BlockExpr : public DelimitedExpr {
  public:
    Token openToken;

    struct Stmt {
      std::unique_ptr<Statement> statement;
      Token delimiter;

      friend std::ostream &operator<<(std::ostream &os, const Stmt &stmt);
    };

    std::vector<Stmt> statements;
    std::unique_ptr<Expr> value;
    Token closeToken;

    void print(std::ostream &os) const override;
    CType *inferType(ParseContext &ctx) override;
  };

  class BracketExpr : public DelimitedExpr {
  public:
    Token openToken;
    std::unique_ptr<Expr> value;
    Token closeToken;

    void print(std::ostream &os) const override;
    CType *inferType(ParseContext &ctx) override;
  };

  class LiteralExpr : public PrimaryExpr {
  public:
    Token value;

    void print(std::ostream &os) const override;
    CType *inferType(ParseContext &ctx) override;
  };

  class VarExpr : public PrimaryExpr {
  public:
    Identifier name;

    void print(std::ostream &os) const override;
    CType *inferType(ParseContext &ctx) override;
  };

  class BinaryExpr : public Expr {
  public:
    std::unique_ptr<Expr> lhs;

    struct Rhs {
      Token operatorToken;
      std::unique_ptr<Expr> expr;

      friend std::ostream &operator<<(std::ostream &os, const Rhs &rhs);
    };

    std::vector<Rhs> terms;

    void print(std::ostream &os) const override;
    CType *inferType(ParseContext &ctx) override;
  };

  class PrefixExpr : public Expr {
  public:
    std::vector<Token> prefixes;
    std::unique_ptr<Expr> expr;

    void print(std::ostream &os) const override;
    CType *inferType(ParseContext &ctx) override;
  };

  class FunCallExpr : public Expr {
  public:
    std::unique_ptr<Expr> function;
    Token openToken;
    std::vector<std::unique_ptr<Expr>> arguments;
    std::vector<Token> commas;
    Token closeToken;

    void print(std::ostream &os) const override;
    CType *inferType(ParseContext &ctx) override;
  };

  class HintedExpr : public Expr {
  public:
    std::unique_ptr<Expr> expr;
    TypeHint hint;

    void print(std::ostream &os) const override;
    CType *inferType(ParseContext &ctx) override;
  };
}
