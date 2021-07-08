#pragma once

#include "Lexer.h"
#include "Type.h"
#include "Tokens.h"

#include <functional>
#include <llvm/IR/IRBuilder.h>

#include <deque>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <vector>
#include <optional>
#include <memory>
#include <variant>
#include <ostream>

namespace ast {
  using Token = lexer::Token<Tok>;
  using CType = type::Type;
  using TPtr = type::TPtr;
  using PType = type::PolyType;

  class Var;

  class Scope {
  public:
    std::map<std::string, std::shared_ptr<Var>> bindings;
    /**
     * Variables lower in the scope stack that have been referenced.
     */
    std::set<std::shared_ptr<Var>> referenced;
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

  class CompileContext {
  public:
    ParseContext &pc;

    llvm::LLVMContext &ctx;
    llvm::IRBuilder<> &builder;
    llvm::Module &module;

    llvm::StructType *unitType;
    llvm::Constant *unitValue;

    std::map<TPtr, llvm::PointerType *, type::CompareType> fTypeCache;

    std::map<std::shared_ptr<type::BaseType>,
        std::function<llvm::Type *(CompileContext &, TPtr)>> transformers;

    CompileContext(llvm::LLVMContext &ctx,
                   llvm::IRBuilder<> &builder,
                   llvm::Module &module,
                   ParseContext &pc);
  };

  class Var {
  public:
    std::shared_ptr<PType> type;
    std::function<llvm::Value *(CompileContext &, TPtr)> emit;

    Var(PType &&type);
  };

  std::ostream &operator<<(std::ostream &os, const Token &token);

  class Statement {
  public:
    virtual ~Statement() = default;

    virtual void inferStatement(ParseContext &ctx) = 0;
    virtual void compileStatement(CompileContext &ctx) = 0;
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
    std::shared_ptr<Var> var;

    Identifier name;
    std::optional<TypeHint> typeHint;

    friend std::ostream &operator<<(std::ostream &os, const RawBinding &binding);
  };

  enum class Position {
    Statement,
    Expr,
    Tail,
  };

  class Expr : public Statement {
  protected:
    virtual TPtr inferType(ParseContext &ctx, Position pos) = 0;
  public:
    TPtr type = nullptr;

    void inferStatement(ParseContext &ctx) override;
    TPtr inferExpr(ParseContext &ctx, Position pos);

    void compileStatement(CompileContext &ctx) override;
    virtual llvm::Value *compileExpr(CompileContext &ctx, Position pos) = 0;
    friend std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Expr> &statement);
  };

  class Binding {
  public:
    std::shared_ptr<Var> var;
    std::map<TPtr, llvm::Value *, type::CompareType> insts;
    Identifier name;

    struct Arguments {
      std::shared_ptr<Var> recurVar;
      std::set<std::shared_ptr<Var>> closed;

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
    std::optional<Token> foreignToken;

    friend std::ostream &operator<<(std::ostream &os, const Binding &binding);

    std::shared_ptr<PType> &inferType(ParseContext &ctx);
    void compile(CompileContext &ctx);
    llvm::Value *compileExpr(CompileContext &ctx, TPtr instType);
  };

  class Defn : public Statement {
  public:
    Token defnToken;
    Binding binding;

    void print(std::ostream &os) const override;
    void inferStatement(ParseContext &ctx) override;
    void compileStatement(CompileContext &ctx) override;
  };

  class Program {
  public:
    std::vector<std::unique_ptr<Statement>> statements;

    friend std::ostream &operator<<(std::ostream &os, const Program &program);
    void inferTypes(ParseContext &ctx);
    void compile(CompileContext &ctx);
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
    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class LetExpr : public Expr {
  public:
    std::shared_ptr<Var> nameVar;
    std::set<std::shared_ptr<Var>> closed;

    Token letToken;
    std::vector<Binding> bindings;
    std::vector<Token> commas;
    Token inToken;
    std::optional<Identifier> name;
    std::unique_ptr<DelimitedExpr> body;

    void print(std::ostream &os) const override;
    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class FnExpr : public Expr {
  public:
    std::shared_ptr<Var> recurVar;
    std::set<std::shared_ptr<Var>> closed;

    Token fnToken;
    std::optional<Identifier> name;
    Binding::Arguments arguments;
    Token eqToken;

    std::unique_ptr<Expr> body;
    void print(std::ostream &os) const override;
    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class LambdaExpr : public Expr {
  public:
    std::set<std::shared_ptr<Var>> closed;

    Token lambdaToken;
    std::vector<RawBinding> arguments;
    std::vector<Token> commas;
    Token dotToken;
    std::unique_ptr<Expr> body;

    void print(std::ostream &os) const override;
    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
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
    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class BracketExpr : public DelimitedExpr {
  public:
    Token openToken;
    std::unique_ptr<Expr> value;
    Token closeToken;

    void print(std::ostream &os) const override;
    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class LiteralExpr : public PrimaryExpr {
  public:
    Token value;

    void print(std::ostream &os) const override;
    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class VarExpr : public PrimaryExpr {
  public:
    Identifier name;
    std::shared_ptr<Var> var;

    void print(std::ostream &os) const override;
    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class BinaryExpr : public Expr {
  public:
    size_t precedence;
    std::unique_ptr<Expr> lhs;

    struct Rhs {
      Token operatorToken;
      std::unique_ptr<Expr> expr;

      friend std::ostream &operator<<(std::ostream &os, const Rhs &rhs);
    };

    std::vector<Rhs> terms;

    void print(std::ostream &os) const override;
    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class PrefixExpr : public Expr {
  public:
    std::vector<Token> prefixes;
    std::unique_ptr<Expr> expr;

    void print(std::ostream &os) const override;
    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class FunCallExpr : public Expr {
  public:
    std::unique_ptr<Expr> function;
    Token openToken;
    std::vector<std::unique_ptr<Expr>> arguments;
    std::vector<Token> commas;
    Token closeToken;

    void print(std::ostream &os) const override;
    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class HintedExpr : public Expr {
  public:
    std::unique_ptr<Expr> expr;
    TypeHint hint;

    void print(std::ostream &os) const override;
    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };
}
