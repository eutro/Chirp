#pragma once

#include "Lexer.h"
#include "Type.h"
#include "Tokens.h"
#include "Err.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/Type.h>
#include <functional>
#include <deque>
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

  class TypeScope {
  public:
    std::map<std::string, std::variant<std::shared_ptr<type::BaseType>, TPtr>> bindings;
  };

  class CompileContext;

  class BinaryTrait : public type::TraitImpl {
  public:
    using Fn = std::function<llvm::Value *(CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs)>;

    Fn app;
    BinaryTrait(Fn &&app);
  };

  class CmpTrait : public type::TraitImpl {
  public:
    using Fn = std::function<llvm::Value *(CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs)>;

    Fn ne, eq, lt, gt, le, ge;
    CmpTrait(Fn &&ne, Fn &&eq, Fn &&lt, Fn &&gt, Fn &&le, Fn &&ge);
  };

  class CollectibleTrait : public type::TraitImpl {
  public:
    llvm::Value *meta;
    CollectibleTrait(llvm::Value *meta);
  };

  class ParseContext {
  public:
    type::TypeContext &tc;

    std::shared_ptr<type::BaseType> funcType;
    std::shared_ptr<type::BaseType> ptrType;
    std::shared_ptr<type::BaseType> unitType;
    std::shared_ptr<type::BaseType> intType;
    std::shared_ptr<type::BaseType> floatType;
    std::shared_ptr<type::BaseType> boolType;
    std::shared_ptr<type::BaseType> stringType;

    std::shared_ptr<type::TypedTrait<BinaryTrait>> addTrait;
    std::shared_ptr<type::TypedTrait<BinaryTrait>> mulTrait;
    std::shared_ptr<type::TypedTrait<BinaryTrait>> subTrait;
    std::shared_ptr<type::TypedTrait<BinaryTrait>> divTrait;
    std::shared_ptr<type::TypedTrait<BinaryTrait>> remTrait;

    std::shared_ptr<type::TypedTrait<CmpTrait>> cmpTrait;

    std::shared_ptr<type::TypedTrait<CollectibleTrait>> collectibleTrait;

    std::deque<Scope> scopes;
    std::deque<TypeScope> typeScopes;

    std::shared_ptr<Var> &introduce(const std::string &name, PType &&type);
    std::shared_ptr<Var> &lookup(const std::string &name);

    std::variant<std::shared_ptr<type::BaseType>, TPtr> &lookupType(const std::string &name);

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

    std::map<std::shared_ptr<type::BaseType>,
        std::function<llvm::DIType *(CompileContext &, TPtr)>> diTransformers;

    llvm::StructType *gcMetaType;
    llvm::FunctionType *visitFnType;
    llvm::FunctionType *metaFnType;
    llvm::Constant *fnMeta;

    llvm::DIBuilder &diBuilder;
    llvm::DICompileUnit *diCU;

    CompileContext(llvm::LLVMContext &ctx,
                   llvm::IRBuilder<> &builder,
                   llvm::DIBuilder &diBuilder,
                   llvm::Module &module,
                   ParseContext &pc);
  };

  class Var {
  public:
    std::shared_ptr<PType> type;
    std::function<llvm::Value *(CompileContext &, TPtr)> emit;

    Var(PType &&type);
  };

  class Visitor;

  class Statement {
  public:
    loc::Span span;

    virtual ~Statement() = default;
    virtual void acceptStatement(Visitor &v) = 0;

    virtual void inferStatement(ParseContext &ctx) = 0;
    virtual void compileStatement(CompileContext &ctx) = 0;
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

    virtual TPtr get(ParseContext &ctx) const = 0;
  };

  class PlaceholderType : public Type {
  public:
    Token placeholder;

    void acceptType(Visitor &v) override;

    TPtr get(ParseContext &ctx) const override;
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

    TPtr get(ParseContext &ctx) const override;
  };

  class TypeHint {
  public:
    Token colon;
    std::unique_ptr<Type> type;
  };

  class RawBinding {
  public:
    std::shared_ptr<Var> var;

    Identifier name;
    std::optional<TypeHint> typeHint;
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

    void acceptStatement(Visitor &v) override;

    virtual void acceptExpr(Visitor &v, Position pos) = 0;

    void inferStatement(ParseContext &ctx) override;
    TPtr inferExpr(ParseContext &ctx, Position pos);

    void compileStatement(CompileContext &ctx) override;
    virtual llvm::Value *compileExpr(CompileContext &ctx, Position pos) = 0;
  };

  class Binding {
  public:
    loc::Span span;

    std::shared_ptr<Var> var;
    std::map<TPtr, llvm::Value *, type::CompareType> insts;
    Identifier name;

    struct TypeArguments {
      Token openToken;
      std::vector<Identifier> idents;
      std::vector<Token> commas;
      Token closeToken;
    };

    struct Arguments {
      std::shared_ptr<Var> recurVar;
      std::set<std::shared_ptr<Var>> closed;

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

    std::shared_ptr<PType> &inferType(ParseContext &ctx);
    void compile(CompileContext &ctx);
    llvm::Value *compileExpr(CompileContext &ctx, TPtr instType);
  };

  class Defn : public Statement {
  public:
    Token defnToken;
    Binding binding;

    void acceptStatement(Visitor &v) override;

    void inferStatement(ParseContext &ctx) override;
    void compileStatement(CompileContext &ctx) override;
  };

  class Program {
  public:
    std::vector<std::unique_ptr<Statement>> statements;
    std::vector<Token> delimiters;

    void inferTypes(ParseContext &ctx);
    void compile(CompileContext &ctx);
  };

  class PrimaryExpr : public Expr {
  };

  class DelimitedExpr : public PrimaryExpr {
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
    };

    struct Else {
      Token elseToken;
      std::unique_ptr<DelimitedExpr> thenExpr;
    };

    std::vector<ElseIf> elseIfClauses;
    std::optional<Else> elseClause;

    void acceptExpr(Visitor &v, Position pos) override;

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
    std::unique_ptr<Expr> body;

    void acceptExpr(Visitor &v, Position pos) override;

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
    std::optional<TypeHint> typeHint;
    Token eqToken;

    std::unique_ptr<Expr> body;

    void acceptExpr(Visitor &v, Position pos) override;

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

    void acceptExpr(Visitor &v, Position pos) override;

    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class BlockExpr : public DelimitedExpr {
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

    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class BracketExpr : public DelimitedExpr {
  public:
    Token openToken;
    std::unique_ptr<Expr> value;
    Token closeToken;

    void acceptExpr(Visitor &v, Position pos) override;

    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class ColonExpr : public DelimitedExpr {
  public:
    Token colonToken;
    std::unique_ptr<Expr> value;

    void acceptExpr(Visitor &v, Position pos) override;

    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class LiteralExpr : public PrimaryExpr {
  public:
    Token value;

    void acceptExpr(Visitor &v, Position pos) override;

    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class VarExpr : public PrimaryExpr {
  public:
    Identifier name;
    std::shared_ptr<Var> var;

    void acceptExpr(Visitor &v, Position pos) override;

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
    };

    std::vector<Rhs> terms;

    void acceptExpr(Visitor &v, Position pos) override;

    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class PrefixExpr : public Expr {
  public:
    std::vector<Token> prefixes;
    std::unique_ptr<Expr> expr;

    void acceptExpr(Visitor &v, Position pos) override;

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

    void acceptExpr(Visitor &v, Position pos) override;

    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
  };

  class HintedExpr : public Expr {
  public:
    std::unique_ptr<Expr> expr;
    TypeHint hint;

    void acceptExpr(Visitor &v, Position pos) override;

    TPtr inferType(ParseContext &ctx, Position pos) override;
    llvm::Value *compileExpr(CompileContext &ctx, Position pos) override;
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
