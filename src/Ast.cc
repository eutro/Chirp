#include "Ast.h"

#include <stdexcept>

namespace ast {
  std::ostream &operator<<(std::ostream &os, const Token &token) {
    os << '"';
    for (char c : token.value) {
      switch (c) {
        case '\n':
          os << "\\n";
          break;
        case '"':
          os << "\\\"";
          break;
        case '\\':
          os << "\\\\";
          break;
        default:
          os << c;
      }
    }
    os << '"';
    return os;
  }

  template<typename Iterable>
  void printMulti(std::ostream &os, const Iterable &ts) {
    for (const auto &el : ts) {
      os << " " << el;
    }
  }

  template<typename IterableA, typename IterableB>
  void printMulti(std::ostream &os, const IterableA &ts, const IterableB &commas) {
    auto it = ts.begin();
    auto ci = commas.begin();
    for (; it != ts.end(); ++it) {
      os << " " << *it;
      if (ci != commas.end()) {
        os << " " << *ci;
        ++ci;
      }
    }
  }

  std::ostream &operator<<(std::ostream &os, const Program &program) {
    os << "(Program";
    printMulti(os, program.statements);
    os << ")";
    return os;
  }
  void Program::inferTypes(ParseContext &ctx) {
    for (auto &stmt : statements) {
      stmt->inferTypes(ctx);
    }
  }

  std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Statement> &statement) {
    statement->print(os);
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Expr> &statement) {
    statement->print(os);
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const std::unique_ptr<DelimitedExpr> &statement) {
    statement->print(os);
    return os;
  }

  ParseContext::ParseContext(type::TypeContext &tc) :
      tc(tc),
      funcType(std::make_shared<type::BaseType>("fn")),
      unitType(std::make_shared<type::BaseType>("unit")),
      intType(std::make_shared<type::BaseType>("int")),
      floatType(std::make_shared<type::BaseType>("float")),
      boolType(std::make_shared<type::BaseType>("bool")),
      stringType(std::make_shared<type::BaseType>("string")) {
    scopes.emplace_back();
  }

  std::shared_ptr<Var> &ParseContext::introduce(const std::string &name, PType &&type) {
    return scopes.back().bindings[name] = std::make_shared<Var>(std::move(type));
  }
  std::shared_ptr<Var> &ParseContext::lookup(const std::string &name) {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
      auto found = it->bindings.find(name);
      if (found != it->bindings.end()) {
        return found->second;
      }
    }
    throw std::runtime_error("Undefined");
  }

  void Defn::print(std::ostream &os) const {
    os << "(Defn " << defnToken << " " << binding << ")";
  }
  void Defn::inferTypes(ParseContext &ctx) {
    ctx.tc.bound.push_back(this->binding.inferType(ctx));
  }

  void Expr::inferTypes(ParseContext &ctx) {
    infer(ctx);
  }
  CType *Expr::infer(ParseContext &ctx) {
    type = inferType(ctx);
    return type;
  }

  void IfExpr::print(std::ostream &os) const {
    os << "(IfExpr ";
    if (type) os << "#\"" << type->get() << "\" ";
    os << ifToken << " " << predExpr << " " << thenExpr;
    printMulti(os, this->elseIfClauses);
    if (this->elseClause) os << " " << *this->elseClause;
    os << ")";
  }
  void IfExpr::inferTypes(ParseContext &ctx) {
    CType *boolType = ctx.tc.push(CType::aggregate(ctx.boolType, {}));
    boolType->unify(*predExpr->infer(ctx));
    thenExpr->inferTypes(ctx);
    for (auto &clause : this->elseIfClauses) {
      boolType->unify(*clause.predExpr->infer(ctx));
      clause.thenExpr->inferTypes(ctx);
    }
    if (elseClause) {
      elseClause->thenExpr->inferTypes(ctx);
    }
  }
  CType *IfExpr::inferType(ParseContext &ctx) {
    if (!elseClause) {
      inferTypes(ctx);
      return ctx.tc.push(CType::aggregate(ctx.unitType, {}));
    }
    CType *boolType = ctx.tc.push(CType::aggregate(ctx.boolType, {}));
    boolType->unify(*predExpr->infer(ctx));
    CType *type = thenExpr->infer(ctx);
    for (auto &clause : elseIfClauses) {
      boolType->unify(*clause.predExpr->infer(ctx));
      type->get().unify(clause.thenExpr->infer(ctx)->get());
    }
    type->get().unify(elseClause->thenExpr->infer(ctx)->get());
    return type;
  }

  void LetExpr::print(std::ostream &os) const {
    os << "(LetExpr ";
    if (type) os << "#\"" << type->get() << "\" ";
    os << letToken;
    printMulti(os, bindings, commas);
    os << " " << inToken;
    if (name) os << " " << *name;
    os << " " << body << ")";
  }
  CType *LetExpr::inferType(ParseContext &ctx) {
    size_t oldSize = ctx.tc.bound.size();
    ctx.scopes.emplace_back();

    CType *type;
    if (name) {
      type = ctx.tc.fresh();
      std::vector<CType *> args(bindings.size() + 1, nullptr);
      size_t i = 0;
      for (auto &b : bindings) {
        auto inferred = b.inferType(ctx);
        if (!inferred->bound.empty()) {
          throw std::runtime_error("Named 'let' may not have polymorphic bindings");
        }
        args[i++] = inferred->type;
        ctx.tc.bound.push_back(inferred);
      }
      args[i] = type;
      CType *funcType = ctx.tc.push(CType::aggregate(ctx.funcType, std::move(args)));
      ctx.introduce(name->ident.value, funcType);
      body->infer(ctx)->get().unify(type->get());
    } else {
      for (auto &b : bindings) {
        ctx.tc.bound.push_back(b.inferType(ctx));
      }
      type = body->infer(ctx);
    }
    ctx.scopes.pop_back();
    ctx.tc.bound.resize(oldSize);
    return type;
  }

  void BlockExpr::print(std::ostream &os) const {
    os << "(BlockExpr ";
    if (type) os << "#\"" << type->get() << "\" ";
    os << openToken;
    printMulti(os, statements);
    os << " " << value << " " << closeToken << ")";
  }
  CType *BlockExpr::inferType(ParseContext &ctx) {
    size_t oldSize = ctx.tc.bound.size();
    ctx.scopes.emplace_back();
    for (auto &stmt : statements) {
      stmt.statement->inferTypes(ctx);
    }
    CType *type = value->infer(ctx);
    ctx.scopes.pop_back();
    ctx.tc.bound.resize(oldSize);
    return type;
  }

  void BracketExpr::print(std::ostream &os) const {
    os << "(BracketExpr ";
    if (type) os << "#\"" << type->get() << "\" ";
    os << openToken << " " << value << " " << closeToken << ")";
  }
  CType *BracketExpr::inferType(ParseContext &ctx) {
    return value->infer(ctx);
  }

  void LiteralExpr::print(std::ostream &os) const {
    os << "(LiteralExpr ";
    if (type) os << "#\"" << type->get() << "\" ";
    os << value << ")";
  }
  CType *LiteralExpr::inferType(ParseContext &ctx) {
    switch (value.type) {
      case Tok::TStr:
        return ctx.tc.push(CType::aggregate(ctx.stringType, {}));
      case Tok::TInt:
        return ctx.tc.push(CType::aggregate(ctx.intType, {}));
      case Tok::TFloat:
        return ctx.tc.push(CType::aggregate(ctx.floatType, {}));
      default:
        return ctx.tc.push(CType::aggregate(ctx.boolType, {}));
    }
  }

  Var::Var(PType &&type) : type(std::make_shared<PType>(type)) {}

  void VarExpr::print(std::ostream &os) const {
    os << "(VarExpr ";
    if (type) os << "#\"" << type->get() << "\" ";
    os << name << ")";
  }
  CType *VarExpr::inferType(ParseContext &ctx) {
    return ctx.tc.inst(*ctx.lookup(name.ident.value)->type);
  }

  void BinaryExpr::print(std::ostream &os) const {
    os << "(BinaryExpr ";
    if (type) os << "#\"" << type->get() << "\" ";
    os << this->lhs;
    printMulti(os, this->terms);
    os << ")";
  }
  CType *BinaryExpr::inferType(ParseContext &ctx) {
    CType *argType = lhs->infer(ctx);
    for (auto &rhs : terms) {
      argType->get().unify(rhs.expr->infer(ctx)->get());
    }
    switch (terms[0].operatorToken.type) {
      case Tok::TNe:
      case Tok::TEq2:
      case Tok::TEq:
      case Tok::TLt:
      case Tok::TGt:
      case Tok::TLe:
      case Tok::TGe:
      case Tok::TOr2:
      case Tok::TAnd2:
        return ctx.tc.push(CType::aggregate(ctx.boolType, {}));
      default:
        return argType;
    }
  }

  void PrefixExpr::print(std::ostream &os) const {
    os << "(PrefixExpr";
    if (type) os << " #\"" << type->get() << "\"";
    printMulti(os, this->prefixes);
    os << " " << this->expr << ")";
  }
  CType *PrefixExpr::inferType(ParseContext &ctx) {
    return expr->infer(ctx);
  }

  void FunCallExpr::print(std::ostream &os) const {
    os << "(FunCallExpr ";
    if (type) os << "#\"" << type->get() << "\" ";
    os << this->function << " " << this->openToken;
    printMulti(os, this->arguments, this->commas);
    os << " " << this->closeToken << ")";
  }
  CType *FunCallExpr::inferType(ParseContext &ctx) {
    CType *funcType = function->infer(ctx);
    std::vector<CType *> argTypes(arguments.size() + 1, nullptr);
    size_t i = 0;
    for (auto &arg : arguments) {
      argTypes[i++] = arg->infer(ctx);
    }
    CType *type = ctx.tc.fresh();
    argTypes[i] = type;
    funcType->get().unify(*ctx.tc.push(CType::aggregate(ctx.funcType, std::move(argTypes))));
    return type;
  }

  void HintedExpr::print(std::ostream &os) const {
    os << "(HintedExpr ";
    if (type) os << "#\"" << type->get() << "\" ";
    os << this->expr << " " << this->hint << ")";
  }
  CType *HintedExpr::inferType(ParseContext &ctx) {
    // TODO type hints
    return expr->infer(ctx);
  }

  std::ostream &operator<<(std::ostream &os, const IfExpr::ElseIf &anIf) {
    os << "(ElseIf " << anIf.elseToken << " " << anIf.ifToken << " " << anIf.predExpr
       << " " << anIf.thenExpr << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const IfExpr::Else &anElse) {
    os << "(Else " << anElse.elseToken << " " << anElse.thenExpr << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const Binding &binding) {
    os << "(Binding ";
    if (binding.type) os << "#\"" << *binding.type << "\" ";
    os << binding.name;
    if (binding.arguments) os << " " << *binding.arguments;
    if (binding.typeHint) os << " " << *binding.typeHint;
    os << " " << binding.eqToken << " " << binding.value << ")";
    return os;
  }

  CType *inferFuncType(ParseContext &ctx,
                       std::vector<RawBinding> &bindings,
                       Identifier *name,
                       std::unique_ptr<Expr> &value) {
    CType *inferred;
    ctx.scopes.emplace_back();
    size_t oldSize = ctx.tc.bound.size();
    std::vector<CType *> params(bindings.size() + 1, nullptr);
    size_t i = 0;
    for (auto &rb : bindings) {
      auto var = ctx.introduce(rb.name.ident.value, params[i++] = ctx.tc.fresh());
      ctx.tc.bound.push_back(var->type);
    }
    CType *retType = params[i] = ctx.tc.fresh();
    inferred = ctx.tc.push(CType::aggregate(ctx.funcType, std::move(params)));
    if (name) {
      ctx.introduce(name->ident.value, inferred);
    }
    value->infer(ctx)->get().unify(retType->get());
    ctx.tc.bound.resize(oldSize);
    ctx.scopes.pop_back();
    return inferred;
  }

  std::shared_ptr<PType> &Binding::inferType(ParseContext &ctx) {
    CType *inferred;
    if (arguments) {
      inferred = inferFuncType(ctx, arguments->bindings, &name, value);
    } else {
      inferred = value->infer(ctx);
    }
    return type = ctx.introduce(name.ident.value, ctx.tc.gen(inferred))->type;
  }

  void FnExpr::print(std::ostream &os) const {
    os << "(FnExpr ";
    if (type) os << "#\"" << type->get() << "\" ";
    os << fnToken << " ";
    if (name) os << *name << " ";
    os << arguments << " " << eqToken << " " << body << ")";
  }
  CType *FnExpr::inferType(ParseContext &ctx) {
    return inferFuncType(ctx, arguments.bindings, name ? &*name : nullptr, body);
  }

  void LambdaExpr::print(std::ostream &os) const {
    os << "(LambdaExpr";
    if (type) os << " #\"" << type->get() << "\"";
    os << " " << lambdaToken;
    printMulti(os, arguments, commas);
    os << " " << dotToken << " " << body << ")";
  }
  CType *LambdaExpr::inferType(ParseContext &ctx) {
    return inferFuncType(ctx, arguments, nullptr, body);
  }

  std::ostream &operator<<(std::ostream &os, const Identifier &identifier) {
    os << "(Identifier " << identifier.ident << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const Binding::Arguments &arguments) {
    os << "(Arguments " << arguments.openToken << " ";
    printMulti(os, arguments.bindings, arguments.commas);
    os << " " << arguments.closeToken << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const TypeHint &hint) {
    os << "(TypeHint " << hint.colon << " " << hint.type << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const RawBinding &binding) {
    os << "(RawBinding " << binding.name;
    if (binding.typeHint) os << *binding.typeHint;
    os << ")";
    return os;
  }

  void PlaceholderType::print(std::ostream &os) const {
    os << "(PlaceholderType " << placeholder << ")";
  }

  void NamedType::print(std::ostream &os) const {
    os << "(NamedType " << raw;
    if (this->parameters) os << " " << *this->parameters;
    os << ")";
  }

  std::ostream &operator<<(std::ostream &os, const NamedType::TypeParameters &parameters) {
    os << "(TypeParameters " << parameters.openToken << " ";
    printMulti(os, parameters.types, parameters.commas);
    os << " " << parameters.closeToken << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Type> &statement) {
    statement->print(os);
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const BlockExpr::Stmt &stmt) {
    os << "(Statement " << stmt.statement << " " << stmt.delimiter << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const BinaryExpr::Rhs &rhs) {
    os << rhs.operatorToken << " " << rhs.expr;
    return os;
  }
}
