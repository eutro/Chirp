#include "Ast.h"

namespace ast {
  TPtr PlaceholderType::get(ParseContext &ctx) const {
    return ctx.tc.fresh();
  }

  TPtr NamedType::get(ParseContext &ctx) const {
    auto &found = ctx.lookupType(raw.ident.value);
    if (parameters) {
      std::vector<TPtr> typeParams(parameters->types.size());
      size_t i = 0;
      for (const auto &param : parameters->types) {
        typeParams[i++] = param->get(ctx);
      }
      if (found.index() != 0) {
        throw std::runtime_error("Names a non-generic type");
      }
      return ctx.tc.push(CType::aggregate(std::get<0>(found), std::move(typeParams)));
    } else {
      if (found.index() == 0) {
        return ctx.tc.push(CType::aggregate(std::get<0>(found), {}));
      } else {
        return std::get<1>(found);
      }
    }
  }

  ParseContext::ParseContext(type::TypeContext &tc) :
      tc(tc),
      funcType(std::make_shared<type::BaseType>("fn")),
      unitType(std::make_shared<type::BaseType>("unit")),
      intType(std::make_shared<type::BaseType>("int")),
      floatType(std::make_shared<type::BaseType>("float")),
      boolType(std::make_shared<type::BaseType>("bool")),
      stringType(std::make_shared<type::BaseType>("string")),

      addTrait(std::make_shared<type::TypedTrait<BinaryTrait>>("Add")),
      mulTrait(std::make_shared<type::TypedTrait<BinaryTrait>>("Mul")),
      subTrait(std::make_shared<type::TypedTrait<BinaryTrait>>("Sub")),
      divTrait(std::make_shared<type::TypedTrait<BinaryTrait>>("Div")),
      remTrait(std::make_shared<type::TypedTrait<BinaryTrait>>("Rem")),

      cmpTrait(std::make_shared<type::TypedTrait<CmpTrait>>("Cmp")),

      collectibleTrait(std::make_shared<type::TypedTrait<CollectibleTrait>>("Collectible")) {
    scopes.emplace_back();
    typeScopes.emplace_back();
    {
      auto &builtinTypes = typeScopes.back().bindings;
      builtinTypes["fn"] = funcType;
      builtinTypes["unit"] = unitType;
      builtinTypes["int"] = intType;
      builtinTypes["float"] = floatType;
      builtinTypes["bool"] = boolType;
      builtinTypes["string"] = stringType;
    }

    intType->impls[addTrait] = std::make_unique<BinaryTrait>
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateAdd(lhs, rhs); });
    intType->impls[mulTrait] = std::make_unique<BinaryTrait>
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateMul(lhs, rhs); });
    intType->impls[subTrait] = std::make_unique<BinaryTrait>
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateSub(lhs, rhs); });
    intType->impls[divTrait] = std::make_unique<BinaryTrait>
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateSDiv(lhs, rhs); });
    intType->impls[remTrait] = std::make_unique<BinaryTrait>
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateSRem(lhs, rhs); });
    intType->impls[cmpTrait] = std::make_unique<CmpTrait>(
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateICmpNE(lhs, rhs); }),
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateICmpEQ(lhs, rhs); }),
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateICmpSLT(lhs, rhs); }),
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateICmpSGT(lhs, rhs); }),
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateICmpSLE(lhs, rhs); }),
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateICmpSGE(lhs, rhs); })
    );

    floatType->impls[addTrait] = std::make_unique<BinaryTrait>
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateFAdd(lhs, rhs); });
    floatType->impls[mulTrait] = std::make_unique<BinaryTrait>
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateFMul(lhs, rhs); });
    floatType->impls[subTrait] = std::make_unique<BinaryTrait>
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateFSub(lhs, rhs); });
    floatType->impls[divTrait] = std::make_unique<BinaryTrait>
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateFDiv(lhs, rhs); });
    floatType->impls[remTrait] = std::make_unique<BinaryTrait>
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateFRem(lhs, rhs); });
    floatType->impls[cmpTrait] = std::make_unique<CmpTrait>(
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateFCmpONE(lhs, rhs); }),
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateFCmpOEQ(lhs, rhs); }),
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateFCmpOLT(lhs, rhs); }),
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateFCmpOGT(lhs, rhs); }),
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateFCmpOLE(lhs, rhs); }),
        ([](CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs) { return ctx.builder.CreateFCmpOGE(lhs, rhs); })
    );
  }

  std::shared_ptr<Var> &ParseContext::introduce(const std::string &name, PType &&type) {
    return scopes.back().bindings[name] = std::make_shared<Var>(std::move(type));
  }
  std::shared_ptr<Var> &ParseContext::lookup(const std::string &name) {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
      auto found = it->bindings.find(name);
      if (found != it->bindings.end()) {
        std::shared_ptr<Var> &var = found->second;
        while (it != scopes.rbegin()) {
          (--it)->referenced.insert(var);
        }
        return var;
      }
    }
    throw std::runtime_error("Undefined");
  }

  std::variant<std::shared_ptr<type::BaseType>, TPtr> &ParseContext::lookupType(const std::string &name) {
    for (auto it = typeScopes.rbegin(); it != typeScopes.rend(); ++it) {
      auto found = it->bindings.find(name);
      if (found != it->bindings.end()) {
        return found->second;
      }
    }
    throw std::runtime_error("Undefined type");
  }

  TPtr maybeHinted(ParseContext &ctx, const std::optional<TypeHint> &hint) {
    return hint ? hint->type->get(ctx) : ctx.tc.fresh();
  }

  TPtr inferFuncType(ParseContext &ctx,
                     const std::optional<Binding::TypeArguments> &typeArguments,
                     std::vector<RawBinding> &bindings,
                     const std::optional<TypeHint> &returnHint,
                     Identifier *name,
                     std::shared_ptr<Var> *recurVar,
                     std::set<std::shared_ptr<Var>> &closed,
                     std::unique_ptr<Expr> &value) {
    TPtr inferred;
    if (typeArguments) {
      ctx.typeScopes.emplace_back();
      auto &typeBindings = ctx.typeScopes.back().bindings;
      for (const auto &ident : typeArguments->idents) {
        if (typeBindings.count(ident.ident.value)) {
          throw std::runtime_error("Name already bound");
        }
        typeBindings[ident.ident.value] = ctx.tc.fresh();
      }
    }
    ctx.scopes.emplace_back();
    size_t oldSize = ctx.tc.bound.size();
    std::vector<TPtr> params(bindings.size() + 1, nullptr);
    size_t i = 0;
    for (auto &rb : bindings) {
      rb.var = ctx.introduce(rb.name.ident.value, params[i++] = maybeHinted(ctx, rb.typeHint));
      ctx.tc.bound.push_back(rb.var->type);
    }
    TPtr retType = params[i] = maybeHinted(ctx, returnHint);
    inferred = ctx.tc.push(CType::aggregate(ctx.funcType, std::move(params)));
    if (name) {
      *recurVar = ctx.introduce(name->ident.value, inferred);
    }
    CType::getAndUnify(ctx.tc, value->inferExpr(ctx, Position::Expr), retType);
    ctx.tc.bound.resize(oldSize);
    closed = std::move(ctx.scopes.back().referenced);
    ctx.scopes.pop_back();
    if (typeArguments) {
      ctx.typeScopes.pop_back();
    }
    return inferred;
  }

  void Program::inferTypes(ParseContext &ctx) {
    for (auto &stmt : statements) {
      stmt->inferStatement(ctx);
    }
  }

  void Defn::inferStatement(ParseContext &ctx) {
    ctx.tc.bound.push_back(this->binding.inferType(ctx));
  }

  void Expr::inferStatement(ParseContext &ctx) {
    inferExpr(ctx, Position::Statement);
  }

  TPtr Expr::inferExpr(ParseContext &ctx, Position pos) {
    return type = inferType(ctx, pos);
  }

  TPtr IfExpr::inferType(ParseContext &ctx, Position pos) {
    bool isStatement = pos == Position::Statement || !elseClause;

    TPtr boolType = ctx.tc.push(CType::aggregate(ctx.boolType, {}));
    CType::getAndUnify(ctx.tc, boolType, predExpr->inferExpr(ctx, Position::Expr));
    TPtr thenType = thenExpr->inferExpr(ctx, pos);
    TPtr type = isStatement ? ctx.tc.push(CType::aggregate(ctx.unitType, {})) : thenType;
    for (auto &clause : elseIfClauses) {
      CType::getAndUnify(ctx.tc, boolType, clause.predExpr->inferExpr(ctx, Position::Expr));
      TPtr elseIfType = clause.thenExpr->inferExpr(ctx, pos);
      if (!isStatement) {
        CType::getAndUnify(ctx.tc, type, elseIfType);
      }
    }
    TPtr elseType = elseClause->thenExpr->inferExpr(ctx, pos);
    if (!isStatement) {
      CType::getAndUnify(ctx.tc, type, elseType);
    }
    return type;
  }

  TPtr LetExpr::inferType(ParseContext &ctx, Position pos) {
    size_t oldSize = ctx.tc.bound.size();
    ctx.scopes.emplace_back();

    TPtr type;
    if (name) {
      type = ctx.tc.fresh();
      std::vector<TPtr> args(bindings.size() + 1, nullptr);
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
      TPtr funcType = ctx.tc.push(CType::aggregate(ctx.funcType, std::move(args)));
      ctx.scopes.emplace_back();
      nameVar = ctx.introduce(name->ident.value, funcType);
      CType::getAndUnify(ctx.tc, body->inferExpr(ctx, pos), type);
      closed = std::move(ctx.scopes.back().referenced);
      for (const auto &b : bindings) {
        closed.erase(b.var);
      }
      ctx.scopes.pop_back();
    } else {
      for (auto &b : bindings) {
        ctx.tc.bound.push_back(b.inferType(ctx));
      }
      type = body->inferExpr(ctx, pos);
    }
    ctx.scopes.pop_back();
    ctx.tc.bound.resize(oldSize);
    return type;
  }

  TPtr BlockExpr::inferType(ParseContext &ctx, Position pos) {
    if (value) {
      size_t oldSize = ctx.tc.bound.size();
      ctx.scopes.emplace_back();
      for (auto &stmt : statements) {
        stmt.statement->inferStatement(ctx);
      }
      TPtr type = value->inferExpr(ctx, pos);
      ctx.scopes.pop_back();
      ctx.tc.bound.resize(oldSize);
      return type;
    } else {
      return ctx.tc.push(CType::aggregate(ctx.unitType, {}));
    }
  }

  TPtr BracketExpr::inferType(ParseContext &ctx, Position pos) {
    return value->inferExpr(ctx, pos);
  }

  TPtr ColonExpr::inferType(ParseContext &ctx, Position pos) {
    return value->inferExpr(ctx, pos);
  }

  TPtr LiteralExpr::inferType(ParseContext &ctx, Position pos) {
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

  TPtr VarExpr::inferType(ParseContext &ctx, Position pos) {
    var = ctx.lookup(name.ident.value);
    return ctx.tc.inst(*var->type);
  }

  TPtr BinaryExpr::inferType(ParseContext &ctx, Position pos) {
    TPtr argType = lhs->inferExpr(ctx, Position::Expr);
    for (auto &rhs : terms) {
      CType::getAndUnify(ctx.tc, argType, rhs.expr->inferExpr(ctx, Position::Expr));
    }
    switch (terms[0].operatorToken.type) {
      case Tok::TOr1:
      case Tok::TAnd1: {
        TPtr intType = ctx.tc.push(CType::aggregate(ctx.intType, {}));
        CType::getAndUnify(ctx.tc, argType, intType);
        return intType;
      }
      case Tok::TOr2:
      case Tok::TAnd2: {
        TPtr boolType = ctx.tc.push(CType::aggregate(ctx.boolType, {}));
        CType::getAndUnify(ctx.tc, argType, boolType);
        return boolType;
      }
      case Tok::TNe:
      case Tok::TEq2:
      case Tok::TEq:
      case Tok::TLt:
      case Tok::TGt:
      case Tok::TLe:
      case Tok::TGe: {
        TPtr cmp = ctx.tc.fresh();
        std::get<0>(cmp->value).traits.insert(ctx.cmpTrait);
        CType::getAndUnify(ctx.tc, argType, cmp);
        return ctx.tc.push(CType::aggregate(ctx.boolType, {}));
      }
      default: {
        TPtr trait = ctx.tc.fresh();
        auto &traits = std::get<0>(trait->value).traits;
        for (auto &rhs : terms) {
          switch (rhs.operatorToken.type) {
            case Tok::TAdd:
              traits.insert(ctx.addTrait);
              break;
            case Tok::TSub:
              traits.insert(ctx.subTrait);
              break;
            case Tok::TMul:
              traits.insert(ctx.mulTrait);
              break;
            case Tok::TDiv:
              traits.insert(ctx.divTrait);
              break;
            default: // Tok::TRem
              traits.insert(ctx.remTrait);
              break;
          }
        }
        CType::getAndUnify(ctx.tc, argType, trait);
        return argType;
      }
    }
  }

  TPtr PrefixExpr::inferType(ParseContext &ctx, Position pos) {
    return expr->inferExpr(ctx, Position::Expr);
  }

  TPtr FunCallExpr::inferType(ParseContext &ctx, Position pos) {
    TPtr funcType = function->inferExpr(ctx, Position::Expr);
    std::vector<TPtr> argTypes(arguments.size() + 1, nullptr);
    size_t i = 0;
    for (auto &arg : arguments) {
      argTypes[i++] = arg->inferExpr(ctx, Position::Expr);
    }
    TPtr type = ctx.tc.fresh();
    argTypes[i] = type;
    CType::getAndUnify(ctx.tc, funcType, ctx.tc.push(CType::aggregate(ctx.funcType, std::move(argTypes))));
    return type;
  }

  TPtr HintedExpr::inferType(ParseContext &ctx, Position pos) {
    TPtr inferred = expr->inferExpr(ctx, pos);
    CType::getAndUnify(ctx.tc, inferred, hint.type->get(ctx));
    return inferred;
  }

  TPtr FnExpr::inferType(ParseContext &ctx, Position pos) {
    return inferFuncType(ctx,
                         arguments.typeArguments,
                         arguments.bindings,
                         typeHint,
                         name ? &*name : nullptr,
                         &recurVar,
                         closed,
                         body);
  }

  TPtr LambdaExpr::inferType(ParseContext &ctx, Position pos) {
    return inferFuncType(ctx, std::nullopt, arguments, std::nullopt, nullptr, nullptr, closed, body);
  }

  std::shared_ptr<PType> &Binding::inferType(ParseContext &ctx) {
    TPtr inferred;
    if (value) {
      if (arguments) {
        inferred = inferFuncType(ctx,
                                 arguments->typeArguments,
                                 arguments->bindings,
                                 typeHint,
                                 &name,
                                 &arguments->recurVar,
                                 arguments->closed,
                                 value);
        var = ctx.introduce(name.ident.value, ctx.tc.gen(inferred));
      } else {
        inferred = value->inferExpr(ctx, Position::Expr);
        if (typeHint) {
          CType::getAndUnify(ctx.tc, inferred, typeHint->type->get(ctx));
        }
        var = ctx.introduce(name.ident.value, inferred);
      }
    } else {
      if (arguments) {
        std::vector<TPtr> params(arguments->bindings.size() + 1);
        size_t i = 0;
        for (auto &binding : arguments->bindings) {
          params[i++] = maybeHinted(ctx, binding.typeHint);
        }
        params[i] = maybeHinted(ctx, typeHint);
        inferred = ctx.tc.push(CType::aggregate(ctx.funcType, std::move(params)));
      } else {
        inferred = maybeHinted(ctx, typeHint);
      }
      // don't generalise FFI functions
      var = ctx.introduce(name.ident.value, inferred);
    }
    return var->type;
  }
}
