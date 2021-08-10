#include "AstInfer.h"
#include "Ast.h"

namespace ast {
  class InferVisitor : public Visitor {
  private:
    TPtr lastType;

    TPtr getType(Type &it) {
      visitType(it);
      return lastType;
    }

    TPtr maybeHinted(const std::optional<TypeHint> &hint) {
      return hint ? getType(*hint->type) : ctx.tc.fresh();
    }

    TPtr inferExpr(Expr &it, Position pos) {
      visitExpr(it, pos);
      return it.type;
    }

    TPtr inferFuncType(const std::optional<Binding::TypeArguments> &typeArguments,
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
        rb.var = ctx.introduce(rb.name.ident.value, params[i++] = maybeHinted(rb.typeHint));
        ctx.tc.bound.push_back(rb.var->type);
      }
      TPtr retType = params[i] = maybeHinted(returnHint);
      inferred = ctx.tc.push(CType::aggregate(ctx.funcType, std::move(params)));
      if (name) {
        *recurVar = ctx.introduce(name->ident.value, inferred);
      }
      CType::getAndUnify(ctx.tc, inferExpr(*value, Position::Expr), retType);
      ctx.tc.bound.resize(oldSize);
      closed = std::move(ctx.scopes.back().referenced);
      ctx.scopes.pop_back();
      if (typeArguments) {
        ctx.typeScopes.pop_back();
      }
      return inferred;
    }

    std::shared_ptr<PType> &inferBindingType(Binding &it) {
      TPtr inferred;
      if (it.value) {
        if (it.arguments) {
          inferred = inferFuncType(it.arguments->typeArguments,
                                   it.arguments->bindings,
                                   it.typeHint,
                                   &it.name,
                                   &it.arguments->recurVar,
                                   it.arguments->closed,
                                   it.value);
          it.var = ctx.introduce(it.name.ident.value, ctx.tc.gen(inferred));
        } else {
          inferred = inferExpr(*it.value, Position::Expr);
          if (it.typeHint) {
            CType::getAndUnify(ctx.tc, inferred, maybeHinted(it.typeHint));
          }
          it.var = ctx.introduce(it.name.ident.value, inferred);
        }
      } else {
        if (it.arguments) {
          std::vector<TPtr> params(it.arguments->bindings.size() + 1);
          size_t i = 0;
          for (auto &binding : it.arguments->bindings) {
            params[i++] = maybeHinted(binding.typeHint);
          }
          params[i] = maybeHinted(it.typeHint);
          inferred = ctx.tc.push(CType::aggregate(ctx.funcType, std::move(params)));
        } else {
          inferred = maybeHinted(it.typeHint);
        }
        // don't generalise FFI functions
        it.var = ctx.introduce(it.name.ident.value, inferred);
      }
      return it.var->type;
    }

  public:
    ParseContext &ctx;
    InferVisitor(ParseContext &ctx): ctx(ctx) {}

    void visitPlaceholderType(PlaceholderType &it) override {
      lastType = ctx.tc.fresh();
    }

    void visitNamedType(NamedType &it) override {
      auto &found = ctx.lookupType(it.raw.ident.value);
      if (it.parameters) {
        std::vector<TPtr> typeParams(it.parameters->types.size());
        size_t i = 0;
        for (const auto &param : it.parameters->types) {
          typeParams[i++] = getType(*param);
        }
        if (found.index() != 0) {
          throw std::runtime_error("Names a non-generic type");
        }
        lastType = ctx.tc.push(CType::aggregate(std::get<0>(found), std::move(typeParams)));
      } else {
        if (found.index() == 0) {
          lastType = ctx.tc.push(CType::aggregate(std::get<0>(found), {}));
        } else {
          lastType = std::get<1>(found);
        }
      }
    }

    void visitDefn(Defn &it) override {
      ctx.tc.bound.push_back(inferBindingType(it.binding));
    }

    void visitExpr(Expr &it, Position pos) override {
      Visitor::visitExpr(it, pos);
      it.type = lastType;
    }

    void visitIfExpr(IfExpr &it, Position pos) override {
      bool isStatement = pos == Position::Statement || !it.elseClause;

      TPtr boolType = ctx.tc.push(CType::aggregate(ctx.boolType, {}));
      CType::getAndUnify(ctx.tc, boolType, inferExpr(*it.predExpr, Position::Expr));
      TPtr thenType = inferExpr(*it.thenExpr, pos);
      TPtr type = isStatement ? ctx.tc.push(CType::aggregate(ctx.unitType, {})) : thenType;
      for (auto &clause : it.elseIfClauses) {
        CType::getAndUnify(ctx.tc, boolType, inferExpr(*clause.predExpr, Position::Expr));
        TPtr elseIfType = inferExpr(*clause.thenExpr, pos);
        if (!isStatement) {
          CType::getAndUnify(ctx.tc, type, elseIfType);
        }
      }
      if (it.elseClause) {
        TPtr elseType = inferExpr(*it.elseClause->thenExpr, pos);
        if (!isStatement) {
          CType::getAndUnify(ctx.tc, type, elseType);
        }
      }
      lastType = type;
    }

    void visitLetExpr(LetExpr &it, Position pos) override {
      size_t oldSize = ctx.tc.bound.size();
      ctx.scopes.emplace_back();

      TPtr type;
      if (it.name) {
        type = ctx.tc.fresh();
        std::vector<TPtr> args(it.bindings.size() + 1, nullptr);
        size_t i = 0;
        for (auto &b : it.bindings) {
          auto inferred = inferBindingType(b);
          if (!inferred->bound.empty()) {
            throw std::runtime_error("Named 'let' may not have polymorphic bindings");
          }
          args[i++] = inferred->type;
          ctx.tc.bound.push_back(inferred);
        }
        args[i] = type;
        TPtr funcType = ctx.tc.push(CType::aggregate(ctx.funcType, std::move(args)));
        ctx.scopes.emplace_back();
        it.nameVar = ctx.introduce(it.name->ident.value, funcType);
        CType::getAndUnify(ctx.tc, inferExpr(*it.body, pos), type);
        it.closed = std::move(ctx.scopes.back().referenced);
        for (const auto &b : it.bindings) {
          it.closed.erase(b.var);
        }
        ctx.scopes.pop_back();
      } else {
        for (auto &b : it.bindings) {
          ctx.tc.bound.push_back(inferBindingType(b));
        }
        type = inferExpr(*it.body, pos);
      }
      ctx.scopes.pop_back();
      ctx.tc.bound.resize(oldSize);
      lastType = type;
    }

    void visitBlockExpr(BlockExpr &it, Position pos) override {
      if (it.value) {
        size_t oldSize = ctx.tc.bound.size();
        ctx.scopes.emplace_back();
        for (auto &stmt : it.statements) {
          visitStatement(*stmt.statement);
        }
        TPtr type = inferExpr(*it.value, pos);
        ctx.scopes.pop_back();
        ctx.tc.bound.resize(oldSize);
        lastType = type;
      } else {
        lastType = ctx.tc.push(CType::aggregate(ctx.unitType, {}));
      }
    }

    void visitBracketExpr(BracketExpr &it, Position pos) override {
      lastType = inferExpr(*it.value, pos);
    }

    void visitColonExpr(ColonExpr &it, Position pos) override {
      lastType = inferExpr(*it.value, pos);
    }

    void visitLiteralExpr(LiteralExpr &it, Position pos) override {
      switch (it.value.type) {
      case Tok::TStr:
        lastType = ctx.tc.push(CType::aggregate(ctx.stringType, {}));
        break;
      case Tok::TInt:
        lastType = ctx.tc.push(CType::aggregate(ctx.intType, {}));
        break;
      case Tok::TFloat:
        lastType = ctx.tc.push(CType::aggregate(ctx.floatType, {}));
        break;
      default:
        lastType = ctx.tc.push(CType::aggregate(ctx.boolType, {}));
        break;
      }
    }

    void visitVarExpr(VarExpr &it, Position pos) override {
      it.var = ctx.lookup(it.name.ident.value);
      lastType = ctx.tc.inst(*it.var->type);
    }

    void visitBinaryExpr(BinaryExpr &it, Position pos) override {
      TPtr argType = inferExpr(*it.lhs, Position::Expr);
      for (auto &rhs : it.terms) {
        CType::getAndUnify(ctx.tc, argType, inferExpr(*rhs.expr, Position::Expr));
      }
      switch (it.terms[0].operatorToken.type) {
      case Tok::TOr1:
      case Tok::TAnd1: {
        TPtr intType = ctx.tc.push(CType::aggregate(ctx.intType, {}));
        CType::getAndUnify(ctx.tc, argType, intType);
        lastType = intType;
        return;
      }
      case Tok::TOr2:
      case Tok::TAnd2: {
        TPtr boolType = ctx.tc.push(CType::aggregate(ctx.boolType, {}));
        CType::getAndUnify(ctx.tc, argType, boolType);
        lastType = boolType;
        return;
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
        lastType = ctx.tc.push(CType::aggregate(ctx.boolType, {}));
        return;
      }
      default: {
        TPtr trait = ctx.tc.fresh();
        auto &traits = std::get<0>(trait->value).traits;
        for (auto &rhs : it.terms) {
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
        lastType = argType;
        return;
      }
      }
    }

    void visitPrefixExpr(PrefixExpr &it, Position pos) override {
      lastType = inferExpr(*it.expr, Position::Expr);
    }

    void visitFunCallExpr(FunCallExpr &it, Position pos) override {
      TPtr funcType = inferExpr(*it.function, Position::Expr);
      std::vector<TPtr> argTypes(it.arguments.size() + 1, nullptr);
      size_t i = 0;
      for (auto &arg : it.arguments) {
        argTypes[i++] = inferExpr(*arg, Position::Expr);
      }
      TPtr type = ctx.tc.fresh();
      argTypes[i] = type;
      CType::getAndUnify(ctx.tc,
                         funcType,
                         ctx.tc.push(CType::aggregate(ctx.funcType, std::move(argTypes))));
      lastType = type;
    }

    void visitHintedExpr(HintedExpr &it, Position pos) override {
      TPtr inferred = inferExpr(*it.expr, pos);
      CType::getAndUnify(ctx.tc, inferred, getType(*it.hint.type));
      lastType = inferred;
    }

    void visitFnExpr(FnExpr &it, Position pos) override {
      lastType =
        inferFuncType(it.arguments.typeArguments,
                      it.arguments.bindings,
                      it.typeHint,
                      it.name ? &*it.name : nullptr,
                      &it.recurVar,
                      it.closed,
                      it.body);
    }

    void visitLambdaExpr(LambdaExpr &it, Position pos) override {
      lastType =
        inferFuncType(std::nullopt,
                      it.arguments,
                      std::nullopt,
                      nullptr,
                      nullptr,
                      it.closed,
                      it.body);
    }
  };

  std::unique_ptr<Visitor> inferenceVisitor(ParseContext &ctx) {
    return std::make_unique<InferVisitor>(ctx);
  }

  ParseContext::ParseContext(type::TypeContext &tc) :
      tc(tc),
      funcType(std::make_shared<type::BaseType>("fn")),
      ptrType(std::make_shared<type::BaseType>("ptr")),
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
      builtinTypes["ptr"] = ptrType;
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
}
