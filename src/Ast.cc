#include "Ast.h"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <stdexcept>
#include <sstream>

namespace ast {
  llvm::Type *toLLVM(CompileContext &ctx, TPtr type) {
    CType &t = *CType::get(type);
    if (t.value.index() == 0) {
      // Unbounded type
      // This may suggest an infinite loop:
      // e.g. `let in endless { endless() }`
      // or unused variables.
      // In this case the type probably doesn't matter,
      // so just use the unit type.
      return ctx.unitType;
    } else {
      auto &aggr = std::get<1>(t.value);
      auto found = ctx.transformers.find(aggr.base);
      if (found == ctx.transformers.end()) {
        throw std::runtime_error("Transformer not found");
      }
      return found->second(ctx, type);
    }
  }

  /**
   * Get the type of a function.
   *
   * @param ctx The compilation context.
   * @param instType The instantiated type of the function, with no free type variables.
   * @return A pair of the function reference type and the raw function type.
   *         Note that the raw function takes as its first parameter the reference.
   */
  std::pair<llvm::PointerType *, llvm::FunctionType *>
  getFuncType(CompileContext &ctx, TPtr instType) {
    auto found = ctx.fTypeCache.find(instType);
    if (found != ctx.fTypeCache.end()) {
      return {found->second,
              static_cast<llvm::FunctionType *>(found
                  ->second
                  ->getPointerElementType()
                  ->getStructElementType(0)
                  ->getPointerElementType())};
    }
    CType::Aggregate &aggr = std::get<CType::Aggregate>(CType::get(instType)->value);
    llvm::Type *retType;
    std::stringstream ss;
    ss << *CType::get(instType);
    llvm::StructType *structType = llvm::StructType::create(ctx.ctx, ss.str());
    llvm::PointerType *pointerType = llvm::PointerType::getUnqual(structType);
    std::vector<llvm::Type *> argTypes(aggr.values.size());
    argTypes[0] = pointerType;
    for (size_t i = 0; i < aggr.values.size(); ++i) {
      if (i == aggr.values.size() - 1) {
        retType = toLLVM(ctx, aggr.values[i]);
      } else {
        argTypes[i + 1] = toLLVM(ctx, aggr.values[i]);
      }
    }
    llvm::FunctionType *funcType = llvm::FunctionType::get(retType, argTypes, false);
    structType->setBody(llvm::PointerType::getUnqual(funcType));
    ctx.fTypeCache[instType] = pointerType;
    return {pointerType, funcType};
  }

  void Program::inferTypes(ParseContext &ctx) {
    for (auto &stmt : statements) {
      stmt->inferStatement(ctx);
    }
  }
  void Program::compile(CompileContext &ctx) {
    auto mainType =
        llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx.ctx), {}, false);
    auto main =
        llvm::Function::Create(mainType, llvm::Function::ExternalLinkage, "main", &ctx.module);
    auto block = llvm::BasicBlock::Create(ctx.ctx, "entry", main);
    ctx.builder.SetInsertPoint(block);
    for (auto &stmt : statements) {
      stmt->compileStatement(ctx);
    }
    ctx.builder.CreateRet(llvm::ConstantInt::get(ctx.ctx, llvm::APInt(32, 0, true)));
  }

  TPtr PlaceholderType::get(ParseContext &ctx) const {
    return ctx.tc.fresh();
  }

  TPtr NamedType::get(ParseContext &ctx) const {
    std::shared_ptr<type::BaseType> &base = ctx.lookupType(raw.ident.value);
    if (parameters) {
      std::vector<TPtr> typeParams(parameters->types.size());
      size_t i = 0;
      for (const auto &param : parameters->types) {
        typeParams[i++] = param->get(ctx);
      }
      return ctx.tc.push(CType::aggregate(base, std::move(typeParams)));
    } else {
      return ctx.tc.push(CType::aggregate(base, {}));
    }
  }

  class BinaryTrait : public type::TraitImpl {
  public:
    using Fn = std::function<llvm::Value *(CompileContext &ctx, llvm::Value *lhs, llvm::Value *rhs)>;

    Fn app;
    BinaryTrait(Fn &&app) : app(app) {}
  };

  class CmpTrait : public type::TraitImpl {
  };

  ParseContext::ParseContext(type::TypeContext &tc) :
      tc(tc),
      funcType(std::make_shared<type::BaseType>("fn")),
      unitType(std::make_shared<type::BaseType>("unit")),
      intType(std::make_shared<type::BaseType>("int")),
      floatType(std::make_shared<type::BaseType>("float")),
      boolType(std::make_shared<type::BaseType>("bool")),
      stringType(std::make_shared<type::BaseType>("string")),

      addTrait(std::make_shared<type::Trait>("Add")),
      mulTrait(std::make_shared<type::Trait>("Mul")),
      subTrait(std::make_shared<type::Trait>("Sub")),
      divTrait(std::make_shared<type::Trait>("Div")),
      remTrait(std::make_shared<type::Trait>("Rem")),

      cmpTrait(std::make_shared<type::Trait>("Cmp")) {
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

  std::shared_ptr<type::BaseType> &ParseContext::lookupType(const std::string &name) {
    for (auto it = typeScopes.rbegin(); it != typeScopes.rend(); ++it) {
      auto found = it->bindings.find(name);
      if (found != it->bindings.end()) {
        return found->second;
      }
    }
    throw std::runtime_error("Undefined type");
  }

  CompileContext::CompileContext(llvm::LLVMContext &ctx,
                                 llvm::IRBuilder<> &builder,
                                 llvm::Module &module,
                                 ParseContext &pc) :
      pc(pc), ctx(ctx), builder(builder), module(module) {
    transformers[pc.funcType] = [](CompileContext &ctx, auto type) { return getFuncType(ctx, type).first; };
    unitType = llvm::StructType::get(ctx, {});
    transformers[pc.unitType] = [](CompileContext &ctx, auto _t) { return ctx.unitType; };
    unitValue = llvm::ConstantStruct::get(unitType, {});
    auto intType = llvm::Type::getInt64Ty(ctx);
    transformers[pc.intType] = [intType](auto _c, auto _t) { return intType; };
    auto floatType = llvm::Type::getDoubleTy(ctx);
    transformers[pc.floatType] = [floatType](auto _c, auto _t) { return floatType; };
    auto boolType = llvm::Type::getInt1Ty(ctx);
    transformers[pc.boolType] = [boolType](auto _c, auto _t) { return boolType; };
    // TODO transformers[pc.stringType] = [](auto _c, auto _t) {};
  }

  void Defn::inferStatement(ParseContext &ctx) {
    ctx.tc.bound.push_back(this->binding.inferType(ctx));
  }
  void Defn::compileStatement(CompileContext &ctx) {
    binding.compile(ctx);
  }

  void Expr::inferStatement(ParseContext &ctx) {
    inferExpr(ctx, Position::Statement);
  }
  TPtr Expr::inferExpr(ParseContext &ctx, Position pos) {
    return type = inferType(ctx, pos);
  }
  void Expr::compileStatement(CompileContext &ctx) {
    compileExpr(ctx, Position::Statement);
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
  llvm::Value *IfExpr::compileExpr(CompileContext &ctx, Position pos) {
    llvm::PHINode *phi;
    if (pos != Position::Statement && elseClause) {
      phi = llvm::PHINode::Create(toLLVM(ctx, type),
                                  2 + elseIfClauses.size(),
                                  "iftmp");
    } else {
      phi = nullptr;
    }

    auto *function = ctx.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(ctx.ctx, "ifthen");
    llvm::BasicBlock *elseBlock = llvm::BasicBlock::Create(ctx.ctx, "ifelse");
    llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(ctx.ctx, "ifcont");

    auto pred = predExpr->compileExpr(ctx, Position::Expr);
    ctx.builder.CreateCondBr(pred, thenBlock, elseBlock);

    function->getBasicBlockList().push_back(thenBlock);
    ctx.builder.SetInsertPoint(thenBlock);
    auto thenValue = thenExpr->compileExpr(ctx, pos);
    ctx.builder.CreateBr(mergeBlock);
    if (phi) phi->addIncoming(thenValue, ctx.builder.GetInsertBlock());

    for (auto &clause : elseIfClauses) {
      auto elseIfThenBlock = llvm::BasicBlock::Create(ctx.ctx, "ifthen");
      auto elseIfElseBlock = llvm::BasicBlock::Create(ctx.ctx, "ifelse");

      function->getBasicBlockList().push_back(elseBlock);
      ctx.builder.SetInsertPoint(elseBlock);

      auto elseIfPred = clause.predExpr->compileExpr(ctx, Position::Expr);
      ctx.builder.CreateCondBr(elseIfPred, elseIfThenBlock, elseIfElseBlock);

      function->getBasicBlockList().push_back(elseIfThenBlock);
      ctx.builder.SetInsertPoint(elseIfThenBlock);
      auto elseIfValue = clause.thenExpr->compileExpr(ctx, pos);
      ctx.builder.CreateBr(mergeBlock);
      if (phi) phi->addIncoming(elseIfValue, ctx.builder.GetInsertBlock());

      elseBlock = elseIfElseBlock;
    }

    function->getBasicBlockList().push_back(elseBlock);
    ctx.builder.SetInsertPoint(elseBlock);
    auto elseValue = elseClause->thenExpr->compileExpr(ctx, pos);
    ctx.builder.CreateBr(mergeBlock);
    if (phi) phi->addIncoming(elseValue, ctx.builder.GetInsertBlock());

    function->getBasicBlockList().push_back(mergeBlock);
    ctx.builder.SetInsertPoint(mergeBlock);
    if (phi) return ctx.builder.Insert(phi);
    return ctx.unitValue;
  }

  llvm::Value *heapAlloc(CompileContext &ctx, llvm::Type *type) {
    llvm::Type *i32Ty = llvm::Type::getInt32Ty(ctx.ctx);
    llvm::Constant *size = llvm::ConstantExpr::getSizeOf(type);
    size = llvm::ConstantExpr::getTruncOrBitCast(size, i32Ty);
    llvm::FunctionCallee malloc = ctx.module
        .getOrInsertFunction("malloc", llvm::Type::getInt8PtrTy(ctx.ctx), i32Ty);
    // TODO garbage collector :)
    return ctx.builder.CreatePointerCast(ctx.builder.CreateCall(malloc, {size}),
                                         llvm::PointerType::getUnqual(type));
  }

  TPtr maybeHinted(ParseContext &ctx, const std::optional<TypeHint> &hint) {
    return hint ? hint->type->get(ctx) : ctx.tc.fresh();
  }

  TPtr inferFuncType(ParseContext &ctx,
                     std::vector<RawBinding> &bindings,
                     const std::optional<TypeHint> &returnHint,
                     Identifier *name,
                     std::shared_ptr<Var> *recurVar,
                     std::set<std::shared_ptr<Var>> &closed,
                     std::unique_ptr<Expr> &value) {
    TPtr inferred;
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
    return inferred;
  }
  llvm::Value *compileFunc(CompileContext &ctx,
                           TPtr type,
                           std::vector<RawBinding> &bindings,
                           Var *recurVar,
                           const std::string &name,
                           const std::set<std::shared_ptr<Var>> &closedVars,
                           Expr &value) {
    llvm::FunctionType *funcType;
    llvm::PointerType *refType;
    std::tie(refType, funcType) = getFuncType(ctx, type);
    auto func = llvm::Function::Create(funcType,
                                       llvm::Function::ExternalLinkage,
                                       name,
                                       &ctx.module);
    llvm::Argument *thisArg = func->getArg(0);
    thisArg->setName(name);
    if (recurVar) {
      recurVar->emit = [thisArg](CompileContext &, TPtr) {
        return thisArg;
      };
    }
    for (size_t arg = 0; arg < bindings.size(); ++arg) {
      llvm::Argument *argV = func->getArg(arg + 1);
      argV->setName(bindings[arg].name.ident.value);
      bindings[arg].var->emit = [argV](CompileContext &, TPtr) {
        return argV;
      };
    }

    llvm::BasicBlock *emitBlock = ctx.builder.GetInsertBlock();
    llvm::BasicBlock::iterator emitPoint = ctx.builder.GetInsertPoint();

    auto block = llvm::BasicBlock::Create(ctx.ctx, "entry", func);
    ctx.builder.SetInsertPoint(block);

    std::vector<llvm::Value *> closureEmitted;
    std::vector<std::function<llvm::Value *(CompileContext &, TPtr)>>
        oldEmissions(closedVars.size(), [](auto, auto) { return nullptr; });
    llvm::StructType *closureStructType;
    llvm::PointerType *closurePointerType;
    std::vector<llvm::Type *> closureTypes;

    if (!closedVars.empty()) {
      closureTypes = {llvm::PointerType::getUnqual(funcType)};
      closureStructType = llvm::StructType::create(ctx.ctx, name + ".clo");
      closureStructType->setBody(closureTypes);
      closurePointerType = llvm::PointerType::getUnqual(closureStructType);
      llvm::Value *closurePtr = ctx.builder.CreatePointerCast(thisArg, closurePointerType);

      std::map<llvm::Value *, llvm::Value *> cache;
      size_t i = 0;
      for (auto &cv : closedVars) {
        auto &oldEmit = oldEmissions[i++] = std::move(cv->emit);
        cv->emit = [closurePtr, closureStructType, &closureTypes, oldEmit, &closureEmitted, &cache, emitBlock, emitPoint]
            (CompileContext &ctx, TPtr type) -> llvm::Value * {
          llvm::BasicBlock *oldBlock = ctx.builder.GetInsertBlock();
          llvm::BasicBlock::iterator oldPoint = ctx.builder.GetInsertPoint();
          ctx.builder.SetInsertPoint(emitBlock, emitPoint);
          llvm::Value *emitted = oldEmit(ctx, type);
          ctx.builder.SetInsertPoint(oldBlock, oldPoint);
          if (llvm::isa<llvm::Constant>(emitted)) {
            return emitted;
          }
          auto found = cache.find(emitted);
          if (found != cache.end()) {
            return found->second;
          }
          closureEmitted.push_back(emitted);

          closureTypes.push_back(emitted->getType());
          closureStructType->setBody(closureTypes);
          unsigned int idx = closureTypes.size() - 1;
          llvm::IntegerType *i32Ty = llvm::Type::getInt32Ty(ctx.ctx);
          llvm::Value *varRef =
              ctx.builder.CreateGEP(closurePtr,
                                    {llvm::ConstantInt::get(i32Ty, 0),
                                     llvm::ConstantInt::get(i32Ty, idx)});
          llvm::LoadInst *load = ctx.builder.CreateLoad(varRef);
          return cache[emitted] = load;
        };
      }
    }

    ctx.builder.CreateRet(value.compileExpr(ctx, Position::Tail));

    ctx.builder.SetInsertPoint(emitBlock, emitPoint);

    {
      size_t i = 0;
      for (auto &cv : closedVars) {
        cv->emit = std::move(oldEmissions[i]);
      }
    }

    llvm::StructType *structType = static_cast<llvm::StructType *>(refType->getPointerElementType());
    if (closureEmitted.empty()) {
      llvm::Constant *constant = llvm::ConstantStruct::get(structType, {func});
      llvm::GlobalVariable *global = new llvm::GlobalVariable(ctx.module,
                                                              structType,
                                                              true,
                                                              llvm::GlobalValue::ExternalLinkage,
                                                              constant,
                                                              name);
      return global;
    } else {
      llvm::Value *allocated = heapAlloc(ctx, closureStructType);
      llvm::IntegerType *i32Ty = llvm::Type::getInt32Ty(ctx.ctx);
      llvm::Value *thisGep = ctx.builder.CreateInBoundsGEP(closureStructType, allocated,
                                                           {llvm::ConstantInt::get(i32Ty, 0),
                                                            llvm::ConstantInt::get(i32Ty, 0)});
      ctx.builder.CreateStore(func, thisGep);
      for (unsigned int i = 0; i < closureEmitted.size(); ++i) {
        llvm::Value *gep = ctx.builder.CreateInBoundsGEP(closureStructType, allocated,
                                                         {llvm::ConstantInt::get(i32Ty, 0),
                                                          llvm::ConstantInt::get(i32Ty, i + 1)});
        ctx.builder.CreateStore(closureEmitted[i], gep);
      }
      return ctx.builder.CreatePointerCast(allocated, llvm::PointerType::getUnqual(structType));
    }
  }
  llvm::Value *compileCall(CompileContext &ctx,
                           TPtr functionType,
                           llvm::Value *function,
                           const std::vector<llvm::Value *> &callArgs) {
    llvm::FunctionType *funcType;
    llvm::PointerType *refType;
    std::tie(refType, funcType) = getFuncType(ctx, functionType);
    std::vector<llvm::Value *> trueArgs = {function};
    trueArgs.insert(trueArgs.begin() + 1, callArgs.begin(), callArgs.end());
    llvm::IntegerType *i32Ty = llvm::Type::getInt32Ty(ctx.ctx);
    llvm::Value *funcPtr
        = ctx.builder.CreateInBoundsGEP(function,
                                        {llvm::ConstantInt::get(i32Ty, 0),
                                         llvm::ConstantInt::get(i32Ty, 0)});
    llvm::Value *funcValuePtr = ctx.builder.CreateLoad(funcPtr, "f");
    return ctx.builder.CreateCall(funcType, funcValuePtr, trueArgs);
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
  llvm::Value *LetExpr::compileExpr(CompileContext &ctx, Position pos) {
    if (name) {
      std::vector<RawBinding> rbs(bindings.size());
      for (int i = 0; i < bindings.size(); ++i) {
        rbs[i].var = bindings[i].var;
        rbs[i].name = bindings[i].name;
      }
      llvm::Value *func = compileFunc(ctx, nameVar->type->type, rbs, nameVar.get(), name->ident.value, closed, *body);
      std::vector<llvm::Value *> args(bindings.size());
      for (int i = 0; i < bindings.size(); ++i) {
        std::shared_ptr<Var> &var = bindings[i].var;
        bindings[i].compile(ctx);
        args[i] = var->emit(ctx, var->type->type);
      }
      return compileCall(ctx, nameVar->type->type, func, args);
    } else {
      for (auto &b : bindings) {
        b.compile(ctx);
      }
      return body->compileExpr(ctx, pos);
    }
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
  llvm::Value *BlockExpr::compileExpr(CompileContext &ctx, Position pos) {
    if (value) {
      for (const auto &stmt : statements) {
        stmt.statement->compileStatement(ctx);
      }
      return value->compileExpr(ctx, pos);
    } else {
      return ctx.unitValue;
    }
  }

  TPtr BracketExpr::inferType(ParseContext &ctx, Position pos) {
    return value->inferExpr(ctx, pos);
  }
  llvm::Value *BracketExpr::compileExpr(CompileContext &ctx, Position pos) {
    return value->compileExpr(ctx, pos);
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
  llvm::Value *LiteralExpr::compileExpr(CompileContext &ctx, Position pos) {
    switch (value.type) {
      case Tok::TStr:
        // TODO
        throw std::runtime_error("Strings not yet implemented");
      case Tok::TInt:
        return llvm::ConstantInt::get(toLLVM(ctx, type), llvm::APInt(64, value.value, 10));
      case Tok::TFloat:
        return llvm::ConstantFP::get(ctx.ctx, llvm::APFloat(llvm::APFloat::IEEEdouble(), value.value));
      case Tok::TTrue:
        return llvm::ConstantInt::get(toLLVM(ctx, type), llvm::APInt(1, 1, false));
      default: // Tok::TFalse:
        return llvm::ConstantInt::get(toLLVM(ctx, type), llvm::APInt(1, 0, false));
    }
  }

  Var::Var(PType &&type) : type(std::make_shared<PType>(type)) {}

  TPtr VarExpr::inferType(ParseContext &ctx, Position pos) {
    var = ctx.lookup(name.ident.value);
    return ctx.tc.inst(*var->type);
  }
  llvm::Value *VarExpr::compileExpr(CompileContext &ctx, Position pos) {
    return var->emit(ctx, type);
  }

  TPtr BinaryExpr::inferType(ParseContext &ctx, Position pos) {
    TPtr argType = lhs->inferExpr(ctx, Position::Expr);
    for (auto &rhs : terms) {
      CType::getAndUnify(ctx.tc, argType, rhs.expr->inferExpr(ctx, Position::Expr));
    }
    switch (terms[0].operatorToken.type) {
      case Tok::TOr1:
      case Tok::TAnd1:
      case Tok::TShLeft:
      case Tok::TShRight2:
      case Tok::TShRight3: {
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
  llvm::Value *BinaryExpr::compileExpr(CompileContext &ctx, Position pos) {
    llvm::Value *lhsV = lhs->compileExpr(ctx, Position::Expr);
    bool isInt = lhsV->getType()->isIntegerTy();
    switch (precedence) {
      case 0: {
        // Tok::TAnd2, Tok::TOr2
        for (const auto &rhs : terms) {
          llvm::Value *rhsV = rhs.expr->compileExpr(ctx, Position::Expr);
          if (rhs.operatorToken.type == Tok::TAnd2) {
            lhsV = ctx.builder.CreateAnd(lhsV, rhsV);
          } else {
            lhsV = ctx.builder.CreateOr(lhsV, rhsV);
          }
        }
        break;
      }
      case 1:
      case 2: {
        // Tok::TNe, Tok::TEq, Tok::TEq2
        // Tok::TLt, Tok::TLe, Tok::TGt, Tok::TGe
        for (const auto &rhs : terms) {
          llvm::Value *rhsV = rhs.expr->compileExpr(ctx, Position::Expr);
          llvm::CmpInst::Predicate pred;
          switch (rhs.operatorToken.type) {
            case Tok::TNe:
              pred = isInt ? llvm::CmpInst::ICMP_NE : llvm::CmpInst::FCMP_ONE;
              break;
            case Tok::TEq:
            case Tok::TEq2:
              pred = isInt ? llvm::CmpInst::ICMP_EQ : llvm::CmpInst::FCMP_OEQ;
              break;
            case Tok::TLt:
              pred = isInt ? llvm::CmpInst::ICMP_SLT : llvm::CmpInst::FCMP_OLT;
              break;
            case Tok::TLe:
              pred = isInt ? llvm::CmpInst::ICMP_SLE : llvm::CmpInst::FCMP_OLE;
              break;
            case Tok::TGt:
              pred = isInt ? llvm::CmpInst::ICMP_SGT : llvm::CmpInst::FCMP_OGT;
              break;
            default /*Tok::TGe*/:
              pred = isInt ? llvm::CmpInst::ICMP_SGE : llvm::CmpInst::FCMP_OGE;
              break;
          }
          lhsV = isInt ?
                 ctx.builder.CreateICmp(pred, lhsV, rhsV) :
                 ctx.builder.CreateFCmp(pred, lhsV, rhsV);
        }
        break;
      }
      default: {
        // Tok::TOr1, Tok::TAnd1
        // Tok::TShLeft, Tok::TShRight2, Tok::TShRight3
        // Tok::TAdd, Tok::TSub
        // Tok::TMul, Tok::TDiv, Tok::TRem
        for (const auto &rhs : terms) {
          llvm::Value *rhsV = rhs.expr->compileExpr(ctx, Position::Expr);
          llvm::Instruction::BinaryOps opc;
          switch (rhs.operatorToken.type) {
            case Tok::TOr1:
              opc = llvm::Instruction::Or;
              break;
            case Tok::TAnd1:
              opc = llvm::Instruction::And;
              break;
            case Tok::TShLeft:
              opc = llvm::Instruction::Shl;
              break;
            case Tok::TShRight2:
              opc = llvm::Instruction::AShr;
              break;
            case Tok::TShRight3:
              opc = llvm::Instruction::LShr;
              break;
            case Tok::TAdd:
              opc = isInt ? llvm::Instruction::Add : llvm::Instruction::FAdd;
              break;
            case Tok::TSub:
              opc = isInt ? llvm::Instruction::Sub : llvm::Instruction::FSub;
              break;
            case Tok::TMul:
              opc = isInt ? llvm::Instruction::Mul : llvm::Instruction::FMul;
              break;
            case Tok::TDiv:
              opc = isInt ? llvm::Instruction::SDiv : llvm::Instruction::FDiv;
              break;
            default: // Tok::TRem
              opc = isInt ? llvm::Instruction::SRem : llvm::Instruction::FRem;
              break;
          }
          lhsV = ctx.builder.CreateBinOp(opc, lhsV, rhsV);
        }
        break;
      }
    }
    return lhsV;
  }

  TPtr PrefixExpr::inferType(ParseContext &ctx, Position pos) {
    return expr->inferExpr(ctx, Position::Expr);
  }
  llvm::Value *PrefixExpr::compileExpr(CompileContext &ctx, Position pos) {
    llvm::Value *value = expr->compileExpr(ctx, Position::Expr);
    bool isInt = value->getType()->isIntegerTy();
    for (const auto &prefix : prefixes) {
      if (prefix.type == Tok::TSub) {
        value = isInt ? ctx.builder.CreateNeg(value) : ctx.builder.CreateFNeg(value);
      }
    }
    return value;
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
  llvm::Value *FunCallExpr::compileExpr(CompileContext &ctx, Position pos) {
    llvm::Value *func = function->compileExpr(ctx, Position::Expr);
    std::vector<llvm::Value *> args(arguments.size());
    for (int i = 0; i < arguments.size(); ++i) {
      args[i] = arguments[i]->compileExpr(ctx, Position::Expr);
    }
    return compileCall(ctx, function->type, func, args);
  }

  TPtr HintedExpr::inferType(ParseContext &ctx, Position pos) {
    TPtr inferred = expr->inferExpr(ctx, pos);
    CType::getAndUnify(ctx.tc, inferred, hint.type->get(ctx));
    return inferred;
  }
  llvm::Value *HintedExpr::compileExpr(CompileContext &ctx, Position pos) {
    return expr->compileExpr(ctx, pos);
  }

  TPtr FnExpr::inferType(ParseContext &ctx, Position pos) {
    return inferFuncType(ctx, arguments.bindings, typeHint, name ? &*name : nullptr, &recurVar, closed, body);
  }
  llvm::Value *FnExpr::compileExpr(CompileContext &ctx, Position pos) {
    return compileFunc(ctx, type, arguments.bindings, recurVar.get(), name ? name->ident.value : "", closed, *body);
  }

  TPtr LambdaExpr::inferType(ParseContext &ctx, Position pos) {
    return inferFuncType(ctx, arguments, std::nullopt, nullptr, nullptr, closed, body);
  }
  llvm::Value *LambdaExpr::compileExpr(CompileContext &ctx, Position pos) {
    return compileFunc(ctx, type, arguments, nullptr, "", closed, *body);
  }

  std::shared_ptr<PType> &Binding::inferType(ParseContext &ctx) {
    TPtr inferred;
    if (value) {
      if (arguments) {
        inferred = inferFuncType(ctx,
                                 arguments->bindings,
                                 typeHint,
                                 &name,
                                 &arguments->recurVar,
                                 arguments->closed,
                                 value);
      } else {
        inferred = value->inferExpr(ctx, Position::Expr);
        if (typeHint) {
          CType::getAndUnify(ctx.tc, inferred, typeHint->type->get(ctx));
        }
      }
      var = ctx.introduce(name.ident.value, ctx.tc.gen(inferred));
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
      // don't generalise FFI types
      var = ctx.introduce(name.ident.value, inferred);
    }
    return var->type;
  }

  void Binding::compile(CompileContext &ctx) {
    if (foreignToken) {
      llvm::Constant *global;
      if (arguments) {
        llvm::PointerType *refType;
        llvm::FunctionType *invokerFuncType;
        std::tie(refType, invokerFuncType) = getFuncType(ctx, var->type->type);

        std::vector<llvm::Type *> types(invokerFuncType->getNumParams() - 1);
        for (int i = 1; i < invokerFuncType->getNumParams(); ++i) {
          types[i - 1] = invokerFuncType->getParamType(i);
        }
        llvm::FunctionType *funcType = llvm::FunctionType::get(invokerFuncType->getReturnType(), types, false);
        auto func = ctx.module.getOrInsertFunction(name.ident.value, funcType);

        llvm::StructType *structType = static_cast<llvm::StructType *>(refType->getPointerElementType());

        auto invoker = llvm::Function::Create(invokerFuncType,
                                              llvm::Function::ExternalLinkage,
                                              name.ident.value + ".invoker",
                                              &ctx.module);

        llvm::BasicBlock *oldBlock = ctx.builder.GetInsertBlock();
        llvm::BasicBlock::iterator oldPoint = ctx.builder.GetInsertPoint();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(ctx.ctx, "entry", invoker);
        ctx.builder.SetInsertPoint(entry);
        std::vector<llvm::Value *> args(invoker->arg_size() - 1);
        for (int i = 1; i < invoker->arg_size(); ++i) {
          args[i - 1] = invoker->getArg(i);
        }
        ctx.builder.CreateRet(ctx.builder.CreateCall(func, args));
        ctx.builder.SetInsertPoint(oldBlock, oldPoint);

        llvm::Constant *constant = llvm::ConstantStruct::get(structType, {invoker});

        global = new llvm::GlobalVariable(ctx.module,
                                          structType,
                                          true,
                                          llvm::GlobalValue::ExternalLinkage,
                                          constant);
      } else {
        global = ctx.module.getOrInsertGlobal(name.ident.value, toLLVM(ctx, var->type->type));
      }
      var->emit = [global](CompileContext &, TPtr) {
        return global;
      };
      return;
    }

    llvm::Function *function = ctx.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *instsBlock = llvm::BasicBlock::Create(ctx.ctx, "insts");
    llvm::BasicBlock *postinstsBlock = llvm::BasicBlock::Create(ctx.ctx, "postinsts");
    ctx.builder.CreateBr(instsBlock);
    function->getBasicBlockList().push_back(instsBlock);
    ctx.builder.SetInsertPoint(instsBlock);
    function->getBasicBlockList().push_back(postinstsBlock);
    llvm::BranchInst *jump = ctx.builder.CreateBr(postinstsBlock);
    ctx.builder.SetInsertPoint(postinstsBlock);
    var->emit = [this, jump](CompileContext &ctx, TPtr type) -> llvm::Value * {

      std::map<TPtr, TPtr> subs;
      TPtr thisType = CType::get(this->var->type->type);
      CType::getFree(thisType, [&subs, &ctx](const TPtr &t) { subs[t] = ctx.pc.tc.fresh(); });
      TPtr instType = CType::replace(thisType, ctx.pc.tc, subs);
      CType::getAndUnify(ctx.pc.tc, type, instType);
      TPtr unitType = ctx.pc.tc.push(CType::aggregate(ctx.pc.unitType, {}));
      for (const auto &sub : subs) {
        TPtr target = CType::get(sub.second);
        if (target->value.index() == 0) {
          // unbounded type
          CType::unify(ctx.pc.tc, target, unitType);
        }
        std::get<0>(sub.first->value).weakParent = target;
      }

      auto found = insts.find(instType);
      if (found != insts.end()) {
        return found->second;
      }

      llvm::BasicBlock *oldBlock = ctx.builder.GetInsertBlock();
      llvm::BasicBlock::iterator oldPoint = ctx.builder.GetInsertPoint();

      ctx.builder.SetInsertPoint(jump);
      llvm::Value *ret = compileExpr(ctx, instType);
      ctx.builder.SetInsertPoint(oldBlock, oldPoint);

      ret->setName(name.ident.value);

      for (const auto &sub : subs) {
        std::get<0>(sub.first->value).weakParent = nullptr;
      }

      return insts[type] = ret;
    };
  }
  llvm::Value *Binding::compileExpr(CompileContext &ctx, TPtr instType) {
    if (arguments) {
      return compileFunc(ctx, instType, arguments->bindings, arguments->recurVar.get(), name.ident.value,
                         arguments->closed, *value);
    } else {
      return value->compileExpr(ctx, Position::Expr);
    }
  }
}
