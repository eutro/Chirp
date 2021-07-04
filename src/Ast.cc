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
  llvm::Type *toLLVM(CompileContext &ctx, CType *type) {
    CType &t = type->get();
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
  getFuncType(CompileContext &ctx, CType *instType) {
    auto found = ctx.fTypeCache.find(instType);
    if (found != ctx.fTypeCache.end()) {
      return {found->second,
              static_cast<llvm::FunctionType *>(found
                  ->second
                  ->getPointerElementType()
                  ->getStructElementType(0)
                  ->getPointerElementType())};
    }
    CType::Aggregate &aggr = std::get<CType::Aggregate>(instType->get().value);
    llvm::Type *retType;
    std::stringstream ss;
    ss << instType->get();
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
        std::shared_ptr<Var> &var = found->second;
        while (it != scopes.rbegin()) {
          (--it)->referenced.insert(var);
        }
        return var;
      }
    }
    throw std::runtime_error("Undefined");
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
  CType *Expr::inferExpr(ParseContext &ctx, Position pos) {
    return type = inferType(ctx, pos);
  }
  void Expr::compileStatement(CompileContext &ctx) {
    compileExpr(ctx, Position::Statement);
  }

  CType *IfExpr::inferType(ParseContext &ctx, Position pos) {
    bool isStatement = pos == Position::Statement || !elseClause;

    CType *boolType = ctx.tc.push(CType::aggregate(ctx.boolType, {}));
    boolType->unify(*predExpr->inferExpr(ctx, Position::Expr));
    CType *thenType = thenExpr->inferExpr(ctx, pos);
    CType *type = isStatement ? ctx.tc.push(CType::aggregate(ctx.unitType, {})) : thenType;
    for (auto &clause : elseIfClauses) {
      boolType->unify(*clause.predExpr->inferExpr(ctx, Position::Expr));
      CType *elseIfType = clause.thenExpr->inferExpr(ctx, pos);
      if (!isStatement) {
        type->get().unify(elseIfType->get());
      }
    }
    CType *elseType = elseClause->thenExpr->inferExpr(ctx, pos);
    if (!isStatement) {
      type->get().unify(elseType->get());
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

  CType *inferFuncType(ParseContext &ctx,
                       std::vector<RawBinding> &bindings,
                       Identifier *name,
                       std::shared_ptr<Var> *recurVar,
                       std::set<std::shared_ptr<Var>> &closed,
                       std::unique_ptr<Expr> &value) {
    CType *inferred;
    ctx.scopes.emplace_back();
    size_t oldSize = ctx.tc.bound.size();
    std::vector<CType *> params(bindings.size() + 1, nullptr);
    size_t i = 0;
    for (auto &rb : bindings) {
      rb.var = ctx.introduce(rb.name.ident.value, params[i++] = ctx.tc.fresh());
      ctx.tc.bound.push_back(rb.var->type);
    }
    CType *retType = params[i] = ctx.tc.fresh();
    inferred = ctx.tc.push(CType::aggregate(ctx.funcType, std::move(params)));
    if (name) {
      *recurVar = ctx.introduce(name->ident.value, inferred);
    }
    value->inferExpr(ctx, Position::Expr)->get().unify(retType->get());
    ctx.tc.bound.resize(oldSize);
    closed = std::move(ctx.scopes.back().referenced);
    ctx.scopes.pop_back();
    return inferred;
  }
  llvm::Value *compileFunc(CompileContext &ctx,
                           CType *type,
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
      recurVar->emit = [thisArg](ast::CompileContext &, ast::CType *) {
        return thisArg;
      };
    }
    for (size_t arg = 0; arg < bindings.size(); ++arg) {
      llvm::Argument *argV = func->getArg(arg + 1);
      argV->setName(bindings[arg].name.ident.value);
      bindings[arg].var->emit = [argV](ast::CompileContext &, ast::CType *) {
        return argV;
      };
    }

    llvm::BasicBlock *emitBlock = ctx.builder.GetInsertBlock();
    llvm::BasicBlock::iterator emitPoint = ctx.builder.GetInsertPoint();

    auto block = llvm::BasicBlock::Create(ctx.ctx, "entry", func);
    ctx.builder.SetInsertPoint(block);

    std::vector<llvm::Value *> closureEmitted;
    std::vector<std::function<llvm::Value *(CompileContext &, CType *)>>
        oldEmissions(closedVars.size(), [](auto, auto) { return nullptr; });
    llvm::StructType *closureStructType;
    llvm::PointerType *closurePointerType;
    std::vector<llvm::Type *> closureTypes;

    if (!closedVars.empty()) {
      closureTypes = {llvm::PointerType::getUnqual(funcType)};
      std::stringstream ss;
      closureStructType = llvm::StructType::create(ctx.ctx, name + ".clo");
      closureStructType->setBody(closureTypes);
      closurePointerType = llvm::PointerType::getUnqual(closureStructType);
      llvm::Value *closurePtr = ctx.builder.CreatePointerCast(thisArg, closurePointerType);

      std::map<llvm::Value *, llvm::Value *> cache;
      size_t i = 0;
      for (auto &cv : closedVars) {
        auto &oldEmit = oldEmissions[i++] = std::move(cv->emit);
        cv->emit = [closurePtr, closureStructType, &closureTypes, oldEmit, &closureEmitted, &cache, emitBlock, emitPoint]
            (ast::CompileContext &ctx, ast::CType *type) -> llvm::Value * {
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
      llvm::Constant *size = llvm::ConstantExpr::getSizeOf(closureStructType);
      llvm::IntegerType *i32Ty = llvm::Type::getInt32Ty(ctx.ctx);
      size = llvm::ConstantExpr::getTruncOrBitCast(size, i32Ty);
      llvm::FunctionCallee malloc = ctx.module
          .getOrInsertFunction("malloc", llvm::Type::getInt8PtrTy(ctx.ctx), i32Ty);
      llvm::Value *allocated = // TODO garbage collector :)
          ctx.builder.CreateBitOrPointerCast(ctx.builder.CreateCall(malloc, {size}),
                                             closurePointerType, "closure");
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
                           CType *functionType,
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

  CType *LetExpr::inferType(ParseContext &ctx, Position pos) {
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
      ctx.scopes.emplace_back();
      nameVar = ctx.introduce(name->ident.value, funcType);
      body->inferExpr(ctx, pos)->get().unify(type->get());
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

  CType *BlockExpr::inferType(ParseContext &ctx, Position pos) {
    size_t oldSize = ctx.tc.bound.size();
    ctx.scopes.emplace_back();
    for (auto &stmt : statements) {
      stmt.statement->inferStatement(ctx);
    }
    CType *type = value->inferExpr(ctx, pos);
    ctx.scopes.pop_back();
    ctx.tc.bound.resize(oldSize);
    return type;
  }
  llvm::Value *BlockExpr::compileExpr(CompileContext &ctx, Position pos) {
    for (const auto &stmt : statements) {
      stmt.statement->compileStatement(ctx);
    }
    return value->compileExpr(ctx, pos);
  }

  CType *BracketExpr::inferType(ParseContext &ctx, Position pos) {
    return value->inferExpr(ctx, pos);
  }
  llvm::Value *BracketExpr::compileExpr(CompileContext &ctx, Position pos) {
    return value->compileExpr(ctx, pos);
  }

  CType *LiteralExpr::inferType(ParseContext &ctx, Position pos) {
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

  CType *VarExpr::inferType(ParseContext &ctx, Position pos) {
    var = ctx.lookup(name.ident.value);
    return ctx.tc.inst(*var->type);
  }
  llvm::Value *VarExpr::compileExpr(CompileContext &ctx, Position pos) {
    return var->emit(ctx, type);
  }

  CType *BinaryExpr::inferType(ParseContext &ctx, Position pos) {
    CType *argType = lhs->inferExpr(ctx, Position::Expr);
    for (auto &rhs : terms) {
      argType->get().unify(rhs.expr->inferExpr(ctx, Position::Expr)->get());
    }
    switch (terms[0].operatorToken.type) {
      case Tok::TOr1:
      case Tok::TAnd1:
      case Tok::TShLeft:
      case Tok::TShRight2:
      case Tok::TShRight3: {
        CType *intType = ctx.tc.push(CType::aggregate(ctx.intType, {}));
        argType->get().unify(*intType);
        return intType;
      }
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
          lhsV = ctx.builder.CreateCmp(pred, lhsV, rhsV);
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

  CType *PrefixExpr::inferType(ParseContext &ctx, Position pos) {
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

  CType *FunCallExpr::inferType(ParseContext &ctx, Position pos) {
    CType *funcType = function->inferExpr(ctx, Position::Expr);
    std::vector<CType *> argTypes(arguments.size() + 1, nullptr);
    size_t i = 0;
    for (auto &arg : arguments) {
      argTypes[i++] = arg->inferExpr(ctx, Position::Expr);
    }
    CType *type = ctx.tc.fresh();
    argTypes[i] = type;
    funcType->get().unify(*ctx.tc.push(CType::aggregate(ctx.funcType, std::move(argTypes))));
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

  CType *HintedExpr::inferType(ParseContext &ctx, Position pos) {
    // TODO type hints
    return expr->inferExpr(ctx, pos);
  }
  llvm::Value *HintedExpr::compileExpr(CompileContext &ctx, Position pos) {
    return expr->compileExpr(ctx, pos);
  }

  CType *FnExpr::inferType(ParseContext &ctx, Position pos) {
    return inferFuncType(ctx, arguments.bindings, name ? &*name : nullptr, &recurVar, closed, body);
  }
  llvm::Value *FnExpr::compileExpr(CompileContext &ctx, Position pos) {
    return compileFunc(ctx, type, arguments.bindings, recurVar.get(), name ? name->ident.value : "", closed, *body);
  }

  CType *LambdaExpr::inferType(ParseContext &ctx, Position pos) {
    return inferFuncType(ctx, arguments, nullptr, nullptr, closed, body);
  }
  llvm::Value *LambdaExpr::compileExpr(CompileContext &ctx, Position pos) {
    return compileFunc(ctx, type, arguments, nullptr, "", closed, *body);
  }

  std::shared_ptr<PType> &Binding::inferType(ParseContext &ctx) {
    CType *inferred;
    if (arguments) {
      inferred = inferFuncType(ctx, arguments->bindings, &name, &arguments->recurVar, arguments->closed, value);
    } else {
      inferred = value->inferExpr(ctx, Position::Expr);
    }
    var = ctx.introduce(name.ident.value, ctx.tc.gen(inferred));
    return var->type;
  }

  void Binding::compile(CompileContext &ctx) {
    llvm::Function *function = ctx.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *instsBlock = llvm::BasicBlock::Create(ctx.ctx, "insts");
    llvm::BasicBlock *postinstsBlock = llvm::BasicBlock::Create(ctx.ctx, "postinsts");
    ctx.builder.CreateBr(instsBlock);
    function->getBasicBlockList().push_back(instsBlock);
    ctx.builder.SetInsertPoint(instsBlock);
    function->getBasicBlockList().push_back(postinstsBlock);
    llvm::BranchInst *jump = ctx.builder.CreateBr(postinstsBlock);
    ctx.builder.SetInsertPoint(postinstsBlock);
    var->emit = [this, jump](CompileContext &ctx, CType *type) -> llvm::Value * {

      std::map<CType *, CType *> subs;
      type::Type &thisType = this->var->type->type->get();
      thisType.getFree([&subs, &ctx](CType *t) { subs[t] = ctx.pc.tc.fresh(); });
      type::Type *instType = thisType.replace(ctx.pc.tc, subs);
      type->get().unify(instType->get());
      type::Type *unitType = ctx.pc.tc.push(CType::aggregate(ctx.pc.unitType, {}));
      for (const auto &sub : subs) {
        type::Type &target = sub.second->get();
        if (target.value.index() == 0) {
          // unbounded type
          target.unify(*unitType);
        }
        std::get<0>(sub.first->value).weakParent = &target;
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
  llvm::Value *Binding::compileExpr(CompileContext &ctx, CType *instType) {
    if (arguments) {
      return compileFunc(ctx, instType, arguments->bindings, arguments->recurVar.get(), name.ident.value,
                         arguments->closed, *value);
    } else {
      return value->compileExpr(ctx, Position::Expr);
    }
  }
}
