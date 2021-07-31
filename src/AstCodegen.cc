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
      if (!std::get<0>(t.value).traits.empty()) {
        throw std::runtime_error("Non-reified type with trait bounds");
      }
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

  llvm::DIType *toDiType(CompileContext &ctx, TPtr type) {
    CType &t = *CType::get(type);
    if (t.value.index() == 0) {
      return ctx.diBuilder.createUnspecifiedType("unit");
    } else {
      auto &aggr = std::get<1>(t.value);
      auto found = ctx.diTransformers.find(aggr.base);
      if (found == ctx.diTransformers.end()) {
        std::stringstream ss;
        ss << t;
        return ctx.diBuilder.createUnspecifiedType(ss.str());
      }
      return found->second(ctx, type);
    }
  }

  llvm::DISubroutineType *toDISRType(CompileContext &ctx, TPtr type) {
    CType::Aggregate &aggr = std::get<1>(CType::get(type)->value);
    std::vector<llvm::Metadata *> argTypes(aggr.values.size());
    for (size_t i = 0; i < aggr.values.size() - 1; ++i) {
      argTypes[i + 1] = toDiType(ctx, aggr.values[i]);
    }
    argTypes.front() = toDiType(ctx, aggr.values.back());
    return ctx.diBuilder.createSubroutineType(ctx.diBuilder.getOrCreateTypeArray(argTypes));
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
    const TPtr &gotten = type::Type::get(instType);
    auto found = ctx.fTypeCache.find(gotten);
    if (found != ctx.fTypeCache.end()) {
      return {found->second,
              llvm::cast<llvm::FunctionType>(found
                                                 ->second
                                                 ->getPointerElementType()
                                                 ->getStructElementType(1)
                                                 ->getPointerElementType())};
    }
    CType::Aggregate &aggr = std::get<CType::Aggregate>(CType::get(instType)->value);
    llvm::Type *retType;
    std::stringstream ss;
    ss << *CType::get(instType);
    llvm::StructType *structType = llvm::StructType::create(ctx.ctx, ss.str());
    llvm::PointerType *pointerType = llvm::PointerType::getUnqual(structType);
    llvm::SmallVector<llvm::Type *, 8> argTypes(aggr.values.size());
    argTypes[0] = pointerType;
    for (size_t i = 0; i < aggr.values.size(); ++i) {
      if (i == aggr.values.size() - 1) {
        retType = toLLVM(ctx, aggr.values[i]);
      } else {
        argTypes[i + 1] = toLLVM(ctx, aggr.values[i]);
      }
    }
    llvm::FunctionType *funcType = llvm::FunctionType::get(retType, argTypes, false);
    structType->setBody(llvm::PointerType::getUnqual(ctx.metaFnType),
                        llvm::PointerType::getUnqual(funcType));
    ctx.fTypeCache[type::Type::copy(gotten)] = pointerType;
    return {pointerType, funcType};
  }

  void Program::compile(CompileContext &ctx) {
    llvm::IntegerType *i32Ty = llvm::Type::getInt32Ty(ctx.ctx);

    auto voidThunkTy = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx.ctx), {}, false);
    auto crpMain = llvm::Function::Create(voidThunkTy, llvm::Function::ExternalLinkage, "crpMain", &ctx.module);
    llvm::DISubprogram *sp = ctx.diBuilder.createFunction(
        ctx.diCU->getFile(), "crpMain", "crpMain", ctx.diCU->getFile(),
        1, ctx.diBuilder.createSubroutineType(ctx.diBuilder.getOrCreateTypeArray(
            {ctx.diBuilder.createUnspecifiedType("unit")})),
        1,
        llvm::DINode::FlagPublic,
        llvm::DISubprogram::SPFlagDefinition
    );
    crpMain->setSubprogram(sp);
    crpMain->setGC("shadow-stack");
    {
      auto block = llvm::BasicBlock::Create(ctx.ctx, "entry", crpMain);
      ctx.builder.SetInsertPoint(block);
      ctx.builder.SetCurrentDebugLocation(llvm::DebugLoc());
      for (auto &stmt : statements) {
        stmt->compileStatement(ctx);
      }
      ctx.builder.CreateRetVoid();
    }

    auto mainType = llvm::FunctionType::get(i32Ty, {}, false);
    auto main = llvm::Function::Create(mainType, llvm::Function::ExternalLinkage, "main", &ctx.module);
    {
      auto block = llvm::BasicBlock::Create(ctx.ctx, "entry", main);
      ctx.builder.SetInsertPoint(block);
      ctx.builder.SetCurrentDebugLocation(llvm::DebugLoc());
      ctx.builder.CreateCall(voidThunkTy, crpMain);
      auto collectGarbage = ctx.module.getOrInsertFunction("gcShutdown", voidThunkTy);
      ctx.builder.CreateCall(collectGarbage);
      ctx.builder.CreateRet(llvm::ConstantInt::get(i32Ty, 0));
    }
  }

  CompileContext::CompileContext(llvm::LLVMContext &ctx,
                                 llvm::IRBuilder<> &builder,
                                 llvm::DIBuilder &diBuilder,
                                 llvm::Module &module,
                                 ParseContext &pc) :
      pc(pc), ctx(ctx), builder(builder), diBuilder(diBuilder), module(module) {
    transformers[pc.funcType] = [](CompileContext &ctx, auto type) { return getFuncType(ctx, type).first; };
    transformers[pc.ptrType] = [](CompileContext &ctx, TPtr type) {
      auto &aggr = std::get<1>(CType::get(type)->value);
      return aggr.values.empty() ?
        llvm::Type::getInt8PtrTy(ctx.ctx) :
        llvm::PointerType::getUnqual(toLLVM(ctx, aggr.values[0]));
    };
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

    gcMetaType = llvm::StructType::create(ctx, "GCMeta");
    visitFnType = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx),
                                          {
                                              llvm::Type::getInt8PtrTy(ctx)->getPointerTo(),
                                              llvm::PointerType::getUnqual(gcMetaType),
                                          },
                                          false);
    metaFnType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(ctx),
        {
            llvm::Type::getInt8PtrTy(ctx),
            llvm::PointerType::getUnqual(visitFnType),
        },
        false
    );
    llvm::PointerType *gcMetaFnPtr = llvm::PointerType::getUnqual(metaFnType);
    gcMetaType->setBody(gcMetaFnPtr);

    {
      llvm::Function *gcFnMetaFn = llvm::Function::Create(metaFnType, llvm::GlobalValue::ExternalLinkage,
                                                          "taggedMetaFn", module);
      llvm::BasicBlock *entry = llvm::BasicBlock::Create(ctx, "entry", gcFnMetaFn);
      llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(ctx, "if.then", gcFnMetaFn);
      llvm::BasicBlock *contBlock = llvm::BasicBlock::Create(ctx, "cont", gcFnMetaFn);
      builder.SetInsertPoint(entry);
      llvm::LoadInst *metaFn = builder.CreateLoad(
          builder.CreatePointerCast(gcFnMetaFn->getArg(0),
                                    llvm::PointerType::getUnqual(gcMetaFnPtr)));
      builder.CreateCondBr(builder.CreateIsNotNull(metaFn), thenBlock, contBlock);
      builder.SetInsertPoint(thenBlock);
      builder.CreateCall(metaFnType, metaFn, {gcFnMetaFn->getArg(0), gcFnMetaFn->getArg(1)});
      builder.CreateBr(contBlock);
      builder.SetInsertPoint(contBlock);
      builder.CreateRetVoid();

      fnMeta = new llvm::GlobalVariable(
          module,
          gcMetaType,
          true,
          llvm::GlobalValue::ExternalLinkage,
          llvm::ConstantStruct::get(gcMetaType, {gcFnMetaFn}),
          "__gc_fn_meta");
    }
    pc.funcType->impls[pc.collectibleTrait] = std::make_unique<CollectibleTrait>(fnMeta);
  }

  void Defn::compileStatement(CompileContext &ctx) {
    binding.compile(ctx);
  }

  void Expr::compileStatement(CompileContext &ctx) {
    compileExpr(ctx, Position::Statement);
  }

  void emitLoc(CompileContext &ctx, loc::SrcLoc &loc) {
    ctx.builder.SetCurrentDebugLocation(
        llvm::DILocation::get(ctx.ctx, loc.line, loc.col,
                              ctx.builder.GetInsertBlock()->getParent()->getSubprogram()));
  }

  llvm::Value *IfExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, span.lo);
    llvm::PHINode *phi;
    if (pos != Position::Statement && elseClause) {
      phi = llvm::PHINode::Create(toLLVM(ctx, type),
                                  2 + elseIfClauses.size(),
                                  "if.tmp");
    } else {
      phi = nullptr;
    }

    auto *function = ctx.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(ctx.ctx, "if.then");
    llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(ctx.ctx, "if.cont");
    llvm::BasicBlock *elseBlock = elseClause ? llvm::BasicBlock::Create(ctx.ctx, "if.else") : mergeBlock;

    auto pred = predExpr->compileExpr(ctx, Position::Expr);
    ctx.builder.CreateCondBr(pred, thenBlock, elseBlock);

    function->getBasicBlockList().push_back(thenBlock);
    ctx.builder.SetInsertPoint(thenBlock);
    auto thenValue = thenExpr->compileExpr(ctx, pos);
    ctx.builder.CreateBr(mergeBlock);
    if (phi) phi->addIncoming(thenValue, ctx.builder.GetInsertBlock());

    for (auto &clause : elseIfClauses) {
      auto elseIfThenBlock = llvm::BasicBlock::Create(ctx.ctx, "if.then");
      auto elseIfElseBlock = llvm::BasicBlock::Create(ctx.ctx, "if.else");

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

    if (elseClause) {
      function->getBasicBlockList().push_back(elseBlock);
      ctx.builder.SetInsertPoint(elseBlock);
      auto elseValue = elseClause->thenExpr->compileExpr(ctx, pos);
      ctx.builder.CreateBr(mergeBlock);
      if (phi) phi->addIncoming(elseValue, ctx.builder.GetInsertBlock());
    }

    function->getBasicBlockList().push_back(mergeBlock);
    ctx.builder.SetInsertPoint(mergeBlock);
    if (phi) return ctx.builder.Insert(phi);
    return ctx.unitValue;
  }

  void gcRoot(CompileContext &ctx, llvm::Value *value, llvm::Value *meta = nullptr) {
    llvm::Function *gcRoot = llvm::Intrinsic::getDeclaration(&ctx.module, llvm::Intrinsic::gcroot);
    llvm::PointerType *i8PtrTy = llvm::Type::getInt8PtrTy(ctx.ctx);

    llvm::AllocaInst *local = ctx.builder.CreateAlloca(i8PtrTy);
    ctx.builder.CreateStore(ctx.builder.CreatePointerCast(value, i8PtrTy), local);
    ctx.builder.CreateCall(gcRoot, {local, meta ? ctx.builder.CreatePointerCast(meta, i8PtrTy)
                                                : llvm::ConstantPointerNull::get(i8PtrTy)});
  }

  llvm::Value *heapAlloc(CompileContext &ctx, llvm::Type *type) {
    llvm::Type *i32Ty = llvm::Type::getInt32Ty(ctx.ctx);
    llvm::PointerType *pointerType = llvm::PointerType::getUnqual(type);
    llvm::PointerType *i8PtrTy = llvm::Type::getInt8PtrTy(ctx.ctx);
    llvm::FunctionCallee malloc = ctx.module.getOrInsertFunction("gcAlloc", i8PtrTy, i32Ty);

    llvm::Constant *size = llvm::ConstantExpr::getTruncOrBitCast(llvm::ConstantExpr::getSizeOf(type), i32Ty);

    llvm::CallInst *allocated = ctx.builder.CreateCall(malloc, {size});
    return ctx.builder.CreatePointerCast(allocated, pointerType);
  }

  llvm::Value *compileFunc(CompileContext &ctx,
                           loc::SrcLoc &pos,
                           TPtr type,
                           std::vector<RawBinding> &bindings,
                           Var *recurVar,
                           const std::string &name,
                           const std::set<std::shared_ptr<Var>> &closedVars,
                           Expr &value) {
    llvm::FunctionType *funcType;
    llvm::PointerType *refType;
    std::tie(refType, funcType) = getFuncType(ctx, type);
    llvm::StructType *structType = llvm::cast<llvm::StructType>(refType->getPointerElementType());

    auto func = llvm::Function::Create(funcType,
                                       llvm::Function::PrivateLinkage,
                                       name,
                                       &ctx.module);
    llvm::DISubprogram *sp = ctx.diBuilder.createFunction(
        ctx.diCU->getFile(), name, func->getName(), ctx.diCU->getFile(),
        pos.line, toDISRType(ctx, type),
        pos.line, llvm::DINode::FlagPrivate, llvm::DISubprogram::SPFlagDefinition
    );
    func->setSubprogram(sp);
    func->setGC("shadow-stack");

    llvm::BasicBlock *emitBlock = ctx.builder.GetInsertBlock();
    llvm::BasicBlock::iterator emitPoint = ctx.builder.GetInsertPoint();

    auto block = llvm::BasicBlock::Create(ctx.ctx, "entry", func);
    ctx.builder.SetInsertPoint(block);
    emitLoc(ctx, pos);
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

    std::vector<llvm::Value *> closureEmitted;
    std::vector<std::function<llvm::Value *(CompileContext &, TPtr)>>
        oldEmissions(closedVars.size(), [](auto, auto) { return nullptr; });
    llvm::StructType *closureStructType;
    llvm::PointerType *closurePointerType;
    std::vector<llvm::Type *> closureTypes;
    llvm::Function *metaFn = nullptr;
    llvm::Value *metaClosurePtr = nullptr;

    if (!closedVars.empty()) {
      for (int i = 0; i < structType->getStructNumElements(); ++i) {
        closureTypes.push_back(structType->getStructElementType(i));
      }
      closureStructType = llvm::StructType::create(ctx.ctx, name + ".closure");
      closureStructType->setBody(closureTypes);
      closurePointerType = llvm::PointerType::getUnqual(closureStructType);
      llvm::Value *closurePtr = ctx.builder.CreatePointerCast(thisArg, closurePointerType);

      std::map<llvm::Value *, llvm::Value *> cache;
      size_t i = 0;
      for (auto &cv : closedVars) {
        auto &oldEmit = oldEmissions[i++] = std::move(cv->emit);
        cv->emit = [
            closurePtr, closurePointerType, closureStructType, &closureTypes,
            &closureEmitted, oldEmit, &cache,
            &name, &metaFn, &metaClosurePtr,
            emitBlock, emitPoint
        ](CompileContext &ctx, TPtr type) -> llvm::Value * {
          llvm::BasicBlock *oldBlock = ctx.builder.GetInsertBlock();
          llvm::BasicBlock::iterator oldPoint = ctx.builder.GetInsertPoint();
          llvm::DebugLoc oldDebug = ctx.builder.getCurrentDebugLocation();
          ctx.builder.SetInsertPoint(emitBlock, emitPoint);
          ctx.builder.SetCurrentDebugLocation(llvm::DebugLoc());
          llvm::Value *emitted = oldEmit(ctx, type);
          ctx.builder.SetInsertPoint(oldBlock, oldPoint);
          ctx.builder.SetCurrentDebugLocation(oldDebug);
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

          CollectibleTrait *collectible = type::TypedTrait<CollectibleTrait>::lookup(ctx.pc.collectibleTrait, type);
          if (collectible) {
            if (!metaFn) {
              metaFn = llvm::Function::Create(ctx.metaFnType,
                                              llvm::GlobalValue::PrivateLinkage,
                                              name + ".meta",
                                              ctx.module);
              llvm::BasicBlock::Create(ctx.ctx, "entry", metaFn);
            }
            ctx.builder.SetInsertPoint(&metaFn->getBasicBlockList().back());
            ctx.builder.SetCurrentDebugLocation(llvm::DebugLoc());
            if (!metaClosurePtr) {
              metaClosurePtr = ctx.builder.CreatePointerCast(metaFn->getArg(0), closurePointerType);
            }
            llvm::Value *gep = ctx.builder.CreateGEP(metaClosurePtr,
                                                     {llvm::ConstantInt::get(i32Ty, 0),
                                                      llvm::ConstantInt::get(i32Ty, idx)});
            llvm::PointerType *i8PtrPtrTy = llvm::Type::getInt8PtrTy(ctx.ctx)->getPointerTo();
            llvm::Value *cast = ctx.builder.CreatePointerCast(gep, i8PtrPtrTy);
            ctx.builder.CreateCall(ctx.visitFnType, metaFn->getArg(1), {cast, collectible->meta});
            ctx.builder.SetInsertPoint(oldBlock, oldPoint);
            ctx.builder.SetCurrentDebugLocation(oldDebug);
          }

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
    emitLoc(ctx, pos);

    {
      size_t i = 0;
      for (auto &cv : closedVars) {
        cv->emit = std::move(oldEmissions[i]);
      }
    }

    llvm::Value *retV;
    if (closureEmitted.empty()) {
      llvm::Constant *constant = llvm::ConstantStruct::get(structType,
                                                           {llvm::ConstantPointerNull::get(
                                                               ctx.metaFnType->getPointerTo()),
                                                            func});
      llvm::GlobalVariable *global = new llvm::GlobalVariable(ctx.module,
                                                              structType,
                                                              true,
                                                              llvm::GlobalValue::ExternalLinkage,
                                                              constant,
                                                              name);
      retV = global;
    } else {
      llvm::Value *allocated = heapAlloc(ctx, closureStructType);
      gcRoot(ctx, allocated, ctx.fnMeta);
      llvm::IntegerType *i32Ty = llvm::Type::getInt32Ty(ctx.ctx);
      {
        llvm::Value *metaFnGep = ctx.builder.CreateInBoundsGEP(closureStructType, allocated,
                                                               {llvm::ConstantInt::get(i32Ty, 0),
                                                                llvm::ConstantInt::get(i32Ty, 0)});
        if (metaFn) {
          auto insertBlock = ctx.builder.GetInsertBlock();
          auto insertPoint = ctx.builder.GetInsertPoint();
          auto insertDebug = ctx.builder.getCurrentDebugLocation();
          ctx.builder.SetInsertPoint(&metaFn->getBasicBlockList().back());
          ctx.builder.SetCurrentDebugLocation(llvm::DebugLoc());
          ctx.builder.CreateRetVoid();
          ctx.builder.SetInsertPoint(insertBlock, insertPoint);
          ctx.builder.SetCurrentDebugLocation(insertDebug);
          ctx.builder.CreateStore(metaFn, metaFnGep);
        } else {
          ctx.builder.CreateStore(llvm::ConstantPointerNull::get(ctx.metaFnType->getPointerTo()), metaFnGep);
        }
      }
      {
        llvm::Value *thisGep = ctx.builder.CreateInBoundsGEP(closureStructType, allocated,
                                                             {llvm::ConstantInt::get(i32Ty, 0),
                                                              llvm::ConstantInt::get(i32Ty, 1)});
        ctx.builder.CreateStore(func, thisGep);
      }
      for (unsigned int i = 0; i < closureEmitted.size(); ++i) {
        llvm::Value *gep = ctx.builder.CreateInBoundsGEP(closureStructType, allocated,
                                                         {llvm::ConstantInt::get(i32Ty, 0),
                                                          llvm::ConstantInt::get(i32Ty,
                                                                                 i + structType->getNumElements())});
        ctx.builder.CreateStore(closureEmitted[i], gep);
      }
      retV = ctx.builder.CreatePointerCast(allocated, llvm::PointerType::getUnqual(structType));
    }
    return retV;
  }
  llvm::Instruction *compileCall(CompileContext &ctx,
                                 const TPtr &functionType,
                                 llvm::Value *function,
                                 const std::vector<llvm::Value *> &callArgs,
                                 const TPtr &retType) {
    llvm::FunctionType *funcType;
    llvm::PointerType *refType;
    std::tie(refType, funcType) = getFuncType(ctx, functionType);
    std::vector<llvm::Value *> trueArgs = {function};
    trueArgs.insert(trueArgs.begin() + 1, callArgs.begin(), callArgs.end());
    llvm::IntegerType *i32Ty = llvm::Type::getInt32Ty(ctx.ctx);
    llvm::Value *funcPtr
        = ctx.builder.CreateInBoundsGEP(function,
                                        {llvm::ConstantInt::get(i32Ty, 0),
                                         llvm::ConstantInt::get(i32Ty, 1)});
    llvm::Value *funcValuePtr = ctx.builder.CreateLoad(funcPtr, "f");

    llvm::CallInst *returned = ctx.builder.CreateCall(funcType, funcValuePtr, trueArgs);
    CollectibleTrait *collectible = type::TypedTrait<CollectibleTrait>::lookup(ctx.pc.collectibleTrait, retType);
    if (collectible) {
      gcRoot(ctx, returned, collectible->meta);
    }
    return returned;
  }

  llvm::Value *LetExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, name ? name->ident.loc : inToken.loc);
    if (name) {
      std::vector<RawBinding> rbs(bindings.size());
      for (int i = 0; i < bindings.size(); ++i) {
        rbs[i].var = bindings[i].var;
        rbs[i].name = bindings[i].name;
      }
      llvm::Value *func = compileFunc(ctx,
                                      name->ident.loc,
                                      nameVar->type->type,
                                      rbs,
                                      nameVar.get(),
                                      name->ident.value,
                                      closed,
                                      *body);
      std::vector<llvm::Value *> args(bindings.size());
      for (int i = 0; i < bindings.size(); ++i) {
        std::shared_ptr<Var> &var = bindings[i].var;
        bindings[i].compile(ctx);
        args[i] = var->emit(ctx, var->type->type);
      }
      return compileCall(ctx, nameVar->type->type, func, args, type);
    } else {
      for (auto &b : bindings) {
        b.compile(ctx);
      }
      return body->compileExpr(ctx, pos);
    }
  }

  llvm::Value *BlockExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, span.lo);
    if (value) {
      for (const auto &stmt : statements) {
        stmt.statement->compileStatement(ctx);
      }
      return value->compileExpr(ctx, pos);
    } else {
      return ctx.unitValue;
    }
  }

  llvm::Value *BracketExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, span.lo);
    return value->compileExpr(ctx, pos);
  }

  llvm::Value *ColonExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, span.lo);
    return value->compileExpr(ctx, pos);
  }

  llvm::Value *LiteralExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, span.lo);
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

  llvm::Value *VarExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, span.lo);
    return var->emit(ctx, type);
  }

  llvm::Value *BinaryExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, span.lo);
    llvm::Value *lhsV = lhs->compileExpr(ctx, Position::Expr);
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
        CmpTrait *cmpTrait = type::TypedTrait<CmpTrait>::lookup(ctx.pc.cmpTrait, lhs->type);
        std::vector<llvm::Value *> comparisons;
        for (const auto &rhs : terms) {
          llvm::Value *rhsV = rhs.expr->compileExpr(ctx, Position::Expr);
          switch (rhs.operatorToken.type) {
            case Tok::TNe:
              comparisons.push_back(cmpTrait->ne(ctx, lhsV, rhsV));
              break;
            case Tok::TEq:
            case Tok::TEq2:
              comparisons.push_back(cmpTrait->eq(ctx, lhsV, rhsV));
              break;
            case Tok::TLt:
              comparisons.push_back(cmpTrait->lt(ctx, lhsV, rhsV));
              break;
            case Tok::TLe:
              comparisons.push_back(cmpTrait->le(ctx, lhsV, rhsV));
              break;
            case Tok::TGt:
              comparisons.push_back(cmpTrait->gt(ctx, lhsV, rhsV));
              break;
            default /*Tok::TGe*/:
              comparisons.push_back(cmpTrait->ge(ctx, lhsV, rhsV));
              break;
          }
          lhsV = rhsV;
        }
        return ctx.builder.CreateAnd(comparisons);
      }
      default: {
        // Tok::TOr1, Tok::TAnd1
        // Tok::TAdd, Tok::TSub
        // Tok::TMul, Tok::TDiv, Tok::TRem
        for (const auto &rhs : terms) {
          llvm::Value *rhsV = rhs.expr->compileExpr(ctx, Position::Expr);
          switch (rhs.operatorToken.type) {
            case Tok::TOr1:
              lhsV = ctx.builder.CreateBinOp(llvm::Instruction::Or, lhsV, rhsV);
              break;
            case Tok::TAnd1:
              lhsV = ctx.builder.CreateBinOp(llvm::Instruction::And, lhsV, rhsV);
              break;
            case Tok::TAdd:
              lhsV = type::TypedTrait<BinaryTrait>::lookup(ctx.pc.addTrait, type)->app(ctx, lhsV, rhsV);
              break;
            case Tok::TSub:
              lhsV = type::TypedTrait<BinaryTrait>::lookup(ctx.pc.subTrait, type)->app(ctx, lhsV, rhsV);
              break;
            case Tok::TMul:
              lhsV = type::TypedTrait<BinaryTrait>::lookup(ctx.pc.mulTrait, type)->app(ctx, lhsV, rhsV);
              break;
            case Tok::TDiv:
              lhsV = type::TypedTrait<BinaryTrait>::lookup(ctx.pc.divTrait, type)->app(ctx, lhsV, rhsV);
              break;
            default: // Tok::TRem
              lhsV = type::TypedTrait<BinaryTrait>::lookup(ctx.pc.remTrait, type)->app(ctx, lhsV, rhsV);
              break;
          }
        }
        break;
      }
    }
    return lhsV;
  }

  llvm::Value *PrefixExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, span.lo);
    llvm::Value *value = expr->compileExpr(ctx, Position::Expr);
    bool isInt = value->getType()->isIntegerTy();
    for (const auto &prefix : prefixes) {
      if (prefix.type == Tok::TSub) {
        value = isInt ? ctx.builder.CreateNeg(value) : ctx.builder.CreateFNeg(value);
      }
    }
    return value;
  }

  llvm::Value *FunCallExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, span.lo);
    llvm::Value *func = function->compileExpr(ctx, Position::Expr);
    std::vector<llvm::Value *> args(arguments.size());
    for (int i = 0; i < arguments.size(); ++i) {
      args[i] = arguments[i]->compileExpr(ctx, Position::Expr);
    }
    return compileCall(ctx, function->type, func, args, type);
  }

  llvm::Value *HintedExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, span.lo);
    return expr->compileExpr(ctx, pos);
  }

  llvm::Value *FnExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, span.lo);
    return compileFunc(ctx, span.lo, type, arguments.bindings, recurVar.get(), name ? name->ident.value : "anon",
                       closed, *body);
  }

  llvm::Value *LambdaExpr::compileExpr(CompileContext &ctx, Position pos) {
    emitLoc(ctx, span.lo);
    return compileFunc(ctx, span.lo, type, arguments, nullptr, "lambda", closed, *body);
  }

  void Binding::compile(CompileContext &ctx) {
    emitLoc(ctx, span.lo);
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

        llvm::StructType *structType = llvm::cast<llvm::StructType>(refType->getPointerElementType());

        auto invoker = llvm::Function::Create(invokerFuncType,
                                              llvm::Function::PrivateLinkage,
                                              name.ident.value + ".invoker",
                                              &ctx.module);
        invoker->setSubprogram(ctx.diBuilder.createFunction(
            ctx.diCU->getFile(), invoker->getName(), invoker->getName(), ctx.diCU->getFile(),
            foreignToken->loc.line, toDISRType(ctx, var->type->type),
            foreignToken->loc.line, llvm::DINode::FlagPrivate, llvm::DISubprogram::SPFlagDefinition
        ));

        llvm::BasicBlock *oldBlock = ctx.builder.GetInsertBlock();
        llvm::BasicBlock::iterator oldPoint = ctx.builder.GetInsertPoint();
        llvm::DebugLoc oldDebug = ctx.builder.getCurrentDebugLocation();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(ctx.ctx, "entry", invoker);
        ctx.builder.SetInsertPoint(entry);
        ctx.builder.SetCurrentDebugLocation(llvm::DebugLoc());
        std::vector<llvm::Value *> args(invoker->arg_size() - 1);
        for (int i = 1; i < invoker->arg_size(); ++i) {
          args[i - 1] = invoker->getArg(i);
        }
        ctx.builder.CreateRet(ctx.builder.CreateCall(func, args));
        ctx.builder.SetInsertPoint(oldBlock, oldPoint);
        ctx.builder.SetCurrentDebugLocation(oldDebug);

        llvm::Constant *constant = llvm::ConstantStruct::get(
            structType,
            {llvm::ConstantPointerNull::get(ctx.metaFnType->getPointerTo()), invoker});

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

    if (var->type->bound.empty()) {
      llvm::Value *varValue = compileExpr(ctx, var->type->type);
      var->emit = [varValue](ast::CompileContext &, ast::TPtr) {
        return varValue;
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
      TPtr thisType = CType::get(var->type->type);
      CType::getFree(thisType, [&subs, &ctx](const TPtr &t) { subs[t] = ctx.pc.tc.fresh(); });
      TPtr instType = CType::replace(thisType, ctx.pc.tc, subs);
      CType::getAndUnify(ctx.pc.tc, type, instType);
      TPtr unitType = ctx.pc.tc.push(CType::aggregate(ctx.pc.unitType, {}));
      for (const auto &sub : subs) {
        TPtr target = CType::get(sub.second);
        std::get<0>(sub.first->value).weakParent = target;
      }

      auto found = insts.find(instType);
      if (found != insts.end()) {
        return found->second;
      }

      llvm::BasicBlock *oldBlock = ctx.builder.GetInsertBlock();
      llvm::BasicBlock::iterator oldPoint = ctx.builder.GetInsertPoint();
      llvm::DebugLoc oldDebug = ctx.builder.getCurrentDebugLocation();

      ctx.builder.SetInsertPoint(jump);
      ctx.builder.SetCurrentDebugLocation(llvm::DebugLoc());
      llvm::Value *ret = compileExpr(ctx, instType);
      ctx.builder.SetInsertPoint(oldBlock, oldPoint);
      ctx.builder.SetCurrentDebugLocation(oldDebug);

      ret->setName(name.ident.value);

      for (const auto &sub : subs) {
        std::get<0>(sub.first->value).weakParent = nullptr;
      }

      return insts[type] = ret;
    };
  }
  llvm::Value *Binding::compileExpr(CompileContext &ctx, TPtr instType) {
    if (arguments) {
      return compileFunc(ctx,
                         span.lo,
                         instType,
                         arguments->bindings,
                         arguments->recurVar.get(),
                         name.ident.value,
                         arguments->closed,
                         *value);
    } else {
      return value->compileExpr(ctx, Position::Expr);
    }
  }
}
