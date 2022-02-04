#include "Intrinsics.h"

#include "../../hir/Builtins.h"
#include "../../../type/TypePrint.h"

namespace lir::codegen {
  thread_local Idx counter;

  template <typename T, std::size_t... Idx, typename C, typename... Args>
  auto implTraitFn(T C::*fn,
                   std::index_sequence<Idx...>,
                   Args&&... args) {
    return [=](::Idx, const std::vector<Value*> &callArgs, CC &cc, LocalCC &lcc) -> llvm::Value * {
      return (lcc.ib.*fn)(lcc.load(*callArgs.at(0)), lcc.load(*callArgs.at(Idx+1))..., args...);
    };
  }

  void implTrait(CC &cc, EmitFn fn) {
    auto &m = cc.emitCall[counter++];
    m[m.size()] = std::move(fn);
  }

  template <std::size_t Arity, typename T, typename C, typename... Args>
  void implTrait(CC &cc,
                 T C::*fn,
                 Args&&... args) {
    implTrait(
      cc,
      implTraitFn(fn, std::make_index_sequence<Arity>{},
                  std::forward<Args>(args)...));
  }

  llvm::FunctionCallee createUnionDispatch(CC &cc, type::infer::Inst::Val &inst) {
    std::vector<Tp> argTys;
    Idx dispatchIdx;
    std::vector<Tp> retTys;
    std::vector<Tp> allRetTys;
    auto it = inst.loggedTys.begin();
    while (!std::holds_alternative<Ty::Placeholder>(it->second->v)) {
      argTys.push_back(it++->second);
    }
    dispatchIdx = std::get<Ty::Placeholder>(it->second->v).i;
    while (!std::holds_alternative<Ty::Placeholder>((++it)->second->v)) {
      retTys.push_back(it->second);
    }
    while (++it != inst.loggedTys.end()) {
      allRetTys.push_back(it->second);
    }

    if (dispatchIdx != 0 || retTys.size() != 1) {
      throw std::runtime_error(util::toStr(
          "ICE: Unsupported union dispatch with ",
          retTys.size(),
          " return types and index ",
          dispatchIdx
      ));
    }

    std::vector<type::infer::Inst::Ref> refs;
    refs.reserve(inst.loggedRefs.size());
    for (const auto &e : inst.loggedRefs) {
      refs.push_back(e.second);
    }

    std::vector<llvm::Type *> argLlTys;
    std::vector<llvm::Metadata *> diTys;
    Tp retCrpTy = retTys.front();
    auto &retTup = getTyTuple(cc, retCrpTy);
    diTys.push_back(std::get<1>(retTup));
    auto &recTup = getTyTuple(cc, argTys.front());
    argLlTys.push_back(std::get<0>(recTup));
    diTys.push_back(std::get<1>(recTup));
    for (Tp argTy : std::get<Ty::Tuple>(argTys.at(1)->v).t) {
      auto argTup = getTyTuple(cc, argTy);
      argLlTys.push_back(std::get<0>(argTup));
      diTys.push_back(std::get<1>(argTup));
    }
    llvm::Type *retLlTy = std::get<0>(retTup);
    llvm::FunctionType *funcTy = llvm::FunctionType::get(retLlTy, argLlTys, false);
    std::string name = util::toStr(argTys.front(), ".dispatch");
    llvm::Function *func = llvm::Function::Create(funcTy,
                                                  llvm::GlobalValue::PrivateLinkage,
                                                  name,
                                                  cc.mod);
    func->setGC(GC_METHOD);
    func->setSubprogram(cc.db.createFunction(
        cc.cu->getFile(), name, func->getName(), cc.cu->getFile(),
        0, cc.db.createSubroutineType(cc.db.getOrCreateTypeArray(diTys)),
        0, llvm::DINode::FlagPrivate, llvm::DISubprogram::SPFlagDefinition
    ));

    cc.deferred.emplace_back([func,
                              retLlTy,
                              retCrpTy,
                              allRetTys = std::move(allRetTys),
                              dispatchIdx,
                              &inst, // owned by inference system
                              argTys = std::move(argTys),
                              argLlTys = std::move(argLlTys),
                              refs = std::move(refs)](CC &cc) {
      bool isUnionOut = std::holds_alternative<Ty::Union>(retCrpTy->v);
      Ty::Union dispatchedUTy = std::get<Ty::Union>(type::uncycle(argTys.at(dispatchIdx))->v);
      std::vector<Tp> &retRawTys = std::get<Ty::Tuple>(allRetTys.at(dispatchIdx)->v).t;
      llvm::IntegerType *i32Ty = llvm::Type::getInt32Ty(cc.ctx);

      llvm::IRBuilder<> ib(cc.ctx);
      LocalCC lcc(cc, ib, func, inst);
      ib.SetCurrentDebugLocation(llvm::DILocation::get(cc.ctx, 0, 0, lcc.scopes.back()));

      std::vector<Value> argsOwned;
      argsOwned.reserve(argLlTys.size());
      std::vector<Value *> args;
      args.reserve(argLlTys.size());
      args.push_back(&argsOwned.emplace_back());
      for (Idx i = 1; i < argLlTys.size(); ++i) {
        args.push_back(&argsOwned.emplace_back(func->getArg(i), argLlTys.at(i), Value::Direct));
      }

      llvm::BasicBlock *entry = llvm::BasicBlock::Create(cc.ctx, "entry", func);
      llvm::BasicBlock *end = llvm::BasicBlock::Create(cc.ctx, "end", func);
      llvm::PHINode *phi = llvm::PHINode::Create(retLlTy, dispatchedUTy.tys.size() + 1, "ret", end);

      ib.SetInsertPoint(entry);
      llvm::Argument *dispatched = func->getArg(dispatchIdx);
      llvm::StructType *uTy = unionTy(cc, llvm::StructType::get(cc.ctx));
      llvm::Value *discGep = ib.CreateStructGEP(uTy, ib.CreatePointerCast(dispatched, uTy->getPointerTo()), 0);
      llvm::Value *disc = ib.CreateLoad(i32Ty, discGep);
      llvm::SwitchInst *switchI = ib.CreateSwitch(disc, end, dispatchedUTy.tys.size());
      phi->addIncoming(llvm::PoisonValue::get(retLlTy), entry);
      for (Idx i = 0; i < dispatchedUTy.tys.size(); ++i) {
        llvm::BasicBlock *bb = llvm::BasicBlock::Create(cc.ctx, util::toStr("case.", i), func);
        switchI->addCase(llvm::ConstantInt::get(i32Ty, i), bb);
        ib.SetInsertPoint(bb);

        auto &ref = refs.at(i);
        llvm::Type *dispatchingTy = getTy(cc, dispatchedUTy.tys.at(i));
        llvm::StructType *sTy = unionTy(cc, dispatchingTy);
        llvm::Value *gep = ib.CreateStructGEP(sTy, ib.CreatePointerCast(dispatched, sTy->getPointerTo()), 1);
        argsOwned.front() = Value(gep, dispatchingTy, Value::Pointer);
        llvm::Value *ret = cc.emitCall.at(ref.first).at(ref.second)(0, args, cc, lcc);
        Tp localRetTy = retRawTys.at(i);
        Value out(ret);
        maybeTemp(lcc, localRetTy, ret, out);
        llvm::Value *unionised = isUnionOut ? unionise(lcc, out, localRetTy, retCrpTy) : lcc.load(out);
        phi->addIncoming(unionised, ib.GetInsertBlock());
        ib.CreateBr(end);
      }
      ib.SetInsertPoint(end);
      ib.CreateRet(phi);
    });

    return {funcTy, func};
  }

  void addIntrinsics(CC &cc, type::infer::Inst::Set &sys) {
    counter = hir::BUILTIN_BLOCKS_START;
    auto &ffiInsts = sys.entities[counter];
    auto &ffiCalls = cc.emitCall[counter];
    counter++;
    for (const auto &inst : ffiInsts) {
      auto fnTy = ffiFnTy(cc, std::get<Ty::FfiFn>(inst.second.loggedTys.at(0)->v));
      ffiCalls.emplace(inst.first, [fnTy](Idx, const std::vector<Value*> &callArgs, CC &cc, LocalCC &lcc)
                       -> llvm::Value * {
        std::vector<llvm::Value *> args;
        args.reserve(callArgs.size() - 1);
        for (auto it = callArgs.begin() + 1; it != callArgs.end(); ++it) {
          args.push_back(lcc.load(**it));
        }
        return lcc.ib.CreateCall(fnTy, lcc.load(*callArgs.front()), args);
      });
    }

    using BinaryAndTwine = llvm::Value *(llvm::IRBuilder<>::*)(llvm::Value*, llvm::Value*, const llvm::Twine&);
    for ([[maybe_unused]] type::IntSize is : type::INT_SIZE_FIXED) {
      for (bool isU : {false, true}) {
        implTrait<1>(cc, /*Add*/ &llvm::IRBuilder<>::CreateAdd, "add", false, false);
        implTrait<1>(cc, /*Sub*/ &llvm::IRBuilder<>::CreateSub, "sub", false, false);
        implTrait<1>(cc, /*Mul*/ &llvm::IRBuilder<>::CreateMul, "mul", false, false);
        implTrait<1>(
          cc, /*Div*/
          isU ?
          &llvm::IRBuilder<>::CreateUDiv :
          &llvm::IRBuilder<>::CreateSDiv,
          "div", false
        );
        implTrait<1>(
          cc, /*Rem*/
          isU ?
          &llvm::IRBuilder<>::CreateURem :
          &llvm::IRBuilder<>::CreateSRem,
          "rem"
        );
        implTrait<1>(cc, /*BitOr*/ (BinaryAndTwine)&llvm::IRBuilder<>::CreateOr, "or");
        implTrait<1>(cc, /*BitAnd*/ (BinaryAndTwine)&llvm::IRBuilder<>::CreateAnd, "and");

        implTrait(
          cc, /*Eq*/
          [](Idx method, const std::vector<Value*> &callArgs, CC &cc, LocalCC &lcc)
          -> llvm::Value * {
            std::array<llvm::ICmpInst::Predicate, 2> cis{
              llvm::ICmpInst::ICMP_NE,
              llvm::ICmpInst::ICMP_EQ,
            };
            return lcc.ib.CreateICmp(
                cis[method],
                lcc.load(*callArgs.front()),
                lcc.load(*callArgs.at(1)),
                "eq"
            );
          }
        );
        implTrait(
          cc, /*Cmp*/
          [isU](Idx method, const std::vector<Value*> &callArgs, CC &cc, LocalCC &lcc)
          -> llvm::Value * {
            std::array<llvm::ICmpInst::Predicate, 4> cis{
              isU ? llvm::ICmpInst::ICMP_ULT : llvm::ICmpInst::ICMP_SLT,
              isU ? llvm::ICmpInst::ICMP_ULE : llvm::ICmpInst::ICMP_SLE,
              isU ? llvm::ICmpInst::ICMP_UGT : llvm::ICmpInst::ICMP_SGT,
              isU ? llvm::ICmpInst::ICMP_UGE : llvm::ICmpInst::ICMP_SGE,
            };
            return lcc.ib.CreateICmp(
                cis[method],
                lcc.load(*callArgs.front()),
                lcc.load(*callArgs.at(1)),
                "cmp"
            );
          }
        );
      }
      implTrait<0>(cc, /*Neg*/ &llvm::IRBuilder<>::CreateNeg, "neg", false, false);
    }
    implTrait<1>(cc, /*BitOr*/ (BinaryAndTwine)&llvm::IRBuilder<>::CreateOr, "or");
    implTrait<1>(cc, /*BitAnd*/ (BinaryAndTwine)&llvm::IRBuilder<>::CreateAnd, "and");

    for ([[maybe_unused]] type::FloatSize fs : type::FLOAT_SIZE_VALUES) {
      implTrait<1>(cc, /*Add*/ &llvm::IRBuilder<>::CreateFAdd, "add", nullptr);
      implTrait<1>(cc, /*Sub*/ &llvm::IRBuilder<>::CreateFSub, "sub", nullptr);
      implTrait<1>(cc, /*Mul*/ &llvm::IRBuilder<>::CreateFMul, "mul", nullptr);
      implTrait<1>(cc, /*Div*/ &llvm::IRBuilder<>::CreateFDiv, "div", nullptr);
      implTrait<1>(cc, /*Rem*/ &llvm::IRBuilder<>::CreateFRem, "rem", nullptr);
      implTrait<0>(cc, /*Neg*/ &llvm::IRBuilder<>::CreateFNeg, "neg", nullptr);
      // NaN can and will ruin your life
      implTrait(
        cc, /*Eq*/
        [](Idx method, const std::vector<Value*> &callArgs, CC &cc, LocalCC &lcc)
        -> llvm::Value * {
          std::array<llvm::FCmpInst::Predicate, 2> cis{
            llvm::FCmpInst::FCMP_ONE,
            llvm::FCmpInst::FCMP_OEQ,
          };
          return lcc.ib.CreateFCmp(
              cis[method],
              lcc.load(*callArgs.front()),
              lcc.load(*callArgs.at(1)),
              "eq"
          );
        }
      );
      implTrait(
        cc, /*Cmp*/
        [](Idx method, const std::vector<Value*> &callArgs, CC &cc, LocalCC &lcc)
        -> llvm::Value * {
          std::array<llvm::FCmpInst::Predicate, 4> cis{
            llvm::FCmpInst::FCMP_OLT,
            llvm::FCmpInst::FCMP_OLE,
            llvm::FCmpInst::FCMP_OGT,
            llvm::FCmpInst::FCMP_OGE,
          };
          return lcc.ib.CreateFCmp(
              cis[method],
              lcc.load(*callArgs.front()),
              lcc.load(*callArgs.at(1)),
              "cmp"
          );
        }
      );
    }

    auto &unionInsts = sys.entities[counter];
    auto &unionCalls = cc.emitCall[counter];
    for (auto &inst : unionInsts) {
      llvm::FunctionCallee call = createUnionDispatch(cc, inst.second);
      unionCalls.emplace(inst.first, [call]
      (Idx method, const std::vector<Value*> &callArgs, CC &cc, LocalCC &lcc) -> llvm::Value * {
        std::vector<llvm::Value *> args;
        args.reserve(callArgs.size());
        for (auto arg : callArgs) {
          args.push_back(lcc.load(*arg));
        }
        return lcc.ib.CreateCall(call, args);
      });
    }
  }
}
