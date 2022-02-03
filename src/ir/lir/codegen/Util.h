#pragma once

#include "../Lir.h"
#include "../../../common/Arena.h"
#include "../../../type/infer/Public.h"

#include <array>
#include <functional>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DIBuilder.h>

namespace lir::codegen {
  using type::Ty;
  using Instantiation = type::infer::Inst::Val;
  using Tp = Ty *;
  struct LocalCC;
  struct CC;
  using EmitFn = std::function<llvm::Value*(Insn&, Insn::CallTrait&, CC&, LocalCC&)>;

  struct GCData {
    llvm::GlobalVariable *metadata;
  };

  using TyTuple = std::tuple<llvm::Type *, llvm::DIType *, std::optional<GCData>>;

  struct Value {
    using ValFn = std::function<llvm::Value*()>;
    // this function MUST derive any collectible pointers from registered GC roots,
    // otherwise they may have been relocated by the collector
    ValFn ref;
    llvm::Type *ty;
    enum Type {
      Pointer,
      Direct,
    };
    Type loadTy;
    Value(): ref([](){return nullptr;}), ty(nullptr), loadTy(Direct) {}
    Value(llvm::Value *v, llvm::Type *ty, Type loadTy):
      ref([=](){return v;}),
      ty(ty),
      loadTy(loadTy)
    {}
    Value(ValFn &&v, llvm::Type *ty, Type loadTy):
      ref(std::forward<ValFn>(v)),
      ty(ty),
      loadTy(loadTy)
    {}
  };

  struct CC {
    llvm::Module &mod;
    llvm::LLVMContext &ctx;

    llvm::DIBuilder &db;
    llvm::DICompileUnit *cu;

    // _[block][inst]
    std::map<Idx, std::map<Idx, EmitFn>> emitCall;
    std::map<type::Ty *, TyTuple> tyCache;
    std::map<std::pair<Tp, Tp>, llvm::FunctionCallee> unionConversions;

    std::map<Idx, Value> vars;

    llvm::StructType *gcMetaTy = nullptr;
    llvm::FunctionType *visitFnTy = nullptr;
    llvm::FunctionType *metaFnTy = nullptr;

    std::vector<std::function<void(CC&)>> deferred;
  };

  struct LocalCC {
    CC &cc;

    llvm::IRBuilder<> &ib;
    llvm::Function *func;
    Instantiation &inst;

    LocalCC(
      CC &cc,
      llvm::IRBuilder<> &ib,
      llvm::Function *func,
      Instantiation &inst
    ): cc(cc), ib(ib), func(func), inst(inst) {}

    std::map<Idx, Value> vars;
    std::map<Insn *, Value> vals;

    Value &varFor(Idx idx);
    llvm::Value *load(Value &v);
    llvm::Value *load(Insn *insn);
    llvm::Value *load(Idx var);
    llvm::Value *reference(Value &v);
    llvm::Value *reference(Insn *insn);
    llvm::Value *reference(Idx var);

    std::map<BasicBlock *, llvm::BasicBlock *> bbs;
    BasicBlock *bb = nullptr;
    Idx paramC = 0;

    std::vector<llvm::DILocalScope *> scopes{func->getSubprogram()};
  };

  llvm::FunctionType *ffiFnTy(CC &cc, type::Ty::FfiFn &v);

  const TyTuple &getTyTuple(CC &cc, type::Ty *ty);

  bool isZeroSize(Tp ty);

  llvm::StructType *adtTy(CC &cc, type::Ty::ADT &v);

  llvm::StructType *unionTy(CC &cc, type::Ty::Union &v);

  llvm::Value *gcAlloc(LocalCC &lcc, llvm::Type *structTy);

  llvm::DILocation *locFromSpan(CC &cc, LocalCC &lcc, const loc::SrcLoc &loc);

  Tp getChirpTy(LocalCC &lcc, Idx i);

  template <typename T = llvm::Type *>
  T getTy(CC &cc, type::Tp ty) {
    return std::get<T>(getTyTuple(cc, ty));
  }

  template <typename T = llvm::Type *>
  T getTy(LocalCC &lcc, Idx i) {
    return getTy<T>(lcc.cc, getChirpTy(lcc, i));
  }

  llvm::Value *unionise(LocalCC &lcc, llvm::Value *inValue, Tp inTy, Tp outTy);

  void gcRoot(CC &cc, llvm::IRBuilder<> &ib, llvm::Value *reference, llvm::Value *meta);

  llvm::AllocaInst *addTemporary(LocalCC &lcc, llvm::Type *ty, llvm::Value *meta);

  void ensureMetaTy(CC &cc);
}
