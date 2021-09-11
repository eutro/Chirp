#pragma once

#include "Lir.h"
#include "../../common/Arena.h"

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
  using Tp = Ty *;
  struct LocalCC;
  struct CC;
  using EmitFn = std::function<llvm::Value*(Insn&, Insn::CallTrait&, CC&, LocalCC&)>;
  using TyPair = std::tuple<llvm::Type *, llvm::DIType *>;

  struct CC {
    arena::InternArena<Ty> &tcx;
    arena::InternArena<type::TraitBound> &tbcx;
    llvm::Module &mod;
    llvm::LLVMContext &ctx;

    llvm::DIBuilder &db;
    llvm::DICompileUnit *cu;

    std::map<TraitImpl::For, EmitFn> emitCall;
    std::map<type::Ty *, TyPair> tyCache;
  };

  struct LocalCC {
    CC &cc;

    llvm::IRBuilder<> &ib;
    llvm::Function *func;
    lir::Instantiation &inst;

    std::map<Insn *, llvm::Value *> vals;
    std::map<BasicBlock *, llvm::BasicBlock *> bbs;
    Idx paramC = 0;

    std::vector<llvm::DILocalScope *> scopes{func->getSubprogram()};
  };

  llvm::FunctionType *ffiFnTy(CC &cc, type::Ty::FfiFn &v);

  const TyPair &getTyPair(CC &cc, type::Ty *ty);

  llvm::Type *adtTy(CC &cc, type::Ty::ADT &v);

  llvm::FunctionType *ffiFnTy(CC &cc, type::Ty::FfiFn &v);

  llvm::DILocation *locFromSpan(CC &cc, LocalCC &lcc, const loc::SrcLoc &loc);

  template <typename T = llvm::Type *>
  T getTy(CC &cc, type::Tp ty) {
    return std::get<T>(getTyPair(cc, ty));
  }

  template <typename T = llvm::Type *>
  T getTy(LocalCC &lcc, Idx i) {
    return getTy<T>(lcc.cc, lcc.inst.types.at(i));
  }
}
