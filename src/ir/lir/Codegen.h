#pragma once

#include "Lir.h"

#include "../../common/Arena.h"

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

namespace lir::codegen {
  struct CodegenResult {
    std::unique_ptr<llvm::Module> mod;
    std::unique_ptr<llvm::LLVMContext> ctx;
  };

  CodegenResult generate(arena::InternArena<type::Ty> &tcx,
                         arena::InternArena<type::TraitBound> &tbcx,
                         Module &mod);
}
