#pragma once

#include "Lir.h"

#include "../../common/Arena.h"

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

namespace lir::codegen {
  struct CodegenResult {
    std::unique_ptr<llvm::LLVMContext> ctx;
    std::unique_ptr<llvm::Module> mod;
  };

  CodegenResult generate(arena::InternArena<type::Ty> &tcx,
                         arena::InternArena<type::TraitBound> &tbcx,
                         Module &mod,
                         const std::string &fileName = "module.crp",
                         const std::string &fileDir = ".");
}
