#pragma once

#include "../Lir.h"
#include "../../../type/infer/Public.h"

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

namespace lir::codegen {
  struct CodegenResult {
    std::unique_ptr<llvm::LLVMContext> ctx;
    std::unique_ptr<llvm::Module> mod;
  };

  CodegenResult generate(type::Tcx &tcx,
                         type::Tbcx &tbcx,
                         type::infer::System &sys,
                         Module &mod,
                         const std::string &fileName = "module.crp",
                         const std::string &fileDir = ".");
}
