#pragma once

#include "../Lir.h"
#include "../../../type/infer/Public.h"

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

namespace lir::codegen {
  using TypeInst = type::infer::Inst;
  struct CodegenResult {
    std::unique_ptr<llvm::LLVMContext> ctx;
    std::unique_ptr<llvm::Module> mod;
  };

  CodegenResult generate(TypeInst::Set &insts,
                         Module &mod,
                         const std::string &fileName = "module.crp",
                         const std::string &fileDir = ".");
}
