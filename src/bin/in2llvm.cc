#include "../ir/tok/Parser.h"
#include "../ir/ast/Lowering.h"
#include "../ir/hir/Infer.h"
#include "../ir/hir/Lowering.h"
#include "../ir/lir/codegen/Codegen.h"

#include <llvm/Support/raw_ostream.h>

#include <iostream>

int main() {
  auto lexed = tok::lexer().lex(std::cin);
  err::ErrorPrintContext epc(lexed.stream.lines, std::cerr);
  auto parsed = tok::parser::parseProgram(lexed);
  err::maybeAbort(epc, parsed.errors);
  auto hir = ast::lower::lowerVisitor()->visitProgram(parsed.program);
  err::maybeAbort(epc, hir.errors);
  type::Tcx tcx;
  auto types = hir::infer::inferenceVisitor(tcx)->visitProgram(hir.program);

  type::infer::Env env{std::move(types.table)};
  type::infer::addInsns(*env.table);
  type::infer::ENV = &env;
  try {
    types.root({}, {});
  } catch (err::LocationError &e) {
    err::ErrorContext ecx;
    e.addToContext(ecx);
    err::maybeAbort(epc, ecx);
  }

  // err::maybeAbort(epc, icx.ecx);
  auto lir = hir::lower::loweringVisitor()->visitProgram(hir.program);
  char *fileName = std::getenv("CRP_FILENAME");
  char *fileDir = std::getenv("CRP_FILEDIR");
  auto llvmRes = lir::codegen::generate(*types.insts,
                                        lir.module,
                                        fileName ? fileName : "module.crp",
                                        fileDir ? fileDir : ".");
  llvmRes.mod->print(llvm::outs(), nullptr);
}
