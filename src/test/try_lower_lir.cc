#include "../ir/tok/Parser.h"
#include "../ir/ast/Lowering.h"
#include "../ir/hir/Infer.h"
#include "../ir/hir/Lowering.h"

#include <iostream>

int main() {
  auto lexed = tok::lexer().lex(std::cin);
  err::ErrorPrintContext epc(lexed.stream.lines, std::cerr);
  auto parsed = tok::parser::parseProgram(lexed);
  err::maybeAbort(epc, parsed.errors);
  auto hir = ast::lower::lowerVisitor()->visitProgram(parsed.program);
  err::maybeAbort(epc, hir.errors);
  auto types = hir::infer::inferenceVisitor()->visitProgram(hir.program);
  err::maybeAbort(epc, types.errors);
  auto lir = hir::lower::loweringVisitor(types)->visitProgram(hir.program);
}
