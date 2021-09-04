#include "../type/TypePrint.h"
#include "../ir/tok/Parser.h"
#include "../ir/ast/Lowering.h"
#include "../ir/hir/Infer.h"
#include <iostream>
#include <sstream>

int main() {
  auto lexed = tok::lexer().lex(std::cin);
  std::cerr << "Lexed" << std::endl;
  err::ErrorPrintContext epc(lexed.stream.lines, std::cerr);

  auto parsed = tok::parser::parseProgram(lexed);
  std::cerr << "Parsed" << std::endl;
  err::maybeAbort(epc, parsed.errors);

  auto hir = ast::lower::lowerVisitor()->visitProgram(parsed.program);
  std::cerr << "Lowered" << std::endl;
  err::maybeAbort(epc, hir.errors);

  auto infer = hir::infer::inferenceVisitor()->visitProgram(hir.program);
  std::cerr << "Inferred" << std::endl;
  err::maybeAbort(epc, infer.errors);

  for (auto &block : infer.insts) {
    hir::Idx i = 0;
    for (auto &inst : block.second.exprTypes) {
      std::cerr << "Instantiation #" << i++ << std::endl;
      for (auto &e : inst) {
        if (!e.first->span) continue;
        std::stringstream ss;
        ss << "has type: " << e.second;
        epc << err::Location().span(*e.first->span, ss.str());
      }
    }
  }
}
