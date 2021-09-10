#include "../type/TypePrint.h"
#include "../ir/tok/Parser.h"
#include "../ir/ast/Lowering.h"
#include "../ir/hir/Infer.h"

#include <sstream>

int main() {
  auto lexed = tok::lexer().lex(std::cin);
  err::ErrorPrintContext epc(lexed.stream.lines, std::cerr);

  auto parsed = tok::parser::parseProgram(lexed);
  err::maybeAbort(epc, parsed.errors);

  auto hir = ast::lower::lowerVisitor()->visitProgram(parsed.program);
  err::maybeAbort(epc, hir.errors);

  auto infer = hir::infer::inferenceVisitor()->visitProgram(hir.program);
  err::maybeAbort(epc, infer.errors);

  for (auto &block : infer.insts) {
    for (Idx i = 0; i < block.second.types.size(); ++i) {
      std::cerr << "Instantiation #" << (i + 1) << std::endl;
      for (auto &e : block.second.exprTypes) {
        if (!e.first->span) continue;
        std::stringstream ss;
        ss << "has type: " << block.second.types.at(i).at(e.second);
        epc << err::Location().span(*e.first->span, ss.str());
      }
    }
  }
}
