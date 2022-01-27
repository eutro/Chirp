#include "../ir/tok/Parser.h"
#include "../ir/ast/Lowering.h"
#include "../ir/hir/Infer.h"
#include "../type/TypePrint.h"

#include <iostream>

using namespace type::infer;

int main() {
  auto lexed = tok::lexer().lex(std::cin);
  err::ErrorPrintContext epc(lexed.stream.lines, std::cerr);
  auto parsed = tok::parser::parseProgram(lexed);
  err::maybeAbort(epc, parsed.errors);
  auto hir = ast::lower::lowerVisitor()->visitProgram(parsed.program);
  err::maybeAbort(epc, hir.errors);
  type::Tcx ttcx;
  auto types = hir::infer::inferenceVisitor(ttcx)->visitProgram(hir.program);

  type::infer::Env env{std::move(types.table)};
  type::infer::addInsns(*env.table);
  type::infer::ENV = &env;
  types.root({}, {});

  for (auto &e : types.insts->entities) {
    std::cout << "* Block " << e.first << "\n";
    for (auto &i : e.second) {
      std::cout << "** Inst " << i.first << "\n";
      auto &v = i.second;
      for (auto &ex : v.loggedTys) {
        std::cout << "- Expr " << ex.first << " - " << ex.second << "\n";
      }
    }
  }
}
