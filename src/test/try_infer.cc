#include "../ir/tok/Parser.h"
#include "../ir/ast/Lowering.h"
#include "../ir/hir/Infer.h"
#include "../ir/hir/Lowering.h"
#include "../ir/lir/Disas.h"
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
  auto types = hir::infer::inferenceVisitor()->visitProgram(hir.program);
  for (auto graph : types.graphs) {
    for (auto &node : graph.nodes.ptrs) {
      std::cerr << "# Task " << node->index << ":\n";
      if (auto tv = dynamic_cast<TVar*>(node.get())) {
        std::cerr << "Type variable (" << tv->ty << "):\n";
      } else if (auto cnstr = dynamic_cast<Constraint::Assigned*>(node.get())) {
        std::cerr << "Assigned:\n";
        std::cerr << cnstr->toTy << " <- " << cnstr->fromTy << "\n";
      } else if (auto cnstr = dynamic_cast<Constraint::Concrete*>(node.get())) {
        std::cerr << "Concrete:\n";
        std::cerr << cnstr->tyA << " == " << cnstr->tyB << "\n";
      } else if (auto cnstr = dynamic_cast<Constraint::Trait*>(node.get())) {
        std::cerr << "Trait:\n";
        std::cerr << cnstr->ty << " => " << cnstr->tb << "\n";
      }
      epc << node->desc;
      if (node->inbound.empty()) {
        std::cerr << "No dependencies.\n";
      } else {
        std::cerr << "Depends on:\n";
        for (auto it = node->inbound.begin();;) {
          std::cerr << *it;
          if (++it == node->inbound.end()) {
            break;
          }
          std::cerr << ", ";
        }
        std::cerr << "\n";
      }
    }
  }
}
