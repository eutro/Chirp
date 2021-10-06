#include "../ir/tok/Parser.h"
#include "../ir/ast/Lowering.h"
#include "../ir/hir/Infer.h"
#include "../ir/hir/Lowering.h"
#include "../ir/lir/Disas.h"
#include "../type/TypePrint.h"
#include "../type/infer/InferenceSeq.h"

#include <deque>
#include <iostream>

using namespace type::infer;

void printStep(err::ErrorPrintContext &epc, const Step &step, Idx idx) {
  std::cerr << "*** Step " << idx << "\n";
  std::visit(overloaded{
      [](const Step::Assign &it) {
        std::cerr << "Assigned:\n";
        std::cerr << it.toTy << " <- " << it.fromTy << "\n";
      },
      [](const Step::Unify &it) {
        std::cerr << "Concrete:\n";
        std::cerr << it.tyA << " == " << it.tyB << "\n";
      },
      [](const Step::ImplTrait &it) {
        std::cerr << "Trait:\n";
        std::cerr << it.ty << " => " << it.trait << "\n";
      },
  }, step.v);
  epc << step.desc;
}

int main() {
  auto lexed = tok::lexer().lex(std::cin);
  err::ErrorPrintContext epc(lexed.stream.lines, std::cerr);
  auto parsed = tok::parser::parseProgram(lexed);
  err::maybeAbort(epc, parsed.errors);
  auto hir = ast::lower::lowerVisitor()->visitProgram(parsed.program);
  err::maybeAbort(epc, hir.errors);
  auto types = hir::infer::inferenceVisitor()->visitProgram(hir.program);
  for (auto graph : types.graphs) {
    InferenceSeq seq = graph;
    std::cerr << "* Graph #" << (graph.index + 1) << "\n";
    std::cerr << "** Free Variables\n";
    for (const auto &free : seq.freeVars) {
      std::cerr << "*** Free #" << free.first.graph << ":" << free.first.index << "\n";
      std::cerr << "Type: " << free.second.ty << "\n";
      epc << free.second.desc;
    }
    std::cerr << "** Bound Variables\n";
    for (const auto &bound : seq.vars) {
      std::cerr << "*** Bound #" << graph.index << ":" << bound.first << "\n";
      std::cerr << "Type: " << bound.second.ty << "\n";
      epc << bound.second.desc;
    }
    std::cerr << "** Steps\n";
    Idx idx = 1;
    for (const auto &step : seq.steps) {
      printStep(epc, step, idx++);
    }
  }
}
