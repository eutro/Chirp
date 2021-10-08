#include "../ir/tok/Parser.h"
#include "../ir/ast/Lowering.h"
#include "../ir/hir/Infer.h"
#include "../ir/hir/RecurVisitor.h"
#include "../type/TypePrint.h"
#include "../type/infer/InferenceSeq.h"

#include <iostream>
#include <variant>
#include <sstream>

using namespace type::infer;

class TyPrinter : public hir::RecurVisitor<
    std::monostate,
    type::infer::Instantiation,
    Idx
> {
public:
  err::ErrorContext ecx;
  std::monostate visitExpr(
      hir::Expr &expr,
      type::infer::Instantiation &inst,
      Idx &counter
  ) override {
    if (expr.span) {
      auto ty = inst.typeVars.at(counter++);
      std::stringstream ss;
      ss << "has type: " << ty;
      ecx.err().span(*expr.span, ss.str());
    } else {
      ++counter;
    }
    return RecurVisitor::visitExpr(expr, inst, counter);
  }
};

void printBlock(
    err::ErrorPrintContext &epc,
    type::infer::InferContext &icx,
    const InferenceSeq *seq,
    hir::Block &block
) {
  auto found = icx.insts.find(seq);
  if (found == icx.insts.end()) return;
  Idx instNo = 1;
  for (auto &inst : found->second) {
    std::cerr << "** Instantiation #" << instNo++ << "\n";
    Idx counter = 0;
    TyPrinter tp;
    tp.visitBlock(block, inst.second, counter);
    for (auto &loc : tp.ecx.errors) {
      epc << loc;
    }
  }
}

int main() {
  auto lexed = tok::lexer().lex(std::cin);
  err::ErrorPrintContext epc(lexed.stream.lines, std::cerr);
  auto parsed = tok::parser::parseProgram(lexed);
  err::maybeAbort(epc, parsed.errors);
  auto hir = ast::lower::lowerVisitor()->visitProgram(parsed.program);
  err::maybeAbort(epc, hir.errors);
  type::TTcx ttcx;
  auto types = hir::infer::inferenceVisitor(ttcx)->visitProgram(hir.program);

  type::infer::InferContext icx(ttcx);
  icx.traits = std::move(types.traits);
  type::infer::Env env;
  auto &topSeq = *types.seqs.front();
  type::infer::runInEnv(icx, env, topSeq, icx.insts[&topSeq][{}]);
  err::maybeAbort(epc, icx.ecx);

  auto seqI = types.seqs.begin();
  std::cerr << "* Top\n";
  printBlock(epc, icx, seqI++->get(), hir.program.topLevel);
  for (auto &ti : hir.program.traitImpls) {
    std::cerr << "* Trait\n";
    for (auto &m : ti.methods) {
      printBlock(epc, icx, seqI++->get(), m);
    }
  }
}
