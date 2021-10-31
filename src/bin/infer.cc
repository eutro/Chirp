#include "../ir/tok/Parser.h"
#include "../ir/ast/Lowering.h"
#include "../ir/hir/Infer.h"
#include "../ir/hir/RecurVisitor.h"
#include "../type/TypePrint.h"
#include "../type/infer/Public.h"

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
    type::infer::System &sys,
    Idx blockIdx,
    hir::Block &block
) {
  std::cerr << "*** Block #" << blockIdx << "\n";
  Idx instCounter = 0;
  for (auto &inst : sys.seqs.at(blockIdx).insts) {
    std::cerr << "**** Instantiation #" << ++instCounter << "\n";
    Idx counter = 0;
    TyPrinter tp;
    tp.visitBlock(block, inst, counter);
    for (auto &loc : tp.ecx.errors) {
      epc << loc;
    }
  }
}

struct LinkFor {
  type::Ty *ty;
  LinkFor(type::Ty *ty) : ty(ty) {}
  friend std::ostream &operator<<(std::ostream &os, const LinkFor &it) {
    if (std::holds_alternative<type::Ty::Placeholder>(it.ty->v)) {
      os << "[[Var " << it.ty << "][" << it.ty << "]]";
    } else {
      os << it.ty;
    }
    return os;
  }
};

void printStep(err::ErrorPrintContext &epc, const Step &step) {
  std::visit(overloaded{
      [](const Step::Assign &it) {
        std::cerr << "Assigned:\n";
        std::cerr << LinkFor(it.toTy);
        for (auto ty : it.fromTy) {
          std::cerr << " <- " << LinkFor(ty) << "\n";
        }
      },
      [](const Step::Unify &it) {
        std::cerr << "Concrete:\n";
        std::cerr << LinkFor(it.tyA) << " == " << LinkFor(it.tyB) << "\n";
      },
      [](const Step::ImplTrait &it) {
        std::cerr << "Trait:\n";
        std::cerr << LinkFor(it.ty) << " => " << it.trait << "\n";
      },
  }, step.v);
  epc << step.desc;
}

void printSeq(err::ErrorPrintContext &epc, const InferenceSeq &seq) {
  std::cerr << "*** Vars\n";
  for (const auto &var : seq.vars) {
    std::cerr << "**** Var " << var.second.ty << "\n";
    epc << var.second.desc;
  }
  Idx idx = 1;
  std::cerr << "*** Steps\n";
  for (const auto &step : seq.steps) {
    std::cerr << "**** Step " << idx++ << "\n";
    printStep(epc, step);
    idx++;
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

  std::cerr << "* Steps\n";
  for (const auto &block : types.sys.seqs) {
    std::cerr << "** Seq\n";
    printSeq(epc, block.seq);
  }

  std::cerr << "* Errors\n";
  type::infer::SolveCtx icx(ttcx);
  type::infer::solveSystem(types.sys, icx, {0});
  err::printErrors(epc, icx.ecx);

  Idx seqI = 0;
  std::cerr << "* Types\n";
  std::cerr << "** Top\n";
  printBlock(epc, types.sys, seqI++, hir.program.topLevel);
  for (auto &ti : hir.program.traitImpls) {
    std::cerr << "** Trait\n";
    for (auto &m : ti.methods) {
      printBlock(epc, types.sys, seqI++, m);
    }
  }
}
