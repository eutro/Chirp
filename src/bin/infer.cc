#include "../ir/tok/Parser.h"
#include "../ir/ast/Lowering.h"
#include "../ir/hir/Infer.h"
#include "../type/TypePrint.h"
#include "../ir/hir/RecurVisitor.h"

#include <iostream>

using namespace type::infer;

class ExprPrinter : public hir::RecurVisitor<std::monostate, Idx> {
public:
  err::ErrorPrintContext &epc;
  type::infer::Inst::Val &inst;
  ExprPrinter(err::ErrorPrintContext &epc, Inst::Val &inst) : epc(epc), inst(inst) {}
  std::monostate visitExpr(hir::Expr &it, Idx &i) override {
    Idx idx = i++;
    RecurVisitor::visitExpr(it, i);
    if (it.span) {
      type::Tp ty = inst.loggedTys.at(idx);
      err::Location err;
      err.span(*it.span, util::toStr("has type: ", ty, " (", idx, ")"));
      if (inst.loggedRefs.count(idx)) {
        std::pair<Inst::EntityIdx, Inst::ValIdx> &ref = inst.loggedRefs.at(idx);
        err.msg(util::toStr("and calls [[*Inst ", ref.first, ":", ref.second, "][", ref.first, ":", ref.second, "]]"));
      }
      epc << err;
    }
    return {};
  }
};

class VarPrinter : public hir::RecurVisitor<std::monostate, Idx> {
public:
  hir::Program &prog;
  err::ErrorPrintContext &epc;
  type::infer::Inst::Val &inst;
  VarPrinter(hir::Program &prog, err::ErrorPrintContext &epc, Inst::Val &inst) : prog(prog), epc(epc), inst(inst) {}
  void visitBlock(hir::Block &it, Idx &idx) override {
    for (auto &b : it.bindings) {
      hir::Definition &def = prog.bindings.at(b);
      if (def.source) {
        epc << err::Location().span(*def.source, util::toStr("var type: ", inst.loggedTys.at(idx), " (", idx, ")"));
      }
      idx++;
    }
    RecurVisitor::visitBlock(it, idx);
  }
};

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
  types.root({}, {});

  err::ErrorPrintContext stdoutEpc(epc.sourceLines, std::cout);
  auto printBlock = [&](hir::Block &b) {
    for (auto &i : types.insts->entities.at(*b.idx)) {
      std::cout << "** Inst " << *b.idx << ":" << i.first << "\n";
      ExprPrinter printer(stdoutEpc, i.second);
      VarPrinter varPrinter(hir.program, stdoutEpc, i.second);
      Idx counter = 0;
      printer.visitBlock(b, counter);
      varPrinter.visitBlock(b, counter);
    }
  };

  std::cout << "* Top Level\n";
  printBlock(hir.program.topLevel);
  for (auto &ti : hir.program.traitImpls) {
    for (auto &b : ti.methods) {
      if (types.insts->entities.count(*b.idx)) {
        std::cout << "* Block " << *b.idx << "\n";
        printBlock(b);
      }
    }
  }
}
