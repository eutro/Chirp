#include "../ir/tok/Parser.h"
#include "../ir/ast/Lowering.h"
#include "../ir/hir/Infer.h"
#include "../ir/hir/Lowering.h"
#include "../ir/lir/Disas.h"
#include "../type/TypePrint.h"

#include <deque>
#include <iostream>

using namespace type::infer;

void printNode(err::ErrorPrintContext &epc, Node *node) {
  std::cerr << "*** Task " << node->ref.graph << ":" << node->ref.index << "\n";
  if (auto tv = dynamic_cast<TVar*>(node)) {
    std::cerr << "Type variable (" << tv->ty << "):\n";
  } else if (auto cnstr = dynamic_cast<Constraint::Assigned*>(node)) {
    std::cerr << "Assigned:\n";
    std::cerr << cnstr->toTy << " <- " << cnstr->fromTy << "\n";
  } else if (auto cnstr = dynamic_cast<Constraint::Concrete*>(node)) {
    std::cerr << "Concrete:\n";
    std::cerr << cnstr->tyA << " == " << cnstr->tyB << "\n";
  } else if (auto cnstr = dynamic_cast<Constraint::Trait*>(node)) {
    std::cerr << "Trait:\n";
    std::cerr << cnstr->ty << " => " << cnstr->tb << "\n";
  }
  epc << node->desc;
  if (node->inbound.empty()) {
    std::cerr << "No dependencies.\n";
  } else {
    std::cerr << "Depends on:\n";
    for (auto it = node->inbound.begin();;) {
      std::cerr << "[[* Task " << it->graph << ":" << it->index
                << "][" << it->graph << ":" << it->index << "]]";
      if (++it == node->inbound.end()) {
        break;
      }
      std::cerr << ", ";
    }
    std::cerr << "\n";
  }
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
    // solve the DAG
    std::map<Node*, std::set<NodeRef>> deps;
    std::deque<Node*> ready;
    for (auto &n : graph.nodes.ptrs) {
      std::set<NodeRef> ds = n->inbound;
      for (auto it = ds.begin(); it != ds.end();) {
        if (it->graph != graph.index) {
          it = ds.erase(it);
        } else {
          ++it;
        }
      }
      if (ds.empty()) {
        ready.push_back(n.get());
      } else {
        deps[n.get()] = std::move(ds);
      }
    }
    std::cerr << "* Graph #" << graph.index << "\n";
    std::cerr << "** Ordering\n";
    while (!ready.empty()) {
      Node *next = ready.front();
      ready.pop_front();
      printNode(epc, next);
      for (auto o : next->outbound) {
        Node *out = types.graphs.at(o.graph).nodes.ptrs.at(o.index).get();
        if (deps.count(out)) {
          auto &ds = deps.at(out);
          ds.erase(next->ref);
          if (ds.empty()) {
            deps.erase(out);
            ready.push_back(out);
          }
        }
      }
    }
    if (deps.empty()) {
    } else {
      std::cerr << "** Unsolved\n";
      for (auto &e : deps) {
        printNode(epc, e.first);
      }
      std::cerr << "\n";
    }
  }
}
