#include <stack>
#include "VM.h"

namespace type::infer {
  thread_local Env *ENV;

  std::vector<Tp> InsnList::operator()(const std::vector<Tp> &args, const std::vector<Constant> &) const {
    if (insns.empty()) return args;
    std::vector<std::vector<Tp>> rets;
    rets.reserve(insns.size());
    std::vector<Tp> insnArgs;
    for (auto &insn : insns) {
      insnArgs.clear();
      insnArgs.reserve(insn.inputs.size());
      for (auto ref : insn.inputs) {
        const std::vector<Tp> *source;
        if (ref.insn) {
          if (rets.size() < *ref.insn) {
            source = &rets.at(*ref.insn);
          } else {
            throw std::runtime_error("ICE: Invocation of non-topological InsnList");
          }
        } else {
          source = &args;
        }
        insnArgs.push_back(source->at(ref.retIdx));
      }
      auto &fn = *ENV->table->lookupFn(insn.key, insn.constants, insnArgs);
      rets.push_back(fn(insnArgs, insn.constArgs));
    }
    return rets.back();
  }

  void InsnList::topSort(InsnList::SccCollapser collapse) {
    struct Node {
      Insn *insn;
      std::vector<Node *> outEdges;
      Idx index = 0;
      Idx lowLink = 0;
      bool onStack = false;
      Node(Insn *insn): insn(insn) {}
    };
    std::vector<std::vector<Node*>> sccs;
    std::vector<Node> nodes;
    nodes.reserve(insns.size());
    for (Insn &insn : insns) {
      nodes.emplace_back(&insn);
    }
    for (Node &node : nodes) {
      for (const VarRef &vr : node.insn->inputs) {
        if (vr.insn) {
          nodes.at(*vr.insn).outEdges.push_back(&node);
        }
      }
    }

    std::stack<Node*> stack;
    Idx index = 1;
    std::function<void(Node&)> connect = [&](Node &node) {
      node.lowLink = node.index = index++;
      stack.push(&node);
      for (Node *outNode : node.outEdges) {
        if (!outNode->index) {
          connect(*outNode);
          node.lowLink = std::min(node.lowLink, outNode->lowLink);
        } else if (outNode->onStack) {
          node.lowLink = std::min(node.lowLink, outNode->index);
        }
      }
      if (node.lowLink == node.index) {
        auto &scc = sccs.emplace_back();
        Node *sccNode;
        do {
          sccNode = stack.top();
          stack.pop();
          sccNode->onStack = false;
          scc.push_back(sccNode);
        } while (sccNode != &node);
      }
    };

    for (auto &node : nodes) {
      if (node.index) continue;
      connect(node);
    }

    std::vector<Insn*> outScc;
    for (auto &scc : sccs) {
      outScc.reserve(scc.size());
      for (const auto &node : scc) {
        outScc.push_back(node->insn);
      }
      collapse(outScc);
    }
    std::vector<Insn> outInsns;
    outInsns.reserve(insns.size());
    for (auto it = sccs.rbegin(); it != sccs.rend(); ++it) {
      for (Node *node : *it) {
        node->index = outInsns.size();
        outInsns.push_back(*node->insn);
      }
    }
    for (Insn &insn : outInsns) {
      for (VarRef &vr : insn.inputs) {
        if (vr.insn) {
          vr.insn = nodes.at(*vr.insn).index;
        }
      }
    }
    insns = std::move(outInsns);
  }

  std::ostream &operator<<(std::ostream &os, const InsnList &list) {
    Idx i = 0;
    for (const Insn &insn : list.insns) {
      os << "$" << i++ << " = " << insn << "\n";
    }
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const Insn &insn) {
    os << insn.key->value;
    if (!insn.constants.empty()) {
      os << "<";
      for (auto it = insn.constants.begin();;) {
        os << *it;
        if (++it == insn.constants.end()) break;
        os << ", ";
      }
      os << ">";
    }
    if (!insn.constArgs.empty()) {
      os << "[";
      for (auto it = insn.constArgs.begin();;) {
        os << *it;
        if (++it == insn.constArgs.end()) break;
        os << ", ";
      }
      os << "]";
    }
    os << "(";
    if (!insn.inputs.empty()) {
      for (auto it = insn.inputs.begin();;) {
        os << *it;
        if (++it == insn.inputs.end()) break;
        os << ", ";
      }
    }
    os << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const VarRef &var) {
    if (var.insn) {
      os << "$" << *var.insn;
    } else {
      os << "$@";
    }
    os << "[" << var.retIdx << "]";
    return os;
  }
}
