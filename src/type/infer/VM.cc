#include <stack>
#include "VM.h"

namespace type::infer {
  thread_local Env *ENV;

  std::vector<Tp> InsnList::operator()(const std::vector<Tp> &args, const std::vector<Constant> &) const {
    std::cerr << "\n\nCurrently evaluating:\n" << *this;
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
          if (*ref.insn < rets.size()) {
            source = &rets.at(*ref.insn);
          } else {
            throw std::runtime_error("ICE: Invocation of non-topological InsnList");
          }
        } else {
          source = &args;
        }
        if (ref.retIdx < source->size()) {
          insnArgs.push_back(source->at(ref.retIdx));
        } else {
          throw std::runtime_error("ICE: Variable reference out of bounds");
        }
      }
      auto &fn = *ENV->table->lookupFn(insn.key, insn.constants, insnArgs);
      rets.push_back(fn(insnArgs, insn.constArgs));
    }
    return rets.at(retInsn);
  }

  void InsnList::topSort(const InsnList::SccCollapser &collapse) {
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
      node.onStack = true;
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
      collapse(*this, outScc);
      outScc.clear();
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
    retInsn = nodes.at(retInsn).index;
    insns = std::move(outInsns);
  }

  std::ostream &operator<<(std::ostream &os, const InsnList &list) {
    Idx i = 0;
    for (const Insn &insn : list.insns) {
      os << "$" << std::dec << i++ << " = " << insn << "\n";
    }
    os << "return $" << std::dec << list.retInsn << "\n";
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
    if (insn.reason || insn.src) {
      os << " #";
      if (insn.src) {
        os << " (" << insn.src->lo << " - " << insn.src->hi << ")";
      }
      if (insn.reason) {
        os << " " << *insn.reason;
      }
    }
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const VarRef &var) {
    os << std::dec;
    if (var.insn) {
      os << "$" << *var.insn;
    } else {
      os << "$@";
    }
    os << "[" << var.retIdx << "]";
    return os;
  }
  bool VarRef::operator<(const VarRef &rhs) const {
    if (insn < rhs.insn)
      return true;
    if (rhs.insn < insn)
      return false;
    return retIdx < rhs.retIdx;
  }
  bool VarRef::operator>(const VarRef &rhs) const {
    return rhs < *this;
  }
  bool VarRef::operator<=(const VarRef &rhs) const {
    return !(rhs < *this);
  }
  bool VarRef::operator>=(const VarRef &rhs) const {
    return !(*this < rhs);
  }
  bool VarRef::operator==(const VarRef &rhs) const {
    return insn == rhs.insn &&
           retIdx == rhs.retIdx;
  }
  bool VarRef::operator!=(const VarRef &rhs) const {
    return !(rhs == *this);
  }
}
