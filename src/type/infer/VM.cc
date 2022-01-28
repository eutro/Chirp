#include "VM.h"
#include "Insns.h"

#include <sstream>
#include <stack>
#include <map>

#ifndef CHIRP_MAX_STACK_DEPTH
#define CHIRP_MAX_STACK_DEPTH 100
#endif

namespace type::infer {
  thread_local Env *ENV;

  std::vector<Tp> InsnList::operator()(const std::vector<Tp> &args, const std::vector<Constant> &) const {
    // std::cerr << "\n\nCurrently evaluating:\n" << *this;
    if (insns.empty()) return args;
    std::vector<std::vector<Tp>> rets;
    rets.reserve(insns.size());
    std::vector<Tp> insnArgs;
    if (ENV->stack.size() >= CHIRP_MAX_STACK_DEPTH) {
      throw std::runtime_error("Exceeded max recursion depth\n");
    }
    ENV->stack.push_back(nullptr);
    for (const Insn &insn : insns) {
      ENV->stack.back() = &insn;
      insnArgs.clear();
      insnArgs.reserve(insn.inputs.size());
      try {
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
      } catch (std::runtime_error &e) {
        std::cerr << "Thrown at: " << insn << "\n";
        throw e;
      }
    }
    ENV->stack.pop_back();
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

  void InsnList::opt() {
    std::vector<Insn> outInsns;
    std::map<VarRef, VarRef> mappedRelocations;
    std::map<VarRef, VarRef> unmappedRelocations;
    std::map<Idx, Idx> insnRelocations;
    std::vector<Constant> logIdcs;
    std::vector<VarRef> logVars;

    // intern all constant constructions within the block
    std::set<Tp> fullTys; // set of all types that include no free variables
    for (auto &insn : insns) {
      if (insn.inputs.empty() && insn.key == ConstructInsn::key()) {
        for (Constant &cnst : insn.constArgs) {
          fullTys.insert(constant_cast<Tp>(cnst));
        }
      }
    }

    if (!fullTys.empty()) {
      Idx tyId = 0;
      std::map<Tp, Idx> fullTyIdcs;
      std::vector<Constant> tys(fullTys.begin(), fullTys.end());
      for (Tp ty : fullTys) {
        fullTyIdcs[ty] = tyId++;
      }
      outInsns.push_back(Insn(ConstructInsn::key(), {}, {}, std::move(tys)));
      {
        Idx insnIdx = 0;
        for (auto &insn : insns) {
          if (insn.inputs.empty() && insn.key == ConstructInsn::key()) {
            Idx retIdx = 0;
            for (Constant &cnst : insn.constArgs) {
              mappedRelocations.emplace(
                VarRef(insnIdx, retIdx++),
                VarRef(0, fullTyIdcs.at(constant_cast<Tp>(cnst)))
              );
            }
          }
          insnIdx++;
        }
      }
    }

    // next step is identity elimination
    {
      Idx insnIdx = 0;
      for (auto &insn : insns) {
        if (insn.key == IdentityInsn::key() && insnIdx != retInsn) {
          for (Idx retIdx = 0; retIdx < insn.inputs.size(); ++retIdx) {
            unmappedRelocations.emplace(VarRef(insnIdx, retIdx), insn.inputs.at(retIdx));
          }
        } else if (insn.key == DeConstructInsn::key()) {
          Tp ty = constant_cast<Tp>(insn.constArgs.at(0));
          if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
            unmappedRelocations.emplace(
              VarRef(insnIdx, 0),
              insn.inputs.at(0)
            );
          }
        } else if (insn.key == ConstructInsn::key()) {
          for (Idx retIdx = 0; retIdx < insn.constArgs.size(); ++retIdx) {
            Tp ty = constant_cast<Tp>(insn.constArgs.at(retIdx));
            if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
              Idx i = std::get<Ty::Placeholder>(ty->v).i;
              unmappedRelocations.emplace(
                VarRef(insnIdx, retIdx),
                insn.inputs.at(i)
              );
            }
          }
        }
        insnIdx++;
      }
    }

    std::set<Idx> liveInsns;
    // garbage collect insns
    // the "roots" are the output insn and any insn with side effects
    {
      std::set<LookupKey *> knownPure
        {IdentityInsn::key(),
         ConstructInsn::key(),
         UnionInsn::key(),
         // DeconstructInsn's can have side effects
         // but CheckInsn's are made for instances where
         // the deconstruction may be fallible
         DeConstructInsn::key()};
      std::deque<Idx> q{};
      for (Idx i = 0; i < insns.size(); ++i) {
        if (i != retInsn && knownPure.count(insns[i].key)) continue;
        q.push_back(i);
        liveInsns.insert(i);
      }
      while (!q.empty()) {
        Idx i = q.front();
        q.pop_front();
        for (VarRef &vr : insns[i].inputs) {
          while (unmappedRelocations.count(vr)) vr = unmappedRelocations.at(vr);
          if (!vr.insn || liveInsns.count(*vr.insn) ||
              mappedRelocations.count(vr))
            continue;
          q.push_back(*vr.insn);
          liveInsns.insert(*vr.insn);
        }
      }
    }

    // compact the insn list
    for (Idx i : liveInsns) {
      Insn &insn = insns[i];
      for (VarRef &vr : insn.inputs) {
        if (mappedRelocations.count(vr)) {
          vr = mappedRelocations.at(vr);
        } else if (vr.insn) {
          vr.insn = insnRelocations.at(*vr.insn);
        }
      }
      if (insn.key == CheckInsn::key()) {
        bool allSame = true;
        VarRef firstVr = insn.inputs.at(0);
        for (VarRef &vr : insn.inputs) {
          if (vr != firstVr) {
            allSame = false;
            break;
          }
        }
        if (allSame) continue;
      } else if (insn.key == LogInsn::key()) {
        if (insn.inputs.size() != insn.constArgs.size()) {
          throw std::runtime_error("LogInsn inputs/constArgs length mismatch");
        }
        std::copy(insn.constArgs.begin(), insn.constArgs.end(), std::back_inserter(logIdcs));
        std::copy(insn.inputs.begin(), insn.inputs.end(), std::back_inserter(logVars));
        continue;
      }
      insnRelocations[i] = (Idx) outInsns.size();
      outInsns.push_back(insn);
    }
    if (logIdcs.size()) {
      outInsns.push_back(Insn(LogInsn::key(), {}, std::move(logVars), std::move(logIdcs)));
    }

    insns = outInsns;
    retInsn = insnRelocations.at(retInsn);
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
