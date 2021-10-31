#include <algorithm>
#include <functional>
#include <stack>
#include <stdexcept>
#include <utility>
#include <variant>

#include "InferenceSeq.h"

namespace type::infer {
  /**
   * Find all strongly connected components of the graph, via Tarjan's algorithm.
   *
   * A property of Tarjan's algorithm is that the SCCs are yielded in a reverse topological order,
   * so the vector returned is in such an order.
   */
  std::vector<std::set<Idx>> findSCCs(const InferenceGraph &graph) {
    struct TNode {
      Idx graphIndex, index, lowLink;
      bool onStack, seen = false;
    };
    std::vector<std::set<Idx>> SCCs;
    std::vector<TNode> tNodes(graph.nodes.ptrs.size());
    std::stack<TNode *> stack;
    Idx idx = 0;
    std::function<void(TNode &, const Node &)> connect = [&](TNode &node, const Node &gNode) {
      node.seen = true;
      node.graphIndex = gNode.ref.index;
      node.lowLink = node.index = idx++;
      stack.push(&node);
      node.onStack = true;
      for (const NodeRef &succ : gNode.outbound) {
        if (succ.graph != graph.index) continue;
        const Node &sNode = *graph.nodes.ptrs.at(succ.index);
        TNode &tN = tNodes.at(sNode.ref.index);
        if (!tN.seen) {
          // unvisited successor, visit it
          connect(tN, sNode);
          // if an even lower node is reachable from the successor,
          // then it is also reachable from here
          node.lowLink = std::min(node.lowLink, tN.lowLink);
        } else if (tN.onStack) {
          // the successor has been seen and is still on the stack;
          // the successor is reachable from this node (obviously)
          // so update lowLink if applicable
          node.lowLink = std::min(node.lowLink, tN.index);
        }
      }
      if (node.lowLink == node.index) {
        // root node of an SCC
        std::set<Idx> scc;
        Idx popped;
        do {
          TNode &topNode = *stack.top();
          stack.pop();
          popped = topNode.graphIndex;
          scc.insert(popped);
          topNode.onStack = false;
        } while (popped != node.graphIndex);
        SCCs.push_back(std::move(scc));
      }
    };
    for (const auto &gNode : graph.nodes.ptrs) {
      TNode &tN = tNodes.at(gNode->ref.index);
      if (tN.seen) continue;
      connect(tN, *gNode);
    }
    return SCCs;
  }

  void appendNode(InferenceSeq &seq, Idx graphIdx, const Node &node) {
    auto ptr = std::visit(overloaded{
        [&](const TVar *tv) -> err::Location * {
          if (tv->ref.graph == graphIdx) {
            auto vi = &seq.vars[tv->ref.index];
            vi->ty = tv->ty;
            return &vi->desc;
          } else {
            return nullptr;
          }
        },
        [&](const Constraint::Concrete *c) {
          return &seq.steps.emplace_back(Step::Unify{c->tyA, c->tyB}).desc;
        },
        [&](const Constraint::Trait *c) {
          return &seq.steps.emplace_back(Step::ImplTrait{c->ty, c->tb, c->idx}).desc;
        },
        [&](const Constraint::Assigned *c) {
          return &seq.steps.emplace_back(Step::Assign{c->toTy, c->fromTy}).desc;
        },
    }, node.asVariant());
    if (ptr) *ptr = node.desc;
  }

  InferenceSeq::InferenceSeq(const InferenceGraph &graph) {
    auto SCCs = findSCCs(graph);
    for (auto it = SCCs.rbegin(); it != SCCs.rend(); ++it) {
      if (it->size() > 1) {
        for (Idx index : *it) {
          const Node &node = *graph.nodes.ptrs.at(index);
          auto variant = node.asVariant();
          if (std::holds_alternative<const Constraint::Trait*>(variant)) {
            throw std::runtime_error("illegal trait constraint within constraint cycle");
          }
        }
      }
      // within a single strongly connected component
      // the order of steps doesn't matter:tm:
      // since anything other than simple unification is forbidden
      for (Idx index : *it) {
        const Node &node = *graph.nodes.ptrs.at(index);
        appendNode(*this, graph.index, node);
      }
    }
  }
}
