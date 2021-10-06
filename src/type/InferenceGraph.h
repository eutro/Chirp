#pragma once

#include "Type.h"

#include "../common/Err.h"

#include <vector>
#include <set>

namespace type::infer {
  struct NodeRef {
    Idx graph, index;
    bool operator<(const NodeRef &o) const;
  };

  struct Node {
    virtual ~Node() = default;
    err::Location desc;
    std::set<NodeRef> inbound, outbound;
    NodeRef ref;
    void connectTo(Node &o);
  };

  struct TVar;

  struct Constraint : Node {
    struct Concrete;
    struct Trait;
    struct Assigned;
  };

  struct InferenceGraph {
    Idx index;
    arena::Arena<Node> nodes;
    template <typename T>
    T &add() {
      auto nIndex = nodes.ptrs.size();
      T &n = *nodes.cast<T>().add();
      n.ref.graph = index;
      n.ref.index = nIndex;
      return n;
    }
    TVar &add(Tcx &tcx, Idx &counter);
  };

  struct Constraint::Concrete : Constraint {
    Tp tyA, tyB;
  };

  struct Constraint::Trait : Constraint {
    Tp ty;
    TraitBound *tb;
  };

  struct Constraint::Assigned : Constraint {
    Tp toTy, fromTy;
  };

  struct TVar : Node {
    Tp ty;
  };
}
