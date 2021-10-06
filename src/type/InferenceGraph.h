#pragma once

#include "Type.h"

#include "../common/Err.h"

#include <vector>
#include <set>

namespace type::infer {
  struct Node {
    virtual ~Node() = default;
    err::Location desc;
    std::set<Idx> inbound, outbound;
    Idx index;
    void connectTo(Node &o);
  };

  struct TVar;

  struct Constraint : Node {
    struct Concrete;
    struct Trait;
    struct Assigned;
  };

  struct InferenceGraph {
    arena::Arena<Node> nodes;
    template <typename T>
    T &add() {
      auto index = nodes.ptrs.size();
      T &n = *nodes.cast<T>().add();
      n.index = index;
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
