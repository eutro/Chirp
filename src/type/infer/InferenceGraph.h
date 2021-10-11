#pragma once

#include "../Type.h"

#include "../../common/Err.h"

#include <vector>
#include <set>

namespace type::infer {
  struct NodeRef {
    Idx graph, index;
    bool operator<(const NodeRef &o) const;
  };

  struct TVar;
  struct Constraint {
    virtual ~Constraint() = default;
    struct Concrete;
    struct Trait;
    struct Assigned;
  };

  using NodeVariant = std::variant<
      const Constraint::Concrete *,
      const Constraint::Trait *,
      const Constraint::Assigned *,
      const TVar *
  >;

  struct Node {
    virtual ~Node() = default;
    virtual NodeVariant asVariant() const = 0;
    err::Location desc;
    std::set<NodeRef> inbound, outbound;
    NodeRef ref;
    void connectTo(Node &o);
  };

  struct InferenceGraph {
    Idx index;
    arena::Arena<Node> nodes;
    template<typename T>
    T &add() {
      auto nIndex = nodes.ptrs.size();
      T &n = *nodes.cast<T>().add();
      n.ref.graph = index;
      n.ref.index = nIndex;
      return n;
    }
    TVar &add(Tcx &tcx, Idx &counter);
  };

  struct Constraint::Concrete : Node, Constraint {
    Tp tyA, tyB;
    NodeVariant asVariant() const override;
  };

  struct Constraint::Trait : Node, Constraint {
    Tp ty;
    TraitBound *tb;
    Idx idx;
    NodeVariant asVariant() const override;
  };

  struct Constraint::Assigned : Node, Constraint {
    Tp toTy;
    std::set<Tp> fromTy;
    NodeVariant asVariant() const override;
  };

  struct TVar : Node {
    Tp ty;
    NodeVariant asVariant() const override;
  };
}
