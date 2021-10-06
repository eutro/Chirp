#include "InferenceGraph.h"

namespace type::infer {
  bool NodeRef::operator<(const NodeRef &o) const {
    return std::make_pair(graph, index) < std::make_pair(o.graph, o.index);
  }
  TVar &InferenceGraph::add(Tcx &tcx, Idx &counter) {
    TVar &tv = add<TVar>();
    tv.ty = tcx.intern(Ty::Placeholder{counter++});
    return tv;
  }
  void Node::connectTo(Node &o) {
    outbound.insert(o.ref);
    o.inbound.insert(ref);
  }
}
