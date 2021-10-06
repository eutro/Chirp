#include "InferenceGraph.h"

namespace type::infer {
  TVar &InferenceGraph::add(Tcx &tcx, Idx &counter) {
    TVar &tv = add<TVar>();
    tv.ty = tcx.intern(Ty::Placeholder{counter++});
    return tv;
  }
  void Node::connectTo(Node &o) {
    outbound.insert(o.index);
    o.inbound.insert(index);
  }
}
