#include "RootVisitor.h"

void chirpVisitRoots(VisitFn visitor) {
  for (StackEntry *r = llvm_gc_root_chain; r; r = r->next) {
    unsigned i = 0;

    // For roots [0, NumMeta), the metadata pointer is in the FrameMap.
    for (unsigned e = r->map->numMeta; i != e; ++i) {
      visitor(&r->roots[i], (GCMeta *) r->map->meta[i]);
    }

    // For roots [NumMeta, NumRoots), the metadata pointer is null.
    for (unsigned e = r->map->numRoots; i != e; ++i) {
      visitor(&r->roots[i], NULL);
    }
  }
}
