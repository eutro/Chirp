// https://github.com/llvm/llvm-project/blob/0fdb25cd954c5aaf86259e713f03d119ab9f2700/llvm/lib/CodeGen/ShadowStackGCLowering.cpp#L173

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stddef.h>

typedef struct FrameMap {
  int32_t numRoots; // Number of roots in stack frame.
  int32_t numMeta;  // Number of metadata descriptors. May be < NumRoots.
  void *meta[];     // May be absent for roots without metadata.
} FrameMap;

typedef struct StackEntry {
  struct StackEntry *next; // Caller's stack entry.
  FrameMap *map;           // Pointer to constant FrameMap.
  void *roots[];           // Stack roots (in-place array, so we pretend).
} StackEntry;

StackEntry *llvm_gc_root_chain;

typedef struct AllocMeta {
  struct AllocMeta *next;
  bool marked;
  max_align_t value[0];
} AllocMeta;

typedef struct GCMeta GCMeta;

typedef void (*VisitFn)(void **, GCMeta *);

struct GCMeta {
  void (*visit)(void *root, VisitFn visitor);
};

AllocMeta *allocated = NULL;

void *gcAlloc(int32_t size) {
  AllocMeta *newMeta = (AllocMeta *) malloc(sizeof(AllocMeta) + size);
  newMeta->next = allocated;
  newMeta->marked = false;
  allocated = newMeta;
  return &newMeta->value;
}

static AllocMeta *getMeta(void *ptr) {
  return (AllocMeta *) ptr - 1;
}

static void visitRoots(VisitFn visitor) {
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

void mark1(void **root, GCMeta *gcMeta) {
  if (!*root) return;
  AllocMeta *aMeta = getMeta(*root);
  if (aMeta->marked) return;
  aMeta->marked = true;
  if (gcMeta) {
    gcMeta->visit(*root, mark1);
  }
}

static void mark() {
  visitRoots(mark1);
}

static void sweep() {
  for (AllocMeta *meta = allocated; meta;) {
    if (meta->marked) {
      meta->marked = false;
      meta = meta->next;
    } else {
      AllocMeta *next = meta->next;
      free(meta);
      meta = next;
    }
  }
}

void collectGarbage() {
  mark();
  sweep();
}

void gcShutdown() {
  sweep();
}
