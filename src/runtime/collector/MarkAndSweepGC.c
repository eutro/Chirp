#ifdef CHIRP_DYNAMIC_COLLECTOR
#define CHIRP_GC_IMPL_NAME MarkAndSweep
#endif
#include "Collector.h"

#include "RootVisitor.h"

#include <stdbool.h>
#include <stdlib.h>

typedef struct AllocMeta {
  struct AllocMeta *next;
  bool marked;
  long double value[0]; // should be max_align_t but that's missing on MSVC
} AllocMeta;

static AllocMeta *allocated = NULL;

void *chirpGcAlloc(uint32_t size, uint32_t align) {
  AllocMeta *newMeta = (AllocMeta *) malloc(sizeof(AllocMeta) + size);
  newMeta->next = allocated;
  newMeta->marked = false;
  allocated = newMeta;
  return &newMeta->value;
}

static AllocMeta *getMeta(void *ptr) {
  return (AllocMeta *) ptr - 1;
}

// Mark Function marks the locations
static void mark1(void **root, GCMeta *gcMeta) {
  if (!*root) return;
  AllocMeta *aMeta = getMeta(*root);
  if (aMeta->marked) return;
  aMeta->marked = true;
  if (gcMeta) {
    gcMeta->visit(*root, mark1);
  }
}

static void mark() {
  chirpVisitRoots(mark1);
}

// Sweep Function frees the unmarked
static void sweep() {
  for (AllocMeta **meta = &allocated; *meta;) {
    if ((*meta)->marked) {
      (*meta)->marked = false;
      meta = &(*meta)->next;
    } else {
      AllocMeta *toFree = *meta;
      *meta = toFree->next;
      free(toFree);
    }
  }
}

// Main garbage collection function
void chirpGc(size_t genc) { 
  mark();
  sweep();
}

void chirpGcInit() {}

void chirpGcShutdown() {
  sweep();
}
