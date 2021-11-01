#ifdef CHIRP_DYNAMIC_COLLECTOR
#define CHIRP_GC_IMPL_NAME Generational
#endif
#include "Collector.h"

#include "RootVisitor.h"

#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

// portable^tm alignof because MSVC can't behave and use stdalign.h
#define alignof(type) ((size_t)(offsetof(struct { char c; type d; }, d)))

#ifndef CHIRP_COLLECTOR_START_HEAP_SIZES
# define CHIRP_COLLECTOR_START_HEAP_SIZES {30000000, 30000000, 30000000}
#endif
#ifndef CHIRP_COLLECTOR_GEN_SIZES
# define CHIRP_COLLECTOR_GEN_SIZES {10, 20, 30}
#endif

static size_t HEAP_SIZES[] = CHIRP_COLLECTOR_START_HEAP_SIZES;
static const size_t GEN_SIZES[] = CHIRP_COLLECTOR_GEN_SIZES;

#define CHIRP_SIZEOF_ARRAY(arr) (sizeof(arr)/sizeof(*arr))
static_assert(CHIRP_SIZEOF_ARRAY(HEAP_SIZES) ==
              CHIRP_SIZEOF_ARRAY(GEN_SIZES),
              "Inconsistent generation count given.");

#define GENERATION_COUNT CHIRP_SIZEOF_ARRAY(HEAP_SIZES)
static_assert(GENERATION_COUNT >= 1, "At least one generation is required.");

typedef struct AllocMeta {
  // NULL if unmarked,
  // dangling if not currently pending replacement
  // ptr to replacement if generation is about to be sweeped
  void *ptr;
  // The next allocation in this heap
  struct AllocMeta *next;
  uint32_t size, align;
} AllocMeta;

static void *HEAPS[GENERATION_COUNT] = {NULL};
static AllocMeta *HEAP_ENDS[GENERATION_COUNT] = {NULL};
static size_t HEAP_OBJECTC[GENERATION_COUNT] = {0};
static size_t TO_COLLECT = 0;

static void initHeap(size_t gen) {
  AllocMeta *heapMeta = HEAP_ENDS[gen] = HEAPS[gen];
  heapMeta->ptr = NULL;
  heapMeta->size = 0;
  heapMeta->align = alignof(AllocMeta);
  heapMeta->next = heapMeta + 1;
}

void chirpGcInit() {
  for (size_t gen = 0; gen < GENERATION_COUNT; ++gen) {
    void *heap = malloc(HEAP_SIZES[gen]);
    if (!heap) {
      fprintf(stderr,
              "Chirp -- Allocation of heap of size %zu for generation %zu failed.\n",
              HEAP_SIZES[gen], gen);
      exit(1);
    }
    HEAPS[gen] = heap;
    initHeap(gen);
  }
}

static void maybeCollect();

static size_t gcd(size_t x, size_t y) {
  while (y != 0) {
    size_t r = x % y;
    x = y;
    y = r;
  }
  return x;
}

static size_t lcm(size_t x, size_t y) {
  return (x * y) / gcd(x, y);
}

static void *incToAlign(void *ptr, size_t align) {
  uintptr_t uptr = (uintptr_t) ptr;
  uintptr_t misalign = uptr % align;
  if (misalign) {
    return (void *) (uptr - misalign + align);
  } else {
    return ptr;
  }
}

static void *appendToGen(size_t gen, size_t size, size_t align, bool collectFirst) {
  while (true) {
    AllocMeta **target = &HEAP_ENDS[gen]->next;
    AllocMeta *newAllocStart = incToAlign((char *) *target + sizeof(AllocMeta), align);
    void *newAllocEnd = (void *) ((uintptr_t) newAllocStart + size);
    if ((uintptr_t) newAllocEnd > (uintptr_t) HEAPS[gen] + HEAP_SIZES[gen]) {
      // TODO what to do if we've passed the end of the heap?
      fprintf(stderr, "Chirp -- Ran out of heap space in generation %zu.\n", gen);
      exit(1);
    }
    if (++HEAP_OBJECTC[gen] > GEN_SIZES[gen]) {
      if (gen + 1 > TO_COLLECT) {
        TO_COLLECT = gen + 1;
      }
      if (collectFirst) {
        maybeCollect();
        continue; // retry
      }
    }
    AllocMeta *newAllocMeta = newAllocStart - 1;
    HEAP_ENDS[gen] = *target = newAllocMeta;
    newAllocMeta->size = size;
    newAllocMeta->align = align;
    newAllocMeta->ptr = NULL; // unmarked
    newAllocMeta->next = newAllocEnd;
    return newAllocStart;
  }
}

void *chirpGcAlloc(uint32_t size, uint32_t align) {
  if (size == 0) return NULL;
  size_t allocAllign = lcm(align, alignof(AllocMeta));
  return appendToGen(0, size, allocAllign, true);
}

static AllocMeta *getMeta(void *ptr) {
  return (AllocMeta *) ptr - 1;
}

static void mark1(void **root, GCMeta *gcMeta) {
  if (!*root) return;
  AllocMeta *aMeta = getMeta(*root);
  if (aMeta->ptr) return;
  aMeta->ptr = *root;
  if (gcMeta) {
    gcMeta->visit(*root, mark1);
  }
}

static void relocate1(void **root, GCMeta *gcMeta) {
  if (!*root) return;
  AllocMeta *aMeta = getMeta(*root);
  if (!aMeta->ptr || aMeta->ptr == *root) return;
  *root = aMeta->ptr;
  if (gcMeta) {
    gcMeta->visit(*root, relocate1);
  }
}

// Marks reachable roots for relocation
static void mark() {
  chirpVisitRoots(mark1);
}

static void relocate() {
  chirpVisitRoots(relocate1);
}

// Sweeps relevant generations and relocates live objects,
// "free"ing dead objects by resetting the buffers they were in.
static void sweep(size_t genc) {
  for (size_t g = 0; g < genc; ++g) {
    for (AllocMeta *meta = ((AllocMeta *) HEAPS[g])->next;
         meta <= HEAP_ENDS[g];
         meta = meta->next) {
      if (meta->ptr) {
        void *moved = appendToGen(genc, meta->size, meta->align, false);
        meta->ptr = moved;
        void *old = meta + 1;
        memcpy(moved, old, meta->size);
      }
    }
    initHeap(g);
    HEAP_OBJECTC[g] = 0;
  }
  relocate();
}

// Run garbage collection for the youngest genc generations.
void chirpGc(size_t genc) {
  mark();
  sweep(genc);
}

static void maybeCollect() {
  while (TO_COLLECT) {
    size_t genc = TO_COLLECT;
    TO_COLLECT = 0;
    chirpGc(genc);
  }
}

void chirpGcShutdown() {
  for (size_t gen = 0; gen < GENERATION_COUNT; ++gen) {
    free(HEAPS[gen]);
  }
}
