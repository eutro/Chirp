#pragma once

#include <stdint.h>
#include <stddef.h>

// https://github.com/llvm/llvm-project/blob/0fdb25cd954c5aaf86259e713f03d119ab9f2700/llvm/lib/CodeGen/ShadowStackGCLowering.cpp#L173

typedef struct GCMeta GCMeta;

typedef void (*VisitFn)(void **, GCMeta *);

struct GCMeta {
  void (*visit)(void *root, VisitFn visitor);
  // true if this pointer is not managed by the garbage collector,
  // but it's a pointer to one that is
  uint8_t pointerTo;
};

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

void chirpVisitRoots(VisitFn visitor);
