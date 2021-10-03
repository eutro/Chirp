#ifdef CHIRP_DYNAMIC_COLLECTOR
#define CHIRP_GC_IMPL_NAME Noop
#endif
#include "Collector.h"

#include <stdlib.h>

void *chirpGcAlloc(uint32_t size, uint32_t align) {
  return malloc(size);
}
void chirpGc(size_t genc) {}
void chirpGcInit(void) {}
void chirpGcShutdown(void) {}
