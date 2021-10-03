#include "Collector.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void *(*chirpGcAllocImpl)(uint32_t size, uint32_t align);
void (*chirpGcImpl)(size_t genc);
void (*chirpGcInitImpl)(void);
void (*chirpGcShutdownImpl)(void);

// chooses the garbage collector implementation based on environmental variables

#define DECLARE_STRATEGY(NAME) \
  void *chirpGcAllocImpl##NAME(uint32_t size, uint32_t align); \
  void chirpGcImpl##NAME(size_t genc);                         \
  void chirpGcInitImpl##NAME(void);                            \
  void chirpGcShutdownImpl##NAME(void)

#define CHOOSE_STRATEGY(NAME) do {                                      \
    chirpGcAllocImpl = chirpGcAllocImpl##NAME;                          \
    chirpGcImpl = chirpGcImpl##NAME;                                    \
    chirpGcInitImpl = chirpGcInitImpl##NAME;                            \
    chirpGcShutdownImpl = chirpGcShutdownImpl##NAME;                    \
  } while(0)

DECLARE_STRATEGY(Noop);
DECLARE_STRATEGY(MarkAndSweep);
DECLARE_STRATEGY(Generational);

void chirpGcInit(void) {
  const char *strat = getenv("CHIRP_COLLECTOR_STRATEGY");
  if (!strat || strcmp(strat, "MarkAndSweep") == 0) {
    CHOOSE_STRATEGY(MarkAndSweep);
  } else if (strcmp(strat, "Noop") == 0) {
    CHOOSE_STRATEGY(Noop);
  } else if (strcmp(strat, "Generational") == 0) {
    CHOOSE_STRATEGY(Generational);
  } else {
    fprintf(stderr, "Chirp -- Unknown garbage collector strategy: %s\n", strat);
    exit(1);
  }
  chirpGcInitImpl();
}

void *chirpGcAlloc(uint32_t size, uint32_t align) { return chirpGcAllocImpl(size, align); }
void chirpGc(size_t genc) { chirpGcImpl(genc); }
void chirpGcShutdown(void) { chirpGcShutdownImpl(); }
