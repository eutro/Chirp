#pragma once

#include <stddef.h>
#include <stdint.h>

#ifdef CHIRP_GC_IMPL_NAME
// all three of these are required so that CHIRP_GC_IMPL_NAME is expanded first...
#define CHIRP_WRAP_FUNCNAME1(NAME, OTHER) NAME##Impl##OTHER
#define CHIRP_WRAP_FUNCNAME0(NAME, OTHER) CHIRP_WRAP_FUNCNAME1(NAME, OTHER)
#define CHIRP_WRAP_FUNCNAME(NAME) CHIRP_WRAP_FUNCNAME0(NAME, CHIRP_GC_IMPL_NAME)
#define chirpGcAlloc CHIRP_WRAP_FUNCNAME(chirpGcAlloc)
#define chirpGc CHIRP_WRAP_FUNCNAME(chirpGc)
#define chirpGcInit CHIRP_WRAP_FUNCNAME(chirpGcInit)
#define chirpGcShutdown CHIRP_WRAP_FUNCNAME(chirpGcShutdown)
#endif

void *chirpGcAlloc(uint32_t size, uint32_t align);
void chirpGc(size_t genc);
void chirpGcInit(void);
void chirpGcShutdown(void);
