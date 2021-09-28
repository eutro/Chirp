#pragma once

#include <stddef.h>
#include <stdint.h>

void *gcAlloc(uint32_t size, uint32_t align);
void collectGarbage(size_t genc);
void gcInit(void);
void gcShutdown(void);
