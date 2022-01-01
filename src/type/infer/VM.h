#pragma once

#include "../Type.h"
#include "Fn.h"
#include "LookupTable.h"

namespace type::infer {
  /**
   * The runtime state of the inference VM.
   */
  struct Env {
    std::unique_ptr<LookupTable> table = newLookupTable();
  };

  extern thread_local Env *ENV;
}
