#pragma once

#include "../Type.h"
#include "Fn.h"
#include "LookupTable.h"

namespace type::infer {
  /**
   * The runtime state of the inference VM.
   */
  struct Env {
    std::unique_ptr<LookupTable> table = LookupTable::create();
  };

  extern thread_local Env *ENV;

  struct VarRef {
    /**
     * The referenced instruction. A null index represents the function arguments.
     */
    std::optional<Idx> insn;
    /**
     * Which return value of the instruction to reference, or the parameter index.
     */
    Idx retIdx;
  };

  struct Insn {
    /**
     * The function to invoke to execute this insn.
     */
    LookupKey *key;
    /**
     * Extra constant arguments to the function.
     * e.g. trait index
     */
    std::vector<Constant> constants;
    /**
     * The arguments to the function.
     */
    std::vector<VarRef> inputs;
    Insn(
      decltype(key) key,
      decltype(constants) &&constants,
      decltype(inputs) &&inputs
    ):
      key(key),
      constants(std::forward<decltype(constants)>(constants)),
      inputs(std::forward<decltype(inputs)>(inputs))
    {}
  };

  struct InsnList {
    std::vector<Insn> insns;
    // implement Fn
    std::vector<Tp> operator()(const std::vector<Tp> &args) const;
  };
}
