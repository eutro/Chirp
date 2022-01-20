#pragma once

#include <ostream>
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
    VarRef(std::optional<Idx> insn, Idx retIdx) : insn(insn), retIdx(retIdx) {}

    friend std::ostream &operator<<(std::ostream &os, const VarRef &var);
    bool operator<(const VarRef &rhs) const;
    bool operator>(const VarRef &rhs) const;
    bool operator<=(const VarRef &rhs) const;
    bool operator>=(const VarRef &rhs) const;
    bool operator==(const VarRef &rhs) const;
    bool operator!=(const VarRef &rhs) const;
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
    /**
     * Extra constant arguments to the function.
     */
    std::vector<Constant> constArgs;
    /**
     * The reason for this instruction.
     */
    std::optional<std::string> reason;
    /**
     * The location this instruction should be traced to.
     */
    std::optional<loc::Span> src;
    Insn(
        decltype(key) key,
        decltype(constants) &&constants,
        decltype(inputs) &&inputs,
        decltype(constArgs) &&constArgs
    ):
        key(key),
        constants(std::forward<decltype(constants)>(constants)),
        inputs(std::forward<decltype(inputs)>(inputs)),
        constArgs(std::forward<decltype(constArgs)>(constArgs))
    {}
    Insn(
        decltype(key) key,
        decltype(constants) &&constants,
        decltype(inputs) &&inputs,
        decltype(constArgs) &&constArgs,
        const std::optional<std::string> &reason,
        std::optional<loc::Span> src
    ):
        key(key),
        constants(std::forward<decltype(constants)>(constants)),
        inputs(std::forward<decltype(inputs)>(inputs)),
        constArgs(std::forward<decltype(constArgs)>(constArgs)),
        reason(reason),
        src(src)
    {}

    friend std::ostream &operator<<(std::ostream &os, const Insn &insn);
  };

  struct InsnList {
    std::vector<Insn> insns;
    Idx retInsn{};
    VarRef lastInsn(Idx i = 0) {
      return VarRef(insns.size() - 1, i);
    }

    using SccCollapser = std::function<void(InsnList &il, const std::vector<Insn*> &)>;
    /**
     * Sort the instructions topologically.
     *
     * A collapse function must be supplied which "collapses" strongly connected components.
     * Cycles are to be expected, with mutually recursive functions that capture each other,
     * for example, so strongly connected components of size >1 (cycles) must be handled.
     *
     * The function must accept a single parameter, the strongly connected component to collapse,
     * and must modify the instructions in this strongly connected component so that there is no longer
     * a cycle.
     */
    void topSort(const SccCollapser &collapse);

    /**
     * Fn implementation.
     *
     * The instruction list must be in topological ordering for this to work.
     */
    std::vector<Tp> operator()(const std::vector<Tp> &args, const std::vector<Constant> &) const;

    friend std::ostream &operator<<(std::ostream &os, const InsnList &list);
  };
  static_assert(std::is_assignable_v<Fn, InsnList>);
}
