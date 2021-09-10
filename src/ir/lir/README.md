# `ir/lir/`

The low level IR, or LIR, is the IR below the HIR.
Types have already been inferred, and method bodies
are in SSA form. This IR is meant as a convenient
representation for lowering to LLVM IR.
