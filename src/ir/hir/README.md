# `ir/hir/`

The high level IR, or HIR, is the IR immediately below the AST.
This has a lot fewer node types than the AST, as common shapes are
merged into single expression types, such as boolean `&&`/`||` operations
and comparison chains. Closures with their captured variables are also
made explicit.

Type inference is done at this level, before it is lowered to the
[`lir`](../lir/).
