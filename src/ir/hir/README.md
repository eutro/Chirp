# `ir/hir/`

The high level IR, or HIR, is the IR immediately below the AST. It is
still faithful in structure to the original, but similar constructs
such as lambdas and `fn`s, `defn`s and `let`s are replaced by common
constructs.
