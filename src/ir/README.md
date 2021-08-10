# `ir/`

The IR module contains a collection of other modules that each
correspond to a given intermediate representation (IR) used when
compiling Chirp source.

These IRs are then processed on their own level before being lowered
to the one below.

The first IR may be considered the token stream lexed from the source
file. That is then "lowered" to the AST by the parser. Then the AST
may be lowered to some other IR, etc.

The final stage of the compiler here may be considered the lowering to
the LLVM IR, which can then be further lowered to machine code
elsewhere.

Current IRs, in order of highest to lowest, are:
- [Tokens](tokens/)
- [AST](ast/)
