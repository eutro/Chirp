# `ir/tokens/`

This module represents the token stream IR. The token stream IR is
obtained from running the lexer on the contents of a source file. It
is then lowered by the parser to the [AST](../ast/).

The lexer code mostly resides elsewhere. The list of tokens can be
found in [`Tokens.h`](../../common/Tokens.h) generated from
[`tokens.txt`](../../../spec/tokens.txt), and the generic Lexer class
can be found [in the `fsm/` module](../../fsm/).

The parser is a hand-written recursive descent parser which tries its
best to match the [described grammar](../../../spec/grammar.txt). It
may not match certain things that are technically grammatically
correct, and since the grammar is ambiguous, especially around
linebreaks, the derivation chosen may not necessarily be an obvious
one. As such, the formal grammar described merely serves as a
guideline for writing the parser, rather than a strict set of rules.
