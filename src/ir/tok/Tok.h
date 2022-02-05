#pragma once

#include "../../common/Tokens.h"
#include "../../fsm/Lexer.h"

template<>
struct fsm::Finished<tok::Tok> {
  tok::Tok rejecting();
  void merge(tok::Tok &lhs, tok::Tok rhs);
};

#ifndef USE_COMPILED_DFA
#define USE_COMPILED_DFA 1
#endif

namespace tok {
#if (USE_COMPILED_DFA)
#include "CompiledDFA.h"
  using LexerType = lexer::Lexer<tok::Tok, CompiledDFA>;
#else
  using LexerType = lexer::Lexer<tok::Tok>;
#endif

  using Token = lexer::Token<Tok>;
  const LexerType &lexer();
}
