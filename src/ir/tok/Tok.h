#pragma once

#include "../../common/Tokens.h"
#include "../../fsm/Lexer.h"

template<>
struct fsm::Finished<tok::Tok> {
  tok::Tok rejecting();
  void merge(tok::Tok &lhs, tok::Tok rhs);
};

namespace tok {
  using Token = lexer::Token<Tok>;
  const lexer::Lexer<tok::Tok> &lexer();
}
