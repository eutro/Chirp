#include "Tok.h"

tok::Tok fsm::Finished<tok::Tok>::rejecting() {
  return tok::Tok::TInvalid;
}

void fsm::Finished<tok::Tok>::merge(tok::Tok &lhs, tok::Tok rhs) {
  lhs = std::max(lhs, rhs);
}

namespace tok {
  const lexer::Lexer<tok::Tok> LEXER(TOKEN_PATTERNS);
  const lexer::Lexer<tok::Tok> &lexer() {
    return LEXER;
  }
}
