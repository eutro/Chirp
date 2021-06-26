#include "Tokens.h"
#include "../lexer/Lexer.tcc"

#include <sstream>

template<> struct fsm::Finished<parser::Tok> {
  parser::Tok rejecting() {
    return parser::Tok::TInvalid;
  }
  void merge(parser::Tok &lhs, parser::Tok rhs) {
    lhs = std::max(lhs, rhs);
  }
};

std::ostream &operator<<(std::ostream &o, const parser::Tok &tok) {
  return o << (int) tok;
}

int main() {
  lexer::Lexer<parser::Tok> lexer(TOKENS);
  std::stringstream ss("defn factorial(x) =\n"
                       "  let fact = 1, i = 1\n"
                       "    in loop ( if i > x ( fact ) else ( loop(fact * i, i + 1) ) )");
  for (auto tok : lexer.lex(ss)) {
    std::cout << tok.value << " (" << tok.type << ")" << std::endl;
  }
}
