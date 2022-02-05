#include "Tok.h"
#include "LexerCommon.h"

int main(int argc, char **argv) {
  const lexer::Lexer<tok::Tok> LEXER(TOKEN_PATTERNS);
  if (argc >= 2 && std::string(argv[1]) == "bin") {
    fsm::Codec<decltype(LEXER.dfa)> codec{};
    codec.encode(LEXER.dfa, std::cout);
  } else {
    LEXER.dfa.compileInto("char", "tok::Tok", std::cout);
  }
}
