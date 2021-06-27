#include "Parser.h"

int main() {
  lexer::Lexer<parser::Tok> lexer(TOKEN_PATTERNS);
  auto lexed = lexer.lex(std::cin);
  for (const auto &tok : lexed) {
    std::cout << tok << " (" << (int) tok.type << ")\n";
  }
}
