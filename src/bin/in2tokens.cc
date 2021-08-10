#include "../ir/tokens/Parser.h"

int main() {
  lexer::Lexer<ast::Tok> lexer(TOKEN_PATTERNS);
  auto tokens = lexer.lex(std::cin);

  for (const auto &tok : tokens) {
    std::cout << tok.value << " (" << (int) tok.type << ")\n";
  }
}
