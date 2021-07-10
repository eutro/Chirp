#include "../Parser.h"

int main() {
  lexer::Lexer<ast::Tok> lexer(TOKEN_PATTERNS);
  auto tokens = lexer.lex(std::cin);

  err::ErrorContext ec;
  ast::Program program = parser::parseProgram(ec, tokens);

  std::cout << program << std::endl;
}
