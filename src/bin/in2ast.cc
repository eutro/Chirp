#include "../Parser.h"

int main() {
  lexer::Lexer<ast::Tok> lexer(TOKEN_PATTERNS);
  ast::Program program = parser::parseProgram(lexer.lex(std::cin));
  std::cout << program << std::endl;
}
