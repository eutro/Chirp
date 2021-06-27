#include "Parser.h"

int main() {
  lexer::Lexer<parser::Tok> lexer(TOKEN_PATTERNS);
  parser::Program program = parser::parseProgram(lexer.lex(std::cin));
  std::cout << program << std::endl;
}
