#include "Parser.h"

int main() {
  lexer::Lexer<parser::Tok> lexer(TOKEN_PATTERNS);
  parser::Program program = parser::parseProgram(lexer.lex(std::cin));
  compiler::TypeContext tc;
  parser::ParseContext ctx(tc);
  program.inferTypes(ctx);
  std::cout << program << std::endl;
}
