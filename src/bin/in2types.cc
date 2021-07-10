#include "../Parser.h"

int main() {
  lexer::Lexer<ast::Tok> lexer(TOKEN_PATTERNS);
  auto tokens = lexer.lex(std::cin);

  err::ErrorContext ec;
  ast::Program program = parser::parseProgram(ec, tokens);

  type::TypeContext tc;
  ast::ParseContext ctx(tc);
  program.inferTypes(ctx);

  std::cout << program << std::endl;
}
