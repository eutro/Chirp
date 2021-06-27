#include "Parser.h"

#include <sstream>

int main() {
  lexer::Lexer<parser::Tok> lexer(TOKEN_PATTERNS);
  std::stringstream ss("defn factorial(x) =\n"
                       "  let fact = 1, i = 1\n"
                       "    in loop ( if i > x { fact } else { loop(fact * i, i + 1) } )");
  parser::Program program = parser::parseProgram(lexer.lex(ss));
  std::cout << program << std::endl;
}
