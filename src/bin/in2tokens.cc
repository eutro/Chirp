#include "../ir/tok/Tok.h"

int main() {
  auto tokens = tok::lexer().lex(std::cin);

  for (const auto &tok : tokens) {
    std::cout << tok.value << " (" << (int) tok.type << ")\n";
  }
}
