#include "Lexer.h"

namespace lexer {
  SrcLoc::SrcLoc() : line(1), col(0) {}
  SrcLoc::SrcLoc(size_t line, size_t col) : line(line), col(col) {}
  std::ostream &operator<<(std::ostream &os, const SrcLoc &loc) {
    os << loc.line << ":" << loc.col;
    return os;
  }
  void SrcLoc::add(const std::string &s) {
    for (char c : s) {
      if (c == '\n') {
        ++line;
        col = 0;
      } else {
        ++col;
      }
    }
  }
}
