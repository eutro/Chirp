#include "Loc.h"

namespace loc {
  SrcLoc::SrcLoc() : line(1), col(0) {}
  SrcLoc::SrcLoc(std::uint32_t line, std::uint32_t col) : line(line), col(col) {}
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
  bool SrcLoc::operator<(const SrcLoc &rhs) const {
    if (line < rhs.line)
      return true;
    if (rhs.line < line)
      return false;
    return col < rhs.col;
  }
  bool SrcLoc::operator>(const SrcLoc &rhs) const {
    return rhs < *this;
  }
  bool SrcLoc::operator<=(const SrcLoc &rhs) const {
    return !(rhs < *this);
  }
  bool SrcLoc::operator>=(const SrcLoc &rhs) const {
    return !(*this < rhs);
  }

  Span::Span() {}
  Span::Span(const loc::SrcLoc &lo, const loc::SrcLoc &hi) :
      lo(lo), hi(hi) {}
}
