#include "Loc.h"

namespace loc {
  SrcLoc::SrcLoc() : line(1), col(0) {}
  SrcLoc::SrcLoc(std::uint32_t line, std::uint32_t col) : line(line), col(col) {}
  std::ostream &operator<<(std::ostream &os, const SrcLoc &loc) {
    os << loc.line << ":" << loc.col;
    return os;
  }
  void SrcLoc::add(const std::string &s) {
    for (auto it = s.begin(); it != s.end(); ++it) {
      char c = *it;
      if (c == '\n') {
        ++line;
        col = 0;
      } else {
        // Count UTF-8 code points
        if ((c & 0b10000000) != 0b00000000) {
          unsigned int len;
          if ((c & 0b11100000) == 0b11000000) {
            len = 2;
          } else if ((c & 0b11110000) == 0b11100000) {
            len = 3;
          } else if ((c & 0b11111000) == 0b11110000) {
            len = 4;
          } else {
            // invalid UTF-8
            len = 1;
          }
          while (len-- && ++it != s.end());
          --it;
        }
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

  Span::Span() = default;
  Span::Span(const loc::SrcLoc &lo, const loc::SrcLoc &hi) :
      lo(lo), hi(hi) {}
  std::ostream &operator<<(std::ostream &os, const Span &span) {
    os << span.lo << "-" << span.hi;
    return os;
  }
  bool Span::operator<(const Span &rhs) const {
    if (lo < rhs.lo)
      return true;
    if (rhs.lo < lo)
      return false;
    return hi < rhs.hi;
  }
  bool Span::operator>(const Span &rhs) const {
    return rhs < *this;
  }
  bool Span::operator<=(const Span &rhs) const {
    return !(rhs < *this);
  }
  bool Span::operator>=(const Span &rhs) const {
    return !(*this < rhs);
  }

  std::ostream &operator<<(std::ostream &os, const std::optional<Span> &maybeSpan) {
    if (maybeSpan) {
      os << *maybeSpan;
    } else {
      os << "<unknown>";
    }
    return os;
  }
}
