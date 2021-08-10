#pragma once

#include <string>
#include <iostream>

namespace loc {
  /**
   * Represents a singular location in a file or string.
   */
  class SrcLoc {
  public:
    /**
     * The line number. Starts at 1.
     */
    size_t line;
    /**
     * The column number. Starts at 0.
     */
    size_t col;

    SrcLoc();
    SrcLoc(size_t line, size_t col);
    friend std::ostream &operator<<(std::ostream &os, const SrcLoc &loc);
    void add(const std::string &s);

    bool operator<(const SrcLoc &rhs) const;
    bool operator>(const SrcLoc &rhs) const;
    bool operator<=(const SrcLoc &rhs) const;
    bool operator>=(const SrcLoc &rhs) const;
  };

  /**
   * Represents a range between two points in a file or string.
   */
  class Span {
  public:
    /**
     * The start location (inclusive).
     */
    SrcLoc lo;
    /**
     * The end location (exclusive).
     */
    SrcLoc hi;

    Span();
    Span(const SrcLoc &lo, const SrcLoc &hi);
  };
}
