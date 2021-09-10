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
    std::uint32_t line;
    /**
     * The column number. Starts at 0.
     */
    std::uint32_t col;

    SrcLoc();
    SrcLoc(std::uint32_t line, std::uint32_t col);
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
