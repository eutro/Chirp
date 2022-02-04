#pragma once

#include <iostream>
#include <vector>
#include <memory>
#include <optional>
#include "Loc.h"

namespace err {
  class ErrorPrintContext {
  public:
    const std::vector<std::string> &sourceLines;
    std::ostream &os;
    ErrorPrintContext(const std::vector<std::string> &sourceLines, std::ostream &os);
  };

  class Line {
  public:
    virtual ~Line() = default;
    virtual void output(ErrorPrintContext &ctx) const = 0;
  };

  class Location {
  private:
    std::vector<std::shared_ptr<Line>> lines;

  public:
    Location &msg(const std::string &msg);

    Location &span(const loc::Span &span, const std::string &msg);

    Location &maybeSpan(std::optional<loc::Span> span, const std::string &msg);

    Location &pos(const loc::SrcLoc &loc, const std::string &msg);

    Location &chain(const Location &o);

    friend ErrorPrintContext &operator<<(ErrorPrintContext &ctx, const Location &err);
  };

  class ErrorContext {
  public:
    std::vector<Location> errors;
    Location &err();
  };

  class LocationError : std::runtime_error {
  public:
    std::vector<Location> locations;

    LocationError &add(Location loc);

    LocationError(const LocationError &e) noexcept;
    LocationError(const std::string &arg, std::vector<Location> locations);
    LocationError(const std::string &msg);

    void addToContext(ErrorContext &ecx);
  };

  void printErrors(ErrorPrintContext &epc, ErrorContext &ec);
  void maybeAbort(ErrorPrintContext &epc, ErrorContext &ec);
}
