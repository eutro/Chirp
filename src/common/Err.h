#pragma once

#include <iostream>
#include <vector>
#include <memory>
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

  class CompileError {
  private:
    std::vector<std::unique_ptr<Line>> lines;

  public:
    CompileError &msg(const std::string &msg);

    CompileError &span(const loc::Span &span, const std::string &msg);

    CompileError &pos(const loc::SrcLoc &loc, const std::string &msg);

    friend ErrorPrintContext &operator<<(ErrorPrintContext &ctx, const CompileError &err);
  };

  class ErrorContext {
  public:
    std::vector<CompileError> errors;
    CompileError &err();
  };

  void maybeAbort(ErrorPrintContext &epc, ErrorContext &ec);
}
