#include "Err.h"

#include <cstdlib>
#include <iomanip>
#include <valarray>

namespace err {
  ErrorPrintContext &operator<<(ErrorPrintContext &ctx, const CompileError &err) {
    for (const auto &line : err.lines) {
      line->output(ctx);
    }
    return ctx;
  }

  CompileError &ErrorContext::err() {
    return errors.emplace_back();
  }

  class RawLine : public Line {
  private:
    std::string line;
  public:
    RawLine(const std::string &line): line(line) {}

    void output(ErrorPrintContext &ctx) const override {
      ctx.os << line << "\n";
    }
  };

  class SpanLine : public Line {
  private:
    std::string line;
    loc::Span span;
  public:
    SpanLine(const loc::Span &span, const std::string &line): span(span), line(line) {}

    void output(ErrorPrintContext &ctx) const override {
      size_t margin = span.hi.line < 100 ? 2 : std::ceil(std::log10(span.hi.line));
      ctx.os << std::setfill(' ') << std::setw(margin) << span.lo.line;
      ctx.os << ": ";
      ctx.os << ctx.sourceLines[span.lo.line - 1] << '\n';
      std::string indent(margin + span.lo.col + 2, ' ');
      std::string marker(span.hi.col - span.lo.col, '~');
      ctx.os << indent << marker << " " << line << "\n";
    }
  };

  class PosLine : public Line {
  private:
    std::string line;
    loc::SrcLoc loc;
  public:
    PosLine(const loc::SrcLoc &loc, const std::string &line): loc(loc), line(line) {}

    void output(ErrorPrintContext &ctx) const override {
      size_t margin = loc.line < 100 ? 2 : std::ceil(std::log10(loc.line));
      ctx.os << std::setfill(' ') << std::setw(margin) << loc.line;
      ctx.os << ": ";
      ctx.os << ctx.sourceLines[loc.line - 1] << '\n';
      std::string indent(margin + loc.col + 2, ' ');
      ctx.os << std::move(indent)
             << "^ " << line << "\n";
    }
  };

  CompileError &CompileError::msg(const std::string &msg) {
    lines.push_back(std::make_unique<RawLine>(msg));
    return *this;
  }
  CompileError &CompileError::span(const loc::Span &span, const std::string &msg) {
    lines.push_back(std::make_unique<SpanLine>(span, msg));
    return *this;
  }
  CompileError &CompileError::pos(const loc::SrcLoc &loc, const std::string &msg) {
    lines.push_back(std::make_unique<PosLine>(loc, msg));
    return *this;
  }

  ErrorPrintContext::ErrorPrintContext(const std::vector<std::string> &sourceLines,
                                       std::ostream &os) :
    sourceLines(sourceLines), os(os) {}

  void maybeAbort(ErrorPrintContext &epc, ErrorContext &ec) {
    if (!ec.errors.empty()) {
      epc.os << "Aborting due to errors:\n";
      for (auto &err : ec.errors) {
        epc << err;
      }
      std::exit(1);
    }
  }
}
