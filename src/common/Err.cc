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

  size_t getMargin(size_t line) {
    return line < 100 ? 2 : std::ceil(std::log10(line));
  }

  void writeLine(ErrorPrintContext &ec, size_t margin, size_t line) {
    ec.os << std::setfill(' ') << std::setw(margin) << line;
    ec.os << ": ";
    ec.os << ec.sourceLines[line - 1] << '\n';
  }

  void indentTo(ErrorPrintContext &ec, size_t margin, size_t col) {
    std::string indent(margin + col + 2, ' ');
    ec.os << indent;
  }

  class SpanLine : public Line {
  private:
    std::string msg;
    loc::Span span;
  public:
    SpanLine(const loc::Span &span, const std::string &msg): span(span), msg(msg) {}

    void output(ErrorPrintContext &ctx) const override {
      size_t margin = getMargin(span.hi.line);
      writeLine(ctx, margin, span.lo.line);
      indentTo(ctx, margin, span.lo.col);

      size_t line = span.lo.line;
      size_t col = span.lo.col;
      while (line < span.hi.line) {
        while (col++ < ctx.sourceLines[line - 1].size()) {
          ctx.os << '~';
        }
        col = 0;
        ctx.os << '\n';
        writeLine(ctx, margin, ++line);
        indentTo(ctx, margin, 0);
      }
      while (col++ < span.hi.col) {
        ctx.os << '~';
      }
      ctx.os << ' ' << msg << '\n';
    }
  };

  class PosLine : public Line {
  private:
    std::string line;
    loc::SrcLoc loc;
  public:
    PosLine(const loc::SrcLoc &loc, const std::string &line): loc(loc), line(line) {}

    void output(ErrorPrintContext &ctx) const override {
      size_t margin = getMargin(loc.line);
      writeLine(ctx, margin, loc.line);
      indentTo(ctx, margin, loc.col);
      ctx.os << "^ " << line << "\n";
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
