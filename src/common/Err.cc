#include "Err.h"

#include <algorithm>
#include <iomanip>
#include <iterator>
#include <valarray>

namespace err {
  ErrorPrintContext &operator<<(ErrorPrintContext &ctx, const Location &err) {
    for (const auto &line : err.lines) {
      line->output(ctx);
    }
    return ctx;
  }

  Location &ErrorContext::err() {
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
    return line < 100 ? 2 : (size_t) std::ceil(std::log10(line));
  }

  void writeLine(ErrorPrintContext &ec, size_t margin, size_t line) {
    auto w = ec.os.width();
    ec.os << std::setfill(' ') << std::setw((int) margin) << line;
    ec.os.width(w);
    ec.os << ": ";
    ec.os << ec.sourceLines[line - 1] << '\n';
  }

  void indentTo(ErrorPrintContext &ec, size_t margin, size_t col) {
    ec.os << std::string(margin + col + 2, ' ');
  }

  class SpanLine : public Line {
  private:
    loc::Span span;
    std::string msg;
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
      ctx.os << std::string(span.hi.col - col, '~');
      ctx.os << ' ' << msg << '\n';
    }
  };

  class PosLine : public Line {
  private:
    loc::SrcLoc loc;
    std::string line;
  public:
    PosLine(const loc::SrcLoc &loc, const std::string &line): loc(loc), line(line) {}

    void output(ErrorPrintContext &ctx) const override {
      size_t margin = getMargin(loc.line);
      writeLine(ctx, margin, loc.line);
      indentTo(ctx, margin, loc.col);
      ctx.os << "^ " << line << "\n";
    }
  };

  Location &Location::msg(const std::string &msg) {
    lines.push_back(std::make_shared<RawLine>(msg));
    return *this;
  }
  Location &Location::span(const loc::Span &span, const std::string &msg) {
    lines.push_back(std::make_shared<SpanLine>(span, msg));
    return *this;
  }
  Location &Location::maybeSpan(std::optional<loc::Span> maybeSpan, const std::string &message) {
    if (maybeSpan) {
      span(*maybeSpan, message);
    } else {
      msg(message);
    }
    return *this;
  }
  Location &Location::pos(const loc::SrcLoc &loc, const std::string &msg) {
    lines.push_back(std::make_shared<PosLine>(loc, msg));
    return *this;
  }
  Location &Location::chain(const Location &o) {
    lines.reserve(o.lines.size());
    for (auto &line : o.lines) {
      lines.push_back(line);
    }
    return *this;
  }

  ErrorPrintContext::ErrorPrintContext(const std::vector<std::string> &sourceLines,
                                       std::ostream &os) :
    sourceLines(sourceLines), os(os) {}

  void printErrors(ErrorPrintContext &epc, ErrorContext &ec) {
    for (auto &err : ec.errors) {
      epc.os << "\nError:\n";
      epc << err;
    }
  }

  void maybeAbort(ErrorPrintContext &epc, ErrorContext &ec) {
    if (!ec.errors.empty()) {
      epc.os << "Aborting due to errors:\n";
      printErrors(epc, ec);
      std::exit(1);
    }
  }
}
