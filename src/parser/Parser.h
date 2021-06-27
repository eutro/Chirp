#pragma once

#include <stdexcept>
#include <string>

#include "Ast.h"

template<> struct fsm::Finished<parser::Tok> {
  parser::Tok rejecting();
  void merge(parser::Tok &lhs, parser::Tok rhs);
};

namespace parser {
  class ParseError : public std::runtime_error {
  public:
    std::string message;
    lexer::SrcLoc loc;

    ParseError(const std::string &message, const lexer::SrcLoc &loc);
    const char *what() const _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_NOTHROW override;
  };

  Program parseProgram(lexer::TokenIter<Tok> &&tokens);
}
