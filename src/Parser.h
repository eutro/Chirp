#pragma once

#include <stdexcept>
#include <string>

#include "Ast.h"

template<>
struct fsm::Finished<ast::Tok> {
  ast::Tok rejecting();
  void merge(ast::Tok &lhs, ast::Tok rhs);
};

namespace parser {
  using namespace ast;

  class ParseError : public std::runtime_error {
  public:
    std::string message;
    lexer::SrcLoc loc;

    ParseError(const std::string &message, const lexer::SrcLoc &loc);
    const char *what() const noexcept override;
  };

  Program parseProgram(lexer::TokenIter<Tok> &&tokens);
}
