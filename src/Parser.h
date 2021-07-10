#pragma once

#include <stdexcept>
#include <string>

#include "Ast.h"
#include "Err.h"

template<>
struct fsm::Finished<ast::Tok> {
  ast::Tok rejecting();
  void merge(ast::Tok &lhs, ast::Tok rhs);
};

namespace parser {
  using namespace ast;

  Program parseProgram(err::ErrorContext &ctx, lexer::TokenIter<Tok> &tokens);
}
