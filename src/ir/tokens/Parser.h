#pragma once

#include "../ast/Ast.h"
#include "../../common/Err.h"

template<>
struct fsm::Finished<ast::Tok> {
  ast::Tok rejecting();
  void merge(ast::Tok &lhs, ast::Tok rhs);
};

namespace parser {
  using namespace ast;

  Program parseProgram(err::ErrorContext &ctx, lexer::TokenIter<Tok> &tokens);
}
