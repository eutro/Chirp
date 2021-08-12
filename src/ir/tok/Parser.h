#pragma once

#include "Tok.h"
#include "../ast/Ast.h"
#include "../../common/Err.h"

namespace tok::parser {
  using namespace ast;

  class ParseResult {
  public:
    Program program;
    err::ErrorContext errors;
  };

  ParseResult parseProgram(lexer::TokenIter<Tok> &tokens);
}
