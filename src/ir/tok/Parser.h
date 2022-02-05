#pragma once

#include "Tok.h"
#include "../ast/Ast.h"
#include "../../common/Err.h"

namespace tok::parser {
  using namespace ast;
  using TokIter = lexer::TokenIter<tok::Tok, LexerType::DFA>;

  class ParseResult {
  public:
    Program program;
    err::ErrorContext errors;
  };

  ParseResult parseProgram(TokIter &tokens);
}
