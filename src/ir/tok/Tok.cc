#include "Tok.h"
#include "LexerCommon.h"

namespace tok {
#if (USE_COMPILED_DFA)
#include "CompiledDFA.cc"
  const LexerType LEXER;
#else
  static unsigned char DFA_DATA[] = {
#include "DFAData.h"
  };
  const LexerType LEXER(DFA_DATA, sizeof(DFA_DATA));
#endif
  const LexerType &lexer() {
    return LEXER;
  }
}
