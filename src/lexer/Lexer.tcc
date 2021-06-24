#include "fsm/NFA.tcc"
#include "fsm/DFA.tcc"
#include "fsm/RegEx.tcc"

#include <vector>
#include <utility>
#include <string>
#include <iostream>

namespace lexer {
  class SrcLoc {
  public:
    size_t line = 1; // starts at 1
    size_t col = 0; // starts at 0
  };

  template <typename TokenType>
  class Token {
  public:
    TokenType type;
    std::string value;
    SrcLoc loc;
  };

  template <class TokenType>
  class Lexer {
  public:
    fsm::DFA<char, TokenType> dfa;

    explicit Lexer(const std::vector<std::pair<TokenType, std::string>> tokens) {
      fsm::NFA<char, TokenType> nfa;
      for (const std::pair<TokenType, std::string> &token : tokens) {
        fsm::re::RegEx<char> regex = fsm::re::parseFromString(token.second);
        size_t tokenStart, tokenEnd;
        std::tie(tokenStart, tokenEnd) = regex.toNfa(nfa);
        nfa.states[tokenEnd].finished = token.first;
        nfa.initial.insert(tokenStart);
      }
      dfa = nfa.toDfa().minimise();
    }

    std::vector<Token<TokenType>> lex(std::istream &in) {
      std::vector<Token<TokenType>> lexed;
      size_t state = dfa.initial;
      int c;
      char cc;
      std::string raw;
      size_t pos = 0;

      size_t lastMatchLoc;
      size_t lastMatchState;
      bool hasMatch = false;

      bool relexing = false;
      while (relexing ?
             (c = (unsigned char) raw[pos], true) :
             (c = in.get()) != -1) {
        cc = (char) c;
        ++pos;
        if (relexing) {
          if (pos >= raw.size()) {
            relexing = false;
          }
        } else {
          raw.push_back(cc);
        }
        if (!dfa.accept(state, cc)) {
        addTok:
          Token<TokenType> tok;
          if (!hasMatch) {
            tok.type = fsm::Finished<TokenType>().rejecting();
            tok.value = std::move(raw);
            raw = "";
          } else {
            tok.type = dfa.states[lastMatchState].finished;
            tok.value = raw.substr(0, lastMatchLoc);
            raw.erase(0, lastMatchLoc);
            hasMatch = false;
            if (!raw.empty()) {
              relexing = true;
            }
          }
          pos = 0;
          lexed.push_back(std::move(tok));
          state = dfa.initial;
        } else if (dfa.states[state].finished != fsm::Finished<TokenType>().rejecting()) {
          lastMatchLoc = pos;
          lastMatchState = state;
          hasMatch = true;
        }
      }
      if (pos != 0) {
        goto addTok;
      }
      return std::move(lexed);
    }
  };
}
