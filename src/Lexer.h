#pragma once

#include "NFA.h"
#include "DFA.h"
#include "RegEx.h"

#include <vector>
#include <utility>
#include <string>
#include <iostream>

namespace lexer {
  /**
   * Represents a location in a file or string.
   */
  class SrcLoc {
  public:
    /**
     * The line number. Starts at 1.
     */
    size_t line;
    /**
     * The column number. Starts at 0.
     */
    size_t col;

    SrcLoc();
    SrcLoc(size_t line, size_t col);
    friend std::ostream &operator<<(std::ostream &os, const SrcLoc &loc);
    void add(const std::string &s);
  };

  /**
   * A token lexed from a stream.
   *
   * @tparam TokenType The type of tokens.
   */
  template<typename TokenType>
  class Token {
  public:
    /**
     * The type of the token.
     */
    TokenType type;
    /**
     * The raw string value of the token.
     */
    std::string value;
    /**
     * The location of the token.
     */
    SrcLoc loc;

    Token() : type(fsm::Finished<TokenType>().rejecting()) {}

    Token(TokenType type, std::string &&value, const SrcLoc &loc) : type(type), value(value), loc(loc) {}
  };

  /**
   * A token stream that can yield tokens from an input stream asynchronously.
   *
   * @tparam TokenType The type of tokens.
   */
  template<typename TokenType>
  class TokenStream {
  public:
    const fsm::DFA<char, TokenType> &dfa;

    std::istream &in;

    TokenStream(const fsm::DFA<char, TokenType> &dfa, std::istream &in) : dfa(dfa), in(in) {}

    /**
     * The state of the DFA.
     */
    size_t state = dfa.initial;

    std::string raw;
    size_t pos = 0;

    size_t lastMatchPos;
    size_t lastMatchState;
    bool hasMatch = false;

    /**
     * Keeps track of the location in the file.
     */
    SrcLoc loc;

    bool relexing = false;

    /**
     * Get the next token from the stream, if any.
     *
     * @param tok The token in which to store data.
     * @return True if a token was read.
     */
    bool next(Token<TokenType> &tok) {
      char c;
      while (relexing ?
             (c = raw[pos], true) :
             !in.get(c).fail()) {
        ++pos;
        if (relexing) {
          if (pos >= raw.size()) {
            relexing = false;
          }
        } else {
          raw.push_back(c);
        }
        if (!dfa.accept(state, c)) {
         addTok:
          pos = 0;
          if (!hasMatch) {
            tok.type = fsm::Finished<TokenType>().rejecting();
            tok.value = std::move(raw);
            raw = "";
          } else {
            hasMatch = false;
            tok.type = dfa.states[lastMatchState].finished;
            if (lastMatchPos == raw.size()) {
              tok.value = std::move(raw);
              raw = "";
            } else {
              tok.value = raw.substr(0, lastMatchPos);
              raw.erase(0, lastMatchPos);
              relexing = true;
            }
          }
          tok.loc = loc;
          loc.add(tok.value);
          state = dfa.initial;
          return true;
        } else if (dfa.states[state].finished != fsm::Finished<TokenType>().rejecting()) {
          lastMatchPos = pos;
          lastMatchState = state;
          hasMatch = true;
        }
      }
      if (pos != 0) {
        goto addTok;
      }
      return false;
    }
  };

  /**
   * An iterable over tokens from a stream.
   *
   * @tparam TokenType The type of tokens.
   */
  template<typename TokenType>
  class TokenIter {
    TokenStream<TokenType> stream;

  public:
    TokenIter(TokenStream<TokenType> &&stream) : stream(stream) {}

    /**
     * The iterator type. Satisfies std::input_iterator_tag.
     */
    struct Iter {
      TokenIter &iter;
      Token<TokenType> tok;
      bool end;

      bool operator==(const Iter &o) {
        return end && o.end;
      }

      bool operator!=(const Iter &o) {
        return !end || !o.end;
      }

      Iter &operator++() {
        end = !iter.stream.next(tok);
        return *this;
      }

      Token<TokenType> &operator*() {
        return tok;
      }

      Token<TokenType> *operator->() {
        return &tok;
      }
    };

    Iter begin() {
      return ++(Iter{*this, Token<TokenType>(), false});
    }

    Iter end() {
      return Iter{*this, Token<TokenType>(), true};
    }
  };

  /**
   * The Lexer tokenises a stream of characters into Tokens,
   * recognising their types and storing their source location.
   *
   * @tparam TokenType The type of tokens.
   */
  template<typename TokenType>
  class Lexer {
  public:
    /**
     * The Deterministic Finite Automaton used to match the stream.
     */
    fsm::DFA<char, TokenType> dfa;

    /**
     * Construct a Lexer from a list of token types to regular expressions that
     * match them.
     *
     * This constructs an NFA from the regular expressions, then converts that to a minimal DFA.
     *
     * @param tokens A vector of pairs of token types to regular expression strings that match them.
     */
    explicit Lexer(const std::vector<std::pair<TokenType, std::string>> &tokens) {
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

    /**
     * Create a stream of tokens from an input stream.
     *
     * The lifetime of this token stream is no longer than that of
     * this and that of the input stream. i.e. the returned stream is valid
     * while the reference to the input stream and the pointer to this is valid.
     *
     * @param in The input stream.
     * @return The token stream.
     */
    TokenStream<TokenType> stream(std::istream &in) const {
      return TokenStream<TokenType>(dfa, in);
    }

    /**
     * Lex an input stream, returning an iterable over tokens lexed from the stream.
     *
     * The lifetime of this iterable is the same as described in {@code stream(in)}.
     * Furthermore, this iterable can only be iterated over once.
     *
     * @param in The input stream.
     * @return An iterable of tokens.
     */
    TokenIter<TokenType> lex(std::istream &in) const {
      return TokenIter<TokenType>(stream(in));
    }
  };
}
