#pragma once

#include "NFA.h"

#include <stdexcept>
#include <vector>
#include <utility>

namespace fsm::re {
  /**
   * The type of a regular expression.
   */
  enum class Type {
    /**
     * Matches the literal symbols.
     */
    Literal,
    /**
     * Matches any one of its children.
     */
    Union,
    /**
     * Matches the concatenation of its children.
     */
    Concat,
    /**
     * Matches its first child 0 or more times.
     */
    KleeneStar,
  };

  /**
   * A regular expression that can be converted into a Nondeterministic Finite Automaton.
   *
   * @tparam S The type of symbols that the RegEx can accept.
   */
  template<typename S>
  class RegEx {
  public:
    Type type;
    std::vector<RegEx<S>> children;
    std::vector<S> symbols;

    explicit RegEx(Type type) : type(type) {}

    /**
     * Add this RegEx to the provided NFA.
     *
     * This method adds at least one new state to the NFA,
     * for which the final state will be default initialized.
     *
     * Uses Thompson's construction algorithm.
     *
     * @tparam F The type of the final tag of states for the NFA.
     * @param nfa The NFA to add the states to.
     * @return A pair of the start and end states that were added to the NFA.
     */
    template<typename F>
    std::pair<size_t, size_t> toNfa(NFA <S, F> &nfa) const {
      switch (type) {
        case Type::Literal: {
          size_t start = nfa.push();
          size_t last = start;
          for (const auto &sym : symbols) {
            nfa.states[last].transitions[sym].insert(last = nfa.push());
          }
          return std::make_pair(start, last);
        }
        case Type::Union: {
          size_t start = nfa.push();
          size_t end = nfa.push();
          for (const RegEx<S> &child : children) {
            size_t eStart, eEnd;
            std::tie(eStart, eEnd) = child.toNfa(nfa);
            nfa.states[start].emptyTransitions.insert(eStart);
            nfa.states[eEnd].emptyTransitions.insert(end);
          }
          for (const auto &sym : symbols) {
            nfa.states[start].transitions[sym].insert(end);
          }
          return std::make_pair(start, end);
        }
        case Type::Concat: {
          if (children.empty()) {
            break;
          } else {
            auto it = children.begin();
            size_t start, end;
            std::tie(start, end) = it->toNfa(nfa);
            ++it;
            for (; it != children.end(); ++it) {
              size_t lastEnd = end;
              size_t first;
              std::tie(first, end) = it->toNfa(nfa);
              nfa.states[lastEnd].emptyTransitions.insert(first);
            }
            return std::make_pair(start, end);
          }
        }
        case Type::KleeneStar: {
          size_t start, end;
          const RegEx<S> &child = children[0];
          std::tie(start, end) = child.toNfa(nfa);
          nfa.states[start].emptyTransitions.insert(end);
          nfa.states[end].emptyTransitions.insert(start);
          return std::make_pair(start, end);
        }
      }
      size_t start = nfa.push();
      return std::make_pair(start, start);
    }
  };

  /**
   * Parse a character RegEx from a string.
   *
   * The following is supported:
   * - Parenthesised groups ('(' and ')').
   * - Literal characters.
   * - Kleene Star/Plus ('*' and '+' postfix).
   * - Pipe unions ('|' infix).
   * - Square bracket single-char unions ('[' and ']').
   *   - Their inversions, using carets ('^').
   *   - Supports ranges with dashes ('-').
   * - Dot matching anything ('.').
   *
   * @param s The string.
   * @return The parsed RegEx.
   */
  RegEx<char> parseFromString(const std::string &s);
}
