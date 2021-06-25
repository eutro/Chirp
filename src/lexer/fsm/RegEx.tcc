#pragma once

#include <vector>
#include <map>
#include <climits>

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
  template <typename S>
  class RegEx {
  public:
    Type type;
    std::vector<RegEx<S>> children;
    std::vector<S> symbols;

    explicit RegEx(Type type): type(type) {}

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
    template <typename F>
    std::pair<size_t, size_t> toNfa(NFA<S, F> &nfa) const {
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
  RegEx<char> parseFromString(const std::string &s) {
    auto start = s.begin();
    auto end = s.end();

    std::deque<RegEx<char>> groupStack;
    groupStack.emplace_back(Type::Concat);
    std::vector<RegEx<char>> unionVec;
    while (start != end) {
      RegEx<char> re(Type::Concat);
      switch (*start) {
        case '(': {
          groupStack.emplace_back(Type::Concat);
          ++start;
          continue;
        }
        case '|': {
          unionVec.push_back(std::move(groupStack.back()));
          groupStack.pop_back();
          groupStack.emplace_back(Type::Concat);
          ++start;
          continue;
        }
        case '[': {
          ++start;
          re.type = Type::Union;
          bool invert = false;
          if (start != end && *start == '^') {
            ++start;
            invert = true;
          }
          while (start != end && *start != ']') {
            if (*start == '\\') {
              ++start;
              if (start == end) {
                throw std::runtime_error("Expected character after \\");
              }
            }
            char nextSym = *start;
            ++start;
            if (start != end && *start == '-') {
              ++start;
              if (start == end || *start == ']') {
                throw std::runtime_error("Expected character after -");
              }
              char endSym = *start;
              for (char sym = nextSym - 1; sym++ != endSym; ) {
                re.symbols.push_back(sym);
              }
            } else {
              re.symbols.push_back(nextSym);
            }
          }
          if (start == end) {
            throw std::runtime_error("Unmatched [");
          }
          ++start;
          if (invert) {
            std::set<char> exclude;
            exclude.insert(re.symbols.begin(), re.symbols.end());
            re.symbols.clear();
            for (int c = 0; c <= CHAR_MAX; ++c) {
              if (exclude.count(c)) continue;
              re.symbols.push_back(c);
            }
          }
          break;
        }
        case ')': {
          if (groupStack.size() == 1) {
            throw std::runtime_error("Unmatched )");
          }
          RegEx<char> top = std::move(groupStack.back());
          groupStack.pop_back();
          if (unionVec.empty()) {
            re = std::move(top);
          } else {
            re.type = Type::Union;
            unionVec.push_back(std::move(top));
            re.children = std::move(unionVec);
            unionVec = std::vector<RegEx<char>>();
          }
          ++start;
          break;
        }
        case '.':
          re.type = Type::Union;
          for (int c = 0; c <= CHAR_MAX; ++c) {
            re.symbols.push_back(c);
          }
          ++start;
          break;
        case '\\':
          ++start;
          if (start == end) {
            throw std::runtime_error("Expected character after \\");
          }
        default:
          re.type = Type::Literal;
          re.symbols.push_back(*start);
          ++start;
          break;
      }
      if (start != end) {
        switch (*start) {
          case '*':
          case '+': {
            RegEx<char> repeated(Type::KleeneStar);
            std::swap(re, repeated);
            if (*start == '+') {
              groupStack.back().children.push_back(repeated);
            }
            re.children.push_back(std::move(repeated));
            ++start;
            break;
          }
        }
      }
      groupStack.back().children.push_back(std::move(re));
    }
    RegEx<char> top = std::move(groupStack.back());
    groupStack.pop_back();
    if (!groupStack.empty()) {
      throw std::runtime_error("Unmatched (");
    }
    RegEx<char> ret(Type::Concat);
    if (unionVec.empty()) {
      ret = std::move(top);
    } else {
      ret.type = Type::Union;
      unionVec.push_back(std::move(top));
      ret.children = std::move(unionVec);
    }
    return std::move(ret);
  }
}
