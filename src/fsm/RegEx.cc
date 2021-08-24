#include "RegEx.h"

#include <climits>

namespace fsm::re {
  static bool readEscape(char &c) {
    switch (c) {
      case 'n':
        c = '\n';
        return true;
      case 'r':
        c = '\r';
        return true;
      case 'v':
        c = '\v';
        return true;
      case 'f':
        c = '\f';
        return true;
      case 't':
        c = '\t';
        return true;
    }
    return false;
  }

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
            char nextSym = *start;
            if (*start == '\\') {
              ++start;
              if (start == end) {
                throw std::runtime_error("Expected character after \\");
              }
              nextSym = *start;
              readEscape(nextSym);
            }
            ++start;
            if (start != end && *start == '-') {
              ++start;
              if (start == end || *start == ']') {
                throw std::runtime_error("Expected character after -");
              }
              char endSym = *start;
              for (char sym = nextSym - 1; sym++ != endSym;) {
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
            unionVec = std::vector<RegEx<char >>();
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
        case '\\': {
          ++start;
          if (start == end) {
            throw std::runtime_error("Expected character after \\");
          }
          char c = *start;
          if (readEscape(c)) {
            re.type = Type::Literal;
            re.symbols.push_back(c);
            ++start;
            goto postfix;
          }
        }
        default:
          re.type = Type::Literal;
          re.symbols.push_back(*start);
          ++start;
          break;
      }
     postfix:
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
    return ret;
  }
}