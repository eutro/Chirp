#pragma once

#include "RegEx.tcc"
#include <deque>

namespace fsm {
  namespace re {
    RegEx<char> parseString(const std::string &s) {
      auto start = s.begin();
      auto end = s.end();

      std::deque<RegEx<char>> stack;
      stack.push_back(RegEx<char>());
      while (start != end) {
        if (*start == '(') {
          stack.push_back(RegEx<char>());
          ++start;
          continue;
        }
        RegEx<char> re;
        switch (*start) {
        case ')': {
          if (stack.size() == 1) {
            throw std::runtime_error("Unmatched )");
          }
          RegEx<char> top = std::move(stack.back());
          stack.pop_back();
          re = std::move(top);
          ++start;
          break;
        }
        case '\\':
          ++start;
          if (start == end) {
            throw std::runtime_error("Expected character after \\");
          }
        default:
          re.type = Type::Direct;
          re.symbols.push_back(*start);
          ++start;
          break;
        }
        if (start != end) {
          switch (*start) {
          case '*':
          case '+': {
            RegEx<char> kleened;
            std::swap(re, kleened);
            if (*start == '+') {
              stack.back().children.push_back(kleened);
            }
            re.type = Type::KleeneStar;
            re.children.push_back(std::move(kleened));
            ++start;
            break;
          }
          }
        }
        stack.back().children.push_back(std::move(re));
      }
      auto ret = std::move(stack.back());
      stack.pop_back();
      if (!stack.empty()) {
        throw std::runtime_error("Unmatched (");
      }
      return std::move(ret);
    }
  }
}
