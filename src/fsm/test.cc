#include "NFA.tcc"
#include "RegEx.tcc"

#include <cstdlib>
#include <iostream>
#include <string>

template<typename F>
void assertMatches(const std::string &regex,
                   fsm::NFA<char, F> &nfa,
                   size_t start,
                   F expected,
                   const std::string &s) {
  F result = nfa.match({start}, s);
  if (result != expected) {
    std::cerr << "Assertion failed:\n"
              << " Matching \"" << s << "\"\n"
              << " With /" << regex << "/\n"
              << "  Expected: <" << expected << ">\n"
              << "  Got: <" << result << ">\n"
              << "NFA:\n" << nfa;
    exit(1);
  }
}

void checkRegex(const std::string &regex,
                const std::vector<std::pair<std::string, bool>> &assertions) {
  fsm::re::RegEx<char> re = fsm::re::parseFromString(regex);
  fsm::NFA<char, bool> nfa;
  size_t start, end;
  std::tie(start, end) = re.toNfa(nfa);
  nfa.states[end].finished = true;

  for (const auto &assertion : assertions) {
    assertMatches(regex, nfa, start, assertion.second, assertion.first);
  }
}

int main() {
  checkRegex(
      "a*bc*",
      {
          {"b",         true},
          {"abc",       true},
          {"aaaabcccc", true},

          {"aaa",       false},
          {"cccc",      false},
          {"aabbccc",   false},
          {"aaaccc",    false},
      });

  checkRegex(
      "[abcd]+",
      {
          {"a",    true},
          {"b",    true},
          {"abab", true},

          {"",     false},
          {"abe",  false},
      }
  );

  checkRegex(
      "(ab|c[dg]|ef)*",
      {
          {"",         true},
          {"cd",       true},
          {"cdef",     true},
          {"abefcdcd", true},
          {"efcdcgab", true},

          {"ace",      false},
      }
  );
}
