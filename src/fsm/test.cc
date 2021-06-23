#include "NFA.tcc"
#include "RegEx.tcc"
#include "re.tcc"

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <stdexcept>
#include <string>

template <typename F>
void assertMatches(fsm::NFA<char, F> &nfa,
                   size_t start,
                   F expected,
                   std::string s) {
  F result = nfa.match({start}, s.begin(), s.end());
  if (result != expected) {
    std::cerr << "Assertion failed:\n"
              << " Matching \"" << s << "\"\n"
              << "  Expected: <" << expected << ">\n"
              << "  Got: <" << result << ">\n"
              << "NFA:\n" << nfa;
    exit(0);
  }
}

int main() {
  fsm::re::RegEx<char> re = fsm::re::parseString("a*bc*");
  fsm::NFA<char, bool> nfa;
  size_t start, end;
  std::tie(start, end) = re.toNfa(nfa);
  nfa.states[end].finished = true;

  assertMatches(nfa, start, true, "b");
  assertMatches(nfa, start, true, "abc");
  assertMatches(nfa, start, true, "aaaabcccc");

  assertMatches(nfa, start, false, "aaa");
  assertMatches(nfa, start, false, "cccc");
  assertMatches(nfa, start, false, "aabbccc");
  assertMatches(nfa, start, false, "aaaccc");
}
