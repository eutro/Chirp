#include "../NFA.h"
#include "../RegEx.h"

#include <cstdlib>
#include <iostream>
#include <string>

template<typename NFA, typename F>
void assertMatches(const std::string &regex,
                   NFA &nfa,
                   const std::string &run,
                   F expected,
                   const std::string &s) {
  F result = nfa.match(s);
  if (result != expected) {
    std::cerr << "Assertion failed:\n"
              << " Matching \"" << s << "\"\n"
              << " With /" << regex << "/\n"
              << "  Expected: <" << expected << ">\n"
              << "  Got: <" << result << ">\n"
              << run << ":\n"
              << nfa;
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
  nfa.initial.insert(start);

  std::cerr << "/" << regex << "/:\n";

  std::cerr << "NFA:\n" << nfa << "\n";
  for (const auto &assertion : assertions) {
    assertMatches(regex, nfa, "NFA", assertion.second, assertion.first);
  }

  fsm::DFA<char, bool> dfa = nfa.toDfa();
  std::cerr << "DFA:\n" << dfa << "\n";
  for (const auto &assertion : assertions) {
    assertMatches(regex, dfa, "DFA", assertion.second, assertion.first);
  }

  fsm::DFA<char, bool> minDfa = dfa.minimise();
  std::cerr << "DFA (min):\n" << minDfa << "\n";
  for (const auto &assertion : assertions) {
    assertMatches(regex, minDfa, "DFA (min)", assertion.second, assertion.first);
  }
}

void assertFailsParse(const std::string &s) {
  try {
    fsm::re::parseFromString(s);
    std::cerr << "RegEx: /" << s << "/ erroneously parsed successfully.\n";
    exit(0);
  } catch (std::runtime_error &ignored) {
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
      "(ab|c[d\\]]|\\)f)*",
      {
          {"",         true},
          {"cd",       true},
          {"cd)f",     true},
          {"ab)fcdcd", true},
          {")fcdc]ab", true},

          {"ac)",      false},
          {"a]",       false},
      }
  );

  checkRegex(
      "(ab(cd)*ef)|ab|ef",
      {
          {"abef",     true},
          {"abcdef",   true},
          {"abcdcdef", true},
          {"ab",       true},
          {"ef",       true},

          {"abcdefcd", false},
      }
  );

  checkRegex(
      "a|",
      {
          {"a",  true},
          {"",   true},

          {"aa", false},
      }
  );

  checkRegex(
      ".+",
      {
          {"a", true},

          {"",  false},
      }
  );

  checkRegex(
      "[b-y]",
      {
          {"a", false},
          {"b", true},
          {"y", true},
          {"z", false},
      }
  );

  checkRegex(
      "\"([^\"\\\\]|\\\\[\"\\\\])*\"",
      {
          {"\"\"",                                                  true},
          {"\"\\\"([^\\\"\\\\\\\\]|\\\\\\\\[\\\"\\\\\\\\])*\\\"\"", true},
          {"\"\\\"",                                                false},
          {"\"\\\"([^\"\\\\\\\\]|\\\\\\\\[\\\"\\\\\\\\])*\\\"\"",   false},
      }
  );

  assertFailsParse("(");
  assertFailsParse(")");
  assertFailsParse("[");
  assertFailsParse("\\");
  assertFailsParse("((())))");
  assertFailsParse("))))(((");
  assertFailsParse("[\\]");
  assertFailsParse("[\\");
  assertFailsParse("[a-]");
}
