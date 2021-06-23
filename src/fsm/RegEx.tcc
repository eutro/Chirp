#pragma once

#include <vector>
#include <map>

namespace fsm {
  namespace re {
    enum class Type {
      Direct,
      Union,
      Concat,
      KleeneStar,
    };

    template <typename S>
    class RegEx {
    public:
      Type type = Type::Concat;
      std::vector<RegEx<S>> children;
      std::vector<S> symbols;

      template <typename F>
      std::pair<size_t, size_t> toNfa(NFA<S, F> &nfa) const {
        size_t start = nfa.push();
        size_t end = nfa.push();
        switch (type) {
        case Type::Direct: {
          if (!symbols.size()) {
            nfa.states[start].emptyTransitions.insert(end);
          } else {
            size_t last = start;
            auto it = symbols.begin();
            do {
              size_t next;
              if (++it == symbols.end()) {
                next = end;
              } else {
                next = nfa.push();
              }
              nfa.states[last].transitions[*(--it)].insert(next);
            } while (++it != symbols.end());
          }
          break;
        }
        case Type::Union: {
          for (const RegEx<S> &child : children) {
            size_t eStart, eEnd;
            std::tie(eStart, eEnd) = child.toNfa(nfa);
            nfa.states[start].emptyTransitions.insert(eStart);
            nfa.states[eEnd].emptyTransitions.insert(end);
          }
          break;
        }
        case Type::Concat: {
          size_t last = start;
          for (const RegEx<S> &child : children) {
            size_t eStart, eEnd;
            std::tie(eStart, eEnd) = child.toNfa(nfa);
            nfa.states[last].emptyTransitions.insert(eStart);
            last = eEnd;
          }
          nfa.states[last].emptyTransitions.insert(end);
          break;
        }
        case Type::KleeneStar: {
          size_t eStart, eEnd;
          const RegEx<S> &child = children[0];
          std::tie(eStart, eEnd) = child.toNfa(nfa);
          nfa.states[start].emptyTransitions.insert(eStart);
          nfa.states[start].emptyTransitions.insert(end);
          nfa.states[eEnd].emptyTransitions.insert(start);
        }
        }
        return std::make_pair(start, end);
      }
    };
  }
}
