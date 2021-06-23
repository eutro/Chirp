#pragma once

#include <map>
#include <set>
#include <vector>
#include <deque>
#include <queue>
#include <iostream>

namespace fsm {
  template <typename S, typename F>
  class NFAState {
  public:
    /**
     * The transitions of this NFA.
     *
     * Each entry maps an input symbol to the states
     * this NFA can transition to upon seeing that symbol.
     */
    std::map<S, std::set<size_t>> transitions;
    /**
     * The set of empty transitions from this state.
     *
     * This is the set of all nodes that are reachable
     * by a single empty transition.
     */
    std::set<size_t> emptyTransitions;
    /**
     * The finished tag of this state.
     */
    F finished;
  };

  template <typename S, typename F>
  class NFA {
  public:
    /**
     * All the states in this NFA.
     */
    std::vector<NFAState<S, F>> states;

    size_t push() {
      states.push_back(NFAState<S, F>());
      return states.size() - 1;
    }

    /**
     * Add to the set all states that are reachable by empty transitions only.
     */
    void eClosure(std::set<size_t> &states) const {
      std::queue<size_t> queue;
      for (size_t curState : states) {
        queue.push(curState);
      }
      while (queue.size()) {
        for (size_t nextState : this->states[queue.front()].emptyTransitions) {
          if (states.count(nextState)) {
            continue;
          }
          states.insert(nextState);
          queue.push(nextState);
        }
        queue.pop();
      }
    }

    void accept(std::set<size_t> &current, S &symbol) const {
      eClosure(current);
      std::set<size_t> nextStates;
      for (size_t curState : current) {
        auto &transitions = states[curState].transitions;
        auto it = transitions.find(symbol);
        if (it != transitions.end()) {
          for (size_t nextState : it->second) {
            nextStates.insert(nextState);
          }
        }
      }
      current = nextStates;
    }

    template <typename Iter>
    F match(std::set<size_t> initial, Iter start, Iter end) const {
      while (start != end) {
        accept(initial, *start);
        ++start;
      }
      eClosure(initial);
      F ret;
      for (auto state : initial) {
        ret |= states[state].finished;
      }
      return std::move(ret);
    }

    friend std::ostream &operator<<(std::ostream &out, const NFA<S, F> &nfa) {
      size_t i = 0;
      for (const NFAState<S, F> &state : nfa.states) {
        out << i << " (" << state.finished << ")";
        for (const size_t &alt : state.emptyTransitions) {
          out << " = " << alt;
        }
        for (const auto &trans : state.transitions) {
          out << "\n- " << trans.first;
          for (const auto &end : trans.second) {
            out << "\n  -> " << end;
          }
        }
        out << "\n";
        i++;
      }
      return out;
    }
  };
}
