#pragma once

#include <map>
#include <set>
#include <vector>
#include <queue>
#include <iostream>

namespace fsm {
  /**
   * A state of an NFA.
   *
   * @tparam S The type of symbols that the NFA can accept.
   * @tparam F The type of the final tag of states.
   */
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

  /**
   * An NFA, a Nondeterministic Finite Automaton, which supports epsilon moves.
   *
   * @tparam S The type of symbols that the NFA can accept.
   * @tparam F The type of the final tag of states.
   */
  template <typename S, typename F>
  class NFA {
  public:
    /**
     * All the states in this NFA.
     */
    std::vector<NFAState<S, F>> states;

    /**
     * Add a state to this NFA.
     *
     * @return The index of the added state.
     */
    size_t push() {
      states.push_back(NFAState<S, F>());
      return states.size() - 1;
    }

    /**
     * Add to the set all states that are reachable by empty transitions only.
     *
     * @param reachable The set of states whose epsilon closure should be computed.
     */
    void eClosure(std::set<size_t> &reachable) const {
      std::queue<size_t> queue;
      for (size_t curState : reachable) {
        queue.push(curState);
      }
      while (!queue.empty()) {
        for (size_t nextState : states[queue.front()].emptyTransitions) {
          if (reachable.count(nextState)) {
            continue;
          }
          reachable.insert(nextState);
          queue.push(nextState);
        }
        queue.pop();
      }
    }

    /**
     * Accept a symbol, updating the current states.
     *
     * The epsilon closure is not computed for the current set of states.
     * If this NFA has empty transitions, eClosure must be called manually
     * before and possibly after accepting.
     *
     * @param current The current set of states of the FSM, to be updated.
     * @param symbol The symbol to accept.
     */
    void accept(std::set<size_t> &current, S &symbol) const {
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

    /**
     * Match an iterator, starting with a set of initial states.
     *
     * @tparam Iter The type of the iterator.
     * @param initial The set of initial states.
     * @param start The start iterator.
     * @param end The end iterator.
     * @return The OR of the set of final states.
     */
    template <typename Iter>
    F match(const std::set<size_t> &initial, Iter start, Iter end) const {
      std::set<size_t> current = initial;
      for (Iter it = start; it != end; ++it) {
        eClosure(current);
        accept(current, *it);
      }
      eClosure(current);
      F ret{};
      for (auto state : current) {
        ret |= states[state].finished;
      }
      return std::move(ret);
    }

    /**
     * Match an iterable, starting with a set of initial states.
     *
     * @tparam Iterable The type of the iterable.
     * @param initial The set of initial states.
     * @param iterable The iterable.
     * @return The OR of the set of final states.
     */
    template <typename Iterable>
    F match(const std::set<size_t> &initial, Iterable iterable) const {
      return match(initial, std::begin(iterable), std::end(iterable));
    }

    /**
     * Write this NFA to out, for debugging purposes.
     *
     * Each state is written as an index, followed by its final tag in brackets,
     * then followed on the same line by all the states that can be reached by
     * a single epsilon move (i.e. without consuming an input symbol).
     *
     * Then each transition of the state is written on a new line as a '-' followed by the
     * symbol to accept, followed by a ' -> ' and all the states that can be reached accepting that symbol.
     *
     * @param out The stream to write this to.
     * @param nfa The NFA to write.
     * @return out
     */
    friend std::ostream &operator<<(std::ostream &out, const NFA<S, F> &nfa) {
      size_t i = 0;
      for (const NFAState<S, F> &state : nfa.states) {
        out << i << " (" << state.finished << ")";
        for (const size_t &alt : state.emptyTransitions) {
          out << " = " << alt;
        }
        for (const auto &trans : state.transitions) {
          out << "\n- " << trans.first << " ->";
          for (const auto &end : trans.second) {
            out << " " << end;
          }
        }
        out << "\n";
        i++;
      }
      return out;
    }
  };
}
