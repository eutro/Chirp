#pragma once

namespace fsm {
  /**
   * A state of a DFA.
   *
   * @tparam S The type of symbols that the DFA can accept.
   * @tparam F The type of the final tag of states.
   */
  template <typename S, typename F>
  class DFAState {
  public:
    /**
     * The transitions of this DFA.
     *
     * Each entry maps an input symbol to the state
     * this DFA can transition to upon seeing that symbol.
     */
    std::map<S, size_t> transitions;
    /**
     * The finished tag of this state.
     */
    F finished;
  };

  /**
   * A DFA, a Deterministic Finite Automaton.
   *
   * @tparam S The type of symbols that the DFA can accept.
   * @tparam F The type of the final tag of states.
   */
  template <typename S, typename F>
  class DFA {
  public:
    /**
     * All the states of this DFA.
     */
    std::vector<DFAState<S, F>> states;

    /**
     * The initial state of this DFA.
     */
    size_t initial;

    /**
     * Add a state to this DFA.
     *
     * @return The index of the added state.
     */
    size_t push() {
      states.emplace_back();
      return states.size() - 1;
    }

    /**
     * Accept a single symbol, replacing the current state with a new state.
     *
     * @param current The current state, to update.
     * @param symbol The symbol to accept.
     * @return Whether it was possible to advance the DFA.
     */
    bool accept(size_t &current, S &symbol) const {
      const auto &transitions = states[current].transitions;
      const auto &found = transitions.find(symbol);
      if (found == transitions.end()) {
        return false;
      } else {
        current = found->second;
        return true;
      }
    }

    /**
     * Match an iterator, starting with a set of initial states.
     *
     * @tparam Iter The type of the iterator.
     * @param initial The set of initial states.
     * @param start The start iterator.
     * @param end The end iterator.
     * @return The "final" tag of the final state.
     */
    template<typename Iter>
    F match(Iter start, Iter end) const {
      size_t current = initial;
      for (Iter it = start; it != end; ++it) {
        if (!accept(current, *it)) {
          return F();
        }
      }
      return states[current].finished;
    }

    /**
     * Match an iterable, starting with a set of initial states.
     *
     * @tparam Iterable The type of the iterable.
     * @param initial The set of initial states.
     * @param iterable The iterable.
     * @return The "final" tag of the final state.
     */
    template<typename Iterable>
    F match(Iterable iterable) const {
      return match(std::begin(iterable), std::end(iterable));
    }

    /**
     * Write this DFA to out, for debugging purposes.
     *
     * Each state is written as an index, followed by its "final" tag in brackets.
     *
     * Then each transition of the state is written on a new line as a '-' followed by the
     * symbol to accept, followed by a ' -> ' and the state that can be reached accepting that symbol.
     *
     * @param out The stream to write this to.
     * @param dfa The DFA to write.
     * @return out
     */
    friend std::ostream &operator<<(std::ostream &out, const DFA<S, F> &dfa) {
      size_t i = 0;
      for (const DFAState<S, F> &state : dfa.states) {
        out << i << " (" << state.finished << ")";
        for (const auto &trans : state.transitions) {
          out << "\n- " << trans.first << " -> " << trans.second;
        }
        out << "\n";
        i++;
      }
      return out;
    }
  };
}
