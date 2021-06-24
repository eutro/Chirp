#pragma once

namespace fsm {
  /**
   * A state of a DFA.
   *
   * @tparam S The type of symbols that the DFA can accept.
   * @tparam F The type of the "finished" tag of states.
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
     *
     * Instead of distinguishing between only final and non-final states,
     * this DFA supports any arbitrary tag for finished states, hence the "finished" tag.
     */
    F finished;
  };

  /**
   * A DFA, a Deterministic Finite Automaton.
   *
   * @tparam S The type of symbols that the DFA can accept.
   * @tparam F The type of the "finished" tag of states.
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

    DFA<S, F> minimise() const {
      // determine alphabet from all edges
      std::set<S> alphabet;
      for (const DFAState<S, F> &state : states) {
        for (const auto &transition : state.transitions) {
          alphabet.insert(transition.first);
        }
      }

      // states are distinguishable iff there's a possible string that leads to a different finish

      // bottommost and rightmost edges are sentinels representing the state reached after there's no transition
      std::vector<std::vector<bool>> distinguishable(states.size() + 1, std::vector<bool>(states.size() + 1, true));
      for (int i = 0; i < states.size(); ++i) {
        for (int j = i + 1; j < states.size(); ++j) {
          // if states have different finish tags then the empty string leads them to a different finish
          distinguishable[i][j] = distinguishable[j][i]
              = states[i].finished != states[j].finished;
        }
      }
      // all states are indistinguishable from themselves
      for (int i = 0; i <= states.size(); ++i) {
        distinguishable[i][i] = false;
      }

      // repeatedly find distinguishable states until fixed point
      bool changed;
      do {
        changed = false;
        for (const S &symbol : alphabet) {
          for (int i = 0; i < states.size() - 1; ++i) {
            // if there's a letter of the alphabet that leads to distinguishable states
            // then the two states are also distinguishable
            auto iFound = states[i].transitions.find(symbol);
            size_t iTransition = iFound != states[i].transitions.end() ? iFound->second : states.size();
            for (int j = i + 1; j < states.size(); ++j) {
              if (distinguishable[i][j]) continue;

              auto jFound = states[j].transitions.find(symbol);
              size_t jTransition = jFound != states[j].transitions.end() ? jFound->second : states.size();
              if (distinguishable[iTransition][jTransition]) {
                distinguishable[i][j] = distinguishable[j][i] = true;
                changed = true;
              }
            }
          }
        }
      } while (changed);

      // group states that are indistinguishable
      DFA<S, F> optimised;
      std::map<size_t, size_t> grouped;
      for (int i = 0; i < states.size(); ++i) {
        for (int j = 0; j < states.size(); ++j) {
          if (distinguishable[i][j]) continue;
          auto found = grouped.find(j);
          if (found != grouped.end()) {
            grouped[i] = found->second;
            goto nextI;
          }
        }
        {
          size_t state = grouped[i] = optimised.push();
          optimised.states[state].finished = states[i].finished;
        }
       nextI:;
      }

      // add their transitions
      for (int i = 0; i < states.size(); ++i) {
        for (const auto &transition : states[i].transitions) {
          optimised.states[grouped[i]].transitions[transition.first] = grouped[transition.second];
        }
      }
      optimised.initial = grouped[initial];
      return std::move(optimised);
    }
  };
}
