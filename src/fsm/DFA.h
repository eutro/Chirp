#pragma once

namespace fsm {
  template<typename F>
  struct Finished {
    F rejecting();
    void merge(F &lhs, F rhs) {
      lhs = std::max(lhs, rhs);
    }
  };

  template<>
  struct Finished<bool> {
    static bool rejecting() {
      return false;
    }
    static void merge(bool &lhs, bool rhs) {
      lhs |= rhs;
    }
  };

  template <typename T, typename T2 = void>
  struct Codec;

  /**
   * A state of a DFA.
   *
   * @tparam S The type of symbols that the DFA can accept.
   * @tparam F The type of the "finished" tag of states.
   */
  template<typename S, typename F>
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
    F finished = Finished<F>().rejecting();
  };

  /**
   * A DFA, a Deterministic Finite Automaton.
   *
   * @tparam S The type of symbols that the DFA can accept.
   * @tparam F The type of the "finished" tag of states.
   */
  template<typename S, typename F>
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

    F finished(size_t state) const {
      return states[state].finished;
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
      for (size_t i = 0; i < states.size(); ++i) {
        for (size_t j = i + 1; j < states.size(); ++j) {
          // if states have different finish tags then the empty string leads them to a different finish
          distinguishable[i][j] = distinguishable[j][i]
              = states[i].finished != states[j].finished;
        }
      }
      // all states are indistinguishable from themselves
      for (size_t i = 0; i <= states.size(); ++i) {
        distinguishable[i][i] = false;
      }

      // repeatedly find distinguishable states until fixed point
      bool changed;
      do {
        changed = false;
        for (const S &symbol : alphabet) {
          for (size_t i = 0; i < states.size() - 1; ++i) {
            // if there's a letter of the alphabet that leads to distinguishable states
            // then the two states are also distinguishable
            auto iFound = states[i].transitions.find(symbol);
            size_t iTransition = iFound != states[i].transitions.end() ? iFound->second : states.size();
            for (size_t j = i + 1; j < states.size(); ++j) {
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
      for (size_t i = 0; i < states.size(); ++i) {
        for (size_t j = 0; j < states.size(); ++j) {
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
      for (size_t i = 0; i < states.size(); ++i) {
        for (const auto &transition : states[i].transitions) {
          optimised.states[grouped[i]].transitions[transition.first] = grouped[transition.second];
        }
      }
      optimised.initial = grouped[initial];
      return optimised;
    }
    
    void compileInto(const std::string &sTy, const std::string &fTy, std::ostream &os) const {
      os << "CompiledDFA::CompiledDFA(): initial(" << initial << ") {}\n";
      os << "bool CompiledDFA::accept(size_t &current, " << sTy << " &symbol) const {\n";
      os << "  switch (current) {\n";
      for (size_t state = 0; state < states.size(); ++state) {
        os << "    case " << state << ":";
        const DFAState<S, F> &stateV = states.at(state);
        if (stateV.transitions.empty()) {
          os << " break;\n";
          continue;
        } else {
          os << "\n";
        }
        os << "      switch (symbol) {\n";
        std::map<size_t, std::vector<const S *>> trev;
        for (auto &e : stateV.transitions) {
          trev[e.second].push_back(&e.first);
        }
        for (auto &e : trev) {
          for (auto it = e.second.begin(); it != e.second.end();) {
            os << "        case ";
            if constexpr (std::is_same_v<S, char>) {
              os << "(char)0x" << std::hex << (unsigned int) (unsigned char) **it;
              os << std::dec;
            } else {
              os << e.first;
            }
            os << ":";
            if (++it != e.second.end()) os << " [[fallthrough]];\n";
            else os << "\n";
          }
          os << "          current = " << e.first << ";\n";
          os << "          return true;\n";
        }
        os << "      }\n";
        os << "      break;\n";
      }
      os << "  }\n";
      os << "  return false;\n";
      os << "}\n";
      os << "" << fTy << " CompiledDFA::finished(size_t state) const {\n";
      os << "  switch (state) {\n";
      for (size_t state = 0; state < states.size(); ++state) {
        os << "    case " << state << ":";
        const DFAState<S, F> &stateV = states.at(state);
        os << " return (" << fTy << ")0x" << std::hex << (size_t) stateV.finished << std::dec << ";\n";
      }
      os << "  }\n";
      os << "  return (" << fTy << ")0;\n";
      os << "}\n";
    }
  };

  template <typename I>
  struct Codec<I, typename std::enable_if<std::is_trivially_copyable_v<I>>::type> {
    void encode(const I &i, std::ostream &os) {
      os.write(reinterpret_cast<const char*>(&i), sizeof(I));
    }
    void decode(I &i, std::istream &is) {
      is.read(reinterpret_cast<char *>(&i), sizeof(I));
    }
  };

  template <typename T>
  struct Codec<std::vector<T>> {
    void encode(const std::vector<T> &i, std::ostream &os) {
      Codec<size_t>().encode(i.size(), os);
      Codec<T> c;
      for (const T &t : i) {
        c.encode(t, os);
      }
    }
    void decode(std::vector<T> &i, std::istream &is) {
      size_t size;
      Codec<size_t>().decode(size, is);
      i.resize(size);
      Codec<T> c;
      for (T &t : i) {
        c.decode(t, is);
      }
    }
  };

  template <typename K, typename V>
  struct Codec<std::map<K, V>> {
    Codec<size_t> ic;
    Codec<K> kc;
    Codec<V> vc;
    void encode(const std::map<K, V> &m, std::ostream &os) {
      ic.encode(m.size(), os);
      for (const auto &kv : m) {
        kc.encode(kv.first, os);
        vc.encode(kv.second, os);
      }
    }
    void decode(std::map<K, V> &m, std::istream &is) {
      size_t size;
      ic.decode(size, is);
      for (size_t i = 0; i < size; ++i) {
        std::pair<K, V> kv;
        kc.decode(kv.first, is);
        vc.decode(kv.second, is);
        m.emplace_hint(m.end(), std::move(kv));
      }
    }
  };

  template <typename S, typename F>
  struct Codec<DFAState<S, F>> {
    Codec<std::map<S, size_t>> tc;
    Codec<F> fc;
    void encode(const DFAState<S, F> &s, std::ostream &os) {
      tc.encode(s.transitions, os);
      fc.encode(s.finished, os);
    }
    void decode(DFAState<S, F> &s, std::istream &is) {
      tc.decode(s.transitions, is);
      fc.decode(s.finished, is);
    }
  };

  template <typename S, typename F>
  struct Codec<DFA<S, F>> {
    Codec<std::vector<DFAState<S, F>>> vc;
    Codec<size_t> ic;
    void encode(const DFA<S, F> &dfa, std::ostream &os) {
      vc.encode(dfa.states, os);
      ic.encode(dfa.initial, os);
    }
    void decode(DFA<S, F> &dfa, std::istream &is) {
      vc.decode(dfa.states, is);
      ic.decode(dfa.initial, is);
    }
  };
}
