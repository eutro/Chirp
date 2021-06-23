#pragma once

namespace fsm {
  template <typename S, typename F>
  class DFAState {
  public:
    std::map<S, std::set<size_t>> transitions;
    F finished;
  };

  template <typename S, typename F>
  class DFA {
  public:
    std::vector<DFAState<S, F>> states;
    
    size_t accept(size_t state, S &symbol) {
      return states[state].transitions[symbol];
    }
  };
}
