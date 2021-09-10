#pragma once

#include "Util.h"

#include <type_traits>
#include <set>
#include <memory>
#include <vector>

#ifdef ARENA_LOGGING
#include <iostream>
#endif

namespace arena {
  template <typename Base, typename T = Base>
  class Arena {
  public:
    std::vector<std::unique_ptr<Base>> ptrs;

    template <typename NT,
              std::enable_if_t<std::is_base_of<Base, NT>::value, bool> = 0>
    Arena<Base, NT> &cast() {
      return static_cast<Arena<Base, NT>&>(*this);
    }

    template <typename ...Args>
    T *add(Args &&...args) {
      return ptrs.emplace_back(std::make_unique<T>(std::forward<Args>(args)...)).get();
    }
  };

  template <typename Base,
            typename Set
            = std::set<std::unique_ptr<Base>,
                       util::DerefCmp<std::less<Base>>>,
            typename T = Base>
  class InternArena {
  public:
    Set interned;

    template <typename NT,
              std::enable_if_t<std::is_base_of<Base, NT>::value, bool> = 0>
    InternArena<Base, NT> &cast() {
      return static_cast<InternArena<Base, NT>&>(*this);
    }

    template <typename ...Args>
    T *intern(Args &&...args) {
      auto value = std::make_unique<T>(std::forward<Args>(args)...);
      auto found = interned.find(value);
      if (found == interned.end()) {
#ifdef ARENA_LOGGING
        std::cerr << "[ARENA] miss\n";
#endif
        found = interned.insert(std::move(value)).first;
      }
#ifdef ARENA_LOGGING
      else {
        std::cerr << "[ARENA] hit\n";
      }
#endif
      return found->get();
    }
  };
}
