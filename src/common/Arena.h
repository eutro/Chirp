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
    std::vector<std::shared_ptr<Base>> ptrs;

    template <typename NT,
              std::enable_if_t<std::is_base_of<Base, NT>::value, bool> = 0>
    Arena<Base, NT> &cast() {
      return *reinterpret_cast<Arena<Base, NT>*>(this);
    }

    template <typename ...Args>
    T *add(Args &&...args) {
      return static_cast<T*>
        (ptrs.emplace_back(std::make_shared<T>(std::forward<Args>(args)...)).get());
    }
  };

  template <typename T>
  struct InternHook;

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
      return *reinterpret_cast<InternArena<Base, NT>*>(this);
    }

    template <typename ...Args>
    T *intern(Args &&...args) {
      auto insert = interned.insert(
        std::make_unique<T>(std::forward<Args>(args)...)
      );
      auto ret = insert.first->get();
      if (insert.second) {
        InternHook<T> hook;
        hook.postIntern(*this, ret);
      }
      return ret;
    }
  };

  template <typename T>
  struct InternHook {
    template <typename Base, typename Set>
    inline void postIntern(InternArena<Base, Set, T> &, T *) {}
  };
}
