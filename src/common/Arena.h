#pragma once

#include <type_traits>
#include <set>
#include <memory>
#include <vector>

namespace arena {
  template <typename Compare>
  struct DerefCmp {
    template <typename LHS, typename RHS>
    bool operator()(const LHS &lhs, const RHS &rhs) const {
      Compare cmp;
      return cmp(*lhs, *rhs);
    }
  };

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
            typename Compare = std::less<Base>,
            typename T = Base>
  class InternArena {
  public:
    std::set<std::unique_ptr<Base>, DerefCmp<Compare>> interned;

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
        found = interned.insert(std::move(value)).first;
      }
      return found->get();
    }
  };
}
