#pragma once

#include "../Type.h"
#include "Fn.h"

namespace type::infer {
  struct LookupKey {
    using P = const LookupKey *;
    std::string value;
    LookupKey(std::string &&s): value(std::forward<std::string>(s)) {}
    bool operator<(const LookupKey &o) const { return value < o.value; }
    static LookupKey::P intern(const std::string &value);
  };

  struct LookupTable {
    virtual ~LookupTable() = default;
    virtual Fn lookupFn(
      LookupKey::P fn,
      const std::vector<Constant> &constants, 
      const std::vector<Tp> &params
    ) = 0;
    virtual void insertFn(
      LookupKey::P fn,
      const std::vector<Constant> &constants, 
      const std::vector<Tp> &params,
      Fn &&fnv
    ) = 0;
    virtual void insertFallback(
        LookupKey::P fn,
        const std::vector<Constant> &constants,
        Fn &&fnv
    ) = 0;
    static std::unique_ptr<LookupTable> create();
  };
}
