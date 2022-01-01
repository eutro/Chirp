#pragma once

#include "../Type.h"
#include "Fn.h"

namespace type::infer {
  struct LookupKey {
    std::string value;
    LookupKey(std::string &&s): value(std::forward<std::string>(s)) {}
    bool operator<(const LookupKey &o) const { return value < o.value; }
    static LookupKey *intern(const std::string &value);
  };

  struct LookupTable {
    virtual Fn *lookupFn(LookupKey *fn, const std::vector<Tp> &params) = 0;
    virtual void insertFn(LookupKey *fn, const std::vector<Tp> &params, Fn &&fnv) = 0;
  };

  std::unique_ptr<LookupTable> newLookupTable();
}
