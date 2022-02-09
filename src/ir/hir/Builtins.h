#pragma once

#include <limits>

namespace hir {
  enum Builtins {
    // Traits
    Fn,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitOr,
    BitAnd,
    Eq,
    Cmp,
    Neg,
    // Types
    BOOL,
    I8, I16, I32, I64, I128,
    U8, U16, U32, U64, U128,
    F16, F32, F64,
    TUPLE,
    UNION,
    FFIFN,
    STRING,
    NULSTRING,
    TYPETOKEN,

    LAST_BUILTIN_
  };

  constexpr Idx BUILTIN_BLOCKS_START = std::numeric_limits<Idx>::max() - Neg * (LAST_BUILTIN_ - BOOL);
}
