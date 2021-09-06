#pragma once

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
    Cmp,
    Neg,
    // Types
    BOOL,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F16, F32, F64,
    TUPLE,
    FFIFN,
  };
}