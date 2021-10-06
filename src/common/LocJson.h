#pragma once

#include "JsonUtil.h"

namespace loc {
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(SrcLoc, line, col);
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Span, lo, hi);
};
