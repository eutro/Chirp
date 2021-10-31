#pragma once

#include "Hir.h"
#include "../../type/Type.h"
#include "../../type/infer/Public.h"

#include <unordered_set>

namespace hir::infer {
  using type::Ty;
  using type::TraitBound;
  using Tp = Ty *;

  struct InferResult {
    type::infer::System sys;
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor(type::TTcx &ttcx);
}
