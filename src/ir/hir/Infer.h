#pragma once

#include "Hir.h"
#include "../../type/Type.h"
#include "../../type/InferenceGraph.h"
#include "../../common/Arena.h"

#include <unordered_set>

namespace hir::infer {
  using type::Ty;
  using type::TraitBound;
  using Tp = Ty *;

  struct InferResult {
    type::Tcx tcx;
    type::Tbcx tbcx;
    std::vector<type::infer::InferenceGraph> graphs;
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor();
}
