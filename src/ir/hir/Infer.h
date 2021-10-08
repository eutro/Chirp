#pragma once

#include "Hir.h"
#include "../../type/Type.h"
#include "../../type/infer/VM.h"

#include <unordered_set>

namespace hir::infer {
  using type::Ty;
  using type::TraitBound;
  using Tp = Ty *;

  struct InferResult {
    std::vector<std::unique_ptr<type::infer::InferenceSeq>> seqs;
    std::map<Idx, type::infer::UnifyMap<type::infer::AbstractTraitImpl>> traits;
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor(type::TTcx &ttcx);
}
