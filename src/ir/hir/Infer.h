#pragma once

#include "Hir.h"
#include "../../type/Type.h"
#include "../../type/infer/Public.h"

namespace hir::infer {
  using type::Ty;
  using type::TraitBound;
  using Tp = Ty *;

  struct InferResult {
    std::vector<type::infer::InsnList> insnLists;
    std::unique_ptr<type::infer::LookupTable> table = type::infer::LookupTable::create();
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor(type::Tcx &ttcx);
}
