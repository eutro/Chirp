#pragma once

#include "Hir.h"
#include "../../type/Type.h"
#include "../../type/infer/Public.h"

namespace hir::infer {
  using type::Ty;
  using type::TraitBound;
  using Tp = Ty *;

  struct InferResult {
    std::shared_ptr<type::infer::Inst::Set> insts;
    std::unique_ptr<type::infer::LookupTable> table = type::infer::LookupTable::create();
    type::infer::Fn root;
    err::ErrorContext ecx;
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor(type::Tcx &ttcx);
}
