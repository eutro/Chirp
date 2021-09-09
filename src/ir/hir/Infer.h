#pragma once

#include "Hir.h"
#include "../../type/Type.h"
#include "../../common/Arena.h"

#include <unordered_set>

namespace hir::infer {
  using type::Ty;
  using type::TraitBound;
  using Tp = Ty *;

  struct InferResult {
    type::Tcx tcx;
    type::Tbcx tbcx;
    err::ErrorContext errors;
    struct BlockInstantiation {
      std::vector<std::vector<Tp>> types;
      std::map<Expr *, Idx> exprTypes;
      std::map<DefIdx, Idx> varTypes;

      std::vector<std::vector<TraitBound *>> traitBounds;
      std::map<Expr *, Idx> traitTypes;
    };
    std::map<Block *, BlockInstantiation> insts;
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor();
}
