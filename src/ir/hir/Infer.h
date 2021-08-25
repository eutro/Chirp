#include "Hir.h"
#include "../../type/Type.h"
#include "../../common/Arena.h"

namespace hir::infer {
  using type::Ty;
  using type::TraitBound;
  using Tp = Ty *;

  struct InferResult {
    err::ErrorContext ecx;
    std::map<Expr*, Tp> exprTypes;
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor();
}
