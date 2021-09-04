#include "Hir.h"
#include "../../type/Type.h"
#include "../../common/Arena.h"

namespace hir::infer {
  using type::Ty;
  using type::TraitBound;
  using Tp = Ty *;

  struct InferResult {
    arena::InternArena<Ty> tcx;
    err::ErrorContext errors;
    struct BlockInstantiation {
      std::set<std::map<Expr *, Tp>> exprTypes;
    };
    std::map<Block *, BlockInstantiation> insts;
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor();
}
