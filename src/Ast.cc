#include "Ast.h"

namespace ast {
  Var::Var(PType &&type) : type(std::make_shared<PType>(type)) {}
  BinaryTrait::BinaryTrait(Fn &&app) : app(app) {}
  CmpTrait::CmpTrait(Fn &&ne, Fn &&eq, Fn &&lt, Fn &&gt, Fn &&le, Fn &&ge) :
      ne(ne), eq(eq), lt(lt), gt(gt), le(le), ge(ge) {}
  CollectibleTrait::CollectibleTrait(llvm::Value *meta) : meta(meta) {}
}
