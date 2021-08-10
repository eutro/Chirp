#include "Ast.h"

namespace ast {
  std::unique_ptr<Visitor> inferenceVisitor(ParseContext &ctx);
}
