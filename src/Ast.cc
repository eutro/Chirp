#include "Ast.h"

namespace ast {
  Var::Var(PType &&type) : type(std::make_shared<PType>(type)) {}
}
