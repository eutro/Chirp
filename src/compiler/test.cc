#include "Expr.h"

int main() {
  compiler::Type a = compiler::Type::named(0);
  compiler::Type b = compiler::Type::named(1);
  compiler::Type c = compiler::Type::named(2);
  compiler::Type d = compiler::Type::aggregate(0, {&a, &b});
  compiler::Type e = compiler::Type::aggregate(0, {&b, &c});
  d.unify(e);
  std::cout << a.get() << " " << b.get() << " "
            << c.get() << std::endl;
}
