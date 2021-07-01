#include "Type.h"

int main() {
  compiler::Type a = compiler::Type::named(0);
  compiler::Type b = compiler::Type::named(1);
  compiler::Type c = compiler::Type::named(2);
  std::shared_ptr<compiler::BaseType> func = std::make_shared<compiler::BaseType>("fn");
  compiler::Type d = compiler::Type::aggregate(func, {&a, &b});
  compiler::Type e = compiler::Type::aggregate(func, {&b, &c});
  d.unify(e);
  std::cout << a.get() << " " << b.get() << " "
            << c.get() << std::endl;
}
