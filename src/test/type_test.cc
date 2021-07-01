#include "../Type.h"

int main() {
  type::Type a = type::Type::named(0);
  type::Type b = type::Type::named(1);
  type::Type c = type::Type::named(2);
  std::shared_ptr<type::BaseType> func = std::make_shared<type::BaseType>("fn");
  type::Type d = type::Type::aggregate(func, {&a, &b});
  type::Type e = type::Type::aggregate(func, {&b, &c});
  d.unify(e);
  std::cout << a.get() << " " << b.get() << " "
            << c.get() << std::endl;
}
