#include "../Type.h"

int main() {
  auto a = std::make_shared<type::Type>(type::Type::named(0));
  auto b = std::make_shared<type::Type>(type::Type::named(1));
  auto c = std::make_shared<type::Type>(type::Type::named(2));
  auto func = std::make_shared<type::BaseType>("fn");
  auto d = std::make_shared<type::Type>(type::Type::aggregate(func, {&a, &b}));
  auto e = std::make_shared<type::Type>(type::Type::aggregate(func, {&b, &c}));
  type::TypeContext ctx;
  type::Type::unify(ctx, d, e);
  std::cout << a.get() << " " << b.get() << " "
            << c.get() << std::endl;
}
