#include <iostream>

#include "../type/infer/VM.h"

using namespace type::infer;

int main() {
  Env env;
  ENV = &env;
  auto foo = LookupKey::intern("foo");
  env.table->insertFn(foo, {}, Fn(0, 0, [](auto args){
    std::cout << "Test\n";
    return args;
  }));
  (*env.table->lookupFn(foo, {}))({});
  ENV = nullptr;
}
