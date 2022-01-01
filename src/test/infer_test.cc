#include <iostream>

#include "../type/infer/VM.h"

using namespace type::infer;

int main() {
  ENV = new Env;
  auto print = LookupKey::intern("print");
  const char *hw = "Hello, world!\n";
  const char *ht = "Hello there!\n";
  ENV->table->insertFn(print, {hw}, {}, [=](auto _) {
    std::cout << hw;
    return _;
  });
  ENV->table->insertFn(print, {ht}, {}, [=](auto _) {
    std::cout << ht;
    return _;
  });

  auto foo = LookupKey::intern("foo");
  InsnList insns;
  insns.insns.emplace_back(Insn(print, {hw}, {}));
  insns.insns.emplace_back(Insn(print, {ht}, {}));
  ENV->table->insertFn(foo, {}, {}, insns);
  (*ENV->table->lookupFn(foo, {}, {}))({});
  delete ENV;
  ENV = nullptr;
}
