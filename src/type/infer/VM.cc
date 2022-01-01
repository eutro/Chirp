#include "VM.h"

namespace type::infer {
  thread_local Env *ENV;

  std::vector<Tp> InsnList::operator()(const std::vector<Tp> &args) const {
    if (insns.empty()) return args;
    std::vector<std::vector<Tp>> rets;
    rets.reserve(insns.size());
    std::vector<Tp> insnArgs;
    for (auto &insn : insns) {
      insnArgs.clear();
      insnArgs.reserve(insn.inputs.size());
      for (auto ref : insn.inputs) {
        const std::vector<Tp> &source = ref.insn ? args : rets.at(*ref.insn);
        insnArgs.push_back(source.at(ref.retIdx));
      }
      auto &fn = *ENV->table->lookupFn(insn.key, insn.constants, insnArgs);
      rets.push_back(fn(insnArgs));
    }
    return rets.back();
  }
}
