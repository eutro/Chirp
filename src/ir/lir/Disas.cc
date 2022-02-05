#include "Disas.h"

#include "Lir.h"

namespace lir::disas {
  void disassemble(const BlockList &bl, std::ostream &os) {
    std::map<const BasicBlock *, Idx> bbs;
    Idx blockIdx = 0;
    for (const auto &b : bl.blocks) {
      bbs[b.get()] = blockIdx++;
    }
    std::map<const Insn *, Idx> insns;
    Idx insnIdx = 0;
    blockIdx = 0;
    Idx indent = 2;
    for (const auto &b : bl.blocks) {
      os << std::string(indent, ' ') << "Block #" << blockIdx++ << ":\n";
      for (const auto &i : b->insns) {
        if (std::holds_alternative<Insn::BlockStart>(i->v)) {
          indent += 2;
          continue;
        }
        if (std::holds_alternative<Insn::BlockEnd>(i->v)) {
          indent -= 2;
          continue;
        }
        Idx ii = insnIdx++;
        insns[i.get()] = ii;
        os << std::string(indent + 2, ' ')
           << "(" << i->ty << ") "
           << "$" << ii << " = ";
        std::visit(overloaded {
          [&](Insn::HeapAlloc &){os << "HeapAlloc";},
          [&](Insn::SetVar &x){os << "SetVar %" << x.var << " $" << insns.at(x.value);},
          [&](Insn::GetVar &x){os << "GetVar %" << x.var;},
          [&](Insn::SetField &x){
            os << "SetField $" << insns.at(x.obj)
            << "[" << x.field << "] @" << insns.at(x.value);
          },
          [&](Insn::GetField &x){os << "GetField $" << insns.at(x.obj) << "[" << x.field << "]";},
          [&](Insn::CallTrait &x){
            os << "CallTrait $" << insns.at(x.obj) << " @" << x.trait << "[" << x.method << "]";
            for (auto &arg : x.args) {
              os << " $" << insns.at(arg);
            }
          },
          [&](Insn::PhiNode &x){
            os << "PhiNode";
            for (auto &arg : x.branches) {
              os << " [#" << bbs.at(arg.block) << ", $" << insns.at(arg.ref) << "]";
            }
          },
          [&](Insn::NewTuple &x){
            os << "NewTuple";
            for (auto &v : x.values) {
              os << " $" << insns.at(v);
            }
          },
          [&](Insn::ForeignRef &x){os << "ForeignRef \"" << x.symbol << "\"";},
          [&](Insn::LiteralString &x){os << "LiteralString \"" << x.value << "\"";},
          [&](Insn::LiteralInt &x){os << "LiteralInt " << x.value;},
          [&](Insn::LiteralFloat &x){os << "LiteralFloat " << x.value;},
          [&](Insn::LiteralBool &x){os << "LiteralBool " << (x.value ? "true" : "false");},
          [](auto&){},
        }, i->v);
        os << "\n";
      }
      os << std::string(indent + 2, ' ');
      std::visit(overloaded {
        [&](Jump::Ret &r){os << "Ret $" << insns.at(r.value);},
        [&](Jump::Br &r){os << "Br #" << bbs.at(r.target);},
        [&](Jump::CondBr &r){os << "CondBr $" << insns.at(r.pred) << " #" << bbs.at(r.thenB) << " #" << bbs.at(r.elseB);},
      }, b->end.v);
      os << "\n";
    }
  }

  void disassemble(const Module &mod, std::ostream &os) {
    os << "Top:\n";
    disassemble(mod.topLevel, os);
    for (auto &ti : mod.traitImpls) {
      os << "Trait:\n";
      for (auto &m : ti.methods) {
        disassemble(m, os);
      }
    }
  }
}
