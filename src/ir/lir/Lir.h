#pragma once

#include "../../common/Idx.h"
#include "../../type/Type.h"

#include <map>
#include <cmath>
#include <cstdint>
#include <variant>
#include <vector>
#include <memory>

namespace lir {
  struct BasicBlock;

  class Insn {
  public:
    struct DeclareVar {};
    struct HeapAlloc {};
    struct SetVar {
      Insn *var;
      Insn *value;
    };
    struct GetVar {
      Insn *var;
    };
    struct SetField {
      Insn *obj;
      Idx variant, field;
      Insn *value;
    };
    struct GetField {
      Insn *obj;
      Idx variant, field;
    };
    struct CallTrait {
      Insn *obj;
      std::vector<Insn *> args;
      Idx trait;
      Idx method;
    };
    struct PhiNode {
      struct Branch {
        Insn *ref;
        BasicBlock *block;
      };
      std::vector<Branch> branches;
    };
    struct NewTuple {
      std::vector<Insn *> values;
    };
    struct ForeignRef {
      std::string symbol;
    };
    struct LiteralString {
      std::string value;
    };
    struct LiteralInt {
      unsigned long long value;
    };
    struct LiteralFloat {
      long double value;
    };
    struct LiteralBool {
      bool value;
    };
    Idx ty;
    std::variant<DeclareVar, HeapAlloc, SetVar, GetVar, SetField, GetField,
                 CallTrait, PhiNode, NewTuple,
                 ForeignRef, LiteralString, LiteralInt, LiteralFloat, LiteralBool>
        v;
    template <typename... Arg>
    Insn(Arg &&...arg): v(std::forward<Arg>(arg)...) {}
  };

  class Jump {
  public:
    struct Ret {
      Insn *value;
    };
    struct Br {
      BasicBlock *target;
    };
    struct CondBr {
      Insn *pred;
      BasicBlock *thenB, *elseB;
    };
    std::variant<Ret, Br, CondBr> v;
    template <typename... Arg>
    Jump(Arg &&...arg): v(std::forward<Arg>(arg)...) {}
  };

  struct BasicBlock {
    std::vector<std::unique_ptr<Insn>> insns;
    Jump end;
    BasicBlock() {}
    BasicBlock(const BasicBlock &) = delete;
    template <typename... Arg>
    Insn *emplace_back(Arg &&...arg) {
      Insn *ret = insns.emplace_back(std::make_unique<Insn>(std::forward<Arg>(arg)...)).get();
      end = Jump::Ret{ret};
      return ret;
    }
  };

  struct BlockList {
    std::vector<std::unique_ptr<BasicBlock>> blocks;
    BlockList() {}
    BlockList(BlockList &&) = default;
    BlockList(const BlockList &) = delete;
    BasicBlock &operator[](Idx i) {
      return *blocks.at(i).get();
    }
    Idx push() {
      blocks.emplace_back(std::make_unique<BasicBlock>());
      return blocks.size() - 1;
    }
  };

  struct Instantiation {
    std::vector<type::TraitBound *> traits;
    std::vector<type::Ty *> types;
  };

  struct TraitImpl {
    struct For {
      type::Ty *ty;
      type::TraitBound *tb;
      bool operator<(const For &o) {
        return std::make_pair(ty, tb) < std::make_pair(o.ty, o.tb);
      }
    };
    std::map<For, Instantiation> instantiations;
    std::vector<BlockList> methods;
  };

  struct Module {
    std::vector<TraitImpl> traitImpls;
    Instantiation instantiation;
    BlockList topLevel;
  };
}
