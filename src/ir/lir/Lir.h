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
    struct HeapAlloc {};
    struct SetVar {
      Idx var;
      Insn *value;
    };
    struct GetVar {
      Idx var;
    };
    struct SetField {
      Insn *obj;
      Idx field;
      Insn *value;
    };
    struct GetField {
      Insn *obj;
      Idx field;
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
      std::string value;
    };
    struct LiteralFloat {
      std::string value;
    };
    struct LiteralBool {
      bool value;
    };
    struct BlockStart {};
    struct BlockEnd {};
    std::optional<loc::Span> span;
    Idx ty;
    std::variant<HeapAlloc, SetVar, GetVar, SetField, GetField,
                 CallTrait, PhiNode, NewTuple, ForeignRef,
                 LiteralString, LiteralInt, LiteralFloat, LiteralBool,
                 BlockStart, BlockEnd>
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
    template <bool JUMP = true, typename... Arg>
    Insn *emplace_back(Arg &&...arg) {
      Insn *ret = insns.emplace_back(std::make_unique<Insn>(std::forward<Arg>(arg)...)).get();
      if constexpr (JUMP) end = Jump::Ret{ret};
      return ret;
    }
  };

  struct Decl {
    std::string name;
    std::optional<loc::Span> span;
    Idx ty;
  };

  struct BlockList {
    Idx blockIdx;
    std::map<Idx, Decl> params, vars;
    std::vector<std::unique_ptr<BasicBlock>> blocks;
    BlockList() {}
    BlockList(BlockList &&) = default;
    BlockList(const BlockList &) = delete;
    BlockList &operator=(BlockList &&o) = default;
    BasicBlock &operator[](Idx i) {
      return *blocks.at(i).get();
    }
    Idx push() {
      blocks.emplace_back(std::make_unique<BasicBlock>());
      return blocks.size() - 1;
    }
  };

  struct TraitImpl {
    std::vector<BlockList> methods;
  };

  struct Module {
    BlockList topLevel;
    std::vector<TraitImpl> traitImpls;
  };
}
