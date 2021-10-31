#pragma once

#include "InferenceSeq.h"

namespace type::infer {
  struct InstRef {
    Idx block, inst;
  };

  struct Instantiation {
    std::map<Idx, Tp> typeVars;
    std::map<Idx, InstRef> traitImpls;
    std::vector<Tp> outputs;
  };

  struct Block {
    InferenceSeq seq;
    std::vector<Instantiation> insts;
  };

  struct AbstractTraitImpl {
    Idx blockIdx;
    std::vector<Tp> inputs, outputs;
  };

  struct System {
    std::vector<Block> seqs;
    std::map<Idx, UnifyMap<AbstractTraitImpl>> traits;
    Instantiation &operator[](const InstRef &ir) {
      return seqs.at(ir.block).insts.at(ir.inst);
    };
  };

  struct SolveCtx {
    TTcx &ttcx;
    err::ErrorContext ecx;
    SolveCtx(TTcx &ttcx): ttcx(ttcx) {}
  };

  void solveSystem(System &sys, SolveCtx &ctx, const std::vector<Idx> &roots);
}
