#pragma once

#include "InferenceSeq.h"
#include "UnifyMap.h"

namespace type::infer {
  struct Instantiation {
    std::map<Idx, Tp> typeVars;
    std::map<
      Idx,
      std::pair<
        const InferenceSeq *,
        Instantiation *>
      > traitImpls;
    std::vector<Tp> outputs;
  };

  struct Env {
    std::map<Tp, Tp> mapping;
    std::map<
      std::pair<Tp, TraitBound *>,
      Instantiation *> traits;
    struct Frame {
      Env &env;
      std::map<Tp, Tp> vars;
      Frame(Env &env): env(env) {}
      ~Frame();
      void assoc(Tp var, Tp value);
    };
  };

  struct InferContext;

  struct AbstractTraitImpl {
    InferenceSeq &steps;
    std::vector<Tp> inputs, outputs;

    AbstractTraitImpl(InferenceSeq &steps): steps(steps) {}

    void operator()(
      InferContext &ctx,
      Env &env,
      const std::vector<Tp> &args,
      Instantiation &inst
    ) const;
  };

  struct InferContext {
    TTcx &ttcx;
    err::ErrorContext ecx;
    std::map<Idx, UnifyMap<AbstractTraitImpl>> traits;
    std::map<
      const InferenceSeq *,
      std::map<
        std::vector<Tp>,
        Instantiation>
      > insts;
    InferContext(TTcx &ttcx): ttcx(ttcx) {}
  };

  void runInEnv(
    InferContext &ctx,
    Env &env,
    const InferenceSeq &seq,
    Instantiation &inst
  );
}
