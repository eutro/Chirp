#pragma once

#include "InferenceSeq.h"
#include "UnifyMap.h"

namespace type::infer {
  struct Env {
    std::map<Tp, Tp> mapping;
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
    InferenceSeq steps;
    std::vector<Tp> inputs, outputs;
    void operator()(
      InferContext &ctx,
      Env &env,
      const std::vector<Tp> &args
    ) const;
  };

  struct InferContext {
    TTcx &ttcx;
    std::map<Idx, UnifyMap<AbstractTraitImpl>> traits;
    InferContext(TTcx &ttcx): ttcx(ttcx) {}
  };

  void runInEnv(InferContext &ctx, Env &env, const InferenceSeq &seq);
}
