#pragma once

#include "InferenceSeq.h"
#include "InferenceSystem.h"

#include <deque>

namespace type::infer {
  using InferContext = System;

  /**
   * The runtime state of the VM.
   */
  struct Env {
    std::map<Tp, Tp> mapping;
    err::ErrorContext &ecx;
    TTcx &ttcx;
    std::deque<err::Location> backtrace;
    std::map<
      std::pair<Tp, TraitBound *>,
      InstRef
      > traits;
    std::map<
      Idx, // block id
      std::map<
        std::vector<Tp>, // inputs
        InstRef> // instantiation
      > instMap;
    Env(TTcx &ttcx, err::ErrorContext &ecx): ecx(ecx), ttcx(ttcx) {}
    struct Frame {
      Env &env;
      std::map<Tp, Tp> vars;
      Frame(Env &env): env(env) {}
      ~Frame();
      void assoc(Tp var, Tp value);
    };
  };

  void runInEnv(
    InferContext &ctx,
    Env &env,
    const InferenceSeq &seq,
    InstRef inst
  );

  void instAti(
    const AbstractTraitImpl &ati,
    InferContext &ctx,
    Env &env,
    const std::vector<Tp> &args,
    InstRef inst
  );
}
