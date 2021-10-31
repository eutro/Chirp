#include "InferenceSystem.h"
#include "VM.h"

namespace type::infer {
  void solveSystem(System &sys, SolveCtx &ctx, const std::vector<Idx> &roots) {
    Env env(ctx.ttcx, ctx.ecx);
    for (Idx root : roots) {
      auto &block = sys.seqs.at(root);
      InstRef ref{root, (Idx) block.insts.size()};
      block.insts.emplace_back();
      runInEnv(sys, env, block.seq, ref);
    }
  }
};
