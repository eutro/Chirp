#pragma once

#include "../Type.h"
#include "InferenceGraph.h"
#include "UnifyMap.h"

#include <vector>
#include <variant>
#include <map>

namespace type::infer {
  struct Step {
    struct Unify {
      Tp tyA, tyB;
    };
    struct Assign {
      Tp toTy, fromTy;
    };
    struct ImplTrait {
      Tp ty;
      TraitBound *trait;
    };
    err::Location desc;
    std::variant<Unify, Assign, ImplTrait> v;
    template<typename... Arg>
    Step(Arg &&... args): v(std::forward<Arg>(args)...) {}
  };

  struct VarInfo {
    err::Location desc;
    Tp ty;
  };

  struct InferenceSeq {
    std::map<NodeRef, VarInfo> freeVars;
    std::map<Idx, VarInfo> vars;
    std::vector<Step> steps;

    /**
     * Convert an inference graph to a sequence of inference steps, in linear time.
     *
     * This is mostly just a topological sort of the strongly connected components of the graph.
     */
    InferenceSeq(const InferenceGraph &graph);

    void run(
      Tcx &tcx, Tbcx &tbcx,
      std::map<NodeRef, Tp> &inputs,
      std::map<Idx, Tp> &outputs,
      std::map<Idx, UnifyMap<InferenceSeq*>> &traits
    );
  };
}
