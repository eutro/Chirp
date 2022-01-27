#pragma once

#include "../Type.h"

#include <map>

namespace type::infer {
  /**
   * An instantiation of an entity (the top level, a trait)
   */
  struct Inst {
    using EntityIdx = Idx;
    using ValIdx = Idx;
    using Ref = std::pair<EntityIdx, ValIdx>;

    struct Val {
      std::map<Idx, Tp> loggedTys;
      std::map<Idx, Ref> loggedRefs;
    };
    /**
     * A set of instantiations obtained from running type inference.
     */
    struct Set {
      std::map<EntityIdx, std::map<ValIdx, Val>> entities;
      Val &operator[](const Ref &ref) {
        return entities.at(ref.first).at(ref.second);
      }
    };
    struct ConstructingSet : Set {
      std::map<Ref, std::vector<Tp>> refRets;
      std::map<EntityIdx, std::map<std::vector<Tp>, Ref>> memo;
      Tcx *tcx;
    };
  };
}
