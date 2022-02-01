#pragma once

#include "../Type.h"

#include <deque>
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
      std::set<Ref> invokingRefs;
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
      std::map<Ref, bool> currentCallsRecur;
      template <typename A, typename B>
      struct BiMap {
        std::map<A, B> forward;
        std::map<B, A> back;
        void insert(const A &a, const B &b) {
          forward.insert({a, b});
          back.insert({b, a});
        }
        void erase(const A &a) {
          if (forward.count(a)) {
            back.erase(forward.at(a));
            forward.erase(a);
          }
        }
        void erase(const B &b) {
          if (back.count(b)) {
            forward.erase(back.at(b));
            back.erase(b);
          }
        }
      };
      std::map<EntityIdx, BiMap<std::vector<Tp>, Ref>> memo;
      Tcx *tcx;

      Ref initCall(
        EntityIdx entity,
        const std::vector<Tp> &args,
        std::vector<Tp> *&rets,
        bool &hasMemo
      );
      bool isValid(Ref ref);
      // returns whether to retry the instantiation
      bool finishCall(Ref inst, bool isRetry);
    private:
      void invalidate(Ref ref);
    };
  };
}
