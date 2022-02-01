#include "Inst.h"

namespace type::infer {
  using Ref = Inst::Ref;
  Ref Inst::ConstructingSet::initCall(
    Idx entity,
    const std::vector<Tp> &args,
    std::vector<Tp> *&rets,
    bool &hasMemo
  ) {
    auto &m = memo[entity];
    if (m.forward.count(args)) {
      Ref ref = m.forward.at(args);
      if (currentCallsRecur.count(ref)) {
        currentCallsRecur.at(ref) = true;
      }
      rets = &refRets.at(ref);
      hasMemo = true;
      return ref;
    }
    auto &es = entities[entity];
    Ref ref = {entity, es.empty() ? 0 : es.rend()->first + 1};
    es.insert({ref.second, {}});
    m.insert(args, ref);
    rets = &refRets.insert({ref, {}}).first->second;
    hasMemo = false;
    currentCallsRecur[ref] = false;
    return ref;
  }

  bool Inst::ConstructingSet::isValid(Ref ref) {
    return refRets.count(ref);
  }

  // returns whether to retry the instantiation
  bool Inst::ConstructingSet::finishCall(Ref inst, bool isRetry) {
    if (!isRetry && currentCallsRecur.at(inst)) {
      // if this is in its transient closure of callers
      // (i.e. this has called itself somewhere down the line),
      // then invalidate all of its callers
      invalidate(inst);
      return true;
    }
    currentCallsRecur.erase(inst);
    return false;
  }

  void Inst::ConstructingSet::invalidate(Ref ref) {
    if (!isValid(ref)) return; // seen
    refRets.erase(ref);
    memo.at(ref.first).erase(ref);
    for (Ref caller : (*this)[ref].invokingRefs) {
      invalidate(caller);
    }
    entities.at(ref.first).erase(ref.second);
  }
}
