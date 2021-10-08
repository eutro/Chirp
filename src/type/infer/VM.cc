#include "VM.h"

#include <deque>
#include <functional>

namespace type::infer {
  Env::Frame::~Frame() {
    for (auto &p : vars) {
      auto it = env.mapping.find(p.first);
      if (p.second) {
        it->second = p.second;
      }
      env.mapping.erase(it);
    }
  }
  void Env::Frame::assoc(Tp var, Tp value) {
    auto found = env.mapping.find(var);
    if (found == env.mapping.end()) {
      env.mapping[var] = value;
      value = nullptr;
    } else {
      std::swap(found->second, value);
    }
    vars[var] = value;
  }

  void AbstractTraitImpl::operator()(
    InferContext &ctx,
    Env &env,
    const std::vector<Tp> &args
  ) const {
    if (inputs.size() != args.size()) {
      throw std::runtime_error("Bad arity to trait impl");
    }

    Env::Frame frame(env);
    for (auto &bound : steps.vars) {
      frame.assoc(bound.second.ty, nullptr);
    }
    {
      auto ii = inputs.begin();
      auto ai = args.begin();
      while (ii != inputs.end()) {
        frame.assoc(*ii++, *ai++);
      }
    }

    runInEnv(ctx, env, steps);
  }

  void runInEnv(InferContext &ctx, Env &env, const InferenceSeq &seq) {
    Env::Frame frame(env);

    auto appRepl = [&](auto ty) -> auto {
      std::function<Tp(Tp)> replacer = [&](Tp ty) {
        if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
          auto found = env.mapping.find(ty);
          if (found != env.mapping.end()) {
            return found->second = replaceTy(ctx.ttcx, found->second, replacer);
          }
        }
        return ty;
      };
      return replaceTy(ctx.ttcx, ty, replacer);
    };

    auto setVar = [&](Tp var, Tp value) {
      bool isFree = false;
      Idx depth = 0;
      auto checkFree = overloaded {
        [&](Tp ty) {
          if (ty == var) {
            isFree = true;
            return ctx.ttcx.tcx.intern(Ty::CyclicRef{depth});
          }
          return ty;
        },
        [&](Tp ty, PreWalk) {
          if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
            ++depth;
          }
          return ty;
        },
        [&](Tp ty, PostWalk) {
          if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
            --depth;
          }
          return ty;
        },
      };
      Tp setTy = replaceTy(ctx.ttcx, value, checkFree);
      if (isFree) {
        setTy = ctx.ttcx.tcx.intern(Ty::Cyclic{setTy});
      }
      frame.assoc(var, setTy);
    };

    auto unify = [&](Tp tyA, Tp tyB) {
      std::set<std::pair<Tp, Tp>> seen;
      std::function<void(Tp, Tp)> innerUnify = [&](Tp tyA, Tp tyB) {
        tyA = uncycle(ctx.ttcx, appRepl(tyA));
        tyB = uncycle(ctx.ttcx, appRepl(tyB));
        if (!seen.insert({tyA, tyB}).second) {
          return; // already seen
        } else if (std::holds_alternative<Ty::Placeholder>(tyA->v)) {
          setVar(tyA, tyB);
        } else if (std::holds_alternative<Ty::Placeholder>(tyB->v)) {
          setVar(tyB, tyA);
        } else if (
          std::holds_alternative<Ty::Err>(tyA->v) ||
          std::holds_alternative<Ty::Err>(tyB->v)
        ) {
          return; // propagate
        } else {
          IndexAndArityCmp cmp;
          if (cmp(tyA, tyB) || cmp(tyB, tyA)) {
            throw std::runtime_error("Diferring base types/arities");
          }
          auto chA = childrenOf({tyA});
          auto chB = childrenOf({tyB}, chA.size());
          for (Idx i = 0; i < chA.size(); ++i) {
            innerUnify(chA[i], chB[i]);
          }
        }
      };
      innerUnify(tyA, tyB);
    };

    for (auto &step : seq.steps) {
      switch (step.v.index()) {
      case util::index_of_type_v<Step::Unify, decltype(step.v)>: {
        auto &s = std::get<Step::Unify>(step.v);
        unify(s.tyA, s.tyB);
        break;
      }
      case util::index_of_type_v<Step::Assign, decltype(step.v)>: {
        auto &s = std::get<Step::Assign>(step.v);
        unify(s.toTy, s.fromTy); // :)
        break;
      }
      case util::index_of_type_v<Step::ImplTrait, decltype(step.v)>:
        auto &s = std::get<Step::ImplTrait>(step.v);
        std::vector<Tp> args;
        auto impl = ctx.traits[s.trait->i].find(ctx.ttcx, args);
        if (!impl) {
          throw std::runtime_error("Trait not implemented");
        }
        (*impl)(ctx, env, args);
        break;
      }
    }
  }
}
