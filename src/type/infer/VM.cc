#include "VM.h"

#include <deque>
#include <functional>
#include <variant>

namespace type::infer {
  Env::Frame::~Frame() {
    for (auto &p : vars) {
      auto it = env.mapping.find(p.first);
      if (it == env.mapping.end()) {
        if (p.second) {
          env.mapping[p.first] = p.second;
        }
      } else {
        if (p.second) {
          it->second = p.second;
        } else {
          env.mapping.erase(it);
        }
      }
    }
  }
  void Env::Frame::assoc(Tp var, Tp value) {
    auto found = env.mapping.find(var);
    if (found == env.mapping.end()) {
      if (value) {
        env.mapping[var] = value;
        value = nullptr;
      } else {
        return;
      }
    } else {
      if (value) {
        std::swap(found->second, value);
      } else {
        value = found->second;
        env.mapping.erase(found);
      }
    }
    vars[var] = value;
  }

  void addBacktrace(Env &env, err::Location &err) {
    for (auto it = env.backtrace.rbegin(); it != env.backtrace.rend(); ++it) {
      err.chain(*it);
    }
  }

  template <typename T>
  T appRepl(InferContext &ctx, Env &env, T ty) {
    std::function<Tp(Tp)> replacer = [&](Tp ty) {
      if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
        auto found = env.mapping.find(ty);
        if (found != env.mapping.end()) {
          return found->second = replaceTy(ctx.ttcx, found->second, replacer);
        }
      } else if (std::holds_alternative<Ty::TraitRef>(ty->v)) {
        auto &tr = std::get<Ty::TraitRef>(ty->v);
        auto recTy = replaceTy(ctx.ttcx, tr.ty, replacer);
        auto trait = replaceTy(ctx.ttcx, tr.trait, replacer);
        auto found = env.traits.find({recTy, trait});
        if (found == env.traits.end() || found->second->outputs.size() <= tr.ref) {
          // propagate error
          // TraitRef usages are dominated by their relevant trait constraints,
          // so reaching here always means that the trait constraint was tried and failed
          return ctx.ttcx.tcx.intern(Ty::Err{});
        }
        return found->second->outputs[tr.ref];
      }
      return ty;
    };
    return replaceTy(ctx.ttcx, ty, replacer);
  }

  void setVar(InferContext &ctx, Env &env, Env::Frame &frame, Tp var, Tp value) {
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
  }

  void unify(InferContext &ctx, Env &env, Env::Frame &frame, Tp tyA, Tp tyB) {
    std::set<std::pair<Tp, Tp>> seen;
    std::function<void(Tp, Tp)> innerUnify = [&](Tp tyA, Tp tyB) {
      tyA = uncycle(ctx.ttcx, appRepl(ctx, env, tyA));
      tyB = uncycle(ctx.ttcx, appRepl(ctx, env, tyB));
      if (tyA == tyB || !seen.insert({tyA, tyB}).second) {
        return; // already seen
      } else if (std::holds_alternative<Ty::Placeholder>(tyA->v)) {
        setVar(ctx, env, frame, tyA, tyB);
      } else if (std::holds_alternative<Ty::Placeholder>(tyB->v)) {
        setVar(ctx, env, frame, tyB, tyA);
      } else if (
        std::holds_alternative<Ty::Err>(tyA->v) ||
        std::holds_alternative<Ty::Err>(tyB->v)
      ) {
        return; // propagate
      } else {
        IndexAndArityCmp cmp;
        if (cmp(tyA, tyB) || cmp(tyB, tyA)) {
          addBacktrace(env, ctx.ecx.err()
            .msg("mismatched types"));
        }
        auto chA = childrenOf({tyA});
        auto chB = childrenOf({tyB}, chA.size());
        for (Idx i = 0; i < chA.size(); ++i) {
          innerUnify(chA[i], chB[i]);
        }
      }
    };
    innerUnify(tyA, tyB);
  }

  void runInEnvWithFrame(
    InferContext &ctx,
    Env &env,
    Env::Frame &frame,
    const InferenceSeq &seq,
    Instantiation &inst
  );

#ifndef CHIRP_INFER_MAX_DEPTH
#define CHIRP_INFER_MAX_DEPTH 256
#endif
#define CHIRP_TOK_TO_STR0(TOK) #TOK
#define CHIRP_TOK_TO_STR(TOK) CHIRP_TOK_TO_STR0(TOK)
#define CHIRP_INFER_MAX_DEPTH_STR CHIRP_TOK_TO_STR(CHIRP_INFER_MAX_DEPTH)

  void AbstractTraitImpl::operator()(
    InferContext &ctx,
    Env &env,
    const std::vector<Tp> &args,
    Instantiation &inst
  ) const {
    if (env.backtrace.size() > CHIRP_INFER_MAX_DEPTH) {
      throw std::runtime_error("Past recursion limit: " CHIRP_INFER_MAX_DEPTH_STR);
    }
    if (inputs.size() != args.size()) {
      throw std::runtime_error("ICE - Bad arity to trait impl");
    }

    // ensure that reentrant calls with the same inputs yield the bottom type;
    // in practice this means recursive calls can be properly inferred:
    //
    // let x = 10 in loop: if x == 0 { true } else { loop(x - 1) }
    //     ~ int                                          ~~~~~ int
    //                                 ~~~~ bool     ~~~~~~~~~~~ never
    //                     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ bool
    //
    // or
    //
    // let in loop: loop()
    //              ~~~~~~ never
    inst.outputs.resize(
      outputs.size(),
      ctx.ttcx.tcx.intern(Ty::Err{}) // TODO never type
    );

    Env::Frame frame(env);
    for (auto &bound : steps.vars) {
      frame.assoc(bound.second.ty, nullptr);
    }
    {
      auto ii = inputs.begin();
      auto ai = args.begin();
      while (ii != inputs.end()) {
        unify(ctx, env, frame, *ii++, *ai++);
      }
    }

    runInEnvWithFrame(ctx, env, frame, steps, inst);
    inst.outputs = appRepl(ctx, env, outputs);
  }

  void runInEnv(
    InferContext &ctx,
    Env &env,
    const InferenceSeq &seq,
    Instantiation &inst
  ) {
    Env::Frame frame(env);
    runInEnvWithFrame(ctx, env, frame, seq, inst);
  }

  void runInEnvWithFrame(
    InferContext &ctx,
    Env &env,
    Env::Frame &frame,
    const InferenceSeq &seq,
    Instantiation &inst
  ) {
    for (auto &step : seq.steps) {
      env.backtrace.emplace_back().msg("caused by").chain(step.desc);
      switch (step.v.index()) {
      case util::index_of_type_v<Step::Unify, decltype(step.v)>: {
        auto &s = std::get<Step::Unify>(step.v);
        unify(ctx, env, frame, s.tyA, s.tyB);
        break;
      }
      case util::index_of_type_v<Step::Assign, decltype(step.v)>: {
        auto &s = std::get<Step::Assign>(step.v);
        unify(ctx, env, frame, s.toTy, s.fromTy); // :)
        break;
      }
      case util::index_of_type_v<Step::ImplTrait, decltype(step.v)>:
        auto &s = std::get<Step::ImplTrait>(step.v);
        std::vector<Tp> args;
        args.reserve(s.trait->s.size() + 1);
        Tp recTy = appRepl(ctx, env, s.ty);
        TraitBound *trait = appRepl(ctx, env, s.trait);
        args.push_back(recTy);
        std::copy(trait->s.begin(), trait->s.end(), std::back_inserter(args));
        for (Tp &arg : args) {
          arg = appRepl(ctx, env, arg);
          if (std::holds_alternative<Ty::Err>(arg->v)) {
            break; // already errored, but implicitly poisoned
          }
        }
        if (const AbstractTraitImpl *impl = ctx.traits[s.trait->i].find(ctx.ttcx, args)) {
          auto &memos = ctx.insts[&impl->steps];
          auto iter = memos.lower_bound(args);
          if (iter == memos.end() || iter->first != args) {
            iter = memos.insert(iter, std::make_pair(args, Instantiation{}));
            env.traits.emplace(std::make_pair(recTy, trait), &iter->second);
            (*impl)(ctx, env, args, iter->second);
          }
          inst.traitImpls.emplace(s.idx, std::make_pair(&impl->steps, &iter->second));
        } else {
          addBacktrace(env, ctx.ecx.err()
              .msg("trait not implemented"));
          // implicitly poisoned
        }
        break;
      }
      env.backtrace.pop_back();
    }

    for (auto &tv : seq.vars) {
      inst.typeVars.emplace_hint(
        inst.typeVars.end(),
        tv.first,
        appRepl(ctx, env, tv.second.ty)
      );
    }
  }
}
