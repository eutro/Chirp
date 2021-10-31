#include "VM.h"

#include "../TypePrint.h"

#include <deque>
#include <functional>
#include <variant>
#include <sstream>

#define CHIRP_VM_DEBUG_STACKTRACES 0

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
    vars.try_emplace(var, value);
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
          return found->second = replaceTy(env.ttcx, found->second, replacer);
        }
      } else if (std::holds_alternative<Ty::TraitRef>(ty->v)) {
        auto &tr = std::get<Ty::TraitRef>(ty->v);
        auto recTy = replaceTy(env.ttcx, tr.ty, replacer);
        auto trait = replaceTy(env.ttcx, tr.trait, replacer);
        auto found = env.traits.find({recTy, trait});
        if (found == env.traits.end() ||
            ctx[found->second].outputs.size() <= tr.ref) {
          // propagate error
          // TraitRef usages are dominated by their relevant trait constraints,
          // so reaching here always means that the trait constraint was tried and failed
          return env.ttcx.tcx.intern(Ty::Err{});
        }
        return ctx[found->second].outputs[tr.ref];
      }
      return ty;
    };
    return replaceTy(env.ttcx, ty, replacer);
  }

  void setVar(InferContext &ctx, Env &env, Env::Frame &frame, Tp var, Tp value) {
    bool isFree = false;
    Idx depth = 0;
    auto checkFree = overloaded {
      [&](Tp ty) {
        if (ty == var) {
          isFree = true;
          return env.ttcx.tcx.intern(Ty::CyclicRef{depth});
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
    Tp setTy = replaceTy(env.ttcx, value, checkFree);
    if (isFree) {
      setTy = env.ttcx.tcx.intern(Ty::Cyclic{setTy});
    }
#if (CHIRP_VM_DEBUG_STACKTRACES)
    {
      std::stringstream ss;
      ss << var << " set to " << setTy;
      addBacktrace(env, env.ecx.err().msg(ss.str()));
    }
#endif
    frame.assoc(var, setTy);
  }

  void unify(InferContext &ctx, Env &env, Env::Frame &frame, Tp tyA, Tp tyB) {
    std::set<std::pair<Tp, Tp>> seen;
    std::function<bool(Tp&, Tp&)> innerUnify = [&](Tp &tyA, Tp &tyB) {
      tyA = uncycle(env.ttcx, appRepl(ctx, env, tyA));
      tyB = uncycle(env.ttcx, appRepl(ctx, env, tyB));
      if (tyA == tyB || !seen.insert({tyA, tyB}).second) {
        return true; // already seen
      } else if (std::holds_alternative<Ty::Placeholder>(tyA->v)) {
        setVar(ctx, env, frame, tyA, tyB);
      } else if (std::holds_alternative<Ty::Placeholder>(tyB->v)) {
        setVar(ctx, env, frame, tyB, tyA);
      } else if (
        std::holds_alternative<Ty::Err>(tyA->v) ||
        std::holds_alternative<Ty::Err>(tyB->v)
      ) {
        return true; // propagate
      } else {
        IndexAndArityCmp cmp;
        if (cmp(tyA, tyB) || cmp(tyB, tyA)) {
          return false;
        }
        auto chA = childrenOf({tyA});
        auto chB = childrenOf({tyB}, chA.size());
        for (Idx i = 0; i < chA.size(); ++i) {
          if (!innerUnify(chA[i], chB[i])) return false;
        }
      }
      return true;
    };
    if (!innerUnify(tyA, tyB)) {
      err::Location &err = env.ecx.err()
          .msg("mismatched types");
      {
        std::stringstream ss;
        ss << tyA;
        err.msg(ss.str()).msg("and");
      }
      {
        std::stringstream ss;
        ss << tyB;
        err.msg(ss.str());
      }
      addBacktrace(env, err);
    }
  }

  Tp assigned(InferContext &ctx, Env &env, Env::Frame &frame, Tp toTy, Tp fromTy) {
    toTy = appRepl(ctx, env, toTy);
    fromTy = appRepl(ctx, env, fromTy);
    if (std::holds_alternative<Ty::Never>(toTy->v)) {
      return fromTy;
    } else if (std::holds_alternative<Ty::Never>(fromTy->v)) {
      return toTy;
    }
    unify(ctx, env, frame, toTy, fromTy);
    return appRepl(ctx, env, toTy);
  }

  void runInEnvWithFrame(
    InferContext &ctx,
    Env &env,
    Env::Frame &frame,
    const InferenceSeq &seq,
    InstRef inst
  );

#ifndef CHIRP_INFER_MAX_DEPTH
#define CHIRP_INFER_MAX_DEPTH 256
#endif
#define CHIRP_TOK_TO_STR0(TOK) #TOK
#define CHIRP_TOK_TO_STR(TOK) CHIRP_TOK_TO_STR0(TOK)
#define CHIRP_INFER_MAX_DEPTH_STR CHIRP_TOK_TO_STR(CHIRP_INFER_MAX_DEPTH)

  void instAti(
    const AbstractTraitImpl &ati,
    InferContext &ctx,
    Env &env,
    const std::vector<Tp> &args,
    InstRef inst
  ) {
    if (env.backtrace.size() > CHIRP_INFER_MAX_DEPTH) {
      throw std::runtime_error("Past recursion limit: " CHIRP_INFER_MAX_DEPTH_STR);
    }
    if (ati.inputs.size() != args.size()) {
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
    ctx[inst].outputs = ati.outputs;

    Env::Frame frame(env);
    auto &steps = ctx.seqs.at(ati.blockIdx).seq;
    for (auto &bound : steps.vars) {
      frame.assoc(bound.second.ty, nullptr);
    }
    {
      auto ii = ati.inputs.begin();
      auto ai = args.begin();
      while (ii != ati.inputs.end()) {
        unify(ctx, env, frame, *ii++, *ai++);
      }
    }
    runInEnvWithFrame(ctx, env, frame, steps, inst);
    ctx[inst].outputs = appRepl(ctx, env, ati.outputs);
    for (Tp &out : ctx[inst].outputs) {
      if (!isComplete(out)) {
        Ty *never = env.ttcx.tcx.intern(Ty::Never{});
        unify(ctx, env, frame, out, never);
        out = never;
      }
    }
  }

  void runInEnv(
    InferContext &ctx,
    Env &env,
    const InferenceSeq &seq,
    InstRef inst
  ) {
    Env::Frame frame(env);
    runInEnvWithFrame(ctx, env, frame, seq, inst);
  }

  void runInEnvWithFrame(
    InferContext &ctx,
    Env &env,
    Env::Frame &frame,
    const InferenceSeq &seq,
    InstRef inst
  ) {
#if (CHIRP_VM_DEBUG_STACKTRACES)
    env.ecx.err().msg("PUSH{");
#endif
    for (auto &step : seq.steps) {
#if (CHIRP_VM_DEBUG_STACKTRACES)
      static Idx idx = 0;
      env.backtrace.emplace_back().msg("caused by").chain(step.desc);
      addBacktrace(env, env.ecx.err().msg("Stack dump " + std::to_string(++idx)));
#endif
      switch (step.v.index()) {
      case util::index_of_type_v<Step::Unify, decltype(step.v)>: {
        auto &s = std::get<Step::Unify>(step.v);
        unify(ctx, env, frame, s.tyA, s.tyB);
        break;
      }
      case util::index_of_type_v<Step::Assign, decltype(step.v)>: {
        auto &s = std::get<Step::Assign>(step.v);
        auto iter = s.fromTy.begin();
        if (iter != s.fromTy.end()) {
          Tp ty = *iter;
          while (++iter != s.fromTy.end()) {
            ty = assigned(ctx, env, frame, ty, *iter);
          }
          unify(ctx, env, frame, s.toTy, ty);
        }
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
        if (isComplete(args) /* otherwise propagate error */) {
          if (const AbstractTraitImpl *impl = ctx.traits[s.trait->i]
              .find(env.ttcx, {args.front()/*TODO dispatch on args too*/})) {
            auto &memos = env.instMap[impl->blockIdx];
            auto iter = memos.lower_bound(args);
            if (iter == memos.end() || iter->first != args) {
              auto &insts = ctx.seqs.at(impl->blockIdx).insts;
              iter = memos.emplace_hint(
                iter,
                args,
                InstRef{impl->blockIdx, (Idx) insts.size()}
              );
              env.traits.emplace(std::make_pair(recTy, trait), iter->second);
              insts.emplace_back();
              instAti(*impl, ctx, env, args, iter->second);
            }
            ctx[inst].traitImpls.emplace(s.idx, iter->second);
          } else {
            err::Location &err = env.ecx.err()
                .msg("trait not implemented");
            {
              std::stringstream ss;
              ss << trait;
              err.msg(ss.str()).msg("on type");
            }
            {
              std::stringstream ss;
              ss << recTy;
              err.msg(ss.str());
            }
            addBacktrace(env, err);
            // implicitly poisoned
          }
        }
        break;
      }
#if (CHIRP_VM_DEBUG_STACKTRACES)
      env.backtrace.pop_back();
#endif
    }
#if (CHIRP_VM_DEBUG_STACKTRACES)
    env.ecx.err().msg("}POP");
#endif

    auto &tvs = ctx[inst].typeVars;
    for (auto &tv : seq.vars) {
      tvs.emplace_hint(
        tvs.end(),
        tv.first,
        appRepl(ctx, env, tv.second.ty)
      );
    }
  }
}