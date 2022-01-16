#include "Insns.h"

namespace type::infer {
  std::vector<Tp> IdentityInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    return tys;
  }
  std::vector<Tp> ConstructInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cArgs) const {
    auto &tyTemplate = constant_cast<TyTemplate>(cArgs.front());
    return {tyTemplate.construct(tys)};
  }
  std::vector<Tp> DeConstructInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cArgs) const {
    auto &tyTemplate = constant_cast<TyTemplate>(cArgs.front());
    return tyTemplate.deconstruct(tys.front());
  }

  Tp TyTemplate::construct(const std::vector<Tp> &tys) const {
    auto replacer = [&tys](Tp ty) {
      if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
        return tys.at(std::get<Ty::Placeholder>(ty->v).i);
      }
      return ty;
    };
    return replaceTy(*targetTy->tcx, targetTy, replacer);
  }
  std::vector<Tp> TyTemplate::deconstruct(Tp ty) const {
    // TODO handle failure
    Idx rets = 0;
    auto countRets = [&rets](Tp ty) {
      if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
        rets = std::max(std::get<Ty::Placeholder>(ty->v).i, rets);
      }
      return ty;
    };
    replaceTy<true>(*targetTy->tcx, targetTy, countRets);
    std::vector<Tp> out(rets, ty->tcx->intern(Ty::Err{}));
    std::function<void(Tp, Tp)> doDeconstruct = [&doDeconstruct, &out](Tp tmplTy, Tp ty) {
      if (std::holds_alternative<Ty::Placeholder>(tmplTy->v)) {
        out.at(std::get<Ty::Placeholder>(tmplTy->v).i) = ty;
      } else if (tmplTy->v.index() != ty->v.index()) {
        return; // output containing errs would indicate that something went wrong
      } else {
        std::visit(
            overloaded{
                [&](Ty::Tuple &lt, Ty::Tuple &rt) {
                  if (lt.t.size() != rt.t.size()) return;
                  auto ltIt = lt.t.begin();
                  auto rtIt = rt.t.begin();
                  for (; ltIt != lt.t.end(); ++ltIt, ++rtIt) {
                    doDeconstruct(*ltIt, *rtIt);
                  }
                },
                [&](Ty::ADT &lt, Ty::ADT &rt) {
                  if (lt.i != rt.i || lt.s.size() != rt.s.size()) return;
                  auto ltIt = lt.s.begin();
                  auto rtIt = rt.s.begin();
                  for (; ltIt != lt.s.end(); ++ltIt, ++rtIt) {
                    doDeconstruct(*ltIt, *rtIt);
                  }
                },
                [&](Ty::FfiFn &lt, Ty::FfiFn &rt) {
                  doDeconstruct(lt.args, rt.args);
                  doDeconstruct(lt.ret, rt.ret);
                },
                [](const auto &, const auto &) {}
            },
            tmplTy->v, ty->v);
      }
    };
    doDeconstruct(targetTy, ty);
    return out;
  }

  std::vector<Tp> OutputInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &constArgs) const {
    constant_cast<std::vector<Tp>*>(constArgs.at(0))->at(constant_cast<size_t>(constArgs.at(1))) = tys.front();
    return std::vector<Tp>();
  }

  std::vector<Tp> TrapInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    throw std::runtime_error("ICE: trap in type inference");
  }

  std::vector<Tp> UnionInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    // TODO union
    return std::vector<Tp>();
  }
}
