#include "Insns.h"

#include <utility>
#include <sstream>

namespace type::infer {
  std::vector<Tp> IdentityInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    return tys;
  }
  std::vector<Tp> ConstructInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cArgs) const {
    std::vector<Tp> ret;
    ret.reserve(cArgs.size());
    for (const Constant &constant : cArgs) {
      if (tys.empty()) {
        ret.push_back(constant_cast<Tp>(constant));
      } else {
        TyTemplate tyTemplate = constant_cast<Tp>(constant);
        ret.push_back(tyTemplate.construct(tys));
      }
    }
    return ret;
  }
  std::vector<Tp> DeConstructInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cArgs) const {
    TyTemplate tyTemplate = constant_cast<Tp>(cArgs.front());
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
    Idx rets = 0;
    auto countRets = [&rets](Tp ty) {
      if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
        rets = std::max(std::get<Ty::Placeholder>(ty->v).i + 1, rets);
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
    doDeconstruct(targetTy, type::uncycle(ty));
    for (Tp tp : out) {
      if (std::holds_alternative<Ty::Err>(tp->v)) {
        std::stringstream ss;
        ss << "ICE: Failed deconstruction";
        ss << "\n to: " << targetTy;
        ss << "\n from: " << ty;
        throw std::runtime_error(ss.str());
      }
    }
    return out;
  }

  std::vector<Tp> OutputInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &constArgs) const {
    constant_cast<std::vector<Tp>*>(constArgs.at(0))->at(constant_cast<size_t>(constArgs.at(1))) = tys.front();
    return std::vector<Tp>();
  }

  std::vector<Tp> TrapInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    throw std::runtime_error("ICE: trap in type inference");
  }

  std::vector<Tp> CheckInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    DeConstructInsn deconstruct;
    deconstruct(tys, {tys.front()});
    return std::vector<Tp>();
  }

  std::vector<Tp> UnionInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    std::vector<Tp> tysC = tys;
    return {unionOf(*tys.at(0)->tcx, tysC)};
  }

  ConstInsn::ConstInsn(std::vector<Tp> ret) : ret(std::move(ret)) {}
  std::vector<Tp> ConstInsn::operator()(const std::vector<Tp> &tys, const std::vector<Constant> &) const {
    return ret;
  }

  thread_local Inst::Val *InstWrapper::CURRENT_INST = nullptr;
}
