#pragma once

#include "Hir.h"

namespace hir {
  template<typename Ret, typename... Arg>
  class RecurVisitor : public ExprVisitor<Ret, Arg &...> {
#define VISIT(TY) virtual Ret visit##TY([[maybe_unused]] TY &it, Arg &...arg) override
  public:
    virtual void visitBlock(Block &it, Arg &...arg) {
      for (auto &e : it.body) {
        this->visitExpr(*e, arg...);
      }
    }
    VISIT(BlockExpr) {
      visitBlock(it.block, arg...);
      return {};
    }
    VISIT(VarExpr) {
      return {};
    }
    VISIT(CondExpr) {
      this->visitExpr(*it.predE, arg...);
      this->visitExpr(*it.thenE, arg...);
      this->visitExpr(*it.elseE, arg...);
      return {};
    }
    VISIT(VoidExpr) {
      return {}; // noop
    }
    VISIT(LiteralExpr) {
      return {}; // noop
    }
    VISIT(BoolExpr) {
      return {}; // noop
    }
    VISIT(BinExpr) {
      this->visitExpr(*it.lhs, arg...);
      this->visitExpr(*it.rhs, arg...);
      return {};
    }
    VISIT(CmpExpr) {
      this->visitExpr(*it.lhs, arg...);
      this->visitExpr(*it.rhs, arg...);
      return {};
    }
    VISIT(NegExpr) {
      this->visitExpr(*it.value, arg...);
      return {};
    }
    VISIT(CallExpr) {
      this->visitExpr(*it.func, arg...);
      for (auto &a : it.args) {
        this->visitExpr(*a, arg...);
      }
      return {};
    }
    VISIT(DefineExpr) {
      this->visitExpr(*it.value, arg...);
      return {};
    }
    VISIT(NewExpr) {
      for (auto &v : it.values) {
        this->visitExpr(*v, arg...);
      }
      return {};
    }
    VISIT(GetExpr) {
      this->visitExpr(*it.value, arg...);
      return {};
    }
    VISIT(TypeExpr) {
      return {}; // noop
    }
    VISIT(ForeignExpr) {
      return {}; // noop
    }
    VISIT(DummyExpr) {
      return {}; // noop
    }
  };
}
