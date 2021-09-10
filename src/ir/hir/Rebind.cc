#include "Rebind.h"

namespace hir::rebind {
  class RebindVisitorImpl : public RebindVisitor {
    Rebinder rb;
  public:
    RebindVisitorImpl(Rebinder &&rb): rb(rb) {}

    void visitPtr(Eptr &ptr) {
      visitExpr(*ptr, &ptr);
    }

    void visitBlock(Block &it) {
      for (auto &e : it.body) {
        visitPtr(e);
      }
    }

    std::monostate visitBlockExpr(BlockExpr &it, Eptr *ref) {
      visitBlock(it.block);
      return {};
    }

    std::monostate visitVarExpr(VarExpr &it, Eptr *ref) {
      auto ptr = rb(it);
      if (ptr) {
        *ref = std::move(ptr);
      }
      return {};
    }

    std::monostate visitCondExpr(CondExpr &it, Eptr *ref) {
      visitPtr(it.predE);
      visitPtr(it.thenE);
      visitPtr(it.elseE);
      return {};
    }

    std::monostate visitVoidExpr(VoidExpr &it, Eptr *ref) {
      return {}; // noop
    }

    std::monostate visitLiteralExpr(LiteralExpr &it, Eptr *ref) {
      return {}; // noop
    }

    std::monostate visitBoolExpr(BoolExpr &it, Eptr *ref) {
      return {}; // noop
    }

    std::monostate visitBinExpr(BinExpr &it, Eptr *ref) {
      visitPtr(it.lhs);
      visitPtr(it.rhs);
      return {};
    }

    std::monostate visitCmpExpr(CmpExpr &it, Eptr *ref) {
      visitPtr(it.lhs);
      visitPtr(it.rhs);
      return {};
    }

    std::monostate visitNegExpr(NegExpr &it, Eptr *ref) {
      visitPtr(it.value);
      return {};
    }

    std::monostate visitCallExpr(CallExpr &it, Eptr *ref) {
      visitPtr(it.func);
      for (auto &a : it.args) {
        visitPtr(a);
      }
      return {};
    }

    std::monostate visitDefineExpr(DefineExpr &it, Eptr *ref) {
      visitPtr(it.value);
      return {};
    }

    std::monostate visitNewExpr(NewExpr &it, Eptr *ref) {
      for (auto &v : it.values) {
        visitPtr(v);
      }
      return {};
    }

    std::monostate visitGetExpr(GetExpr &it, Eptr *ref) {
      visitPtr(it.value);
      return {};
    }

    std::monostate visitForeignExpr(ForeignExpr &it, Eptr *ref) {
      return {}; // noop
    }

    std::monostate visitDummyExpr(DummyExpr &it, Eptr *ref) {
      return {}; // noop
    }
  };

  std::unique_ptr<RebindVisitor> rebindVisitor(Rebinder &&rb) {
    return std::make_unique<RebindVisitorImpl>(std::forward<Rebinder>(rb));
  }
}
