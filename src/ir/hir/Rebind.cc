#include "Rebind.h"

#include <functional>

namespace hir::rebind {
  class RebindVisitorImpl : public RebindVisitor {
    Rebinder rb;
  public:
    RebindVisitorImpl(Rebinder &&rb): rb(rb) {}

    void visitPtr(Eptr &ptr, Idx depth) {
      visitExpr(*ptr, &ptr, depth);
    }

    void visitBlock(Block &it, Idx depth) {
      for (auto &e : it.body) {
        visitPtr(e, depth + 1);
      }
    }

    std::monostate visitBlockExpr(BlockExpr &it, Eptr *ref, Idx depth) {
      visitBlock(it.block, depth);
      return {};
    }

    std::monostate visitVarExpr(VarExpr &it, Eptr *ref, Idx depth) {
      auto ptr = rb(it, depth);
      if (ptr) {
        *ref = std::move(ptr);
      }
      return {};
    }

    std::monostate visitCondExpr(CondExpr &it, Eptr *ref, Idx depth) {
      visitPtr(it.predE, depth);
      visitPtr(it.thenE, depth);
      visitPtr(it.elseE, depth);
      return {};
    }

    std::monostate visitVoidExpr(VoidExpr &it, Eptr *ref, Idx depth) {
      return {}; // noop
    }

    std::monostate visitLiteralExpr(LiteralExpr &it, Eptr *ref, Idx depth) {
      return {}; // noop
    }

    std::monostate visitBinExpr(BinExpr &it, Eptr *ref, Idx depth) {
      visitPtr(it.lhs, depth);
      visitPtr(it.rhs, depth);
      return {};
    }

    std::monostate visitCmpExpr(CmpExpr &it, Eptr *ref, Idx depth) {
      for (auto &e : it.exprs) {
        visitPtr(e, depth);
      }
      return {};
    }

    std::monostate visitNegExpr(NegExpr &it, Eptr *ref, Idx depth) {
      visitPtr(it.value, depth);
      return {};
    }

    std::monostate visitCallExpr(CallExpr &it, Eptr *ref, Idx depth) {
      visitPtr(it.func, depth);
      for (auto &a : it.args) {
        visitPtr(a, depth);
      }
      return {};
    }

    std::monostate visitDefineExpr(DefineExpr &it, Eptr *ref, Idx depth) {
      visitPtr(it.value, depth);
      return {};
    }

    std::monostate visitNewExpr(NewExpr &it, Eptr *ref, Idx depth) {
      for (auto &v : it.values) {
        visitPtr(v, depth);
      }
      return {};
    }

    std::monostate visitGetExpr(GetExpr &it, Eptr *ref, Idx depth) {
      visitPtr(it.value, depth);
      return {};
    }

    std::monostate visitForeignExpr(ForeignExpr &it, Eptr *ref, Idx depth) {
      return {}; // noop
    }

    std::monostate visitDummyExpr(DummyExpr &it, Eptr *ref, Idx depth) {
      return {}; // noop
    }
  };

  std::unique_ptr<RebindVisitor> rebindVisitor(Rebinder &&rb) {
    return std::make_unique<RebindVisitorImpl>(std::forward<Rebinder>(rb));
  }
}
