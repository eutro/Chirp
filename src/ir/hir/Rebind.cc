#include "Rebind.h"

namespace hir::rebind {
  class RebindVisitorImpl : public RebindVisitor {
    Rebinder rb;
    TypeRebinder trb;
  public:
    RebindVisitorImpl(Rebinder &&rb, TypeRebinder &&trb):
        rb(std::forward<Rebinder>(rb)),
        trb(std::forward<TypeRebinder>(trb)) {}

    void rebindType(Type &hint) {
      if (hint.base) {
        trb(hint);
      }
      for (Type &t : hint.params) rebindType(t);
    }

    void visitPtr(Eptr &ptr) {
      visitExpr(*ptr, &ptr);
      if (trb) {
        for (Type &hint : ptr->hints) {
          rebindType(hint);
        }
      }
    }

    void visitBlock(Block &it) {
      for (auto &e : it.body) {
        visitPtr(e);
      }
    }

    std::monostate visitBlockExpr([[maybe_unused]] BlockExpr &it, [[maybe_unused]] Eptr *ref) override {
      visitBlock(it.block);
      return {};
    }

    std::monostate visitVarExpr([[maybe_unused]] VarExpr &it, [[maybe_unused]] Eptr *ref) override {
      auto ptr = rb(it);
      if (ptr) {
        *ref = std::move(ptr);
      }
      return {};
    }

    std::monostate visitCondExpr([[maybe_unused]] CondExpr &it, [[maybe_unused]] Eptr *ref) override {
      visitPtr(it.predE);
      visitPtr(it.thenE);
      visitPtr(it.elseE);
      return {};
    }

    std::monostate visitVoidExpr([[maybe_unused]] VoidExpr &it, [[maybe_unused]] Eptr *ref) override {
      return {}; // noop
    }

    std::monostate visitLiteralExpr([[maybe_unused]] LiteralExpr &it, [[maybe_unused]] Eptr *ref) override {
      return {}; // noop
    }

    std::monostate visitBoolExpr([[maybe_unused]] BoolExpr &it, [[maybe_unused]] Eptr *ref) override {
      return {}; // noop
    }

    std::monostate visitBinExpr([[maybe_unused]] BinExpr &it, [[maybe_unused]] Eptr *ref) override {
      visitPtr(it.lhs);
      visitPtr(it.rhs);
      return {};
    }

    std::monostate visitCmpExpr([[maybe_unused]] CmpExpr &it, [[maybe_unused]] Eptr *ref) override {
      visitPtr(it.lhs);
      visitPtr(it.rhs);
      return {};
    }

    std::monostate visitNegExpr([[maybe_unused]] NegExpr &it, [[maybe_unused]] Eptr *ref) override {
      visitPtr(it.value);
      return {};
    }

    std::monostate visitCallExpr([[maybe_unused]] CallExpr &it, [[maybe_unused]] Eptr *ref) override {
      visitPtr(it.func);
      for (auto &a : it.args) {
        visitPtr(a);
      }
      return {};
    }

    std::monostate visitDefineExpr([[maybe_unused]] DefineExpr &it, [[maybe_unused]] Eptr *ref) override {
      visitPtr(it.value);
      return {};
    }

    std::monostate visitNewExpr([[maybe_unused]] NewExpr &it, [[maybe_unused]] Eptr *ref) override {
      for (auto &v : it.values) {
        visitPtr(v);
      }
      return {};
    }

    std::monostate visitGetExpr([[maybe_unused]] GetExpr &it, [[maybe_unused]] Eptr *ref) override {
      visitPtr(it.value);
      return {};
    }

    std::monostate visitForeignExpr([[maybe_unused]] ForeignExpr &it, [[maybe_unused]] Eptr *ref) override {
      return {}; // noop
    }

    std::monostate visitDummyExpr([[maybe_unused]] DummyExpr &it, [[maybe_unused]] Eptr *ref) override {
      return {}; // noop
    }
  };

  std::unique_ptr<RebindVisitor> rebindVisitor(Rebinder &&rb, TypeRebinder &&trb) {
    return std::make_unique<RebindVisitorImpl>(std::forward<Rebinder>(rb), std::forward<TypeRebinder>(trb));
  }
}
