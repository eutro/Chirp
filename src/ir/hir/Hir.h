#pragma once


#include "../Visitor.h"
#include "../../common/Loc.h"
#include "../../common/Idx.h"

#include <functional>
#include <map>
#include <set>
#include <utility>
#include <variant>
#include <cstdint>
#include <vector>
#include <memory>
#include <optional>

namespace hir {
  using DefIdx = Idx;

  class Type {
  public:
    std::optional<loc::Span> source;
    std::optional<DefIdx> base;
    std::vector<Type> params;
  };

  struct DefType {
    struct Variable {
      bool global;
      std::vector<Type> hints;
    };
    struct Type {};
    struct Trait {};
    struct ADT {
      struct Variant {
        std::vector<DefIdx> values;
      };
      std::vector<Variant> variants;
    };
    std::variant<Variable, Trait, Type, ADT> v;
    template <typename... Arg>
    DefType(Arg &&... arg): v(std::forward<Arg>(arg)...) {}
  };

  class Definition {
  public:
    std::string name;
    std::optional<loc::Span> source;
    DefType defType = DefType::Type{};
  };

  class ErasedExprVisitor;

  enum class Pos {
    /**
     * An expression is in expr position if its value will be used.
     */
    Expr,
    /**
     * An expression is in stmt position if its value is ignored.
     */
    Stmt,
  };

  class Expr {
  public:
    std::optional<loc::Span> span;
    Pos pos = Pos::Expr;
    std::vector<Type> type;

    virtual ~Expr();

    virtual _acceptDef(Expr) = 0;
  };

  using Eptr = std::unique_ptr<Expr>;

  class Block {
  public:
    std::optional<loc::Span> span;
    std::vector<DefIdx> bindings;
    std::vector<Eptr> body;
  };

  class TraitImpl {
  public:
    std::vector<DefIdx> params;
    Type type, trait;
    loc::Span source;
    std::vector<Type> types;
    std::vector<Block> methods;
  };

  class Program {
  public:
    std::map<DefIdx, Definition> bindings;
    std::vector<TraitImpl> traitImpls;
    Block topLevel;
  };

  class BlockExpr : public Expr {
  public:
    Block block;

    _acceptDef(Expr) override;
  };

  class VarExpr : public Expr {
  public:
    DefIdx ref;

    _acceptDef(Expr) override;
  };

  class CondExpr : public Expr {
  public:
    Eptr predE, thenE, elseE;

    _acceptDef(Expr) override;
  };

  class VoidExpr : public Expr {
  public:
    _acceptDef(Expr) override;
  };

  class LiteralExpr : public Expr {
  public:
    enum Type {
      Int, Float, String,
    };
    Type type;
    std::string value;

    _acceptDef(Expr) override;
  };

  class BoolExpr : public Expr {
  public:
    bool value;

    _acceptDef(Expr) override;
  };

  class BinExpr : public Expr {
  public:
    enum Op {
      BitOr, BitAnd,
      Add, Sub,
      Mul, Div,
      Rem,
    };
    Op op;
    Eptr lhs, rhs;

    _acceptDef(Expr) override;
  };

  class CmpExpr : public Expr {
  public:
    enum Op {
      Ne, Eq,
      Lt, Le, Gt, Ge,
    };
    Op op;
    Eptr lhs, rhs;

    _acceptDef(Expr) override;
  };

  class NegExpr : public Expr {
  public:
    Eptr value;

    _acceptDef(Expr) override;
  };

  class CallExpr : public Expr {
  public:
    Eptr func;
    std::vector<Eptr> args;

    _acceptDef(Expr) override;
  };

  class DefineExpr : public Expr {
  public:
    Idx idx;
    Eptr value;

    _acceptDef(Expr) override;
  };

  class NewExpr : public Expr {
  public:
    Idx adt;
    Idx variant;
    std::vector<Eptr> values;

    _acceptDef(Expr) override;
  };

  class GetExpr : public Expr {
  public:
    Idx adt;
    Idx variant;
    Idx field;
    Eptr value;

    _acceptDef(Expr) override;
  };

  class ForeignExpr : public Expr {
  public:
    std::string name;

    _acceptDef(Expr) override;
  };

  class DummyExpr : public Expr {
  public:
    _acceptDef(Expr) override;
  };

  class ErasedExprVisitor {
  public:
    _EvisitVirtual(Expr)
    _EvisitVirtual(BlockExpr)
    _EvisitVirtual(VarExpr)
    _EvisitVirtual(CondExpr)
    _EvisitVirtual(VoidExpr)
    _EvisitVirtual(LiteralExpr)
    _EvisitVirtual(BoolExpr)
    _EvisitVirtual(BinExpr)
    _EvisitVirtual(CmpExpr)
    _EvisitVirtual(NegExpr)
    _EvisitVirtual(CallExpr)
    _EvisitVirtual(DefineExpr)
    _EvisitVirtual(NewExpr)
    _EvisitVirtual(GetExpr)
    _EvisitVirtual(ForeignExpr)
    _EvisitVirtual(DummyExpr)
  };

  template <typename Ret=std::monostate, typename ...Arg>
  class ExprVisitor : public ErasedExprVisitor {
  public:
    _EvisitImpl(Expr)
    _EvisitImpl(BlockExpr)
    _EvisitImpl(VarExpr)
    _EvisitImpl(CondExpr)
    _EvisitImpl(VoidExpr)
    _EvisitImpl(LiteralExpr)
    _EvisitImpl(BoolExpr)
    _EvisitImpl(BinExpr)
    _EvisitImpl(CmpExpr)
    _EvisitImpl(NegExpr)
    _EvisitImpl(CallExpr)
    _EvisitImpl(DefineExpr)
    _EvisitImpl(NewExpr)
    _EvisitImpl(GetExpr)
    _EvisitImpl(ForeignExpr)
    _EvisitImpl(DummyExpr)

    virtual ~ExprVisitor() = default;
    virtual _typedRoot(Expr);
    virtual _typedVisit(BlockExpr) = 0;
    virtual _typedVisit(VarExpr) = 0;
    virtual _typedVisit(CondExpr) = 0;
    virtual _typedVisit(VoidExpr) = 0;
    virtual _typedVisit(LiteralExpr) = 0;
    virtual _typedVisit(BoolExpr) = 0;
    virtual _typedVisit(BinExpr) = 0;
    virtual _typedVisit(CmpExpr) = 0;
    virtual _typedVisit(NegExpr) = 0;
    virtual _typedVisit(CallExpr) = 0;
    virtual _typedVisit(DefineExpr) = 0;
    virtual _typedVisit(NewExpr) = 0;
    virtual _typedVisit(GetExpr) = 0;
    virtual _typedVisit(ForeignExpr) = 0;
    virtual _typedVisit(DummyExpr) = 0;
  };

  template <typename Ret=std::monostate, typename ...Arg>
  class ProgramVisitor {
  public:
    virtual ~ProgramVisitor() = default;
    virtual Ret visitProgram(Program &it, Arg...) = 0;
  };
}
