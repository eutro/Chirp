#include "Lowering.h"
#include "Ast.h"

#include <deque>
#include <map>
#include <memory>
#include <optional>
#include <string>

namespace ast::lower {
  class Bindings {
    struct ScopeSet {
      std::vector<const std::string*> names;
      std::map<std::string, hir::Idx> bound;
    };

    std::deque<ScopeSet> sets = {{}};

  public:
    void push() {
      sets.emplace_back();
    }

    void pop() {
      sets.pop_back();
    }

    const std::vector<const std::string*> &names() {
      return sets.back().names;
    }

    hir::Idx introduce(const std::string &name) {
      auto &ss = sets.back();
      hir::Idx idx = ss.names.size();
      ss.names.push_back(&ss.bound.insert({name, idx}).first->first);
      return idx;
    }

    struct Var {
      hir::Idx block, idx;
    };

    std::optional<Var> lookup(const std::string &name) {
      Var var;
      var.block = 0;
      for (auto it = sets.rbegin(); it != sets.rend(); ++it, ++var.block) {
        auto found = it->bound.find(name);
        if (found != it->bound.end()) {
          var.idx = found->second;
          return var;
        }
      }
      return std::nullopt;
    }
  };

  typedef std::unique_ptr<hir::Expr> Eptr;

  enum class StmtVisit {
    Bindings,
    Expr,
  };

  class LoweringVisitor :
    public ProgramVisitor<LowerResult>,
    public ExprVisitor<Eptr, hir::Pos>,
    public StatementVisitor<Eptr, StmtVisit> {
  public:
    err::ErrorContext errs;
    hir::Program program;
    Bindings bindings;

    void addBindingsToBlock(hir::Block &block) {
      for (const std::string *name : bindings.names()) {
        auto &b = block.bindings.emplace_back();
        b.name = *name;
      }
    }

    void introduceBinding(Binding &it) {
      bindings.introduce(it.name.ident.value);
    }

    Eptr visitBindingExpr(Binding &it) {
      if (it.foreignToken) {
        errs
          .err()
          .msg("FFI not implemented yet")
          .pos(it.foreignToken->loc, "here");
        return std::make_unique<hir::DummyExpr>(); // TODO ffi
      }
      std::unique_ptr<hir::DefineExpr> retExpr;
      if (it.arguments) {
        auto expr = std::make_unique<hir::DefunExpr>();
        bindings.push();
        for (auto &rb : it.arguments->bindings) {
          bindings.introduce(rb.name.ident.value);
        }
        addBindingsToBlock(expr->block);
        expr->block.body.push_back(visitExpr(*it.value, hir::Pos::Tail));
        bindings.pop();
        retExpr = std::move(expr);
      } else {
        auto expr = std::make_unique<hir::DefvalExpr>();
        expr->value = visitExpr(*it.value, hir::Pos::Expr);
        retExpr = std::move(expr);
      }
      retExpr->idx = bindings.lookup(it.name.ident.value)->idx;
      return retExpr;
    }

    LowerResult visitProgram(Program &it) override {
      for (auto &stmt : it.statements) {
        visitStatement(*stmt, StmtVisit::Bindings);
      }

      addBindingsToBlock(program.topLevel);
      for (auto &stmt : it.statements) {
        program.topLevel.body.push_back(visitStatement(*stmt, StmtVisit::Expr));
      }
      LowerResult res;
      res.program = std::move(program);
      res.errors = std::move(errs);
      return res;
    }

    Eptr visitDefn(Defn &it, StmtVisit type) override {
      if (type == StmtVisit::Bindings) {
        introduceBinding(it.binding);
        return nullptr;
      } else {
        return visitBindingExpr(it.binding);
      }
    }

    Eptr visitExpr(Expr &it, StmtVisit type) override {
      if (type != StmtVisit::Expr) return nullptr;
      return ExprVisitor::visitExpr(it, hir::Pos::Stmt);
    }

    Eptr visitExpr(Expr &it, hir::Pos pos) override {
      Eptr e = ExprVisitor::visitExpr(it, pos);
      e->span = it.span;
      e->pos = pos;
      return e;
    }

    Eptr visitIfExpr(IfExpr &it, hir::Pos pos) override {
      hir::Pos branchPos = it.elseClause ? pos : hir::Pos::Stmt;
      Eptr elseE = it.elseClause ?
        visitExpr(*it.elseClause->thenExpr, branchPos) :
        std::make_unique<hir::VoidExpr>();
      for (auto iter = it.elseIfClauses.rbegin();
           iter != it.elseIfClauses.rend();
           ++iter) {
        auto ifE = std::make_unique<hir::CondExpr>();
        ifE->predE = visitExpr(*iter->predExpr, hir::Pos::Expr);
        ifE->thenE = visitExpr(*iter->thenExpr, branchPos);
        ifE->elseE = std::move(elseE);
        elseE = std::move(ifE);
      }
      auto ifE = std::make_unique<hir::CondExpr>();
      ifE->predE = visitExpr(*it.predExpr, hir::Pos::Expr);
      ifE->thenE = visitExpr(*it.thenExpr, branchPos);
      ifE->elseE = std::move(elseE);
      return ifE;
    }

    Eptr visitLetExpr(LetExpr &it, hir::Pos pos) override {
      bindings.push();

      for (auto &b : it.bindings) {
        introduceBinding(b);
      }

      // TODO create a closure if it's named

      auto expr = std::make_unique<hir::BlockExpr>();
      addBindingsToBlock(expr->block);

      expr->block.body.push_back(visitExpr(*it.body, pos));
      bindings.pop();
      return expr;
    }

    Eptr visitFnExpr(FnExpr &it, hir::Pos pos) override {
      // TODO closures
      errs.err().msg("unimplemented").span(it.span, "sorry :(");
      return std::make_unique<hir::DummyExpr>();
    }

    Eptr visitLambdaExpr(LambdaExpr &it, hir::Pos pos) override {
      errs.err().msg("unimplemented").span(it.span, "sorry :(");
      return std::make_unique<hir::DummyExpr>();
    }

    Eptr visitBlockExpr(BlockExpr &it, hir::Pos pos) override {
      if (!it.value) {
        return std::make_unique<hir::VoidExpr>();
      }

      bindings.push();
      for (auto &stmt : it.statements) {
        visitStatement(*stmt.statement, StmtVisit::Bindings);
      }

      auto expr = std::make_unique<hir::BlockExpr>();
      addBindingsToBlock(expr->block);

      auto &body = expr->block.body;
      for (auto &stmt : it.statements) {
        body.push_back(visitStatement(*stmt.statement, StmtVisit::Expr));
      }
      body.push_back(visitExpr(*it.value, pos));
      bindings.pop();
      return expr;
    }

    Eptr visitBracketExpr(BracketExpr &it, hir::Pos pos) override {
      return visitExpr(*it.value, pos);
    }

    Eptr visitColonExpr(ColonExpr &it, hir::Pos pos) override {
      return visitExpr(*it.value, pos);
    }

    Eptr visitLiteralExpr(LiteralExpr &it, hir::Pos pos) override {
      auto expr = std::make_unique<hir::LiteralExpr>();
      expr->value = it.value.value;
      switch (it.value.type) {
      case Tok::TInt:
        expr->type = hir::LiteralExpr::Type::Int;
        break;
      case Tok::TFloat:
        expr->type = hir::LiteralExpr::Type::Float;
        break;
      case Tok::TStr:
        expr->type = hir::LiteralExpr::Type::String;
        break;
      default:
        expr->type = hir::LiteralExpr::Type::Bool;
        break;
      }
      return expr;
    }

    Eptr visitVarExpr(VarExpr &it, hir::Pos pos) override {
      auto lookup = bindings.lookup(it.name.ident.value);
      if (lookup) {
        auto expr = std::make_unique<hir::VarExpr>();
        expr->block = lookup->block;
        expr->idx = lookup->idx;
        return expr;
      } else {
        errs
          .err()
          .msg("Name '" + it.name.ident.value + "' is not defined")
          .span(it.span, "here");
        return std::make_unique<hir::DummyExpr>();
      }
    }

    Eptr visitBinaryExpr(BinaryExpr &it, hir::Pos pos) override {
      // all our operators are left-associative
      std::unique_ptr<hir::Expr> retExpr = visitExpr(*it.lhs, hir::Pos::Expr);
      for (auto &term : it.terms) {
        auto binExpr = std::make_unique<hir::BinExpr>();
        binExpr->lhs = std::move(retExpr);
        binExpr->rhs = visitExpr(*term.expr, hir::Pos::Expr);
        switch (term.operatorToken.type) {
        case Tok::TAnd2:
          binExpr->op = hir::BinExpr::Op::LogAnd;
          break;
        case Tok::TOr2:
          binExpr->op = hir::BinExpr::Op::LogOr;
          break;
        case Tok::TNe:
          binExpr->op = hir::BinExpr::Op::Ne;
          break;
        case Tok::TEq:
        case Tok::TEq2:
          binExpr->op = hir::BinExpr::Op::Eq;
          break;
        case Tok::TLt:
          binExpr->op = hir::BinExpr::Op::Lt;
          break;
        case Tok::TLe:
          binExpr->op = hir::BinExpr::Op::Le;
          break;
        case Tok::TGt:
          binExpr->op = hir::BinExpr::Op::Gt;
          break;
        case Tok::TGe:
          binExpr->op = hir::BinExpr::Op::Ge;
          break;
        case Tok::TOr1:
          binExpr->op = hir::BinExpr::Op::BitOr;
          break;
        case Tok::TAnd1:
          binExpr->op = hir::BinExpr::Op::BitAnd;
          break;
        case Tok::TAdd:
          binExpr->op = hir::BinExpr::Op::Add;
          break;
        case Tok::TSub:
          binExpr->op = hir::BinExpr::Op::Sub;
          break;
        case Tok::TMul:
          binExpr->op = hir::BinExpr::Op::Mul;
          break;
        case Tok::TDiv:
          binExpr->op = hir::BinExpr::Op::Div;
          break;
        case Tok::TRem:
          binExpr->op = hir::BinExpr::Op::Rem;
          break;
        default:
          // unreachable
          break;
        }
        retExpr = std::move(binExpr);
      }
      return retExpr;
    }

    Eptr visitPrefixExpr(PrefixExpr &it, hir::Pos pos) override {
      auto retExpr = visitExpr(*it.expr, hir::Pos::Expr);
      for (auto iter = it.prefixes.rbegin(); iter != it.prefixes.rend(); ++iter) {
        if (iter->type == Tok::TSub) {
          auto expr = std::make_unique<hir::NegExpr>();
          expr->value = std::move(retExpr);
          retExpr = std::move(expr);
        }
      }
      return retExpr;
    }

    Eptr visitFunCallExpr(FunCallExpr &it, hir::Pos pos) override {
      auto expr = std::make_unique<hir::CallExpr>();
      expr->func = visitExpr(*it.function, hir::Pos::Expr);
      expr->args.reserve(it.arguments.size());
      for (auto &arg : it.arguments) {
        expr->args.push_back(visitExpr(*arg, hir::Pos::Expr));
      }
      return expr;
    }

    Eptr visitHintedExpr(HintedExpr &it, hir::Pos pos) override {
      return visitExpr(*it.expr, pos);
    }
  };

  std::unique_ptr<ProgramVisitor<LowerResult>> lowerVisitor() {
    return std::make_unique<LoweringVisitor>();
  }
}
