#include "Lowering.h"
#include "Ast.h"
#include "../hir/Rebind.h"
#include "../../common/Util.h"

#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <variant>

namespace ast::lower {
  class Bindings {
    using Ridge = std::function<void(const hir::DefIdx &)>;

    struct ScopeSet {
      std::vector<hir::DefIdx> defs;
      std::map<const std::string *, hir::DefIdx, util::DerefCmp<>> bound;
      std::optional<Ridge> ridge;
    };

    std::vector<ScopeSet> sets = {{}};

  public:
    void push() {
      sets.emplace_back();
    }

    void push(Ridge &&ridge) {
      sets.emplace_back().ridge = std::move(ridge);
    }

    void pop() {
      sets.pop_back();
    }

    const std::vector<hir::DefIdx> &defs() {
      return sets.back().defs;
    }

    hir::Idx introduce(const std::string &name, hir::DefIdx idx) {
      auto &ss = sets.back();
      ss.bound[&name] = idx;
      ss.defs.push_back(idx);
      return idx;
    }

    std::optional<hir::DefIdx> lookup(const std::string &name) {
      std::vector<Ridge*> ridges;
      for (auto it = sets.rbegin(); it != sets.rend(); ++it) {
        auto found = it->bound.find(&name);
        if (found != it->bound.end()) {
          for (auto &r : ridges) {
            (*r)(found->second);
          }
          return found->second;
        }
        if (it->ridge) {
          ridges.push_back(&*it->ridge);
        }
      }
      return std::nullopt;
    }
  };

  typedef std::unique_ptr<hir::Expr> Eptr;

  template <typename T>
  std::unique_ptr<T> withSpan(const loc::Span &span) {
    auto tptr = std::make_unique<T>();
    tptr->span = span;
    return tptr;
  }

  class LoweringVisitor :
    public ProgramVisitor<LowerResult>,
    public TypeVisitor<hir::Type>,
    public ExprVisitor<Eptr, hir::Pos>,
    public StatementVisitor<Eptr, std::optional<hir::Idx>> {
  public:
    err::ErrorContext errs;
    hir::Program program;
    Bindings bindings;
    Bindings typeBindings;
    hir::DefIdx defNo = 0;

    void builtinType(const std::string &name) {
      auto tyIdx = defNo++;
      auto &newTy = program.typeBindings[tyIdx];
      newTy.name = name;
      auto &import = program.typeImports.emplace_back();
      import.moduleIdx = 0;
      import.name = name;
      import.defIdx = tyIdx;
      typeBindings.introduce(newTy.name, tyIdx);
    }

    LoweringVisitor() {
      builtinType("bool");

      builtinType("i8");
      builtinType("i16");
      builtinType("i32");
      builtinType("i64");

      builtinType("u8");
      builtinType("u16");
      builtinType("u32");
      builtinType("u64");

      builtinType("f16");
      builtinType("f32");
      builtinType("f64");

      builtinType("fn");

      typeBindings.push();
    }

    void addBindingsToBlock(hir::Block &block) {
      block.bindings = bindings.defs();
      block.typeBindings = typeBindings.defs();
    }

    hir::Type visitHint(std::optional<TypeHint> &hint) {
      return hint ? visitType(*hint->type) : hir::Type();
    }

    /**
     * Introduce a binding, return the index of the variable in the block.
     */
    hir::Idx introduceBinding(Binding &it) {
      auto idx = defNo++;
      hir::Binding &b = program.bindings[idx];
      b.name = it.name.ident.value;
      b.source = it.name.ident.span();
      b.type = it.arguments ? hir::Type() : visitHint(it.typeHint);
      return bindings.introduce(b.name, idx);
    }

    class BindingVisitor :
      public StatementVisitor<std::optional<hir::Idx>> {
    private:
      LoweringVisitor &lv;
    public:
      BindingVisitor(LoweringVisitor &lv): lv(lv) {}

      std::optional<hir::Idx> visitDefn(Defn &it) override {
        return lv.introduceBinding(it.binding);
      }

      std::optional<hir::Idx> visitExpr(Expr &it) override {
        return std::nullopt;
      }
    };

    hir::Type visitNamedType(NamedType &it) override {
      hir::Type ty;
      ty.source = it.span;
      auto found = typeBindings.lookup(it.raw.ident.value);
      if (found) {
        ty.base = found;
      } else {
        errs
          .err()
          .msg("Type '" + it.raw.ident.value + "' is not defined")
          .span(it.raw.ident.span(), "here");
      }
      if (it.parameters) {
        ty.params.reserve(it.parameters->types.size());
        for (auto &t : it.parameters->types) {
          ty.params.push_back(visitType(*t));
        }
      }
      return ty;
    }

    hir::Type visitPlaceholderType(PlaceholderType &it) override {
      return hir::Type();
    }

    template <typename Opt>
    auto maybePtr(Opt &it) {
      return it ? &*it : nullptr;
    }

    template <typename Param>
    Eptr fnExpr(Binding::TypeArguments *typeParams,
                TypeHint *retHint,
                std::vector<Param> &params,
                const loc::Span &source,
                Expr &body) {
      std::set<hir::DefIdx> closed;
      bindings.push([&closed](const hir::DefIdx &vr) {
        closed.insert(vr);
      });
      typeBindings.push();
      if (typeParams) {
        for (auto &tp : typeParams->idents) {
          auto idx = defNo++;
          auto &tB = program.typeBindings[idx];
          tB.name = tp.ident.value;
          tB.source = tp.ident.span();
          typeBindings.introduce(tB.name, idx);
        }
      }

      for (auto &p : params) {
        auto idx = defNo++;
        auto &b = program.bindings[idx];
        b.name = p.name.ident.value;
        b.source = p.name.ident.span();
        b.type = visitHint(p.typeHint);
        bindings.introduce(b.name, idx);
      }

      auto blockE = withSpan<hir::BlockExpr>(source);
      // FIXME this actually gets dropped
      if (retHint) blockE->type = visitType(*retHint->type);

      hir::Block &block = blockE->block;
      addBindingsToBlock(block);
      block.body.push_back(visitExpr(body, hir::Pos::Expr));
      bindings.pop();
      typeBindings.pop();

      auto thisIdx = defNo++;
      {
        auto &thisB = program.bindings[thisIdx];
        thisB.name = "this";
        thisB.source = source;
        block.bindings.push_back(thisIdx);
      }

      hir::Idx typeIdx = defNo++;
      hir::ADT &closure = program.types.emplace_back();
      hir::ADT::Variant &variant = closure.variants.emplace_back();
      closure.id = typeIdx;
      variant.values.reserve(closed.size());
      for (auto &cv : closed) {
        auto idx = defNo++;
        auto &field = program.bindings[idx];
        variant.values.push_back(idx);
        auto &data = program.bindings.at(cv);
        field.name = data.name;
        field.source = data.source;
      }

      auto expr = withSpan<hir::NewExpr>(source);
      expr->adt = typeIdx;
      expr->variant = 0;
      expr->values.reserve(closed.size());
      for (auto &cv : closed) {
        auto varE = withSpan<hir::VarExpr>(source);
        varE->ref = cv;
        expr->values.push_back(std::move(varE));
      }

      std::map<hir::DefIdx, hir::Idx> mapping;
      hir::Idx closedIdx = 0;
      for (auto &cv : closed) {
        mapping[cv] = closedIdx++;
      }

      hir::rebind::rebindVisitor([&mapping, typeIdx, thisIdx]
                                 (hir::VarExpr &varE) -> Eptr {
        auto ref = varE.ref;
        auto found = mapping.find(ref);
        if (found != mapping.end()) {
          auto getE = withSpan<hir::GetExpr>(varE.span);
          getE->adt = typeIdx;
          getE->variant = 0;
          getE->field = found->second;
          auto thisE = withSpan<hir::VarExpr>(varE.span);
          thisE->ref = thisIdx;
          getE->value = std::move(thisE);
          return getE;
        }
        return nullptr;
      })->visitExpr(*blockE, nullptr);

      program.fnImpls.push_back(std::move(block));

      return expr;
    }

    template <typename Param>
    Eptr recFnExpr(Identifier &name,
                   Binding::TypeArguments *typeParams,
                   TypeHint *retHint,
                   std::vector<Param> &params,
                   const loc::Span &source,
                   Expr &body) {
      bindings.push();
      typeBindings.push();
      auto expr = withSpan<hir::BlockExpr>(source);

      auto refIdx = defNo++;
      auto &recBinding = program.bindings[refIdx];
      recBinding.name = name.ident.value;
      recBinding.source = name.ident.span();
      auto idx = bindings.introduce(recBinding.name, refIdx);
      expr->block.bindings.push_back(idx);
      auto defE = withSpan<hir::DefineExpr>(source);
      defE->idx = idx;
      defE->value = fnExpr(typeParams, retHint, params, source, body);
      expr->block.body.push_back(std::move(defE));

      auto varE = withSpan<hir::VarExpr>(source);
      varE->ref = refIdx;
      expr->block.body.push_back(std::move(varE));

      bindings.pop();
      typeBindings.pop();
      return expr;
    }

    Eptr visitBindingExpr(Binding &it, hir::Idx idx) {
      auto retExpr = withSpan<hir::DefineExpr>(it.span);
      retExpr->idx = idx;
      if (it.foreignToken) {
        auto fe = withSpan<hir::ForeignExpr>(it.foreignToken->span());
        fe->name = it.name.ident.value;
        retExpr->value = std::move(fe);
      } else if (it.arguments) {
        retExpr->value = recFnExpr(it.name,
                                   maybePtr(it.arguments->typeArguments),
                                   maybePtr(it.typeHint),
                                   it.arguments->bindings,
                                   it.span,
                                   *it.value);
      } else {
        retExpr->value = visitExpr(*it.value, hir::Pos::Expr);
      }
      return retExpr;
    }

    LowerResult visitProgram(Program &it) override {
      std::vector<std::optional<hir::Idx>> indeces;
      indeces.reserve(it.statements.size());
      for (auto &stmt : it.statements) {
        indeces.push_back(BindingVisitor(*this).visitStatement(*stmt));
      }

      addBindingsToBlock(program.topLevel);
      auto &body = program.topLevel.body;
      auto iter = indeces.begin();
      for (auto &stmt : it.statements) {
        body.push_back(visitStatement(*stmt, *iter++));
      }
      LowerResult res;
      res.program = std::move(program);
      res.errors = std::move(errs);
      return res;
    }

    Eptr visitDefn(Defn &it, std::optional<hir::Idx> idx) override {
      return visitBindingExpr(it.binding, *idx);
    }

    Eptr visitExpr(Expr &it, std::optional<hir::Idx>) override {
      return ExprVisitor::visitExpr(it, hir::Pos::Stmt);
    }

    Eptr visitExpr(Expr &it, hir::Pos pos) override {
      Eptr e = ExprVisitor::visitExpr(it, pos);
      e->pos = pos;
      return e;
    }

    Eptr visitIfExpr(IfExpr &it, hir::Pos pos) override {
      hir::Pos branchPos = it.elseClause ? pos : hir::Pos::Stmt;
      Eptr elseE = it.elseClause ?
        visitExpr(*it.elseClause->thenExpr, branchPos) :
        withSpan<hir::VoidExpr>(it.span);
      for (auto iter = it.elseIfClauses.rbegin();
           iter != it.elseIfClauses.rend();
           ++iter) {
        auto ifE = withSpan<hir::CondExpr>(loc::Span(iter->ifToken.loc,
                                                     iter->thenExpr->span.hi));
        ifE->predE = visitExpr(*iter->predExpr, hir::Pos::Expr);
        ifE->thenE = visitExpr(*iter->thenExpr, branchPos);
        ifE->elseE = std::move(elseE);
        elseE = std::move(ifE);
      }
      auto ifE = withSpan<hir::CondExpr>(it.span);
      ifE->predE = visitExpr(*it.predExpr, hir::Pos::Expr);
      ifE->thenE = visitExpr(*it.thenExpr, branchPos);
      ifE->elseE = std::move(elseE);
      return ifE;
    }

    Eptr visitLetExpr(LetExpr &it, hir::Pos pos) override {
      bindings.push();
      typeBindings.push();
      for (auto &b : it.bindings) {
        introduceBinding(b);
      }

      auto blockE = withSpan<hir::BlockExpr>(it.span);
      auto &block = blockE->block;
      addBindingsToBlock(block);

      hir::Idx idx = 0;
      for (auto &b : it.bindings) {
        block.body.push_back(visitBindingExpr(b, idx++));
      }

      if (it.name) {
        auto call = withSpan<hir::CallExpr>(it.span);
        call->func = recFnExpr(*it.name, nullptr, nullptr, it.bindings, it.span, *it.body);
        call->args.reserve(it.bindings.size());
        auto refs = bindings.defs().begin();
        for (auto &b : it.bindings) {
          auto var = withSpan<hir::VarExpr>(it.span);
          var->ref = *refs++;
          call->args.push_back(std::move(var));
        }
      } else {
        block.body.push_back(visitExpr(*it.body, pos));
      }
      return blockE;
    }

    Eptr visitFnExpr(FnExpr &it, hir::Pos pos) override {
      return it.name ?
        recFnExpr(*it.name,
                  maybePtr(it.arguments.typeArguments),
                  maybePtr(it.typeHint),
                  it.arguments.bindings,
                  it.span,
                  *it.body) :
        fnExpr(maybePtr(it.arguments.typeArguments),
               maybePtr(it.typeHint),
               it.arguments.bindings,
               it.span,
               *it.body);
    }

    Eptr visitLambdaExpr(LambdaExpr &it, hir::Pos pos) override {
      return fnExpr(nullptr, nullptr, it.arguments, it.span, *it.body);
    }

    Eptr visitBlockExpr(BlockExpr &it, hir::Pos pos) override {
      if (!it.value) {
        return withSpan<hir::VoidExpr>(it.span);
      }

      bindings.push();
      typeBindings.push();

      std::vector<std::optional<hir::Idx>> indeces;
      indeces.reserve(it.statements.size());
      for (auto &stmt : it.statements) {
        indeces.push_back(BindingVisitor(*this).visitStatement(*stmt.statement));
      }

      auto expr = withSpan<hir::BlockExpr>(it.span);
      addBindingsToBlock(expr->block);

      auto &body = expr->block.body;
      auto iter = indeces.begin();
      for (auto &stmt : it.statements) {
        body.push_back(visitStatement(*stmt.statement, *iter++));
      }
      body.push_back(visitExpr(*it.value, pos));
      bindings.pop();
      typeBindings.pop();
      return expr;
    }

    Eptr visitBracketExpr(BracketExpr &it, hir::Pos pos) override {
      return visitExpr(*it.value, pos);
    }

    Eptr visitColonExpr(ColonExpr &it, hir::Pos pos) override {
      return visitExpr(*it.value, pos);
    }

    Eptr visitLiteralExpr(LiteralExpr &it, hir::Pos pos) override {
      if (it.value.type != Tok::TTrue &&
          it.value.type != Tok::TFalse) {
        auto expr = withSpan<hir::BoolExpr>(it.span);
        expr->value = it.value.type == Tok::TTrue;
        return expr;
      }
      auto expr = withSpan<hir::LiteralExpr>(it.span);
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
        break;
      }
      return expr;
    }

    Eptr visitVarExpr(VarExpr &it, hir::Pos pos) override {
      auto lookup = bindings.lookup(it.name.ident.value);
      if (lookup) {
        auto expr = withSpan<hir::VarExpr>(it.span);
        expr->ref = *lookup;
        return expr;
      } else {
        errs
          .err()
          .msg("Name '" + it.name.ident.value + "' is not defined")
          .span(it.span, "here");
        return withSpan<hir::DummyExpr>(it.span);
      }
    }

    Eptr visitBinaryExpr(BinaryExpr &it, hir::Pos pos) override {
      switch (it.terms.front().operatorToken.type) {
      case Tok::TEq:
      case Tok::TEq2:
      case Tok::TNe:
      case Tok::TGe:
      case Tok::TLt:
      case Tok::TLe:
      case Tok::TGt: {
        auto blockE = withSpan<hir::BlockExpr>(it.span);
        blockE->block.bindings.reserve(it.terms.size() + 1);
        auto addTemp = [&blockE, this](Expr &expr) {
          auto tmpIdx = defNo++;
          program.bindings[tmpIdx].source = expr.span;
          blockE->block.bindings.push_back(tmpIdx);
          auto defE = withSpan<hir::DefineExpr>(expr.span);
          defE->idx = tmpIdx;
          defE->value = visitExpr(expr, hir::Pos::Expr);
          return tmpIdx;
        };
        hir::DefIdx lastVar = addTemp(*it.lhs);
        loc::Span lhsSpan = it.lhs->span;
        loc::SrcLoc end = it.terms.back().expr->span.hi;
        for (auto &term : it.terms) {
          hir::DefIdx thisVar = addTemp(*term.expr);
          auto condE = withSpan<hir::CondExpr>(loc::Span(lhsSpan.lo, end));
          auto falseE = withSpan<hir::BoolExpr>(term.operatorToken.span());
          falseE->value = false;
          condE->elseE = std::move(falseE);
          hir::CmpExpr::Op cmp;
          switch (term.operatorToken.type) {
          case Tok::TEq:
          case Tok::TEq2:
            cmp = hir::CmpExpr::Op::Eq;
            break;
          case Tok::TNe:
            cmp = hir::CmpExpr::Op::Ne;
            break;
          case Tok::TGe:
            cmp = hir::CmpExpr::Op::Ge;
            break;
          case Tok::TLt:
            cmp = hir::CmpExpr::Op::Lt;
            break;
          case Tok::TLe:
            cmp = hir::CmpExpr::Op::Le;
            break;
          case Tok::TGt:
            cmp = hir::CmpExpr::Op::Gt;
            break;
          default:
            break; // unreachable
          }
          auto predE = withSpan<hir::CmpExpr>(loc::Span(lhsSpan.lo, term.expr->span.hi));
          auto lhsE = withSpan<hir::VarExpr>(lhsSpan);
          lhsE->ref = lastVar;
          predE->lhs = std::move(lhsE);
          auto rhsE = withSpan<hir::VarExpr>(term.expr->span);
          rhsE->ref = thisVar;
          predE->rhs = std::move(rhsE);
          predE->op = cmp;
          lastVar = thisVar;
        }
        return blockE;
      }
      case Tok::TAnd2:
      case Tok::TOr2: {
        std::unique_ptr<hir::Expr> retExpr = visitExpr(*it.lhs, hir::Pos::Expr);
        for (auto &term : it.terms) {
          // X && Y -> if X { Y } else { false }
          // X || Y -> if X { true } else { Y }
          auto logicExpr = withSpan<hir::CondExpr>(it.span);
          logicExpr->predE = std::move(retExpr);
          std::unique_ptr<hir::Expr> *cont;
          auto boolE = withSpan<hir::BoolExpr>(term.operatorToken.span());
          if (term.operatorToken.type == Tok::TAnd2) {
            cont = &logicExpr->thenE;
            boolE->value = false;
            logicExpr->elseE = std::move(boolE);
          } else {
            cont = &logicExpr->elseE;
            boolE->value = true;
            logicExpr->thenE = std::move(boolE);
          }
          *cont = visitExpr(*term.expr, hir::Pos::Expr);
          retExpr = std::move(logicExpr);
        }
        return retExpr;
      }
      default:
        break;
      }
      // all our operators are left-associative
      std::unique_ptr<hir::Expr> retExpr = visitExpr(*it.lhs, hir::Pos::Expr);
      for (auto &term : it.terms) {
        auto binExpr = withSpan<hir::BinExpr>(it.span);
        binExpr->lhs = std::move(retExpr);
        binExpr->rhs = visitExpr(*term.expr, hir::Pos::Expr);
        switch (term.operatorToken.type) {
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
          break; // unreachable
        }
        retExpr = std::move(binExpr);
      }
      return retExpr;
    }

    Eptr visitPrefixExpr(PrefixExpr &it, hir::Pos pos) override {
      auto retExpr = visitExpr(*it.expr, hir::Pos::Expr);
      for (auto iter = it.prefixes.rbegin(); iter != it.prefixes.rend(); ++iter) {
        if (iter->type == Tok::TSub) {
          auto expr = withSpan<hir::NegExpr>(it.span);
          expr->value = std::move(retExpr);
          retExpr = std::move(expr);
        }
      }
      return retExpr;
    }

    Eptr visitFunCallExpr(FunCallExpr &it, hir::Pos pos) override {
      auto expr = withSpan<hir::CallExpr>(it.span);
      expr->func = visitExpr(*it.function, hir::Pos::Expr);
      expr->args.reserve(it.arguments.size());
      for (auto &arg : it.arguments) {
        expr->args.push_back(visitExpr(*arg, hir::Pos::Expr));
      }
      return expr;
    }

    Eptr visitHintedExpr(HintedExpr &it, hir::Pos pos) override {
      auto expr = visitExpr(*it.expr, pos);
      expr->type = visitType(*it.hint.type);
      return expr;
    }
  };

  std::unique_ptr<ProgramVisitor<LowerResult>> lowerVisitor() {
    return std::make_unique<LoweringVisitor>();
  }
}
