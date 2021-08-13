#include "Lowering.h"
#include "../hir/Rebind.h"
#include "Ast.h"

#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <variant>

namespace ast::lower {
  template <typename Data=std::monostate>
  class Bindings {
    using Ridge = std::function<void(const hir::VarRef &)>;

    struct ScopeSet {
      class Binding {
      public:
        const std::string *name;
        loc::Span source;
        Data data;

        Binding(const std::string &name,
                loc::Span &&source,
                Data &&data):
          name(&name),
          source(source),
          data(std::forward<Data>(data)) {}
      };

      std::vector<Binding> data;
      std::map<std::string, hir::Idx> bound;

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

    const std::vector<typename ScopeSet::Binding> &datas() {
      return sets.back().data;
    }

    const typename ScopeSet::Binding &dataFor(const hir::VarRef &vr) {
      return (sets.rbegin() + vr.block)->data[vr.idx];
    }

    hir::Idx introduce(const std::string &name,
                       const loc::Span &source,
                       Data &&extraData) {
      auto &ss = sets.back();
      hir::Idx idx = ss.data.size();
      auto &nameS = ss.bound.insert({name, idx}).first->first;
      ss.data.emplace_back(nameS, loc::Span(source), std::forward<Data>(extraData));
      return idx;
    }

    std::optional<hir::VarRef> lookup(const std::string &name) {
      hir::VarRef ref;
      ref.block = 0;
      auto it = sets.rbegin();
      std::vector<std::pair<Ridge*, hir::Idx>> ridges;
      while (it != sets.rend()) {
        auto found = it->bound.find(name);
        if (found != it->bound.end()) {
          ref.idx = found->second;
          for (auto &r : ridges) {
            hir::VarRef rRef = ref;
            rRef.block -= r.second;
            (*r.first)(rRef);
          }
          return ref;
        }
        ++ref.block;
        if (it->ridge) {
          ridges.push_back({&*it->ridge, ref.block});
        }
        ++it;
      }
      return std::nullopt;
    }
  };

  typedef std::unique_ptr<hir::Expr> Eptr;

  class LoweringVisitor :
    public ProgramVisitor<LowerResult>,
    public TypeVisitor<hir::Type>,
    public ExprVisitor<Eptr, hir::Pos>,
    public StatementVisitor<Eptr, std::optional<hir::Idx>> {
  public:
    err::ErrorContext errs;
    hir::Program program;
    Bindings<std::shared_ptr<hir::Type>> bindings;
    Bindings<> typeBindings;

    void builtinType(const std::string &name) {
      typeBindings.introduce(name, loc::Span(), {});
      auto &import = program.typeImports.emplace_back();
      import.moduleIdx = 0;
      import.name = name;
    }

    LoweringVisitor() {
      builtinType("fn");
      builtinType("int");
      builtinType("float");
      typeBindings.push();
    }

    void addBindingsToBlock(hir::Block &block) {
      block.bindings.reserve(bindings.datas().size());
      for (auto &data : bindings.datas()) {
        auto &b = block.bindings.emplace_back();
        b.name = *data.name;
        b.source = data.source;
        b.type = std::move(*data.data);
      }
      block.typeBindings.reserve(typeBindings.datas().size());
      for (auto &data : typeBindings.datas()) {
        auto &b = block.typeBindings.emplace_back();
        b.name = *data.name;
        b.source = data.source;
      }
    }

    hir::Type visitHint(std::optional<TypeHint> &hint) {
      return hint ? visitType(*hint->type) : hir::Type();
    }

    hir::Idx introduceBinding(Binding &it) {
      return bindings.introduce(it.name.ident.value,
                                it.name.ident.span(),
                                it.arguments ?
                                std::make_shared<hir::Type>() :
                                std::make_shared<hir::Type>
                                (visitHint(it.typeHint)));
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
          ty.params.push_back(std::make_unique<hir::Type>(visitType(*t)));
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
                loc::Span &source,
                Expr &body) {
      std::set<hir::VarRef> closed;
      bindings.push([&closed](const hir::VarRef &vr) {
        closed.insert(vr);
      });
      typeBindings.push();
      if (typeParams) {
        for (auto &tp : typeParams->idents) {
          typeBindings.introduce(tp.ident.value, tp.ident.span(), {});
        }
      }

      for (auto &p : params) {
        bindings.introduce(p.name.ident.value,
                           p.name.ident.span(),
                           std::make_shared<hir::Type>
                           (visitHint(p.typeHint)));
      }

      auto blockE = std::make_unique<hir::BlockExpr>();
      // FIXME this actually gets dropped
      if (retHint) blockE->type = visitType(*retHint->type);

      hir::Block &block = blockE->block;
      addBindingsToBlock(block);
      block.body.push_back(visitExpr(body, hir::Pos::Expr));
      bindings.pop();
      typeBindings.pop();
      auto &thisB = block.bindings.emplace_back();
      thisB.name = "this";
      thisB.source = source;

      hir::Idx typeIdx = program.types.size();
      hir::ADT &closure = program.types.emplace_back();
      hir::ADT::Variant &variant = closure.variants.emplace_back();
      variant.values.reserve(closed.size());
      for (auto &cv : closed) {
        auto &field = variant.values.emplace_back();
        auto &data = bindings.dataFor(cv);
        field.name = *data.name;
        field.source = data.source;
      }

      auto expr = std::make_unique<hir::NewExpr>();
      expr->adt = typeIdx;
      expr->variant = 0;
      expr->values.reserve(closed.size());
      for (auto &cv : closed) {
        auto varE = std::make_unique<hir::VarExpr>();
        varE->ref = cv;
        expr->values.push_back(std::move(varE));
      }

      std::map<hir::VarRef, hir::Idx> mapping;
      hir::Idx closedIdx = 0;
      for (auto &cv : closed) {
        mapping[cv] = closedIdx++;
      }

      hir::rebind::rebindVisitor([&mapping, typeIdx]
                                 (hir::VarExpr &varE,
                                  hir::Idx depth) -> Eptr {
        auto ref = varE.ref;
        ref.block -= depth;
        auto found = mapping.find(ref);
        if (found != mapping.end()) {
          auto getE = std::make_unique<hir::GetExpr>();
          getE->adt = typeIdx;
          getE->variant = 0;
          getE->field = found->second;
          auto thisE = std::make_unique<hir::VarExpr>();
          thisE->ref.block = depth;
          thisE->ref.idx = mapping.size();
          getE->value = std::move(thisE);
          return getE;
        }
        return nullptr;
      })->visitExpr(*blockE, nullptr, 0);

      program.fnImpls.push_back(std::move(block));

      expr->span = source;

      return expr;
    }

    template <typename Param>
    Eptr recFnExpr(Identifier &name,
                   Binding::TypeArguments *typeParams,
                   TypeHint *retHint,
                   std::vector<Param> &params,
                   loc::Span &source,
                   Expr &body) {
      bindings.push();
      typeBindings.push();
      auto expr = std::make_unique<hir::BlockExpr>();

      auto idx = bindings.introduce(name.ident.value,
                                    name.ident.span(),
                                    std::make_shared<hir::Type>());
      expr->block.bindings.emplace_back().name = name.ident.value;

      auto defE = std::make_unique<hir::DefineExpr>();
      defE->idx = idx;
      defE->value = fnExpr(typeParams, retHint, params, source, body);
      expr->block.body.push_back(std::move(defE));

      auto varE = std::make_unique<hir::VarExpr>();
      varE->ref.block = 0;
      varE->ref.idx = idx;
      expr->block.body.push_back(std::move(varE));

      expr->span = source;

      bindings.pop();
      typeBindings.pop();
      return expr;
    }

    Eptr visitBindingExpr(Binding &it, hir::Idx idx) {
      auto retExpr = std::make_unique<hir::DefineExpr>();
      retExpr->idx = idx;
      if (it.foreignToken) {
        auto fe = std::make_unique<hir::ForeignExpr>();
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
      typeBindings.push();
      for (auto &b : it.bindings) {
        introduceBinding(b);
      }

      auto blockE = std::make_unique<hir::BlockExpr>();
      auto &block = blockE->block;
      addBindingsToBlock(block);

      hir::Idx idx = 0;
      for (auto &b : it.bindings) {
        block.body.push_back(visitBindingExpr(b, idx++));
      }

      if (it.name) {
        auto call = std::make_unique<hir::CallExpr>();
        call->func = recFnExpr(*it.name, nullptr, nullptr, it.bindings, it.span, *it.body);
        call->args.reserve(it.bindings.size());
        idx = 0;
        for (auto &b : it.bindings) {
          auto var = std::make_unique<hir::VarExpr>();
          var->ref.block = 0;
          var->ref.idx = idx;
          var->span = b.span;
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
        return std::make_unique<hir::VoidExpr>();
      }

      bindings.push();
      typeBindings.push();

      std::vector<std::optional<hir::Idx>> indeces;
      indeces.reserve(it.statements.size());
      for (auto &stmt : it.statements) {
        indeces.push_back(BindingVisitor(*this).visitStatement(*stmt.statement));
      }

      auto expr = std::make_unique<hir::BlockExpr>();
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
        expr->ref.block = lookup->block;
        expr->ref.idx = lookup->idx;
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
      switch (it.terms.front().operatorToken.type) {
      case Tok::TEq:
      case Tok::TEq2:
      case Tok::TNe:
      case Tok::TGe:
      case Tok::TLt:
      case Tok::TLe:
      case Tok::TGt: {
        auto expr = std::make_unique<hir::CmpExpr>();
        expr->exprs.push_back(visitExpr(*it.lhs, hir::Pos::Expr));
        for (auto &term : it.terms) {
          switch (term.operatorToken.type) {
          case Tok::TEq:
          case Tok::TEq2:
            expr->ops.push_back(hir::CmpExpr::Op::Eq);
            break;
          case Tok::TNe:
            expr->ops.push_back(hir::CmpExpr::Op::Ne);
            break;
          case Tok::TGe:
            expr->ops.push_back(hir::CmpExpr::Op::Ge);
            break;
          case Tok::TLt:
            expr->ops.push_back(hir::CmpExpr::Op::Lt);
            break;
          case Tok::TLe:
            expr->ops.push_back(hir::CmpExpr::Op::Le);
            break;
          case Tok::TGt:
            expr->ops.push_back(hir::CmpExpr::Op::Gt);
            break;
          default:
            break; // unreachable
          }
          expr->exprs.push_back(visitExpr(*term.expr, hir::Pos::Expr));
        }
        return expr;
      }
      default:
        break;
      }
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
      auto expr = visitExpr(*it.expr, pos);
      expr->type = visitType(*it.hint.type);
      return expr;
    }
  };

  std::unique_ptr<ProgramVisitor<LowerResult>> lowerVisitor() {
    return std::make_unique<LoweringVisitor>();
  }
}
