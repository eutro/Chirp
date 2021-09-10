#include "Lowering.h"
#include "../hir/Rebind.h"
#include "../hir/Builtins.h"
#include "../../common/Util.h"

#include <sstream>

namespace ast::lower {
  using DefType = hir::DefType;

  class Bindings {
    using Ridge = std::function<void(hir::DefIdx)>;

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

    Idx introduce(const std::string &name, hir::DefIdx idx) {
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
  std::unique_ptr<T> withSpan(std::optional<loc::Span> span) {
    auto tptr = std::make_unique<T>();
    tptr->span = span;
    return tptr;
  }

  class LoweringVisitor :
    public ProgramVisitor<LowerResult>,
    public TypeVisitor<hir::Type>,
    public ExprVisitor<Eptr, hir::Pos>,
    public StatementVisitor<Eptr, std::optional<Idx>> {
  public:
    err::ErrorContext errs;
    hir::Program program;
    Bindings bindings;
    Bindings typeBindings;
    hir::DefIdx defNo = 0;

    hir::DefIdx introduceDef(hir::Definition &&def) {
      hir::DefIdx id = defNo++;
      program.bindings[id] = std::forward<hir::Definition>(def);
      return id;
    }

    hir::DefIdx introduceDef(Bindings &b, hir::Definition &&def) {
      auto id = introduceDef(std::forward<hir::Definition>(def));
      b.introduce(program.bindings.at(id).name, id);
      return id;
    }

    hir::DefIdx builtinType(const std::string &name, DefType type) {
      return introduceDef(typeBindings, hir::Definition{
          name,
          std::nullopt,
          std::move(type)
        });
    }

    LoweringVisitor() {
      builtinType("Fn", DefType::Trait{});
      builtinType("Add", DefType::Trait{});
      builtinType("Sub", DefType::Trait{});
      builtinType("Mul", DefType::Trait{});
      builtinType("Div", DefType::Trait{});
      builtinType("Rem", DefType::Trait{});
      builtinType("BitOr", DefType::Trait{});
      builtinType("BitAnd", DefType::Trait{});
      builtinType("Cmp", DefType::Trait{});
      builtinType("Neg", DefType::Trait{});

      builtinType("bool", DefType::Type{});

      builtinType("i8", DefType::Type{});
      builtinType("i16", DefType::Type{});
      builtinType("i32", DefType::Type{});
      builtinType("i64", DefType::Type{});

      builtinType("u8", DefType::Type{});
      builtinType("u16", DefType::Type{});
      builtinType("u32", DefType::Type{});
      builtinType("u64", DefType::Type{});

      builtinType("f16", DefType::Type{});
      builtinType("f32", DefType::Type{});
      builtinType("f64", DefType::Type{});

      builtinType("tuple", DefType::Type{});
      builtinType("ffifn", DefType::Type{});

      typeBindings.push();
    }

    void addBindingsToBlock(hir::Block &block) {
      block.bindings = bindings.defs();
    }

    hir::Type visitHint(std::optional<TypeHint> &hint) {
      return hint ? visitType(*hint->type) : hir::Type();
    }

    hir::DefIdx introduceBinding(Binding &it) {
      return introduceDef(bindings, hir::Definition{
          it.name.ident.value,
          it.name.ident.span(),
            DefType::Variable{{}},
        });
    }

    class BindingVisitor :
      public StatementVisitor<std::optional<Idx>> {
    private:
      LoweringVisitor &lv;
    public:
      BindingVisitor(LoweringVisitor &lv): lv(lv) {}

      std::optional<Idx> visitDefn(Defn &it) override {
        return lv.introduceBinding(it.binding);
      }

      std::optional<Idx> visitExpr(Expr &it) override {
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

    hir::Type visitTupleType(TupleType &it) override {
      hir::Type ty;
      ty.base = hir::TUPLE;
      ty.params.reserve(it.types.size());
      for (auto &t : it.types) {
        ty.params.push_back(visitType(*t));
      }
      return ty;
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
      bindings.push([&closed](hir::DefIdx vr) {
        closed.insert(vr);
      });
      typeBindings.push();
      if (typeParams) {
        for (auto &tp : typeParams->idents) {
          introduceDef(typeBindings, hir::Definition{
              tp.ident.value,
              tp.ident.span(),
                DefType::Type{},
            });
        }
      }

      Idx arity = params.size();

      for (auto &p : params) {
        introduceDef(bindings, hir::Definition{
            p.name.ident.value,
            p.name.ident.span(),
            DefType::Variable{{visitHint(p.typeHint)}},
          });
      }

      auto blockE = withSpan<hir::BlockExpr>(std::nullopt);

      hir::Block &block = blockE->block;
      addBindingsToBlock(block);
      auto bodyE = visitExpr(body, hir::Pos::Expr);
      if (retHint) {
        bodyE->type.push_back(visitType(*retHint->type));
      }
      block.body.push_back(std::move(bodyE));
      bindings.pop();
      typeBindings.pop();

      Idx typeIdx = introduceDef(hir::Definition{
          "",
          source,
            DefType::ADT{},
        });
      DefType::ADT &closure = std::get<DefType::ADT>(program.bindings.at(typeIdx).defType.v);

      DefType::ADT::Variant &variant = closure.variants.emplace_back();
      variant.values.reserve(closed.size());
      for (auto &cv : closed) {
        auto &data = program.bindings.at(cv);
        variant.values.push_back(introduceDef(hir::Definition{
              data.name,
              data.source,
                DefType::Variable{},
            }));
      }

      hir::TraitImpl &fnImpl = program.traitImpls.emplace_back();
      fnImpl.source = source;
      fnImpl.params.reserve(closed.size() + arity);

      hir::Type &adtType = fnImpl.type;
      adtType.base = typeIdx;
      adtType.params.reserve(closed.size());
      for (auto &cv : closed) {
        Idx paramIdx = introduceDef(hir::Definition{
            "",
              program.bindings.at(cv).source,
              DefType::Type{}
          });
        adtType.params.emplace_back().base = paramIdx;
        fnImpl.params.push_back(paramIdx);
      }

      hir::Type fnArgsTy;
      fnArgsTy.base = hir::TUPLE;
      fnArgsTy.params.reserve(params.size());
      {
        Idx i = 0;
        for (Param &param : params) {
          Idx paramIdx = introduceDef(hir::Definition{
              "",
                param.name.ident.span(),
                DefType::Type{},
                });
          fnArgsTy.params.emplace_back().base = paramIdx;
          std::get<DefType::Variable>(program.bindings.at(block.bindings[i++]).defType.v)
                     .hints.emplace_back().base = paramIdx;
          fnImpl.params.push_back(paramIdx);
        }
      }
      hir::Type &fnType = fnImpl.trait;
      fnType.base = hir::Fn;
      Idx retIdx = introduceDef(hir::Definition{
          "",
            body.span,
            DefType::Type{}
        });
      fnImpl.params.push_back(retIdx);
      block.body.back()->type.emplace_back().base = retIdx;
      hir::Type retTy;
      retTy.base = retIdx;
      fnType.params = {fnArgsTy, retTy};

      auto expr = withSpan<hir::NewExpr>(std::nullopt);
      expr->adt = typeIdx;
      expr->variant = 0;
      expr->values.reserve(closed.size());
      for (auto &cv : closed) {
        auto varE = withSpan<hir::VarExpr>(std::nullopt);
        varE->ref = cv;
        expr->values.push_back(std::move(varE));
      }

      std::map<hir::DefIdx, Idx> mapping;
      Idx closedIdx = 0;
      for (auto &cv : closed) {
        mapping[cv] = closedIdx++;
      }

      auto thisIdx = introduceDef(hir::Definition{
          "this",
          source,
          DefType::Variable{{adtType}},
        });
      block.bindings.push_back(thisIdx);

      hir::rebind::rebindVisitor([&mapping, typeIdx, thisIdx]
                                 (hir::VarExpr &varE) -> Eptr {
        auto ref = varE.ref;
        auto found = mapping.find(ref);
        if (found != mapping.end()) {
          auto getE = withSpan<hir::GetExpr>(varE.span);
          getE->adt = typeIdx;
          getE->variant = 0;
          getE->field = found->second;
          auto thisE = withSpan<hir::VarExpr>(std::nullopt);
          thisE->ref = thisIdx;
          getE->value = std::move(thisE);
          return getE;
        }
        return nullptr;
      })->visitExpr(*blockE, nullptr);

      fnImpl.methods.push_back(std::move(block));

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
      auto expr = withSpan<hir::BlockExpr>(std::nullopt);

      auto idx = introduceDef(bindings, hir::Definition{
          name.ident.value,
          name.ident.span(),
            DefType::Variable{},
        });
      expr->block.bindings.push_back(idx);
      auto defE = withSpan<hir::DefineExpr>(std::nullopt);
      defE->idx = idx;
      defE->value = fnExpr(typeParams, retHint, params, source, body);
      expr->block.body.push_back(std::move(defE));

      auto varE = withSpan<hir::VarExpr>(std::nullopt);
      varE->ref = idx;
      expr->block.body.push_back(std::move(varE));

      bindings.pop();
      typeBindings.pop();
      return expr;
    }

    Eptr visitBindingExpr(Binding &it, Idx idx, std::optional<loc::Span> span) {
      auto retExpr = withSpan<hir::DefineExpr>(span);
      retExpr->idx = idx;
      if (it.foreignToken) {
        auto fe = withSpan<hir::ForeignExpr>(it.foreignToken->span());
        fe->name = it.name.ident.value;
        if (it.arguments) {
          hir::Type &fType = fe->type.emplace_back();
          fType.base = hir::FFIFN;
          fType.params.reserve(2);
          hir::Type &argTypes = fType.params.emplace_back();
          argTypes.base = hir::TUPLE;
          argTypes.params.reserve(it.arguments->bindings.size());
          for (auto &rb : it.arguments->bindings) {
            argTypes.params.push_back(visitHint(rb.typeHint));
          }
          fType.params.push_back(visitHint(it.typeHint));
        }
        retExpr->value = std::move(fe);
      } else if (it.arguments) {
        // not recFnExpr because it's already captured
        retExpr->value = fnExpr(maybePtr(it.arguments->typeArguments),
                                maybePtr(it.typeHint),
                                it.arguments->bindings,
                                it.span,
                                *it.value);
      } else {
        retExpr->value = visitExpr(*it.value, hir::Pos::Expr);
        if (it.typeHint) {
          retExpr->type.push_back(visitHint(it.typeHint));
        }
      }
      return retExpr;
    }

    LowerResult visitProgram(Program &it) override {
      std::vector<std::optional<Idx>> indeces;
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
      body.push_back(withSpan<hir::VoidExpr>(std::nullopt));
      LowerResult res;
      res.program = std::move(program);
      res.errors = std::move(errs);
      return res;
    }

    Eptr visitDefn(Defn &it, std::optional<Idx> idx) override {
      return visitBindingExpr(it.binding, *idx, it.span);
    }

    Eptr visitExpr(Expr &it, std::optional<Idx>) override {
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
      std::vector<hir::DefIdx> ids;
      for (auto &b : it.bindings) {
        ids.push_back(introduceBinding(b));
      }

      auto blockE = withSpan<hir::BlockExpr>(it.span);
      auto &block = blockE->block;
      addBindingsToBlock(block);

      Idx idx = 0;
      for (auto &b : it.bindings) {
        block.body.push_back(visitBindingExpr(b, ids[idx++], std::nullopt));
      }

      if (it.name) {
        auto call = withSpan<hir::CallExpr>(it.span);
        call->func = recFnExpr(*it.name, nullptr, nullptr, it.bindings, it.span, *it.body);
        call->args.reserve(it.bindings.size());
        auto refs = bindings.defs().begin();
        for (auto &b : it.bindings) {
          auto var = withSpan<hir::VarExpr>(std::nullopt);
          var->ref = *refs++;
          call->args.push_back(std::move(var));
        }
        block.body.push_back(std::move(call));
      } else {
        block.body.push_back(visitExpr(*it.body, pos));
      }
      bindings.pop();
      typeBindings.pop();
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

      std::vector<std::optional<Idx>> indeces;
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
      if (it.value.type == Tok::TTrue ||
          it.value.type == Tok::TFalse) {
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
      default: throw 0;
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

        auto addTemp = [&blockE, this](hir::BlockExpr &bE, Expr &expr)
          -> hir::DefIdx {
          hir::DefIdx tmpIdx = introduceDef(hir::Definition{
              "",
                expr.span,
                DefType::Variable{},
                });
          blockE->block.bindings.push_back(tmpIdx);
          auto defE = withSpan<hir::DefineExpr>(expr.span);
          defE->idx = tmpIdx;
          defE->value = visitExpr(expr, hir::Pos::Expr);
          bE.block.body.push_back(std::move(defE));
          return tmpIdx;
        };

        std::function<Eptr(Idx, hir::DefIdx)> addRhs =
          [&](Idx i, hir::DefIdx lastVar) -> Eptr {
            auto &term = it.terms.at(i);
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
            default: throw 0;
            }

            auto predE = withSpan<hir::BlockExpr>(std::nullopt);
            hir::DefIdx thisVar = addTemp(*predE, *term.expr);
            {
              auto cmpE = withSpan<hir::CmpExpr>(std::nullopt);
              {
                auto lhsE = withSpan<hir::VarExpr>(std::nullopt);
                lhsE->ref = lastVar;
                cmpE->lhs = std::move(lhsE);
                auto rhsE = withSpan<hir::VarExpr>(std::nullopt);
                rhsE->ref = thisVar;
                cmpE->rhs = std::move(rhsE);
                cmpE->op = cmp;
              }
              predE->block.body.push_back(std::move(cmpE));
            }
            if (i + 1 >= it.terms.size()) {
              return predE;
            } else {
              auto condE = withSpan<hir::CondExpr>(std::nullopt);
              auto falseE = withSpan<hir::BoolExpr>(std::nullopt);
              falseE->value = false;
              condE->predE = std::move(predE);
              condE->thenE = addRhs(i + 1, thisVar);
              condE->elseE = std::move(falseE);
              return condE;
            }
          };
        hir::DefIdx lhsVar = addTemp(*blockE, *it.lhs);
        blockE->block.body.push_back(addRhs(0, lhsVar));
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
          auto boolE = withSpan<hir::BoolExpr>(std::nullopt);
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
      expr->type.push_back(visitType(*it.hint.type));
      return expr;
    }
  };

  std::unique_ptr<ProgramVisitor<LowerResult>> lowerVisitor() {
    return std::make_unique<LoweringVisitor>();
  }
}
