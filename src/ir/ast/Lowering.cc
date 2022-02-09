#include "Lowering.h"
#include "../hir/Rebind.h"
#include "../hir/Builtins.h"
#include "../../common/Util.h"

#include <sstream>
#include <numeric>

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
    public TypeVisitor<hir::Type, std::vector<Idx> *, Idx *>,
    public ExprVisitor<Eptr, hir::Pos>,
    public StatementVisitor<Eptr, std::vector<Idx> &, Idx &> {
  public:
    err::ErrorContext errs;
    hir::Program program;
    Bindings bindings;
    Bindings typeBindings;
    hir::DefIdx defNo = 0;
    Idx blockIdx = 0;

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

    void addBuiltins() {
      builtinType("Fn", DefType::Trait{});
      builtinType("Add", DefType::Trait{});
      builtinType("Sub", DefType::Trait{});
      builtinType("Mul", DefType::Trait{});
      builtinType("Div", DefType::Trait{});
      builtinType("Rem", DefType::Trait{});
      builtinType("BitOr", DefType::Trait{});
      builtinType("BitAnd", DefType::Trait{});
      builtinType("Eq", DefType::Trait{});
      builtinType("Cmp", DefType::Trait{});
      builtinType("Neg", DefType::Trait{});

      builtinType("bool", DefType::Type{0,0});

      builtinType("i8", DefType::Type{0,0});
      builtinType("i16", DefType::Type{0,0});
      builtinType("i32", DefType::Type{0,0});
      builtinType("i64", DefType::Type{0,0});
      builtinType("i128", DefType::Type{0,0});

      builtinType("u8", DefType::Type{0,0});
      builtinType("u16", DefType::Type{0,0});
      builtinType("u32", DefType::Type{0,0});
      builtinType("u64", DefType::Type{0,0});
      builtinType("u128", DefType::Type{0,0});

      builtinType("f16", DefType::Type{0,0});
      builtinType("f32", DefType::Type{0,0});
      builtinType("f64", DefType::Type{0,0});

      builtinType("tuple", DefType::Type{0,std::nullopt});
      builtinType("union", DefType::Type{0,std::nullopt});
      builtinType("ffifn", DefType::Type{2,2});
      builtinType("string", DefType::Type{0,0});
      builtinType("cstr", DefType::Type{0,0});
      builtinType("type", DefType::Type{1,1});
    }

    LoweringVisitor() {
      addBuiltins();
      typeBindings.push();
    }

    void addBindingsToBlock(hir::Block &block) {
      block.bindings = bindings.defs();
      for (Idx def : typeBindings.defs()) {
        if (std::holds_alternative<DefType::TypeVar>(program.bindings.at(def).defType.v)) {
          block.typeBindings.push_back(def);
        }
      }
    }

    hir::Type visitHint(std::optional<TypeHint> &hint) {
      return hint ?
             visitType(*hint->type, nullptr, nullptr) :
             hir::Type();
    }

    hir::DefIdx introduceBinding(Binding &it) {
      return introduceDef(bindings, hir::Definition{
          it.name.ident.value,
          it.name.ident.span(),
          DefType::Variable{},
        });
    }

    class BindingVisitor :
      public StatementVisitor<std::monostate, std::vector<Idx>&, bool>,
      public TypeVisitor<std::monostate, std::vector<Idx>&> {
    private:
      LoweringVisitor &lv;
    public:
      BindingVisitor(LoweringVisitor &lv): lv(lv) {}

      std::monostate visitDefn(Defn &it, std::vector<Idx> &idcs, bool topLevel) override {
        auto idx = lv.introduceBinding(it.binding);
        idcs.push_back(idx);
        return {};
      }

      std::monostate visitExpr(Expr &it, std::vector<Idx> &idcs, bool topLevel) override {
        return {};
      }

      std::monostate visitTypeDefn(TypeDefn &it, std::vector<Idx> &idcs, bool topLevel) override {
        idcs.push_back(lv.introduceDef(lv.typeBindings, hir::Definition{
            it.name.ident.value,
            it.name.ident.span(),
            DefType::Type{0,0}
        }));
        if (std::holds_alternative<TypeDefn::ADT>(it.val)) {
          lv.introduceDef(hir::Definition{
              it.name.ident.value,
              it.name.ident.span(),
              DefType::ADT{} // to populate later
          });
          auto &adt = std::get<TypeDefn::ADT>(it.val);
          for (auto &ty : adt.types) {
            if (ty.accessor) {
              if (auto accessor = lv.bindings.lookup(ty.accessor->ident.value)) {
                idcs.push_back(*accessor - 1);
              } else {
                idcs.push_back(lv.introduceDef(hir::Definition {
                    ty.accessor->ident.value,
                    ty.accessor->ident.span(),
                    DefType::ADT{}
                }));
                lv.introduceDef(lv.bindings, hir::Definition{
                    ty.accessor->ident.value,
                    ty.accessor->ident.span(),
                    DefType::Variable{}
                });
              }
            }
            visitType(*ty.ty, idcs);
          }
        } else {
          auto &alias = std::get<TypeDefn::Alias>(it.val);
          visitType(*alias.type, idcs);
        }
        return {};
      }

      std::monostate visitTraitImpl(TraitImpl &it, std::vector<Idx> &idcs, bool topLevel) override {
        return {};
      }

      std::monostate visitNamedType(NamedType &it, std::vector<Idx> &idcs) override {
        if (it.parameters) {
          for (auto &p : it.parameters->types) {
            visitType(*p, idcs);
          }
        }
        return {};
      }
      std::monostate visitPlaceholderType(PlaceholderType &it, std::vector<Idx> &idcs) override {
        return {};
      }
      std::monostate visitTupleType(TupleType &it, std::vector<Idx> &idcs) override {
        for (auto &p : it.types) {
          visitType(*p, idcs);
        }
        return {};
      }
      std::monostate visitUnionType(UnionType &it, std::vector<Idx> &idcs) override {
        for (auto &p : it.types) {
          visitType(*p, idcs);
        }
        return {};
      }
      std::monostate visitTypeDefn(TypeDefn &it, std::vector<Idx> &idcs) override {
        visitTypeDefn(it, idcs, false);
        return {};
      }
    };

    hir::Type visitType(Type &it, std::vector<Idx> *idcs, Idx *idx) override {
      hir::Type ty = TypeVisitor::visitType(it, idcs, idx);
      ty.source = it.span;
      if (ty.base) {
        auto &def = program.bindings.at(*ty.base);
        if (std::holds_alternative<DefType::Type>(def.defType.v)) {
          auto &typeDef = std::get<DefType::Type>(def.defType.v);
          if (ty.params.size() < typeDef.minArity || (typeDef.maxArity && ty.params.size() > *typeDef.maxArity)) {
            errs
                .err()
                .span(it.span, util::toStr("Type ", def.name, " called with the wrong number of arguments"))
                .msg(typeDef.minArity == typeDef.maxArity ? util::toStr("  expected: ", typeDef.minArity) :
                     typeDef.maxArity ? util::toStr("  expected between: ", typeDef.minArity, " and ", *typeDef.maxArity) :
                     util::toStr("  expected at least: ", typeDef.minArity))
                .msg(util::toStr("  got: ", ty.params.size()));
          }
        } else if (std::holds_alternative<DefType::TypeVar>(def.defType.v)) {
          if (!ty.params.empty()) {
            errs.err().span(it.span, util::toStr("Type ", def.name, " is a type variable and does not take arguments"));
          }
        }
      }
      return ty;
    }

    hir::Type visitNamedType(NamedType &it, std::vector<Idx> *idcs, Idx *idx) override {
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
          ty.params.push_back(visitType(*t, idcs, idx));
        }
      }
      return ty;
    }

    hir::Type visitPlaceholderType(PlaceholderType &it, std::vector<Idx> *idcs, Idx *idx) override {
      return hir::Type();
    }

    hir::Type visitTupleType(TupleType &it, std::vector<Idx> *idcs, Idx *idx) override {
      hir::Type ty;
      ty.base = hir::TUPLE;
      ty.params.reserve(it.types.size());
      for (auto &t : it.types) {
        ty.params.push_back(visitType(*t, idcs, idx));
      }
      return ty;
    }

    hir::Type visitUnionType(UnionType &it, std::vector<Idx> *idcs, Idx *idx) override {
      hir::Type ty;
      ty.base = hir::UNION;
      ty.params.reserve(it.types.size());
      for (auto &t : it.types) {
        ty.params.push_back(visitType(*t, idcs, idx));
      }
      return ty;
    }

    hir::Type visitTypeDefn(TypeDefn &it, std::vector<Idx> *idcs, Idx *idx) override {
      hir::Type ty;
      BindingVisitor bv(*this);
      if (!idcs) {
        std::vector<Idx> idcsO;
        bv.visitTypeDefn(it, idcsO, false);
        idcs = &idcsO;
        Idx i = 0;
        idx = &i;
      }
      ty.base = idcs->at(*idx);
      visitStatement(it, *idcs, *idx);
      return ty;
    }

    template <typename Opt>
    auto maybePtr(Opt &it) {
      return it ? &*it : nullptr;
    }

    template <typename Param>
    Eptr fnExpr(const std::string &name,
                TypeParams *typeParams,
                TypeHint *retHint,
                std::vector<Param> &params,
                const loc::Span &source,
                Expr &body) {
      // std::set<hir::DefIdx> globalRefs;
      std::set<hir::DefIdx> closed;
      std::set<hir::DefIdx> closedTypes;
      bindings.push([&](hir::DefIdx vr) {
        // if (std::get<DefType::Variable>(program.bindings.at(vr).defType.v).global) {
        //   globalRefs.insert(vr);
        // } else {
          closed.insert(vr);
        // }
      });
      typeBindings.push([&](hir::DefIdx vr) {
        if (std::holds_alternative<DefType::TypeVar>(program.bindings.at(vr).defType.v)) {
          closedTypes.insert(vr);
        }
      });
      if (typeParams) {
        for (auto &tp : typeParams->idents) {
          introduceDef(typeBindings, hir::Definition{
              tp.ident.value,
              tp.ident.span(),
                DefType::TypeVar{},
            });
        }
      }

      std::vector<hir::Type> paramTys;
      paramTys.reserve(params.size());
      for (auto &p : params) {
        introduceDef(bindings, hir::Definition{
            p.name.ident.value,
            p.name.ident.span(),
            DefType::Variable{},
          });
        paramTys.push_back(visitHint(p.typeHint));
      }

      auto blockE = withSpan<hir::BlockExpr>(std::nullopt);

      hir::Block &block = blockE->block;
      block.span = source;
      addBindingsToBlock(block);
      auto bodyE = visitExpr(body, hir::Pos::Expr);
      if (retHint) {
        bodyE->hints.push_back(visitType(*retHint->type, nullptr, nullptr));
      }
      block.body.push_back(std::move(bodyE));
      bindings.pop();
      typeBindings.pop();

      std::string tyName = name;
      bool upcaseNext = true;
      for (auto it = tyName.begin(); it != tyName.end();) {
        if (upcaseNext) {
          *it = (char) std::toupper(*it);
          upcaseNext = false;
        }
        if (*it == '_') {
          upcaseNext = true;
          it = tyName.erase(it);
        } else {
          ++it;
        }
      }
      Idx typeIdx = introduceDef(hir::Definition{
          std::move(tyName),
          source,
            DefType::ADT{},
        });
      auto &closure = std::get<DefType::ADT>(program.bindings.at(typeIdx).defType.v);

      closure.fields.reserve(closed.size());
      for (auto &ct : closedTypes) {
        auto &data = program.bindings.at(ct);
        Idx paramIdx = introduceDef(hir::Definition{
            data.name,
            data.source,
            DefType::TypeVar{}
        });
        closure.params.push_back(paramIdx);
      }
      for (auto &cv : closed) {
        auto &data = program.bindings.at(cv);
        Idx paramIdx = introduceDef(hir::Definition{
            data.name,
            data.source,
            DefType::TypeVar{}
        });
        closure.params.push_back(paramIdx);
        hir::Type ty;
        ty.base = paramIdx;
        ty.source = data.source;
        closure.fields.push_back(std::move(ty));
      }

      hir::TraitImpl &fnImpl = program.traitImpls.emplace_back();
      fnImpl.source = source;

      hir::Type &adtType = fnImpl.type;
      adtType.base = typeIdx;
      adtType.params.reserve(closure.params.size());
      for (auto &p : closure.params) {
        hir::Definition &def = program.bindings.at(p);
        Idx paramIdx = introduceDef(hir::Definition{
            def.name,
            def.source,
            DefType::TypeVar{}
        });
        hir::Type &param = adtType.params.emplace_back();
        param.base = paramIdx;
        param.source = def.source;
      }

      hir::Type fnArgsTy;
      fnArgsTy.base = hir::TUPLE;
      fnArgsTy.params.reserve(params.size());
      {
        Idx i = 0;
        for (Param &param : params) {
          auto &hint = paramTys.at(i);
          auto &var = std::get<DefType::Variable>(program.bindings.at(block.bindings[i]).defType.v);
          if (hint.base) {
            fnArgsTy.params.push_back(hint);
            var.hints.push_back(hint);
          } else {
            Idx paramIdx = introduceDef(hir::Definition{
                param.name.ident.value,
                param.name.ident.span(),
                DefType::TypeVar{},
            });
            fnArgsTy.params.emplace_back().base = paramIdx;
            var.hints.emplace_back().base = paramIdx;
          }
          i++;
        }
      }
      hir::Type &fnType = fnImpl.trait;
      fnType.base = hir::Fn;
      Idx retIdx = introduceDef(hir::Definition{
          "ret",
            body.span,
            DefType::TypeVar{}
        });
      block.body.back()->hints.emplace_back().base = retIdx;

      fnType.params = {fnArgsTy};
      hir::Type retTy;
      retTy.base = retIdx;
      fnImpl.types.push_back(retTy);

      auto expr = withSpan<hir::NewExpr>(source);
      expr->adt = typeIdx;
      expr->types.reserve(closure.params.size());
      for (auto &ct : closedTypes) {
        hir::Type &ty = expr->types.emplace_back();
        ty.base = ct;
      }
      expr->values.reserve(closed.size());
      for (auto &cv : closed) {
        auto &def = program.bindings.at(cv);
        auto varE = withSpan<hir::VarExpr>(std::nullopt);
        varE->ref = cv;
        Idx extractedTy = introduceDef(hir::Definition{
            def.name,
            def.source,
            DefType::TypeVar{}
        });
        hir::Type &hint = varE->hints.emplace_back();
        hint.base = extractedTy;
        hint.source = def.source;
        expr->types.push_back(hint);
        expr->values.push_back(std::move(varE));
      }

      std::map<hir::DefIdx, Idx> mapping;
      std::map<hir::DefIdx, Idx> typeMapping;
      Idx closedIdx = 0;
      for (auto &cv : closed) {
        mapping[cv] = closedIdx++;
      }
      closedIdx = 0;
      for (auto &tv : closedTypes) {
        typeMapping[tv] = *adtType.params.at(closedIdx++).base;
      }

      auto thisIdx = introduceDef(hir::Definition{
          name,
          source,
          DefType::Variable{{adtType}},
        });
      block.bindings.push_back(thisIdx);

      hir::rebind::rebindVisitor(
          [&mapping, &typeMapping, typeIdx, thisIdx]
              (hir::VarExpr &varE) -> Eptr {
            auto ref = varE.ref;
            auto found = mapping.find(ref);
            if (found != mapping.end()) {
              auto getE = withSpan<hir::GetExpr>(varE.span);
              getE->adt = typeIdx;
              getE->field = found->second;
              auto thisE = withSpan<hir::VarExpr>(std::nullopt);
              thisE->ref = thisIdx;
              getE->value = std::move(thisE);

              getE->span = varE.span;
              getE->pos = varE.pos;
              getE->hints = std::move(varE.hints);
              return getE;
            }
            auto oFound = typeMapping.find(ref);
            if (oFound != typeMapping.end()) {
              varE.ref = oFound->second;
            }
            return nullptr;
          },
          [&typeMapping](hir::Type &ty) {
            auto found = typeMapping.find(*ty.base);
            if (found != typeMapping.end()) {
              ty.base = found->second;
            }
          }
      )->visitExpr(*blockE, nullptr);

      block.idx = blockIdx++;
      fnImpl.methods.push_back(std::move(block));

      return expr;
    }

    template <typename Param>
    Eptr recFnExpr(Identifier &name,
                   TypeParams *typeParams,
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
      defE->value = fnExpr(name.ident.value, typeParams, retHint, params, source, body);
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
        hir::Type &fType = fe->hints.emplace_back();
        if (it.arguments) {
          fType.base = hir::FFIFN;
          fType.params.reserve(2);
          hir::Type &argTypes = fType.params.emplace_back();
          argTypes.base = hir::TUPLE;
          argTypes.params.reserve(it.arguments->bindings.size());
          for (auto &rb : it.arguments->bindings) {
            argTypes.params.push_back(visitHint(rb.typeHint));
          }
          fType.params.push_back(visitHint(it.typeHint));
        } else {
          fType = visitHint(it.typeHint);
        }
        retExpr->value = std::move(fe);
      } else if (it.arguments) {
        // not recFnExpr because it's already captured
        retExpr->value = fnExpr(it.name.ident.value,
                                maybePtr(it.arguments->typeParams),
                                maybePtr(it.typeHint),
                                it.arguments->bindings,
                                it.span,
                                *it.value);
      } else {
        retExpr->value = visitExpr(*it.value, hir::Pos::Expr);
        if (it.typeHint) {
          retExpr->hints.push_back(visitHint(it.typeHint));
        }
      }
      return retExpr;
    }

    LowerResult visitProgram(Program &it) override {
      std::vector<std::vector<Idx>> indeces;
      indeces.reserve(it.statements.size());
      BindingVisitor bv(*this);
      for (auto &stmt : it.statements) {
        bv.visitStatement(*stmt, indeces.emplace_back(), true);
      }

      addBindingsToBlock(program.topLevel);
      program.topLevel.idx = blockIdx++;
      auto &body = program.topLevel.body;
      auto iter = indeces.begin();
      for (auto &stmt : it.statements) {
        Idx iidx = 0;
        body.push_back(visitStatement(*stmt, *iter++, iidx));
      }
      body.push_back(withSpan<hir::VoidExpr>(std::nullopt));
      LowerResult res;
      res.program = std::move(program);
      res.errors = std::move(errs);
      return res;
    }

    Eptr visitDefn(Defn &it, std::vector<Idx> &idcs, Idx &idx) override {
      return visitBindingExpr(it.binding, idcs.at(idx), it.span);
    }

    Eptr visitExpr(Expr &it, std::vector<Idx> &idcs, Idx &idx) override {
      return ExprVisitor::visitExpr(it, hir::Pos::Stmt);
    }

    Eptr visitTypeDefn(TypeDefn &it, std::vector<Idx> &idcs, Idx &iidx) override {
      Idx idx = idcs.at(iidx++);
      hir::Definition &def = program.bindings.at(idx);
      hir::BlockExpr theBlock;
      theBlock.span = ((Statement &) it).span;
      auto &typeDef = std::get<DefType::Type>(def.defType.v);
      if (std::holds_alternative<TypeDefn::Alias>(it.val)) {
        auto &alias = std::get<TypeDefn::Alias>(it.val);
        typeDef.value = visitType(*alias.type, &idcs, &iidx);
      } else {
        auto &adt = std::get<TypeDefn::ADT>(it.val);
        Idx adtIdx = idx + 1; // type of the ADT introduced
        auto &adtDef = std::get<DefType::ADT>(program.bindings.at(adtIdx).defType.v);
        hir::Type ty;
        ty.base = adtIdx;
        ty.source = ((Type &)it).span;
        typeDef.value = ty;
        Idx fieldIdx = 0;
        for (const auto &fieldTy : adt.types) {
          Idx accAdtIdx;
          if (fieldTy.accessor) {
            accAdtIdx = idcs.at(iidx++);
            if (accAdtIdx > adtIdx) {
              auto defineE = withSpan<hir::DefineExpr>(fieldTy.accessor->ident.span());
              defineE->idx = accAdtIdx + 1; // acc var idx
              auto newE = withSpan<hir::NewExpr>(defineE->span);
              newE->adt = accAdtIdx;
              defineE->value = std::move(newE);
              theBlock.block.body.push_back(std::move(defineE));
            }
          }
          hir::Type fieldHirTy = visitType(*fieldTy.ty, &idcs, &iidx);
          adtDef.fields.push_back(fieldHirTy);
          if (fieldTy.accessor) {
            hir::TraitImpl &ti = program.traitImpls.emplace_back();
            ti.source = fieldTy.accessor->ident.span();
            ti.type.base = accAdtIdx;
            ti.types.push_back(fieldHirTy);
            ti.trait.base = hir::Fn;
            hir::Type &argsTy = ti.trait.params.emplace_back();
            argsTy.base = hir::TUPLE;
            argsTy.params = {ty};
            hir::Block &accessor = ti.methods.emplace_back();
            accessor.idx = blockIdx++;
            Idx accessedIdx;
            accessor.bindings = {
                accessedIdx = introduceDef(hir::Definition{
                  "accessed",
                  ty.source,
                  DefType::Variable{}
                }),
                introduceDef(hir::Definition{
                  fieldTy.accessor->ident.value,
                  fieldTy.accessor->ident.span(),
                  DefType::Variable{}
                })
            };
            auto &getExpr = (hir::GetExpr &) *accessor.body.emplace_back(withSpan<hir::GetExpr>(fieldTy.accessor->ident.span()));
            getExpr.field = fieldIdx;
            getExpr.adt = adtIdx;
            auto varE = withSpan<hir::VarExpr>(getExpr.span);
            varE->ref = accessedIdx;
            getExpr.value = std::move(varE);
          }
          fieldIdx++;
        }
        {
          hir::TraitImpl &ti = program.traitImpls.emplace_back();
          ti.source = *typeDef.value->source;
          ti.type.source = ti.source;
          ti.type.base = hir::TYPETOKEN;
          ti.type.params = {ty};
          ti.types.emplace_back(ty);
          ti.trait.base = hir::Fn;
          hir::Type &argsTy = ti.trait.params.emplace_back();
          argsTy.base = hir::TUPLE;
          argsTy.params = adtDef.fields;

          hir::Block &ctor = ti.methods.emplace_back();
          ctor.idx = blockIdx++;
          ctor.bindings.reserve(argsTy.params.size());
          for (Idx i = 0; i < argsTy.params.size(); ++i) {
            ctor.bindings.push_back(introduceDef(hir::Definition{
              util::toStr("arg_", i),
              argsTy.params.at(i).source,
              DefType::Variable{}
            }));
          }
          auto &newExpr = (hir::NewExpr &) *ctor.body.emplace_back(withSpan<hir::NewExpr>(typeDef.value->source));
          newExpr.adt = adtIdx;
          for (Idx i : ctor.bindings) {
            auto varE = withSpan<hir::VarExpr>(program.bindings.at(i).source);
            varE->ref = i;
            newExpr.values.push_back(std::move(varE));
          }
          ctor.bindings.push_back(introduceDef(hir::Definition{
              "construct",
              argsTy.source,
              DefType::Variable{}
          }));
        }
      }
      theBlock.block.body.push_back(withSpan<hir::VoidExpr>(((Statement &)it).span));
      return std::make_unique<hir::BlockExpr>(std::move(theBlock));
    }
    
    Eptr visitTraitImpl(TraitImpl &it, std::vector<Idx> &idcs, Idx &idx) override {
      hir::TraitImpl impl;
      // TODO lower trait implementations
      throw util::ICE("TODO");
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
        for ([[maybe_unused]] auto &b : it.bindings) {
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
                  maybePtr(it.arguments.typeParams),
                  maybePtr(it.typeHint),
                  it.arguments.bindings,
                  it.span,
                  *it.body) :
        fnExpr("fn",
               maybePtr(it.arguments.typeParams),
               maybePtr(it.typeHint),
               it.arguments.bindings,
               it.span,
               *it.body);
    }

    Eptr visitLambdaExpr(LambdaExpr &it, hir::Pos pos) override {
      return fnExpr(util::toStr("lambda@", it.lambdaToken.loc.line, ":", it.lambdaToken.loc.col),
                    nullptr,
                    nullptr,
                    it.arguments,
                    it.span,
                    *it.body);
    }

    Eptr visitBlockExpr(BlockExpr &it, hir::Pos pos) override {
      if (!it.value) {
        return withSpan<hir::VoidExpr>(it.span);
      }

      bindings.push();
      typeBindings.push();

      std::vector<std::vector<Idx>> indeces;
      indeces.reserve(it.statements.size());
      BindingVisitor bv(*this);
      for (auto &stmt : it.statements) {
        bv.visitStatement(*stmt.statement, indeces.emplace_back(), false);
      }

      auto expr = withSpan<hir::BlockExpr>(it.span);
      expr->block.span = it.span;
      addBindingsToBlock(expr->block);

      auto &body = expr->block.body;
      auto iter = indeces.begin();
      for (auto &stmt : it.statements) {
        Idx iidx = 0;
        body.push_back(visitStatement(*stmt.statement, *iter++, iidx));
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
      default: throw util::ICE("Impossible literal expression token type.");
      }
      return expr;
    }

    Eptr visitVarExpr(VarExpr &it, hir::Pos pos) override {
      if (auto lookup = bindings.lookup(it.name.ident.value)) {
        auto expr = withSpan<hir::VarExpr>(it.span);
        expr->ref = *lookup;
        return expr;
      } else if (auto typeLookup = typeBindings.lookup(it.name.ident.value)) {
        auto expr = withSpan<hir::TypeExpr>(it.span);
        expr->type.base = *typeLookup;
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

        loc::SrcLoc lastLoc;
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
            default: throw util::ICE("Impossible binary expression token type.");
            }

            auto predE = withSpan<hir::BlockExpr>(std::nullopt);
            hir::DefIdx thisVar = addTemp(*predE, *term.expr);
            {
              auto cmpE = withSpan<hir::CmpExpr>(loc::Span(lastLoc, term.expr->span.hi));
              lastLoc = term.expr->span.lo;
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
        lastLoc = it.lhs->span.lo;
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
      expr->hints.push_back(visitType(*it.hint.type, nullptr, nullptr));
      return expr;
    }
  };

  std::unique_ptr<ProgramVisitor<LowerResult>> lowerVisitor() {
    return std::make_unique<LoweringVisitor>();
  }
}
