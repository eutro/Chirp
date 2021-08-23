#include "Hir.h"
#include "../../type/Type.h"
#include "../../common/Arena.h"

#include <map>
#include <optional>
#include <stdexcept>
#include <variant>

namespace hir::infer {
  class Module {
  public:
    std::map<std::string,
             std::variant<type::BaseType*,
                          type::Trait*>>
    typeMapping;
  };

  class Var {
  public:
    std::vector<type::TypeRef> instantiations;
  };

  type::Trait makeFnTrait() {
    type::Trait fnTrait;
    fnTrait.types.preVariadic = 0;
    fnTrait.types.postVariadic = 1;
    type::TypeMapping &callMethod = fnTrait.methods.emplace_back();
    callMethod.idces = {0, 1};
    return fnTrait;
  }

  type::Trait FN_TRAIT = makeFnTrait();

  type::BaseType makeUnitType() {
    type::BaseType unitType;
    unitType.types.preVariadic = 0;
    unitType.types.postVariadic = std::nullopt;
    return unitType;
  }

  type::BaseType UNIT_TYPE = makeUnitType();

  class InferenceVisitor :
    public ExprVisitor<type::TypeRef> {

    std::vector<Module> modules;
    Program &prog;

    arena::Arena<type::Type*> typeRefs;
    arena::Arena<type::Type> types;
    arena::Arena<type::BaseType> baseTypes;

    std::map<Expr*, type::TypeRef> exprTypes;
    std::map<Block*, std::vector<Var>> blockVars;

    std::vector<Block*> blocks;
    std::vector<std::vector<Var>> vars;

    err::ErrorContext ecx;

    type::Type *unitType = types.add();

    InferenceVisitor(Program &prog):
      prog(prog)
    {
      Module &builtins = modules.emplace_back();
      builtins.typeMapping["fn"] = &FN_TRAIT;
      builtins.typeMapping["unit"] = &UNIT_TYPE;

      auto &cT = (*unitType->concrete = type::ConcreteType());
      cT.base = &UNIT_TYPE;

      Idx idx = 0;
      for (ADT &ty : prog.types) {
        type::BaseType *bT = baseTypes.add();
        type::BaseType::TraitImpl &fnImpl = bT->traitImpls.emplace_back();
        fnImpl.trait = &FN_TRAIT;

        Block &fnBlock = prog.fnImpls[idx++];

        type::TypeMapping &tM = fnImpl.methodImpls.emplace_back();
      }
    }

    void pushBlock(Block &block) {
      blocks.push_back(&block);
      auto &topVars = vars.emplace_back();
      for (Binding &b : block.bindings) {
        topVars.emplace_back();
      }
    }

    void popBlock() {
      Block *backBlock = blocks.back();
      blockVars[backBlock] = std::move(vars.back());
      blocks.pop_back();
      vars.pop_back();
    }

    type::TypeRef freshType() {
      return typeRefs.add(types.add());
    }

    type::TypeRef resolveType(hir::Type &ty) {
      if (ty.base) {
        if (ty.base->block >= blocks.size()) {
          Import &import = prog.typeImports[ty.base->idx];
          std::variant<type::BaseType *, type::Trait *> found =
              modules[import.moduleIdx].typeMapping.at(import.name);

          type::TypeRef newType = freshType();
          std::vector<type::TypeRef> *params;
          if (found.index() == 0) {
            type::ConcreteType &ct = *(newType->concrete = type::ConcreteType());
            ct.base = std::get<0>(found);
            params = &ct.params;
          } else {
            type::TraitBoundary &tb = newType->bounds.emplace_back();
            tb.trait = std::get<1>(found);
            params = &tb.params;
          }
          params->reserve(ty.params.size());
          for (std::unique_ptr<hir::Type> &param : ty.params) {
            params->push_back(resolveType(*param));
          }

          return newType;
        } else {
          Block *block = *(blocks.rbegin() + ty.base->block);
          // TODO lol
          return freshType();
        }
      } else {
        return freshType();
      }
    }

    type::TypeRef visitExpr(Expr &it) {
      auto itType = resolveType(it.type);
      type::Unification(ecx, itType, ExprVisitor::visitExpr(it))
        .run();
      return exprTypes[&it] = itType;
    }

    type::TypeRef visitBlockExpr(BlockExpr &it) {
      pushBlock(it.block);
      type::TypeRef retT = nullptr;
      for (auto &stmt : it.block.body) {
        retT = visitExpr(*stmt);
      }
      popBlock();
      return retT;
    }

    type::TypeRef visitVarExpr(VarExpr &it) {
      auto ty = freshType();
      auto &var = (vars.rbegin() + it.ref.block)->at(it.ref.idx);
      var.instantiations.push_back(ty);
      return ty;
    }

    type::TypeRef visitCondExpr(CondExpr &it) {
      auto predT = visitExpr(*it.predE);
      auto elseT = visitExpr(*it.elseE);
      if (it.pos == Pos::Expr) {
        type::Unification(ecx, predT, elseT)
          .run();
        return predT;
      } else {
        return typeRefs.add(unitType);
      }
    }

    type::TypeRef visitVoidExpr(VoidExpr &it) {
      return typeRefs.add(unitType);
    }

    type::TypeRef visitLiteralExpr(LiteralExpr &it) {
      return freshType();
    }

    type::TypeRef visitBinExpr(BinExpr &it) {
      return freshType();
    }

    type::TypeRef visitCmpExpr(CmpExpr &it) {
      return freshType();
    }

    type::TypeRef visitNegExpr(NegExpr &it) {
      return freshType();
    }

    type::TypeRef visitCallExpr(CallExpr &it) {
      return freshType();
    }

    type::TypeRef visitDefineExpr(DefineExpr &it) {
      return freshType();
    }

    type::TypeRef visitNewExpr(NewExpr &it) {
      return freshType();
    }

    type::TypeRef visitGetExpr(GetExpr &it) {
      return freshType();
    }

    type::TypeRef visitForeignExpr(ForeignExpr &it) {
      return freshType();
    }

    type::TypeRef visitDummyExpr(DummyExpr &it) {
      ecx.err()
        .msg("Invalid expression")
        .span(it.span, "here");
      return freshType();
    }
  };
}
