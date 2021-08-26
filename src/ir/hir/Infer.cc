#include "Infer.h"

#include "../../type/TypePrint.h"

#include <array>
#include <cstddef>
#include <iostream>
#include <memory>
#include <string>
#include <variant>

namespace hir::infer {
  enum BuiltinTraits {
    fn,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitOr,
    BitAnd,
    Cmp,
    Neg,
  };

  class InferenceVisitor :
    public ExprVisitor<Tp>,
    public ProgramVisitor<InferResult> {
  public:
    arena::InternArena<Ty> tcx;
    arena::InternArena<type::TraitBound> tbcx;

    std::map<DefIdx, Tp> varTypes;

    std::map<DefIdx, ADT*> adts;

    err::ErrorContext ecx;

    std::map<Expr*, Tp> exprTypes;

    InferenceVisitor() {
      /*
      Module &builtins = modules[0];
      builtins.types["bool"] = {tcx.intern(Ty::Bool{})};

      builtins.types["i8"] = {tcx.intern(Ty::Int{type::IntSize::i8})};
      builtins.types["i16"] = {tcx.intern(Ty::Int{type::IntSize::i16})};
      builtins.types["i32"] = {tcx.intern(Ty::Int{type::IntSize::i32})};
      builtins.types["i64"] = {tcx.intern(Ty::Int{type::IntSize::i64})};

      builtins.types["u8"] = {tcx.intern(Ty::UInt{type::IntSize::i8})};
      builtins.types["u16"] = {tcx.intern(Ty::UInt{type::IntSize::i16})};
      builtins.types["u32"] = {tcx.intern(Ty::UInt{type::IntSize::i32})};
      builtins.types["u64"] = {tcx.intern(Ty::UInt{type::IntSize::i64})};

      builtins.types["f16"] = {tcx.intern(Ty::Float{type::FloatSize::f16})};
      builtins.types["f32"] = {tcx.intern(Ty::Float{type::FloatSize::f32})};
      builtins.types["f64"] = {tcx.intern(Ty::Float{type::FloatSize::f64})};

      Idx tc = 0;
      builtins.types["fn"] = {BuiltinTraits::fn};

      builtins.types["Add"] = {Add};
      builtins.types["Sub"] = {Sub};
      builtins.types["Mul"] = {Mul};
      builtins.types["Div"] = {Div};
      builtins.types["Rem"] = {Rem};
      builtins.types["BitOr"] = {BitOr};
      builtins.types["BitAnd"] = {BitAnd};
      builtins.types["Cmp"] = {Cmp};
      builtins.types["Neg"] = {Neg};
      */
    }

    Idx paramC = 0;
    Tp visitBlock(Block &block) {
      Idx oldParamC = paramC;
      for (auto &b : block.bindings) {
        varTypes[b] = freshType();
      }
      Tp last;
      for (auto &e : block.body) {
        last = visitExpr(*e);
      }
      paramC = oldParamC;
      return last;
    }

    InferResult visitProgram(Program &p) {
      /*
      for (auto &import : p.typeImports) {
        tds[import.defIdx] = modules.at(import.moduleIdx).types.at(import.name);
      }
      */

      for (auto &adt : p.types) {
        adts[adt.id] = &adt;
      }

      for (auto &i : p.fnImpls) {
        visitBlock(i);
      }

      visitBlock(p.topLevel);

      InferResult res;
      res.ecx = std::move(ecx);
      res.exprTypes = std::move(exprTypes);
      return res;
    }

    Tp freshType() { return tcx.intern(Ty::Placeholder{paramC++}); }
    Tp unitType() { return tcx.intern(Ty::Tuple{{}}); }
    Tp boolType() { return tcx.intern(Ty::Bool{}); }

    void constrain(Tp param, TraitBound *bound) {
      std::cerr << "Constraining " << param << " to " << bound << std::endl;
    }
    void constrain(Tp receiver, Tp inbound) {
      std::cerr << "Constraining " << receiver << " and " << inbound << std::endl;
    }

    Tp visitExpr(Expr &it) {
      return exprTypes[&it] = ExprVisitor::visitExpr(it);
    }

    Tp visitBlockExpr(BlockExpr &it) {
      return visitBlock(it.block);
    }
    Tp visitVarExpr(VarExpr &it) {
      return varTypes.at(it.ref);
    }
    Tp visitCondExpr(CondExpr &it) {
      Tp predT = visitExpr(*it.predE);
      Tp thenT = visitExpr(*it.thenE);
      Tp elseT = visitExpr(*it.elseE);
      constrain(boolType(), predT);
      if (it.pos == Pos::Expr) {
        Tp retT = freshType();
        constrain(retT, thenT);
        constrain(retT, elseT);
        return retT;
      } else {
        return unitType();
      }
    }
    Tp visitVoidExpr(VoidExpr &it) {
      return unitType();
    }
    Tp visitLiteralExpr(LiteralExpr &it) {
      switch (it.type) {
      case LiteralExpr::Int:
        return tcx.intern(Ty::Int{type::IntSize::i64});
      case LiteralExpr::Float:
        return tcx.intern(Ty::Float{type::FloatSize::f64});
      case LiteralExpr::String:
        return tcx.intern(Ty::String{});
      }
    }
    Tp visitBoolExpr(BoolExpr &it) {
      return boolType();
    }
    Tp visitBinExpr(BinExpr &it) {
      Tp lhsType = visitExpr(*it.lhs);
      Tp rhsType = visitExpr(*it.rhs);
      BuiltinTraits trait;
      switch (it.op) {
      case BinExpr::BitOr: trait = BuiltinTraits::BitOr; break;
      case BinExpr::BitAnd: trait = BuiltinTraits::BitAnd; break;
      case BinExpr::Add: trait = BuiltinTraits::Add; break;
      case BinExpr::Sub: trait = BuiltinTraits::Sub; break;
      case BinExpr::Mul: trait = BuiltinTraits::Mul; break;
      case BinExpr::Div: trait = BuiltinTraits::Div; break;
      case BinExpr::Rem: trait = BuiltinTraits::Rem; break;
      }
      TraitBound *traitBound = tbcx.intern(TraitBound{trait, {rhsType}});
      constrain(lhsType, traitBound);
      return tcx.intern(Ty::TraitRef{lhsType, traitBound, 0});
    }
    Tp visitCmpExpr(CmpExpr &it) {
      Tp lhsType = visitExpr(*it.lhs);
      Tp rhsType = visitExpr(*it.rhs);
      TraitBound *traitBound = tbcx.intern(TraitBound{BuiltinTraits::Cmp, {rhsType}});
      constrain(lhsType, traitBound);
      return boolType();
    }
    Tp visitNegExpr(NegExpr &it) {
      Tp exprType = visitExpr(*it.value);
      TraitBound *traitBound = tbcx.intern(TraitBound{BuiltinTraits::Neg});
      constrain(exprType, traitBound);
      return tcx.intern(Ty::TraitRef{exprType, traitBound, 0});
    }
    Tp visitCallExpr(CallExpr &it) {
      Tp funcTy = visitExpr(*it.func);
      Tp retTy = freshType();
      std::vector<Tp> argTys;
      argTys.reserve(it.args.size());
      for (Eptr &e : it.args) {
        argTys.push_back(visitExpr(*e));
      }
      Tp argsTy = tcx.intern(Ty::Tuple{argTys});
      TraitBound *traitBound = tbcx.intern(TraitBound{BuiltinTraits::fn, {argsTy, retTy}});

      constrain(funcTy, traitBound);
      return retTy;
    }
    Tp visitDefineExpr(DefineExpr &it) {
      Tp definedTy = visitExpr(*it.value);
      constrain(varTypes.at(it.idx), definedTy);
      return unitType();
    }
    Tp visitNewExpr(NewExpr &it) {
      std::vector<Tp> argTys;
      argTys.reserve(it.values.size());
      for (Eptr &e : it.values) {
        argTys.push_back(visitExpr(*e));
      }
      return tcx.intern(Ty::ADT{it.adt, {it.variant}, argTys});
    }
    Tp visitGetExpr(GetExpr &it) {
      std::vector<Tp> argTys;
      ADT &adt = *adts.at(it.adt);
      auto size = adt.variants.at(it.variant).values.size();
      argTys.reserve(size);
      for (Idx i = 0; i < size; ++i) argTys.push_back(freshType());
      Tp adtTy = tcx.intern(Ty::ADT{it.adt, {it.variant}, argTys});
      constrain(adtTy, visitExpr(*it.value));
      Tp retTy = argTys.at(it.field);
      return retTy;
    }
    Tp visitForeignExpr(ForeignExpr &it) {
      return freshType();
    }
    Tp visitDummyExpr(DummyExpr &it) {
      // suppressed
      return tcx.intern(Ty::Err{});
    }
  };

  std::unique_ptr<ProgramVisitor<InferResult>> inferenceVisitor() {
    return std::make_unique<InferenceVisitor>();
  }
}
