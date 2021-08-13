#include "Json.h"
#include "Hir.h"

#include <memory>

namespace loc {
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(SrcLoc, line, col);
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Span, lo, hi);
};

#define EMPTY_JSON(TYPE)                                \
  void to_json(nlohmann::json &j, const TYPE &e) {}     \
  void from_json(const nlohmann::json& j, TYPE &p) {}

#define FORWARD_DEFINE_JSON(TYPE)                   \
  void to_json(nlohmann::json &j, const TYPE &e);   \
  void from_json(const nlohmann::json& j, TYPE &p);

namespace hir {
  FORWARD_DEFINE_JSON(BlockExpr)
  FORWARD_DEFINE_JSON(VarExpr)
  FORWARD_DEFINE_JSON(CondExpr)
  FORWARD_DEFINE_JSON(VoidExpr)
  FORWARD_DEFINE_JSON(LiteralExpr)
  FORWARD_DEFINE_JSON(BinExpr)
  FORWARD_DEFINE_JSON(CmpExpr)
  FORWARD_DEFINE_JSON(NegExpr)
  FORWARD_DEFINE_JSON(CallExpr)
  FORWARD_DEFINE_JSON(DefineExpr)
  FORWARD_DEFINE_JSON(NewExpr)
  FORWARD_DEFINE_JSON(GetExpr)
  FORWARD_DEFINE_JSON(ForeignExpr)
  FORWARD_DEFINE_JSON(DummyExpr)
}

namespace hir::json {
  using json = nlohmann::json;
#define JSON_VISIT(TYPE) json visit##TYPE(TYPE &it) { return {{"type", #TYPE}, {"value", it}}; }
  class JsonExprVisitor : public ExprVisitor<json> {
    JSON_VISIT(BlockExpr)
    JSON_VISIT(VarExpr)
    JSON_VISIT(CondExpr)
    JSON_VISIT(VoidExpr)
    JSON_VISIT(LiteralExpr)
    JSON_VISIT(BinExpr)
    JSON_VISIT(CmpExpr)
    JSON_VISIT(NegExpr)
    JSON_VISIT(CallExpr)
    JSON_VISIT(DefineExpr)
    JSON_VISIT(NewExpr)
    JSON_VISIT(GetExpr)
    JSON_VISIT(ForeignExpr)
    JSON_VISIT(DummyExpr)
  };
}

namespace nlohmann {
  template <>
  struct adl_serializer<hir::Eptr> {
    static void to_json(json &j, const hir::Eptr &v) {
      if (v) {
        j = hir::json::JsonExprVisitor().visitExpr(*v);
      } else {
        j = nullptr;
      }
    }

#define FROM_EXPR_JSON(TYPE)                    \
    if (t == #TYPE) {                           \
    auto expr = std::make_unique<hir::TYPE>();  \
    j.get_to(*expr); return; }

    static void from_json(const json &j, hir::Eptr &opt) {
      const auto &t = j.at("type");
      FROM_EXPR_JSON(BlockExpr);
      FROM_EXPR_JSON(VarExpr);
      FROM_EXPR_JSON(CondExpr);
      FROM_EXPR_JSON(VoidExpr);
      FROM_EXPR_JSON(LiteralExpr);
      FROM_EXPR_JSON(BinExpr);
      FROM_EXPR_JSON(CmpExpr);
      FROM_EXPR_JSON(NegExpr);
      FROM_EXPR_JSON(CallExpr);
      FROM_EXPR_JSON(DefineExpr);
      FROM_EXPR_JSON(NewExpr);
      FROM_EXPR_JSON(GetExpr);
      FROM_EXPR_JSON(ForeignExpr);
      FROM_EXPR_JSON(DummyExpr);
      opt = nullptr;
    }
  };
}

namespace hir {
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Binding, name, source)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Block, bindings, body)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(ADT::Variant, values)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(ADT, variants)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Program, types, fnImpls, topLevel)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(BlockExpr, block)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(VarRef, block, idx)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(VarExpr, ref)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(CondExpr, predE, thenE, elseE)
  EMPTY_JSON(VoidExpr)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(LiteralExpr, type, value)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(BinExpr, op, lhs, rhs)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(CmpExpr, exprs, ops)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(NegExpr, value)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(CallExpr, func, args)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(DefineExpr, idx, value)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(NewExpr, adt, variant, values)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(GetExpr, adt, variant, field, value)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(ForeignExpr, name)
  EMPTY_JSON(DummyExpr)
}

namespace hir::json {
  json to_json(const Program &prog) {
    return prog;
  }
  Program from_json(const json &j) {
    return j;
  }
}
