#include "Json.h"

#include "../../common/JsonUtil.h"
#include "../../common/LocJson.h"

namespace hir {
  FORWARD_DEFINE_JSON(BlockExpr)
  FORWARD_DEFINE_JSON(VarExpr)
  FORWARD_DEFINE_JSON(CondExpr)
  FORWARD_DEFINE_JSON(VoidExpr)
  FORWARD_DEFINE_JSON(LiteralExpr)
  FORWARD_DEFINE_JSON(BoolExpr)
  FORWARD_DEFINE_JSON(BinExpr)
  FORWARD_DEFINE_JSON(CmpExpr)
  FORWARD_DEFINE_JSON(NegExpr)
  FORWARD_DEFINE_JSON(CallExpr)
  FORWARD_DEFINE_JSON(DefineExpr)
  FORWARD_DEFINE_JSON(NewExpr)
  FORWARD_DEFINE_JSON(GetExpr)
  FORWARD_DEFINE_JSON(ForeignExpr)
  FORWARD_DEFINE_JSON(DummyExpr)

  FORWARD_DEFINE_JSON(Type)
}

namespace hir::json {
  using json = nlohmann::json;
#define JSON_VISIT(TYPE) json visit##TYPE(TYPE &it) { return {{"class", #TYPE}, {"value", it}}; }
  class JsonExprVisitor : public ExprVisitor<json> {
    JSON_VISIT(BlockExpr)
    JSON_VISIT(VarExpr)
    JSON_VISIT(CondExpr)
    JSON_VISIT(VoidExpr)
    JSON_VISIT(LiteralExpr)
    JSON_VISIT(BoolExpr)
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
  template <typename T>
  struct adl_serializer<std::unique_ptr<T>> {
    static void to_json(json &j, const std::unique_ptr<T> &v) {
      if (v) {
        j = *v;
      } else {
        j = nullptr;
      }
    }
    static void from_json(const json &j, std::unique_ptr<T> &opt) {
      if (j) {
        opt = std::make_unique<T>();
        j.get_to(*opt);
      } else {
        opt = nullptr;
      }
    }
  };

  template <>
  struct adl_serializer<hir::Eptr> {
    static void to_json(json &j, const hir::Eptr &v) {
      if (v) {
        j = hir::json::JsonExprVisitor().visitExpr(*v);
        j["span"] = v->span;
        j["pos"] = v->pos;
        j["hints"] = v->hints;
      } else {
        j = nullptr;
      }
    }

#define FROM_EXPR_JSON(TYPE)                    \
    else if (t == #TYPE) {                      \
    auto expr = std::make_unique<hir::TYPE>();  \
    j.at("value").get_to(*expr); opt = std::move(expr); }

    static void from_json(const json &j, hir::Eptr &opt) {
      const auto &t = j.at("type");
      if (false);
      FROM_EXPR_JSON(BlockExpr)
        FROM_EXPR_JSON(VarExpr)
        FROM_EXPR_JSON(CondExpr)
        FROM_EXPR_JSON(VoidExpr)
        FROM_EXPR_JSON(LiteralExpr)
        FROM_EXPR_JSON(BoolExpr)
        FROM_EXPR_JSON(BinExpr)
        FROM_EXPR_JSON(CmpExpr)
        FROM_EXPR_JSON(NegExpr)
        FROM_EXPR_JSON(CallExpr)
        FROM_EXPR_JSON(DefineExpr)
        FROM_EXPR_JSON(NewExpr)
        FROM_EXPR_JSON(GetExpr)
        FROM_EXPR_JSON(ForeignExpr)
        FROM_EXPR_JSON(DummyExpr)
      else {
        opt = nullptr;
        return;
      }
      j.at("span").get_to(opt->span);
      j.at("pos").get_to(opt->pos);
      j.at("hints").get_to(opt->hints);
    }
  };
}

namespace hir {
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Type, source, base, params)

  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Definition, name, source)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(DefType, v);
  EMPTY_JSON(DefType::Type);
  EMPTY_JSON(DefType::Trait);
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(DefType::ADT, values, types)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(DefType::Variable, hints);
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Block, bindings, body)

  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(BlockExpr, block)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(VarExpr, ref)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(CondExpr, predE, thenE, elseE)
  EMPTY_JSON(VoidExpr)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(LiteralExpr, type, value)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(BoolExpr, value)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(BinExpr, op, lhs, rhs)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(CmpExpr, op, lhs, rhs)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(NegExpr, value)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(CallExpr, func, args)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(DefineExpr, idx, value)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(NewExpr, adt, values)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(GetExpr, adt, field, value)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(ForeignExpr, name)
  EMPTY_JSON(DummyExpr)

  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(TraitImpl, type, trait, types, methods, params, source)

#define inline // remove inline from the next one, it has to be emitted
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Program, bindings, traitImpls, topLevel)
#undef inline
}
