#include "Json.h"
#include "Hir.h"
#include "nlohmann/json.hpp"

#include <memory>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>

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
  struct adl_serializer<std::optional<T>> {
    static void to_json(json &j, const std::optional<T> &v) {
      if (v) {
        j = *v;
      }
    }
    static void from_json(const json &j, std::optional<T> &opt) {
      opt = j;
    }
  };

  template <typename... T>
  struct adl_serializer<std::variant<T...>> {
    static void to_json(json &j, const std::variant<T...> &v) {
      j["index"] = v.index();
      std::visit([&j](auto x) { j["value"] = x; }, v);
    }

    template <std::size_t Idx, typename Ty>
    static void from_json_idx_ty(const json &j, std::variant<T...> &v) {
      v = std::variant<T...>(std::in_place_index<Idx>, j.get<Ty>());
    }

    template <std::size_t Idx>
    static bool from_json_idx(const json &j, std::variant<T...> &v) {
      using Ty = std::variant_alternative_t<Idx, std::variant<T...>>;
      from_json_idx_ty<Idx, Ty>(j, v);
      return true;
    }

    template<std::size_t... Idx>
    static void from_json(const json &j, std::variant<T...> &v,
                          std::size_t idx,
                          std::integer_sequence<std::size_t, Idx...>) {
      if (!((idx == Idx && from_json_idx<Idx>(j, v))
            || ...)) {
        throw std::bad_variant_access();
      }
    }

    static void from_json(const json &j, std::variant<T...> &v) {
      std::size_t idx = j.at("index").get<std::size_t>();
      using Indices = typename std::index_sequence_for<T...>;
      from_json(j.at("value"), v, idx, Indices{});
    }
  };

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
        j["type"] = v->type;
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
      j.at("type").get_to(opt->type);
    }
  };
}

namespace hir {
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Type, source, base, params)

  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Definition, name, source)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Definition::DefType, v);
  EMPTY_JSON(Definition::DefType::Type);
  EMPTY_JSON(Definition::DefType::Trait);
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Definition::DefType::Variable, hints);
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
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(NewExpr, adt, variant, values)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(GetExpr, adt, variant, field, value)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(ForeignExpr, name)
  EMPTY_JSON(DummyExpr)

  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(ADT::Variant, values)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(ADT, id, variants)
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(TraitImpl, type, trait, types, methods, params, source)

#define inline // remove inline from the next one, it has to be emitted
  NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Program, bindings, types, traitImpls, topLevel)
#undef inline
}
