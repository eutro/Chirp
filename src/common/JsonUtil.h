#pragma once

#include <nlohmann/json.hpp>

#define EMPTY_JSON(TYPE)                                \
  void to_json(nlohmann::json &, const TYPE &) {}     \
  void from_json(const nlohmann::json &, TYPE &) {}

#define FORWARD_DEFINE_JSON(TYPE)                   \
  void to_json(nlohmann::json &j, const TYPE &e);   \
  void from_json(const nlohmann::json& j, TYPE &p);

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
    static bool try_from_json(const json &j, std::variant<T...> &v, std::size_t idx) {
      using Ty = std::variant_alternative_t<Idx, std::variant<T...>>;
      if (idx == Idx) {
        from_json_idx_ty<Idx, Ty>(j, v);
        return true;
      }
      return false;
    }

    template<std::size_t... Idx>
    static void from_json(const json &j, std::variant<T...> &v,
                          std::size_t idx,
                          std::integer_sequence<std::size_t, Idx...>) {
      if (!(try_from_json<Idx>(j, v, idx) || ...)) {
        throw std::bad_variant_access();
      }
    }

    static void from_json(const json &j, std::variant<T...> &v) {
      std::size_t idx = j.at("index").get<std::size_t>();
      using Indices = typename std::index_sequence_for<T...>;
      from_json(j.at("value"), v, idx, Indices{});
    }
  };
}
