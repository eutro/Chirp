#pragma once

#include <nlohmann/json.hpp>

#include "Hir.h"

namespace hir {
  void to_json(nlohmann::json &json, const Program &program);
  void from_json(const nlohmann::json &json, Program &program);
}
