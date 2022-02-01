#pragma once

#include <string>
#include <tuple>
#include <memory>

#include "Util.h"

namespace logging {
  class Marker {
  public:
    const char *name;
    bool isEnabled;
    Marker(const char *name, bool defaultEnabled) noexcept;
  };

  const extern Marker INFO;
  const extern Marker DEBUG;
  const extern Marker TRACE;

  struct nullstream : std::ostream {
    nullstream() : std::ios(nullptr), std::ostream(nullptr) {}
  };

  extern nullstream null_out;

  class Logger {
    const char *name;
    std::ostream *os;
    std::unique_ptr<std::ostream> maybeOwned;

  public:
    Logger(const char *name) noexcept;

    template <typename... Args>
    std::ostream &log(const Marker &marker, const Args &...args) const {
      if (!marker.isEnabled) return null_out;
      *os << "[" << name << "][" << marker.name << "]: ";
      ((*os << args), ...);
      return *os;
    }

    template <typename... Args>
    std::ostream &info(const Args &...args) const {
      return log(INFO, args...);
    }

    template <typename... Args>
    std::ostream &debug(const Args &...args) const {
      return log(DEBUG, args...);
    }

    template <typename... Args>
    std::ostream &trace(const Args &...args) const {
      return log(TRACE, args...);
    }
  };

  const extern Logger CHIRP;
}
