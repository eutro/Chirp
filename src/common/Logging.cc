#include "Logging.h"

#include <utility>
#include <stdexcept>
#include <iostream>
#include <fstream>

namespace logging {
  Marker::Marker(const char *name, bool defaultEnabled) noexcept: name(name) {
    std::string envVar = "CHIRP_LOG_MARKER_" + std::string(name) + "_ENABLED";
    char *envVal = getenv(envVar.c_str());
    if (envVal) {
      std::string envString(envVal);
      if (envString == "true") {
        isEnabled = true;
      } else if (envString == "false") {
        isEnabled = false;
      } else {
        std::cerr << "WARNING: "
                  << envVar << " was not 'true' or 'false', but '"
                  << envString + "', defaulting to "
                  << (defaultEnabled ? "true" : "false")
                  << "\n";
        isEnabled = defaultEnabled;
      }
    } else {
      isEnabled = defaultEnabled;
    }
  }

  Logger::Logger(const char *name) noexcept: name(name) {
    std::string envVar = "CHIRP_LOGGER_" + std::string(name) + "_REDIRECT";
    char *envVal = getenv(envVar.c_str());
    if (envVal) {
      std::string envString(envVal);
      if (envString == "stderr") {
        os = &std::cerr;
      } else if (envString == "stdout") {
        os = &std::cout;
      } else if (envString == "null") {
        os = &null_out;
      } else {
        maybeOwned = std::make_unique<std::ofstream>(envString);
        os = maybeOwned.get();
      }
    } else {
      os = &std::cerr;
    }
  }

  nullstream null_out{}; // NOLINT(cert-err58-cpp)
  const Marker INFO("INFO", true); // NOLINT(cert-err58-cpp)
  const Marker DEBUG("DEBUG", false); // NOLINT(cert-err58-cpp)
  const Marker TRACE("TRACE", false); // NOLINT(cert-err58-cpp)
  const Logger CHIRP("CHIRP"); // NOLINT(cert-err58-cpp)
}
