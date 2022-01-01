#pragma once

#include "../Type.h"

#include <any>
#include <cstring>
#include <functional>
#include <stdexcept>
#include <string>

namespace type::infer {
  /**
   * A wrapper for constants that can be bitwise compared.
   */
  struct Constant {
    size_t size;
    char *raw;

    Constant(const Constant &o);

    template <typename T>
    Constant(const T &v):
      size(sizeof(T)),
      raw(new char[size])
    {
      std::memcpy(raw, &v, size);
    }

    ~Constant() {
      delete[] raw;
    }

    bool operator==(const Constant &o) const {
      if (size != o.size) return false;
      return !std::strncmp(raw, o.raw, size);
    }

    bool operator<(const Constant &o) const {
      if (size < o.size) return true;
      if (o.size < size) return false;
      return std::strncmp(raw, o.raw, size) < 0;
    }

    friend std::ostream &operator<<(std::ostream &o, const Constant &c) {
      o << "0x";
      for (char *i = c.raw; i != c.raw + c.size; ++i) {
        o << std::hex << (int) *i;
      }
      return o;
    }
  };

  template <typename T>
  const T &constant_cast(const Constant &c) {
    if (c.size != sizeof(T)) throw std::runtime_error("Bad constant cast");
    return *(T*)c.raw;
  }

  using Fn = std::function<std::vector<Tp>(const std::vector<Tp> &)>;
}
