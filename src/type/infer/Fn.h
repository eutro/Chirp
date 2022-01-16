#pragma once

#include "../Type.h"

#include <cstring>
#include <functional>
#include <stdexcept>
#include <string>

namespace type::infer {
  struct Constant;

  template <typename T>
  const T &constant_cast(const Constant &c);

  /**
   * A wrapper for constants that can be bitwise compared.
   */
  struct Constant {
    struct TypeData {
      using OutputFn = std::function<void(std::ostream&, const Constant &)>;
      const std::type_info &ty;
      OutputFn output;
      TypeData(const std::type_info &ty, const OutputFn &output)
        : ty(ty), output(output) {}
      bool operator==(const TypeData &rhs) const {
        return ty == rhs.ty;
      }
      bool operator!=(const TypeData &rhs) const {
        return !(rhs == *this);
      }

      template <typename T>
      static TypeData of() {
        return TypeData(typeid(T), outputFor<T>());
      }

      template <typename T>
      static OutputFn outputFor() {
        if constexpr (util::op_valid_t<std::ostream&, const T&, util::left_shift>::value) {
          return streamOutput<T>;
        } else {
          return hexOutput;
        }
      }

      template <typename T>
      static std::ostream &streamOutput(std::ostream &os, const Constant &c) {
        return os << constant_cast<T>(c);
      }

      static std::ostream &hexOutput(std::ostream &os, const Constant &c) {
        os << "0x";
        for (char *i = c.raw; i != c.raw + c.size; ++i) {
          os << std::hex << (int) *i;
        }
        return os;
      }
    };
    TypeData ty;
    size_t size;
    char *raw;

    Constant(const Constant &o);

    template <
        typename T,
        typename = std::enable_if<std::is_trivially_copyable_v<T>>
        >
    Constant(const T &v):
      ty(TypeData::of<T>()),
      size(sizeof(T)),
      raw(new char[size])
    {
      std::memcpy(raw, &v, size);
    }

    ~Constant() {
      delete[] raw;
    }

    bool operator==(const Constant &o) const {
      if (size != o.size || ty != o.ty) return false;
      return !std::strncmp(raw, o.raw, size);
    }

    bool operator<(const Constant &o) const {
      if (size < o.size) return true;
      if (o.size < size) return false;
      return std::strncmp(raw, o.raw, size) < 0;
    }

    friend std::ostream &operator<<(std::ostream &os, const Constant &c) {
      c.ty.output(os, c);
      return os;
    }
  };

  template<typename T>
  const T &constant_cast(const Constant &c) {
    if (c.size != sizeof(T) || c.ty != Constant::TypeData::of<T>()) {
      throw std::runtime_error("Bad constant cast");
    }
    return *(T*)c.raw;
  }

  using Fn = std::function<std::vector<Tp>(const std::vector<Tp> &tys, const std::vector<Constant> &args)>;
}
