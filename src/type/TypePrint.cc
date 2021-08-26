#include "TypePrint.h"
#include "Type.h"
#include <variant>

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

using namespace type;

std::ostream &operator<<(std::ostream &os, const IntSize s) {
  switch (s) {
  case IntSize::i8: os << "8"; break;
  case IntSize::i16: os << "16"; break;
  case IntSize::i32: os << "32"; break;
  case IntSize::i64: os << "64"; break;
  case IntSize::i128: os << "128"; break;
  }
  return os;
}

std::ostream &operator<<(std::ostream &os, const FloatSize s) {
  switch (s) {
  case FloatSize::f16: os << "16"; break;
  case FloatSize::f32: os << "32"; break;
  case FloatSize::f64: os << "64"; break;
  }
  return os;
}

std::ostream &operator<<(std::ostream &os, Ty *t) {
  std::visit(overloaded {
      [&](Ty::Err&) {
        os << "err";
      },
        [&](Ty::Bool&) {
          os << "bool";
        },
        [&](Ty::Int &t) {
          os << "i" << t.s;
        },
        [&](Ty::UInt &t) {
          os << "u" << t.s;
        },
        [&](Ty::Float &t) {
          os << "f" << t.s;
        },
        [&](Ty::Placeholder &t) {
          Idx i = t.i;
          os << "tv::";
          while (true) {
            os << (char) ('A' + (i % 26));
            if (i <= 25) return;
            i /= 26;
          }
          os << "";
        },
        [&](Ty::ADT &t) {
          os << "adt[" << t.i << "]";
          os << "{";
          for (auto &v : t.v) {
            os << v << ", ";
          }
          os << "}";
          os << "<";
          for (auto &s : t.s) {
            os << s << ", ";
          }
          os << ">";
        },
        [&](Ty::Dyn &t) {
          os << "dyn{";
          for (auto &tb : t.t) {
            os << tb << ", ";
          }
          os << "}";
        },
        [&](Ty::Tuple &t) {
          os << "(";
          for (auto &t : t.t) {
            os << t << ", ";
          }
          os << ")";
        },
        [&](Ty::TraitRef &t) {
          os << "<" << t.ty << " as " << t.trait << ">";
          os << "::{" << t.ref << "}";
        },
        [&](Ty::String &s) {
          os << "str";
        },
    }, t->v);
  return os;
}

std::ostream &operator<<(std::ostream &os, type::TraitBound *tb) {
  os << "trait[" << tb->i << "]";
  os << "<";
  for (auto &s : tb->s) {
    os << s << ", ";
  }
  os << ">";
  return os;
}
