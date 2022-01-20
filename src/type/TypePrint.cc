#include "TypePrint.h"

using namespace type;

std::ostream &operator<<(std::ostream &os, const IntSize s) {
  os << bitCount(s);
  return os;
}

std::ostream &operator<<(std::ostream &os, const FloatSize s) {
  os << bitCount(s);
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
          while (true) {
            os << (char) ('A' + (i % 26));
            if (i <= 25) return;
            i /= 26;
          }
        },
        [&](Ty::ADT &t) {
          os << "adt[" << t.i << "]";
          if (!t.s.empty()) {
            os << "<";
            for (auto iter = t.s.begin(); iter != t.s.end();) {
              os << *iter;
              if (++iter != t.s.end()) os << ",";
            }
            os << ">";
          }
        },
        [&](Ty::Never &t) {
          os << "!";
        },
        [&](Ty::Tuple &t) {
          os << "#(";
          for (auto iter = t.t.begin(); iter != t.t.end();) {
            os << *iter;
            if (++iter != t.t.end()) os << ",";
          }
          os << ")";
        },
        [&](Ty::String &) {
          os << "str";
        },
        [&](Ty::Cyclic &c) {
          os << "#{" << c.ty << "}";
        },
        [&](Ty::CyclicRef &r) {
          os << "#" << r.depth;
        },
        [&](Ty::FfiFn &f) {
          os << "ffifn<" << f.args << ", " << f.ret << ">";
        },
    }, t->v);
  return os;
}

namespace type::print {
  void printTy(type::Ty *t) {
    std::cerr << t << std::endl;
  }
}
