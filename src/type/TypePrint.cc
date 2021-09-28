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
          os << "tv::";
          while (true) {
            os << (char) ('A' + (i % 26));
            if (i <= 25) return;
            i /= 26;
          }
        },
        [&](Ty::ADT &t) {
          os << "adt[" << t.i << "]";
          os << "{";
          for (auto &v : t.v) {
            os << v << ",";
          }
          os << "}";
          os << "<";
          for (auto &s : t.s) {
            os << s << ",";
          }
          os << ">";
        },
        [&](Ty::Dyn &t) {
          os << "dyn{";
          for (auto &tb : t.t) {
            os << tb << ",";
          }
          os << "}";
        },
        [&](Ty::Tuple &t) {
          os << "(";
          for (auto &t : t.t) {
            os << t << ",";
          }
          os << ")";
        },
        [&](Ty::TraitRef &t) {
          os << "<" << t.ty << " as " << t.trait << ">";
          os << "::" << t.ref;
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

std::ostream &operator<<(std::ostream &os, type::TraitBound *tb) {
  os << "trait[" << tb->i << "]";
  os << "<";
  for (auto &s : tb->s) {
    os << s << ",";
  }
  os << ">";
  return os;
}

namespace type::print {
  void printTy(type::Ty *t) {
    std::cerr << t << std::endl;
  }
}
