#include "Type.h"

using namespace type;

std::ostream &operator<<(std::ostream &os, const IntSize s) {
  os << bitCount(s);
  return os;
}

std::ostream &operator<<(std::ostream &os, const FloatSize s) {
  os << bitCount(s);
  return os;
}

static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::Err &) {
  os << "err";
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::Bool &) {
  os << "bool";
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::Int &t) {
  os << "i" << t.s;
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::UInt &t) {
  os << "u" << t.s;
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::Float &t) {
  os << "f" << t.s;
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::Placeholder &t) {
  Idx i = t.i;
  while (true) {
    os << (char) ('A' + (i % 26));
    if (i <= 25) return;
    i /= 26;
  }
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::ADT &t) {
  auto found = ty->tcx->names.find(t.i);
  if (found != ty->tcx->names.end()) {
    os << found->second;
  } else {
    os << "adt[" << t.i << "]";
  }
  if (!t.s.empty()) {
    os << "<";
    for (auto iter = t.s.begin(); iter != t.s.end();) {
      os << *iter;
      if (++iter != t.s.end()) os << ",";
    }
    os << ">";
  }
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::Union &t) {
  os << "U{";
  for (auto iter = t.tys.begin(); iter != t.tys.end();) {
    os << *iter;
    if (++iter != t.tys.end()) os << ",";
  }
  os << "}";
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::Tuple &t) {
  os << "#(";
  for (auto iter = t.t.begin(); iter != t.t.end();) {
    os << *iter;
    if (++iter != t.t.end()) os << ",";
  }
  os << ")";
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::String &s) {
  os << (s.nul ? "cstr" : "str");
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::Cyclic &c) {
  os << "#{" << c.ty << "}";
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::CyclicRef &r) {
  os << "#" << r.depth;
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::FfiFn &f) {
  os << "ffifn<" << f.args << ", " << f.ret << ">";
}
static void printIt(std::ostream &os, [[maybe_unused]] Tp ty, const Ty::Undetermined &u) {
  os << "?{";
  for (auto it = u.ref.begin(); it != u.ref.end();) {
    os << *it;
    if (++it != u.ref.end()) os << ",";
  }
  os << "}";
}

std::ostream &operator<<(std::ostream &os, Tp ty) {
  std::visit([&](const auto &t) { printIt(os, ty, t); }, ty->v);
  return os;
}

namespace type::print {
  void printTy(type::Tp t) {
    std::cerr << t << std::endl;
  }
}
