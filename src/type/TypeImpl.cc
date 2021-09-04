#include "Type.h"

#define IMPL_OPS(TYPE, LHSA, RHSA)                               \
  bool type::TYPE::operator<(const type::TYPE &o) const {        \
    return std::make_tuple LHSA < std::make_tuple RHSA; }        \
  bool type::TYPE::operator==(const type::TYPE &o) const {       \
    return std::make_tuple LHSA == std::make_tuple RHSA; }
#define IMPL_SINGLETON(TYPE)                                            \
  bool type::TYPE::operator<(const type::TYPE &o) const { return false; } \
  bool type::TYPE::operator==(const type::TYPE &o) const { return true; }
#include "TypeImpl.h"
