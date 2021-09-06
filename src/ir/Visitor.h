#pragma once

#include <any>
#include <string>
#include <tuple>
#include <functional>
#include <utility>

namespace visitor {
  template <typename T>
  class Holder {
  public:
    T value;

    Holder(const Holder<T> &o):
      value(std::move(const_cast<Holder<T>&>(o).value))
    {}
    Holder(T &&v): value(std::forward<T>(v)) {}
  };

  class MovingAny {
  public:
    std::any value;

    template <typename T>
    MovingAny(T &&v): value(Holder(std::forward<T>(v))) {}

    template <typename T>
    T cast() {
      return std::move(std::any_cast<Holder<T>>(value).value);
    }
  };

  template <typename T>
  T any_cast(MovingAny &&any) {
    return std::move(std::any_cast<Holder<T>>(std::move(any.value)).value);
  }
}

#define _acceptDef(NAME)                                                \
  visitor::MovingAny accept##NAME(Erased##NAME##Visitor &v, visitor::MovingAny &&args)
#define _acceptImpl(NAME, TYPE)                                         \
  visitor::MovingAny TYPE::accept##NAME(Erased##NAME##Visitor &v, visitor::MovingAny &&args) { \
    return v._Evisit##TYPE(*this, std::forward<visitor::MovingAny>(args)); }

#define _Evisit(TYPE) visitor::MovingAny _Evisit##TYPE(TYPE &it, visitor::MovingAny &&args)
#define _EvisitVirtual(TYPE) virtual _Evisit(TYPE) = 0;
#define _EvisitImpl(TYPE) _Evisit(TYPE) override {                      \
    return std::apply                                                   \
      ([this, &it](std::remove_reference_t<Arg> *...arg) { return visit##TYPE(it, *arg...); }, \
       visitor::any_cast<std::tuple<std::remove_reference_t<Arg> *...>>(std::forward<visitor::MovingAny>(args))); }

#define _typedVisit(TYPE) Ret visit##TYPE(TYPE &it, Arg... args)
#define _typedRoot(TYPE) _typedVisit(TYPE) {                            \
    return visitor::any_cast<Ret>(it.accept##TYPE(*this, std::make_tuple(&args...))); }
