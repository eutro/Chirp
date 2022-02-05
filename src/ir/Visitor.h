#pragma once

#include <any>
#include <string>
#include <tuple>
#include <functional>
#include <utility>
#include <memory>

namespace visitor {
  class MovingAny {
  public:
    void *value;

    template <typename T>
    MovingAny(T &&v): value((void *) new T(std::forward<T>(v))) {}
  };

  template <typename T>
  T any_cast(MovingAny &&any) {
    // unsafe here, but as long as we any_cast EXACTLY once it's all good
    T *tPtr = (T *) any.value;
    T moved = std::move(*tPtr);
    delete tPtr;
    return moved;
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
