#include "Runtime.h"
#include <stdio.h>

Unit putf(double d) {
  printf("%f", d);
  return (Unit) {};
}

Unit puti(long long i) {
  printf("%lld", i);
  return (Unit) {};
}

Unit println() {
  puts("");
  return (Unit) {};
}
