#include <stdio.h>

void putf(double d) {
  printf("%f", d);
}

void puti(long long i) {
  printf("%lld", i);
}

void println() {
  puts("");
}

long long getC() {
  return getchar();
}

void putC(long long c) {
  putchar((int) c);
}
