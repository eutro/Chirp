#include <stdio.h>

void putF(double d) {
  printf("%f", d);
}

void putI(long long i) {
  printf("%lld", i);
}

void println() {
  putchar('\n');
}

long long getC() {
  return getchar();
}

void putC(long long c) {
  putchar((int) c);
}

struct String {
  unsigned long long len;
  char *bytes;
};

void putS(struct String s) {
  fwrite(s.bytes, sizeof(char), s.len * sizeof(char), stdout);
}
