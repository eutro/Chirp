#include <cassert>
#include "../type/Type.h"

using namespace type;

int main() {
  Tcx tcx;
  Tp ph1 = tcx.intern(Ty::Placeholder{0});
  assert(ph1->countFree(ph1));
  Tp tup1 = tcx.intern(Ty::Tuple{{ph1}});
  assert(tup1->countFree(ph1));
  Tp cyclic = maybeCycle(tup1, ph1);
  assert(cyclic == tcx.intern(Ty::Cyclic{tcx.intern(Ty::Tuple{{tcx.intern(Ty::CyclicRef{0})}})}));
  assert(replaceTy(tup1, ph1, cyclic) == uncycle(cyclic));
}
