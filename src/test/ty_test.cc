#include <cassert>
#include "../type/Type.h"

using namespace type;

int main() {
  Tcx tcx;
  Tp ph1 = tcx.intern(Ty::Placeholder{0});
  assert(ph1->countFree(ph1));
  Tp tup1 = tcx.intern(Ty::Tuple{{ph1}});
  assert(tup1->countFree(ph1));
  Tp cyclic1 = maybeCycle(tup1, ph1);
  assert(cyclic1 == tcx.intern(Ty::Cyclic{tcx.intern(Ty::Tuple{{tcx.intern(Ty::CyclicRef{0})}})}));
  assert(replaceTy(tup1, ph1, cyclic1) == uncycle(cyclic1));

  Tp ph2 = tcx.intern(Ty::Placeholder{1});
  Tp tup2 = tcx.intern(Ty::Tuple{{ph1, ph2}});
  Tp cyclic2 = maybeCycle(maybeCycle(tup2, ph1), ph2);
  assert(cyclic2 ==
         tcx.intern(Ty::Cyclic{
                 tcx.intern(Ty::Cyclic{
                     tcx.intern(Ty::Tuple{{tcx.intern(Ty::CyclicRef{0}),
                                           tcx.intern(Ty::CyclicRef{1})}})})}));
  std::cerr << cyclic2 << " -> " << uncycle(cyclic2) << " -> " << uncycle(uncycle(cyclic2));
}
