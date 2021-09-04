IMPL_OPS(TraitBound, (i, s), (o.i, o.s));

IMPL_SINGLETON(Ty::Err);
IMPL_SINGLETON(Ty::Bool);
IMPL_OPS(Ty::Int, (s), (o.s));
IMPL_OPS(Ty::UInt, (s), (o.s));
IMPL_OPS(Ty::Float, (s), (o.s));
IMPL_OPS(Ty::Placeholder, (i), (o.i));
IMPL_OPS(Ty::ADT, (i, v, s), (o.i, o.v, o.s));
IMPL_OPS(Ty::Dyn, (t), (o.t));
IMPL_OPS(Ty::Tuple, (t), (o.t));
IMPL_OPS(Ty::TraitRef, (ty, trait, ref), (o.ty, o.trait, o.ref));
IMPL_SINGLETON(Ty::String);
IMPL_OPS(Ty::Cyclic, (ty), (o.ty));
IMPL_OPS(Ty::CyclicRef, (depth), (o.depth));

IMPL_OPS(Ty, (v), (o.v));
