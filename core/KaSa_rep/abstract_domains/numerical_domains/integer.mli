type unbounded = Bounded of int | Infinity

val plus : unbounded -> unbounded -> unbounded
val minl : unbounded list -> unbounded
val max : unbounded -> unbounded -> unbounded
val min : unbounded -> unbounded -> unbounded
val div2 : unbounded -> unbounded
val p : unbounded -> unbounded -> bool
