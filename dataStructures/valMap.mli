(** Implementation of Set using [Pervasive.compare] and providing [random] *)

type 'a tree

val print :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a tree -> unit
val size : 'a tree -> int
val random_val : 'a tree -> 'a
val empty: 'a tree
val is_empty: 'a tree -> bool
val add: 'a -> 'a tree -> 'a tree
val remove: 'a -> 'a tree -> 'a tree
val mem : 'a -> 'a tree -> bool
val total : 'a tree -> int

val fold: ('b -> 'a -> 'a) -> 'b tree -> 'a -> 'a
val iter: ('b -> unit) -> 'b tree -> unit
