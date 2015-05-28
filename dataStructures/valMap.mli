type key = int
type tree

val print : Format.formatter -> tree -> unit
val size : tree -> int
val random_val : tree -> key
val empty: tree
val is_empty: tree -> bool
val add: key -> tree -> tree
val remove: key -> tree -> tree
val mem : key -> tree -> bool
val total : tree -> int

val fold: (key -> 'a -> 'a) -> tree -> 'a -> 'a
val iter: (key -> unit) -> tree -> unit
