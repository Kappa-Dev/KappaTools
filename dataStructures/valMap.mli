module type Content =
  sig
    type t
    val to_f: t -> float
  end

module type ValMap =
  sig
    type key = int
    type tree
    type content
    val print : Format.formatter -> tree -> unit
    val size : tree -> int
    val random_val : tree -> (key*content)
    val empty: tree
    val is_empty: tree -> bool
    val add: key -> content -> tree -> tree
    val remove: key -> tree -> tree
    val find : key -> tree -> content
    val total : tree -> float

    val fold: (key -> content -> 'a -> 'a) -> tree -> 'a -> 'a
    val iter: (key -> content -> unit) -> tree -> unit
  end

module Make(C:Content):ValMap with type content=C.t
