type t = F of float | I of int | I64 of Int64.t
val compare : t -> t -> int
val is_greater : t -> t -> bool
val is_smaller : t -> t -> bool
val is_equal : t -> t -> bool
val add : t -> t -> t
val sub : t -> t -> t
val mult : t -> t -> t
val min : t -> t -> t
val max : t -> t -> t
val succ : t -> t
val pred : t -> t
val neg : t -> t
val to_float : t -> float
val to_int : t -> int
val is_zero : t -> bool
val is_strictly_positive : t -> bool
val print : out_channel -> t -> unit
val to_string : t -> string
val iteri : (t -> 'a -> 'a) -> 'a -> t -> 'a

val of_un_alg_op : Term.un_alg_op -> t -> t
val of_bin_alg_op : Term.bin_alg_op -> t -> t -> t
