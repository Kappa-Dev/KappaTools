(** Kappa numbers (either float, int or int64) and operations on them *)

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
val print : Format.formatter -> t -> unit
val to_string : t -> string

(** [iter f x n]
returns f (n - k) (... (f (n - 1) (f n x))) where k < n <= k+1 *)
val iteri : (t -> 'a -> 'a) -> 'a -> t -> 'a

val of_un_alg_op : Term.un_alg_op -> t -> t
val of_bin_alg_op : Term.bin_alg_op -> t -> t -> t
val of_compare_op : Term.compare_op -> t -> t -> bool

val getMaxEventValue : unit -> t
val getMaxTimeValue : unit -> t
val getPointNumberValue : unit -> t
