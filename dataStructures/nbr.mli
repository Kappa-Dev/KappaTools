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
val to_float : t -> float option
(** [None] when infinity or Not a Number *)

val to_int : t -> int
val zero : t
val is_zero : t -> bool
val one : t
val is_strictly_positive : t -> bool
val print : Format.formatter -> t -> unit

val print_option : Format.formatter -> t -> unit
(** Prints nothing in case of infinity or Not a Number *)

val iteri : (t -> 'a -> 'a) -> 'a -> t -> 'a
(** [iter f x n]
@return f (n - k) (... (f (n - 1) (f n x))) where k < n <= k+1 *)

val to_string : t -> string
val of_string : string -> t
(** @raise Failure "float_of_string" *)

val to_json : t -> Yojson.Basic.json
val of_json : Yojson.Basic.json -> t
(** @raise Yojson.Basic.Util.Type_error if incorrect *)

val of_un_alg_op : Operator.un_alg_op -> t -> t
val of_bin_alg_op : Operator.bin_alg_op -> t -> t -> t
val of_compare_op : Operator.compare_op -> t -> t -> bool
