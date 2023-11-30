type t = { num: int; den: int }

val add : t -> t -> t
val sub : t -> t -> t
val mult : t -> t -> t
val inv : t -> t option
val div : t -> t -> t option
val is_equal : t -> t -> bool
val of_int : int -> t
val is_zero : t -> bool
val one : t
val zero : t
