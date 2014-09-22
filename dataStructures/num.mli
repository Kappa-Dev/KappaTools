type t = F of float | I of int | I64 of Int64.t
val cast_bin_op :
  op_f:(float -> float -> float) ->
  ?op_i:(int -> int -> int) ->
  ?op_i64:(Int64.t -> Int64.t -> Int64.t) -> t -> t -> t
val cast_un_op :
  ?op_f:(float -> float) ->
  ?op_i:(int -> int) -> ?op_i64:(Int64.t -> Int64.t) -> t -> t
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
val float_of_num : t -> float
val int_of_num : t -> int
val is_zero : t -> bool
val is_strictly_positive : t -> bool
val print : out_channel -> t -> unit
val to_string : t -> string
val iteri : (t -> 'a -> 'a) -> 'a -> t -> 'a
