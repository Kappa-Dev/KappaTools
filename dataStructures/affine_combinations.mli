type aff_combination

val sum: aff_combination -> aff_combination -> aff_combination
val mul_scal: int -> aff_combination -> aff_combination
val div_scal: aff_combination -> int -> aff_combination option
val linearise: (unit,unit) Alg_expr.e Locality.annot -> aff_combination
val of_int: int -> aff_combination
val necessarily_equal: aff_combination -> aff_combination -> bool
