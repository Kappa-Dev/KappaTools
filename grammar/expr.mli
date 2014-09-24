open Ast

val print_alg : out_channel -> alg_expr -> unit
val alg_to_string : unit -> alg_expr -> string

val print_bool : out_channel -> bool_expr -> unit
val bool_to_string : unit -> bool_expr -> string
