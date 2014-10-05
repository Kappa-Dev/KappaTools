open Ast

val print_ast_alg : out_channel -> mixture ast_alg_expr -> unit
val ast_alg_to_string : unit -> mixture ast_alg_expr -> string

val print_bool : out_channel -> mixture ast_bool_expr -> unit
val bool_to_string : unit -> mixture ast_bool_expr -> string
