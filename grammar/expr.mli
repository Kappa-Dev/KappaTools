open Ast

val print_ast_alg : out_channel -> ast_alg_expr -> unit
val ast_alg_to_string : unit -> ast_alg_expr -> string

val print_bool : out_channel -> ast_bool_expr -> unit
val bool_to_string : unit -> ast_bool_expr -> string
