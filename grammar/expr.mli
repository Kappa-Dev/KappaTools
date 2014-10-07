val print_ast_alg : out_channel -> Ast.mixture Ast.ast_alg_expr -> unit
val ast_alg_to_string : unit -> Ast.mixture Ast.ast_alg_expr -> string

val print_bool : out_channel -> Ast.mixture Ast.ast_bool_expr -> unit
val bool_to_string : unit -> Ast.mixture Ast.ast_bool_expr -> string
