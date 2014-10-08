val print_ast_alg : out_channel -> Ast.mixture Ast.ast_alg_expr -> unit
val ast_alg_to_string : unit -> Ast.mixture Ast.ast_alg_expr -> string

val print_bool :
  (out_channel -> 'a -> unit) -> out_channel -> 'a Ast.bool_expr -> unit
val bool_to_string :
  (unit -> 'a -> string) -> unit -> 'a Ast.bool_expr -> string

val print_ast_bool :
  out_channel -> Ast.mixture Ast.ast_alg_expr Ast.bool_expr -> unit
val ast_bool_to_string :
  unit -> Ast.mixture Ast.ast_alg_expr Ast.bool_expr -> string
