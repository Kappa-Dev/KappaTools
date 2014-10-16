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

type alg_expr =
    BIN_ALG_OP of
      Term.bin_alg_op * alg_expr Term.with_pos * alg_expr Term.with_pos
  | UN_ALG_OP of Term.un_alg_op * alg_expr Term.with_pos
  | STATE_ALG_OP of Term.state_alg_op
  | ALG_VAR of int
  | KAPPA_INSTANCE of int
  | TOKEN_ID of int
  | CONST of Nbr.t

(** [compile_alg variable_map token_map (fresh_mix_id, mix_list) alg_pos] *)
val compile_alg :
  (int * 'b) Mods.StringMap.t -> int Mods.StringMap.t -> ?max_allowed_var:int ->
  int * 'a list -> 'a Ast.ast_alg_expr Term.with_pos ->
  (int * 'a list) * alg_expr Term.with_pos
val compile_bool :
  (int * 'b) Mods.StringMap.t -> int Mods.StringMap.t -> int * 'a list ->
  'a Ast.ast_alg_expr Ast.bool_expr Term.with_pos ->
  (int * 'a list) * alg_expr Ast.bool_expr Term.with_pos

val deps_of_alg_expr : alg_expr -> Term.DepSet.t
val deps_of_bool_expr : alg_expr Ast.bool_expr -> (Term.DepSet.t * Nbr.t option)
