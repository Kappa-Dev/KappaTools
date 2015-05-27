(** Algebraic and boolean term management *)

(** {6 Printers} *)

val print_ast_mix : Format.formatter -> Ast.mixture -> unit
val print_ast_alg : Format.formatter -> Ast.mixture Ast.ast_alg_expr -> unit

val print_bool :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a Ast.bool_expr -> unit
val print_ast_bool :
  Format.formatter -> Ast.mixture Ast.ast_alg_expr Ast.bool_expr -> unit

type alg_expr =
    BIN_ALG_OP of
      Term.bin_alg_op * alg_expr Term.with_pos * alg_expr Term.with_pos
  | UN_ALG_OP of Term.un_alg_op * alg_expr Term.with_pos
  | STATE_ALG_OP of Term.state_alg_op
  | ALG_VAR of int
  | KAPPA_INSTANCE of Connected_component.t array list
  | TOKEN_ID of int
  | CONST of Nbr.t

(** {6 Compilers } *)

(** [compile_alg variable_map token_map (fresh_mix_id, mix_list) alg_pos] *)
val compile_alg :
  ?label:string -> int Mods.StringMap.t -> int Mods.StringMap.t ->
  ?max_allowed_var:int ->
  (string list * (string * string) list) Export_to_KaSim.String2Map.t ->
  Connected_component.Env.t ->
  Ast.mixture Ast.ast_alg_expr Term.with_pos ->
  Connected_component.Env.t * alg_expr Term.with_pos
val compile_bool :
  int Mods.StringMap.t -> int Mods.StringMap.t ->
  (string list * (string * string) list) Export_to_KaSim.String2Map.t ->
  Connected_component.Env.t ->
  Ast.mixture Ast.ast_alg_expr Ast.bool_expr Term.with_pos ->
  Connected_component.Env.t * alg_expr Ast.bool_expr Term.with_pos

val deps_of_alg_expr : alg_expr -> Term.DepSet.t
val deps_of_bool_expr : alg_expr Ast.bool_expr -> (Term.DepSet.t * Nbr.t list)
