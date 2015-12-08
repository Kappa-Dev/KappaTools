(** Algebraic and boolean expression manager *)

(** {6 Compilers } *)

(** [compile_alg variable_map token_map (fresh_mix_id, mix_list) alg_pos] *)
val compile_alg :
  ?origin:Operator.rev_dep ->
  (string list * (string * string) list) Export_to_KaSim.String2Map.t ->
  Counter.t -> Connected_component.Env.t ->
  (LKappa.rule_mixture,int) Ast.ast_alg_expr Location.annot ->
  Connected_component.Env.t * Alg_expr.t Location.annot
val compile_pure_alg :
  Counter.t -> (LKappa.rule_mixture,int) Ast.ast_alg_expr Location.annot ->
  Alg_expr.t Location.annot
val compile_bool :
  (string list * (string * string) list) Export_to_KaSim.String2Map.t ->
  Counter.t -> Connected_component.Env.t ->
  (LKappa.rule_mixture,int) Ast.ast_alg_expr Ast.bool_expr Location.annot ->
  Connected_component.Env.t * Alg_expr.t Ast.bool_expr Location.annot

val stops_of_bool_expr :
  (Operator.DepSet.t * Operator.DepSet.t *
     Operator.DepSet.t array * Operator.DepSet.t array) ->
  Alg_expr.t Ast.bool_expr -> Nbr.t list
