val lnk_t : Environment.t -> Format.formatter -> Mixture.lnk_t -> unit

val mixture : bool -> Environment.t -> Format.formatter -> Mixture.t -> unit

val alg_expr : Environment.t -> Format.formatter -> Expr.alg_expr -> unit

val print_expr_val :
  Environment.t -> (Environment.t -> 'a -> Nbr.t) -> Format.formatter ->
  'a Ast.print_expr Term.with_pos list -> unit

val modification :
  Environment.t -> Format.formatter -> Primitives.modification -> unit
val perturbation :
  Environment.t -> Format.formatter -> Primitives.perturbation -> unit
