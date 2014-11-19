val lnk_t : Environment.t -> Format.formatter -> Mixture.lnk_t -> unit

val mixture : bool -> Environment.t -> Format.formatter -> Mixture.t -> unit

val alg_expr : Environment.t -> Format.formatter -> Expr.alg_expr -> unit

val modification :
  Environment.t -> Format.formatter -> Primitives.modification -> unit
val perturbation :
  Environment.t -> Format.formatter -> Primitives.perturbation -> unit
