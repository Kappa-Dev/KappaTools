(** Printers (user readable) of Kappa compiled units *)

val alg_expr : ?env:Environment.t -> Format.formatter -> Alg_expr.t -> unit

val print_expr_val :
  ('a -> Nbr.t) -> Format.formatter -> 'a Ast.print_expr list -> unit

val elementary_rule :
  ?env:Environment.t -> Format.formatter -> Primitives.elementary_rule -> unit
val modification :
  ?env:Environment.t -> Format.formatter -> Primitives.modification -> unit
val perturbation :
  ?env:Environment.t -> Format.formatter -> Primitives.perturbation -> unit

val env :
  Format.formatter -> Environment.t -> unit
