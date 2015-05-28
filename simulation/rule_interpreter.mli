type t

val print_heap : Format.formatter -> ValMap.tree -> unit

val empty : Environment.t -> t

val value_alg :
  get_alg:(int -> Expr.alg_expr) -> Mods.Counter.t -> t ->
  Expr.alg_expr -> Nbr.t
val value_bool :
  get_alg:(int -> Expr.alg_expr) -> Mods.Counter.t -> t ->
  Expr.alg_expr Ast.bool_expr -> bool
val apply_rule :
  get_alg:(int -> Expr.alg_expr) -> Connected_component.Env.t -> Mods.Counter.t
  -> t -> Primitives.elementary_rule -> t option
val force_rule :
  get_alg:(int -> Expr.alg_expr) -> Connected_component.Env.t -> Mods.Counter.t
  -> t -> Primitives.elementary_rule ->
  (t  * Connected_component.Matching.t list option)

val print : Environment.t -> Format.formatter -> t -> unit
