val value_alg :
  Mods.Counter.t -> ?time:float -> get_alg:(int -> Expr.alg_expr) ->
  get_mix:(Connected_component.t array list -> Nbr.t) ->
  get_tok:(int -> Nbr.t) -> Expr.alg_expr -> Nbr.t
val value_bool :
  Mods.Counter.t -> ?time:float -> get_alg:(int -> Expr.alg_expr) ->
  get_mix:(Connected_component.t array list -> Nbr.t) ->
  get_tok:(int -> Nbr.t) -> Expr.alg_expr Ast.bool_expr -> bool
