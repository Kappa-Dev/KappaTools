(**[value_alg c ?t get_alg get_mix get_tok expr_alg]* with [get_mix [interp1;...;interpn]]*)
val value_alg :
  Mods.Counter.t -> ?time:float -> get_alg:(int -> Expr.alg_expr) ->
  get_mix:(Connected_component.t array list -> Nbr.t) ->
  get_tok:(int -> Nbr.t) -> Expr.alg_expr -> Nbr.t

val value_bool :
  Mods.Counter.t -> ?time:float -> get_alg:(int -> Expr.alg_expr) ->
  get_mix:(Connected_component.t array list -> Nbr.t) ->
  get_tok:(int -> Nbr.t) -> Expr.alg_expr Ast.bool_expr -> bool
