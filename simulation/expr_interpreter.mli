(**[value_alg c ?t get_alg get_mix get_tok expr_alg]* with [get_mix [interp1;...;interpn]]*)
val value_alg :
  Mods.Counter.t -> ?time:float -> get_alg:(int -> Alg_expr.t) ->
  get_mix:(Connected_component.t array list -> Nbr.t) ->
  get_tok:(int -> Nbr.t) -> Alg_expr.t -> Nbr.t

val value_bool :
  Mods.Counter.t -> ?time:float -> get_alg:(int -> Alg_expr.t) ->
  get_mix:(Connected_component.t array list -> Nbr.t) ->
  get_tok:(int -> Nbr.t) -> Alg_expr.t Ast.bool_expr -> bool
