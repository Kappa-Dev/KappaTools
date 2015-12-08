(** Algebraic expression computation *)
(** As soon as you've got an graph available, I'll probably prefer use
{!module:Rule_interpreter}.value_* *)

val value_alg :
  Counter.t -> ?time:float -> get_alg:(int -> Alg_expr.t) ->
  get_mix:(Connected_component.t array list -> Nbr.t) ->
  get_tok:(int -> Nbr.t) -> Alg_expr.t -> Nbr.t
(** [value_alg c ?t get_alg get_mix get_tok expr_alg] with [get_mix [interp1;...;interpn]] *)

val value_bool :
  Counter.t -> ?time:float -> get_alg:(int -> Alg_expr.t) ->
  get_mix:(Connected_component.t array list -> Nbr.t) ->
  get_tok:(int -> Nbr.t) -> Alg_expr.t Ast.bool_expr -> bool
