type ode_var_id = int

type variable =
  | Expr of int
  | Init of int
  | Initbis of int
  | Concentration of int
  | Deriv of int
  | Obs of int
  | Jacobian of int * int
  | Tinit
  | Tend
  | InitialStep
  | Period_t_points
  | Rate of int
  | Rated of int
  | Rateun of int
  | Rateund of int
  | N_rules
  | N_ode_var
  | N_var
  | N_obs
  | N_rows
  | Tmp
  | Current_time


let rec is_expr_const expr = (* constant propagation is already done *)
  (* Yes, but I prefer not to rely on assumptions that may change *)
  match
    expr
  with
  | Alg_expr.CONST _,_ -> true
  | Alg_expr.IF (a,b,c),_ ->
    is_bool_const a && is_expr_const b && is_expr_const c
  | Alg_expr.BIN_ALG_OP (_,a,b),_ ->
    is_expr_const a && is_expr_const b
  | Alg_expr.UN_ALG_OP _,_
  | Alg_expr.ALG_VAR _,_
  | Alg_expr.STATE_ALG_OP _,_
  | Alg_expr.TOKEN_ID _,_
  | Alg_expr.KAPPA_INSTANCE _,_ -> false
and is_bool_const expr =
  match
    expr
  with
  | Alg_expr.TRUE,_ | Alg_expr.FALSE,_ -> true
  | Alg_expr.COMPARE_OP (_,a,b),_ ->
    is_expr_const a && is_expr_const b
  | Alg_expr.BOOL_OP (_,a,b),_ ->
    is_bool_const a && is_bool_const b
