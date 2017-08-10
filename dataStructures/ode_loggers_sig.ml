type ode_var_id = int

let int_of_ode_var_id i = i

type variable =
  | Expr of int
  | Init of int
  | Initbis of int
  | Concentration of int
  | Deriv of int
  | Obs of int
  | Jacobian of int * int
  | Jacobian_var of int * int
  | Tinit
  | Tend
  | InitialStep
  | MaxStep
  | RelTol
  | AbsTol
  | Period_t_points
  | Rate of int
  | Rated of int
  | Rateun of int
  | Rateund of int
  | Stochiometric_coef of int * int
  | Jacobian_stochiometric_coef of int * int * int
  | Jacobian_rate of int * int
  | Jacobian_rated of int * int
  | Jacobian_rateun of int * int
  | Jacobian_rateund of int * int
  | N_max_stoc_coef
  | N_rules
  | N_ode_var
  | N_var
  | N_obs
  | N_rows
  | Tmp
  | Current_time
  | Time_scale_factor
  | NonNegative


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
  | (Alg_expr.UN_ALG_OP (_,a)
    | Alg_expr.DIFF_KAPPA_INSTANCE (a,_)
    | Alg_expr.DIFF_TOKEN (a,_)),_ ->
    is_expr_const a
  | Alg_expr.ALG_VAR _,_
  | Alg_expr.STATE_ALG_OP _,_
  | Alg_expr.TOKEN_ID _,_
  | Alg_expr.KAPPA_INSTANCE _,_  -> false
and is_bool_const expr =
  match
    expr
  with
  | Alg_expr.TRUE,_ | Alg_expr.FALSE,_ -> true
  | Alg_expr.COMPARE_OP (_,a,b),_ ->
    is_expr_const a && is_expr_const b
  | Alg_expr.BIN_BOOL_OP (_,a,b),_ ->
    is_bool_const a && is_bool_const b
  | Alg_expr.UN_BOOL_OP (_,a),_ -> is_bool_const a

let rec is_expr_time_homogeneous expr =
  (* does not take into account symbolic propagation of expression *)
      match
        expr
      with
      | Alg_expr.CONST _,_ -> true
      | Alg_expr.IF (a,b,c),_ ->
        is_bool_time_homogeneous a && is_expr_time_homogeneous b && is_expr_time_homogeneous c
      | Alg_expr.BIN_ALG_OP (_,a,b),_ ->
        is_expr_time_homogeneous a && is_expr_time_homogeneous b
      | (Alg_expr.UN_ALG_OP (_,a)
        | Alg_expr.DIFF_KAPPA_INSTANCE (a,_)
        | Alg_expr.DIFF_TOKEN (a,_)), _ ->
        is_expr_time_homogeneous a
      | Alg_expr.STATE_ALG_OP
          ( Operator.EVENT_VAR
          | Operator.CPUTIME
          | Operator.NULL_EVENT_VAR
          | Operator.TMAX_VAR
          | Operator.EMAX_VAR) ,_
      | Alg_expr.ALG_VAR _,_
      | Alg_expr.TOKEN_ID _,_
      | Alg_expr.KAPPA_INSTANCE _,_ -> true
      | Alg_expr.STATE_ALG_OP (Operator.TIME_VAR),_ -> false

and is_bool_time_homogeneous expr =
      match
        expr
      with
      | Alg_expr.TRUE,_ | Alg_expr.FALSE,_ -> true
      | Alg_expr.COMPARE_OP (_,a,b),_ ->
        is_expr_time_homogeneous a && is_expr_time_homogeneous b
      | Alg_expr.BIN_BOOL_OP (_,a,b),_ ->
        is_bool_time_homogeneous a && is_bool_time_homogeneous b
      | Alg_expr.UN_BOOL_OP (_,a),_ ->
        is_bool_time_homogeneous a

let is_expr_alias expr =
  match
    expr
  with
  | (Alg_expr.STATE_ALG_OP _ | Alg_expr.TOKEN_ID _ | Alg_expr.KAPPA_INSTANCE _
    | Alg_expr.CONST _ | Alg_expr.IF _ | Alg_expr.BIN_ALG_OP _
    | Alg_expr.DIFF_KAPPA_INSTANCE _ | Alg_expr.DIFF_TOKEN _
    | Alg_expr.UN_ALG_OP _),_ -> None
  | Alg_expr.ALG_VAR x,_ -> Some x

let string_of_array_name var =
  match var with
  | NonNegative -> "nonnegative"
  | Rate _ -> "k"
  | Rated _ -> "kd"
  | Rateun _ -> "kun"
  | Rateund _ -> "kdun"
  | Stochiometric_coef _ -> "stoc"
  | Expr _ -> "var"
  | Obs _ -> "obs"
  | Init _ -> "init"
  | Initbis _ -> "Init"
  | Concentration _ -> "y"
  | Deriv _ -> "dydt"
  | Jacobian _ -> "jac"
  | Jacobian_var _ -> "jacvar"
  | Jacobian_rate _ -> "jack"
  | Jacobian_rated _ -> "jackd"
  | Jacobian_rateun _ -> "jackun"
  | Jacobian_rateund _ -> "jackund"
  | Jacobian_stochiometric_coef _ -> "jacstoc"
  | Tinit -> "tinit"
  | Tend -> "tend"
  | InitialStep -> "initialstep"
  | MaxStep -> "maxstep"
  | RelTol -> "reltol"
  | AbsTol -> "abstol"
  | Period_t_points -> "period"
  | N_ode_var -> "nodevar"
  | N_var -> "nvar"
  | N_obs -> "nobs"
  | N_rows -> "nrows"
  | N_rules -> "nrules"
  | N_max_stoc_coef -> "max_stoc_coef"
  | Tmp -> "tmp"
  | Current_time -> "t"
  | Time_scale_factor -> "t_correct_dimmension"
