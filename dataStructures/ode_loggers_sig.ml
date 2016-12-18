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


let is_expr_alias expr =
  match
    expr
  with
  | Alg_expr.STATE_ALG_OP _,_
  | Alg_expr.TOKEN_ID _,_
  | Alg_expr.KAPPA_INSTANCE _,_
  | Alg_expr.CONST _,_
  | Alg_expr.IF _,_
  | Alg_expr.BIN_ALG_OP _,_
  | Alg_expr.UN_ALG_OP _,_ -> false
  | Alg_expr.ALG_VAR _,_ -> true

let string_of_variable var =
  match var with
  | Rate int -> Printf.sprintf "k(%i)" int
  | Rated int -> Printf.sprintf "kd(%i)" int
  | Rateun int -> Printf.sprintf "kun(%i)" int
  | Rateund int -> Printf.sprintf "kdun(%i)" int
  | Expr int -> Printf.sprintf "var(%i)" int
  | Obs int -> Printf.sprintf "obs(%i)" int
  | Init int -> Printf.sprintf "init(%i)" int
  | Initbis int -> Printf.sprintf "Init(%i)" int
  | Concentration int -> Printf.sprintf "y(%i)" int
  | Deriv int -> Printf.sprintf "dydt(%i)" int
  | Jacobian (int1,int2) -> Printf.sprintf "Jac(%i,%i)" int1 int2
  | Tinit -> "tinit"
  | Tend -> "tend"
  | InitialStep -> "initialstep"
  | Period_t_points -> "period_t_point"
  | N_ode_var -> "nodevar"
  | N_var -> "nvar"
  | N_obs -> "nobs"
  | N_rules -> "nrules"
  | N_rows -> "nrows"
  | Tmp -> "tmp"
  | Current_time -> "t"

let string_of_array_name var =
  match var with
  | Rate _ -> "k"
  | Rated _ -> "kd"
  | Rateun _ -> "kun"
  | Rateund _ -> "kdun"
  | Expr _ -> "var"
  | Obs _ -> "obs"
  | Init _ -> "init"
  | Initbis _ -> "Init"
  | Concentration _ -> "y"
  | Deriv _ -> "dydt"
  | Jacobian _ -> "Jac"
  | Tinit -> "tinit"
  | Tend -> "tend"
  | InitialStep -> "initialstep"
  | Period_t_points -> "period_t_point"
  | N_ode_var -> "nodevar"
  | N_var -> "nvar"
  | N_obs -> "nobs"
  | N_rows -> "nrows"
  | N_rules -> "nrules"
  | Tmp -> "tmp"
  | Current_time -> "t"
