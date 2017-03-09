type ode_var_id = int

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
  | Time_scale_factor

val is_expr_const: (ode_var_id,int) Alg_expr.e Locality.annot -> bool
val is_expr_time_homogeneous: ('a,int) Alg_expr.e Locality.annot -> bool
val is_expr_alias: (ode_var_id,int) Alg_expr.e Locality.annot -> ode_var_id option
val is_bool_const: (ode_var_id,int) Alg_expr.bool Locality.annot -> bool
val int_of_ode_var_id: ode_var_id -> int
val string_of_variable: variable -> string
val string_of_array_name: variable -> string
