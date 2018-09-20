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

val int_of_ode_var_id: ode_var_id -> int
val string_of_array_name: variable -> string
