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
