type side = LHS | RHS
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

val int_of_ode_var_id : ode_var_id -> int
val string_of_array_name : variable -> string

type t

val extend_logger : csv_sep:string -> Loggers.t -> t
val get_encoding_format : t -> Loggers.encoding
val fprintf : t -> ('a, Format.formatter, unit) format -> 'a
val print_newline : t -> unit
val print_breakable_hint : t -> unit
val flush_buffer : t -> Format.formatter -> unit
val flush_logger : t -> unit
val has_forbidden_char : t -> string -> bool
val formatter_of_logger : t -> Format.formatter option
val string_of_un_op : t -> Operator.un_alg_op -> string
val string_of_bin_op : t -> Operator.bin_alg_op -> string
val string_of_compare_op : t -> Operator.compare_op -> string
val string_of_un_bool_op : t -> Operator.un_bool_op -> string
val string_of_bin_bool_op : t -> Operator.bin_bool_op -> string

val get_expr :
  t -> variable -> (ode_var_id, ode_var_id) Alg_expr.e Locality.annot option

val set_expr :
  t -> variable -> (ode_var_id, ode_var_id) Alg_expr.e Locality.annot -> unit

val is_const : t -> variable -> bool
val get_fresh_obs_id : t -> int
val get_fresh_reaction_id : t -> int
val get_fresh_meta_id : t -> int
val set_id_of_global_parameter : t -> variable -> string -> unit
val get_id_of_global_parameter : t -> variable -> string
val is_dangerous_ode_variable : t -> variable -> bool
val flag_dangerous : t -> variable -> string -> unit
val allocate_fresh_name : t -> string -> string -> string
val allocate : t -> string -> unit
val set_ode : mode:Loggers.encoding -> string -> unit
val get_ode : mode:Loggers.encoding -> string
val string_of_variable : side:side -> t -> variable -> string
val variable_of_derived_variable : variable -> ode_var_id -> variable
val csv_sep : t -> string
val lift : t -> Loggers.t
