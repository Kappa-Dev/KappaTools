(**
  * graph_loggers.ml
  *
  * a module for KaSim
  * Jérôme Feret, projet Antique, INRIA Paris
  *
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * Creation: 23/05/2016
  * Last modification: 25/05/2016
  * *
  *
  *
  * Copyright 2016  Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type variable =
  | Expr of int
  | Init of int
  | Deriv of int
  | Obs of int
  | Jacobian of int * int
  | Tinit
  | Tend
  | InitialStep
  | Num_t_points
  | Rate of int
  | Rated of int
  | Rateun of int
  | Rateund of int
  | N_rules
  | N_ode_var
  | N_obs
  | Tmp


type ('a,'b) network_handler =
  {
    int_of_obs: 'b -> int;
    int_of_kappa_instance:  'a -> int;
    int_of_token_id: 'b -> int;
  }

type options =
  | Comment of string


val print_ode_preamble:
  Loggers.t ->
  ?filter_in:Loggers.encoding list option ->
  ?filter_out:Loggers.encoding list -> unit ->
  unit

val declare_global: Loggers.t -> variable -> unit
val print_options: Loggers.t -> unit
val print_license_check: Loggers.t -> unit
val print_integrate: Loggers.t -> unit
val print_interpolate: Loggers.t -> unit
val print_dump_plots: Loggers.t -> unit

val initialize: Loggers.t -> variable -> unit
val associate: ?init_mode:bool -> Loggers.t -> variable -> ('a,'b) Ast.ast_alg_expr Location.annot -> ('a,'b) network_handler -> unit
val increment: ?init_mode:bool -> Loggers.t -> variable -> ('a,'b) Ast.ast_alg_expr Location.annot -> ('a,'b) network_handler -> unit
val associate_nrows: Loggers.t -> unit

val consume: Loggers.t -> variable -> nauto_in_species:int -> nauto_in_lhs:int -> variable -> variable list -> unit

val product: Loggers.t -> variable -> nauto_in_species:int -> nauto_in_lhs:int -> variable -> variable list -> unit

val print_comment:
  Loggers.t ->
  ?filter_in:Loggers.encoding list option ->
  ?filter_out:Loggers.encoding list ->
  string -> unit

val open_procedure: Loggers.t -> string -> string -> string list -> unit
val return: Loggers.t -> string -> unit
val close_procedure: Loggers.t -> unit
