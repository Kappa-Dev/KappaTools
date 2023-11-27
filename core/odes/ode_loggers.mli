(**
  * graph_loggers.ml
  *
  * a module for KaSim
  * Jérôme Feret, projet Antique, INRIA Paris
  *
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * Creation: 22/07/2016
  * Last modification: Time-stamp: <Jul 25 2016>* *
  *
  *
  * Copyright 2016  Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type correct = Div of int | Mul of int | Nil
type options = Comment of string

val print_ode_preamble :
  Ode_loggers_sig.t ->
  (Ode_loggers_sig.t -> unit) ->
  may_be_not_time_homogeneous:bool ->
  count:Ode_args.count ->
  rule_rate_convention:Remanent_parameters_sig.rate_convention ->
  ?reaction_rate_convention:Remanent_parameters_sig.rate_convention ->
  ?filter_in:Loggers.encoding list option ->
  ?filter_out:Loggers.encoding list ->
  unit ->
  unit

val declare_global : Ode_loggers_sig.t -> Ode_loggers_sig.variable -> unit

val print_options :
  compute_jacobian:bool ->
  pos:(int -> bool) ->
  nodevar:int ->
  Ode_loggers_sig.t ->
  unit

val print_license_check : Ode_loggers_sig.t -> unit
val print_integrate : nobs:int -> nodevar:int -> Ode_loggers_sig.t -> unit
val print_interpolate : Ode_loggers_sig.t -> unit

val print_dump_plots :
  nobs:int ->
  data_file:string ->
  command_line:string ->
  titles:string list ->
  Ode_loggers_sig.t ->
  unit

val initialize :
  nodevar:int -> Ode_loggers_sig.t -> Ode_loggers_sig.variable -> unit

val associate :
  propagate_constants:bool ->
  ?init_mode:bool ->
  ?comment:string ->
  (int -> string) ->
  Ode_loggers_sig.t ->
  Ode_loggers_sig.t ->
  Loggers.t ->
  Ode_loggers_sig.variable ->
  (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Alg_expr.e
  Loc.annoted ->
  (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Network_handler.t ->
  unit

val increment :
  ?init_mode:bool ->
  ?comment:string ->
  (int -> string) ->
  Ode_loggers_sig.t ->
  Loggers.t ->
  Ode_loggers_sig.variable ->
  (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Alg_expr.e
  Loc.annoted ->
  (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Network_handler.t ->
  unit

val associate_nrows : Ode_loggers_sig.t -> unit
val associate_t : Ode_loggers_sig.t -> int -> unit
val init_time : Ode_loggers_sig.t -> int -> unit
val start_time : Ode_loggers_sig.t -> float -> unit
val declare_init : ?comment:string -> Ode_loggers_sig.t -> int -> unit
val associate_nonnegative : Ode_loggers_sig.t -> bool -> unit
val show_time_advance : Ode_loggers_sig.t -> unit
val launch_main : Ode_loggers_sig.t -> unit

val consume :
  Ode_loggers_sig.t ->
  Ode_loggers_sig.variable ->
  nauto_in_species:int ->
  nauto_in_lhs:int ->
  nocc:int ->
  Ode_loggers_sig.variable ->
  (Ode_loggers_sig.variable * correct) list ->
  unit

val produce :
  Ode_loggers_sig.t ->
  Ode_loggers_sig.variable ->
  nauto_in_species:int ->
  nauto_in_lhs:int ->
  nocc:int ->
  Ode_loggers_sig.variable ->
  (Ode_loggers_sig.variable * correct) list ->
  unit

val consume_jac :
  Ode_loggers_sig.t ->
  Ode_loggers_sig.variable ->
  nauto_in_species:int ->
  nauto_in_lhs:int ->
  nocc:int ->
  Ode_loggers_sig.variable ->
  (int * correct) list ->
  Mods.IntSet.t ->
  unit

val produce_jac :
  Ode_loggers_sig.t ->
  Ode_loggers_sig.variable ->
  nauto_in_species:int ->
  nauto_in_lhs:int ->
  nocc:int ->
  Ode_loggers_sig.variable ->
  (int * correct) list ->
  Mods.IntSet.t ->
  unit

val update_token_jac :
  Ode_loggers_sig.t ->
  Ode_loggers_sig.variable ->
  nauto_in_lhs:int ->
  nocc:int ->
  Ode_loggers_sig.variable ->
  Ode_loggers_sig.variable ->
  (Ode_loggers_sig.ode_var_id * correct) list ->
  Mods.IntSet.t ->
  dep_mixture:Mods.IntSet.t ->
  dep_token:Mods.IntSet.t ->
  unit

val update_token :
  Ode_loggers_sig.t ->
  Ode_loggers_sig.variable ->
  nauto_in_lhs:int ->
  nocc:int ->
  Ode_loggers_sig.variable ->
  Ode_loggers_sig.variable ->
  (Ode_loggers_sig.variable * correct) list ->
  unit

val print_newline : Ode_loggers_sig.t -> unit

val print_comment :
  ?breakline:bool ->
  Ode_loggers_sig.t ->
  ?filter_in:Loggers.encoding list option ->
  ?filter_out:Loggers.encoding list ->
  string ->
  unit

val open_procedure :
  Ode_loggers_sig.t -> string -> string -> string list -> unit

val return : Ode_loggers_sig.t -> string -> unit
val close_procedure : Ode_loggers_sig.t -> unit

val smash_reactions :
  Loggers.encoding -> Remanent_parameters_sig.parameters -> bool

val print_alg_expr_few_parenthesis :
  ?init_mode:bool ->
  (int -> string) ->
  Ode_loggers_sig.t ->
  Loggers.t ->
  (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Alg_expr.e
  Loc.annoted ->
  (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Network_handler.t ->
  unit

val is_time :
  (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Alg_expr.e
  Loc.annoted ->
  bool
