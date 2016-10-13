(**
  * export.mli
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: June 30 2016
  * Last modification: Time-stamp: <Oct 13 2016>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type state
type parameters = Remanent_parameters_sig.parameters
type errors = Exception.method_handler
type internal_contact_map

type internal_influence_map =
  Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t *
  Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t

type handler = Cckappa_sig.kappa_handler

type c_compilation = Cckappa_sig.compil

type reachability_analysis

type ode_flow

type ctmc_flow

val init:
  unit -> state

val get_parameters: state -> parameters

val set_parameters: parameters -> state -> state

val get_contact_map:
  ?accuracy_level:Remanent_state.accuracy_level ->
  state -> state * internal_contact_map

val get_handler: state -> state * handler

val get_errors: state -> errors

val set_errors: errors -> state -> state

val get_c_compilation: state -> state * c_compilation

val get_influence_map:
  ?accuracy_level:Remanent_state.accuracy_level ->
  state -> state * internal_influence_map

val get_reachability_analysis: state -> state * reachability_analysis

val get_ctmc_flow: state -> state * ctmc_flow

val get_ode_flow: state -> state * ode_flow

val dump_c_compil: state -> c_compilation -> state

val output_contact_map: ?loggers:Loggers.t -> ?accuracy_level:Remanent_state.accuracy_level -> state -> state

val output_influence_map: ?loggers:Loggers.t -> ?accuracy_level:Remanent_state.accuracy_level -> state -> state
