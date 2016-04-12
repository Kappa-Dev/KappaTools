(*
  * analyzer_headers.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 30th of January
  * Last modification:
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

(** type declarations and values shared among the abstract domains *)

type compilation_result

(** type of the static information to be passed to each domain, let us
    start by this signature at the moment. In a first step, we are going
    to use only one module, and provide it with all the static information
    that you have computed and that you are using so far. Then, we will
    introduce a collection of independent modules, and dispatch this
    information between what is common, and what is specific to each
    domain.*)

type global_static_information
type global_dynamic_information

type kasa_state = unit

(** This is the type of the encoding of a chemical mixture as a result of
    compilation *)

type initial_state = Cckappa_sig.enriched_init

val initialize_global_information:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Mvbdu_wrapper.Mvbdu.handler ->
  Cckappa_sig.compil ->
  Cckappa_sig.kappa_handler ->
  Exception.method_handler * global_static_information * global_dynamic_information

val get_parameter: global_static_information -> Remanent_parameters_sig.parameters

val get_compilation_information: global_static_information -> compilation_result

val get_bdu_common_static : global_static_information -> Common_static.bdu_common_static

val set_bdu_common_static:
  Common_static.bdu_common_static ->
  global_static_information ->
  global_static_information

val get_agent_name :
  global_static_information ->
  Ckappa_sig.c_agent_name Ckappa_sig.RuleAgent_map_and_set.Map.t
    
val set_agent_name :
  Ckappa_sig.c_agent_name Ckappa_sig.RuleAgent_map_and_set.Map.t ->
  global_static_information ->
  global_static_information

val get_side_effects:
  global_static_information ->
  Common_static.half_break_action * Common_static.remove_action

val set_side_effects :
  Common_static.half_break_action * Common_static.remove_action -> 
  global_static_information ->
  global_static_information

val get_potential_side_effects :
  global_static_information -> 
  Common_static.potential_partner_free * Common_static.potential_partner_bind

val set_potential_side_effects:
  Common_static.potential_partner_free * Common_static.potential_partner_bind -> 
  global_static_information ->
  global_static_information

val get_bonds_rhs : 
  global_static_information ->
  Ckappa_sig.PairAgentSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t
     
val set_bonds_rhs :
  Ckappa_sig.PairAgentSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t ->
  global_static_information ->
  global_static_information

val get_bonds_lhs : 
  global_static_information ->
  Ckappa_sig.PairAgentSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t

val set_bonds_lhs :
  Ckappa_sig.PairAgentSiteState_map_and_set.Set.t Ckappa_sig.Rule_map_and_set.Map.t ->
  global_static_information ->
  global_static_information

val compute_initial_state:
  Exception.method_handler ->
  global_static_information ->
  Exception.method_handler * initial_state list

val get_kappa_handler: global_static_information -> Cckappa_sig.kappa_handler

val get_cc_code: global_static_information -> Cckappa_sig.compil

val get_mvbdu_handler: global_dynamic_information -> Mvbdu_wrapper.Mvbdu.handler

val set_mvbdu_handler: 
  Mvbdu_wrapper.Mvbdu.handler ->
  global_dynamic_information ->
  global_dynamic_information
