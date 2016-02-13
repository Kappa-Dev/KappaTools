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

type rule_id = int

(** type of the static information to be passed to each domain, let us
    start by this signature at the moment. In a first step, we are going
    to use only one module, and provide it with all the static information
    that you have computed and that you are using so far. Then, we will
    introduce a collection of independent modules, and dispatch this
    information between what is common, and what is specific to each
    domain.*)

type global_static_information
type global_dynamic_information 

type event =
| Check_rule of rule_id

type precondition = unit

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

val dummy_precondition: precondition

val get_parameter: global_static_information -> Remanent_parameters_sig.parameters

val get_compilation_information: global_static_information -> compilation_result

val get_bdu_common_static : global_static_information -> Bdu_analysis_type.bdu_common_static

val compute_initial_state:
  Exception.method_handler ->
  global_static_information ->
  Exception.method_handler * initial_state list

val get_kappa_handler: global_static_information -> Cckappa_sig.kappa_handler

val get_cc_code: global_static_information -> Cckappa_sig.compil

val get_mvbdu_handler: global_dynamic_information -> Mvbdu_wrapper.Mvbdu.handler

val set_mvbdu_handler: Mvbdu_wrapper.Mvbdu.handler -> global_dynamic_information -> global_dynamic_information
