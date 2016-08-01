(*
   * communication.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 22th of February
   * Last modification: Time-stamp: <Aug 01 2016>
   *
   * Abstract domain to record live rules
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

type path_defined_in =
  | LHS of Cckappa_sig.enriched_rule
  | RHS of Cckappa_sig.enriched_rule
  | Pattern

type event =
| Dummy (* to avoid compilation warning *)
| Check_rule of Ckappa_sig.c_rule_id
| See_a_new_bond of
    ((Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
        (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state))

type step =
  {
    site_out: Ckappa_sig.c_site_name;
    site_in: Ckappa_sig.c_site_name;
    agent_type_in: Ckappa_sig.c_agent_name
  }

type path =
  {
    defined_in: path_defined_in;
    agent_id: Ckappa_sig.c_agent_id;
    relative_address: step list;
    site: Ckappa_sig.c_site_name;
  }

module type PathMap =
sig
  type 'a t
  val empty: 'a -> 'a t
  val add: path -> 'a -> 'a t -> 'a t
  val find: path -> 'a t -> 'a option
end

module PathMap:PathMap

type precondition

type 'a fold =
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Ckappa_sig.c_agent_name ->
  Ckappa_sig.c_site_name ->
  Exception.method_handler *
    ((Remanent_parameters_sig.parameters ->
      Ckappa_sig.c_state ->
      Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state ->
      Exception.method_handler * 'a ->
      Exception.method_handler * 'a) ->
     Exception.method_handler -> 'a ->
     Exception.method_handler * 'a) Usual_domains.flat_lattice

val dummy_precondition: precondition

val is_the_rule_applied_for_the_first_time:
  precondition -> Usual_domains.maybe_bool

val the_rule_is_applied_for_the_first_time:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  precondition ->
  Exception.method_handler * precondition

val the_rule_is_not_applied_for_the_first_time:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  precondition ->
  Exception.method_handler * precondition

val get_state_of_site:
  Exception.method_handler ->
  Analyzer_headers.global_dynamic_information ->
  precondition ->
  path ->
  Exception.method_handler * Analyzer_headers.global_dynamic_information * precondition *
    Ckappa_sig.c_state list Usual_domains.flat_lattice

type prefold = { fold: 'a. 'a fold}

(*fill in is_enable where it output the precondition, take the
  precondition, refine, the previous result, and output the new
  precondition*)

val refine_information_about_state_of_site:
  precondition ->
  (Exception.method_handler ->
   Analyzer_headers.global_dynamic_information ->
   path ->
   Ckappa_sig.c_state list Usual_domains.flat_lattice ->
   Exception.method_handler * Analyzer_headers.global_dynamic_information *
     Ckappa_sig.c_state list Usual_domains.flat_lattice) ->
  precondition

val get_potential_partner:
  precondition ->
  (Exception.method_handler -> Ckappa_sig.c_agent_name -> Ckappa_sig.c_site_name -> Ckappa_sig.c_state ->
   Exception.method_handler * precondition *
   (((Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state) Usual_domains.flat_lattice)))

val fold_over_potential_partners:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  precondition ->
  Ckappa_sig.c_agent_name ->
  Ckappa_sig.c_site_name ->
  (Remanent_parameters_sig.parameters ->
   Ckappa_sig.c_state ->
   Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state ->
   Exception.method_handler * 'a -> Exception.method_handler * 'a) ->
  'a ->
  Exception.method_handler * precondition * 'a Usual_domains.top_or_not

val overwrite_potential_partners_map:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  precondition ->
  (Exception.method_handler -> Ckappa_sig.c_agent_name ->
   Ckappa_sig.c_site_name ->
   Ckappa_sig.c_state ->
   Exception.method_handler *
   (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state) Usual_domains.flat_lattice)
  -> prefold ->
  Exception.method_handler * precondition

val post_condition:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Ckappa_sig.c_rule ->
  precondition ->
  Analyzer_headers.global_dynamic_information ->
  path ->
  Exception.method_handler * Analyzer_headers.global_dynamic_information *
  Ckappa_sig.c_state list Usual_domains.flat_lattice
