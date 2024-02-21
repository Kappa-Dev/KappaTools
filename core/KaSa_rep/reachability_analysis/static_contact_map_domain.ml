(**
   * contact_map_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
   *
   * Creation: 2016, the 22th of February
   * Last modification: Time-stamp: <Dec 04 2018>
   *
   * Abstract domain to record live rules
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false
let _ = local_trace

module Domain = struct
  type static_information = {
    global_static_information: Analyzer_headers.global_static_information;
  }

  type local_dynamic_information = unit

  type dynamic_information = {
    local: local_dynamic_information;
    global: Analyzer_headers.global_dynamic_information;
  }

  (**************************************************************************)
  (*local static information*)

  let get_global_static_information static = static.global_static_information
  let lift f x = f (get_global_static_information x)
  let get_parameter static = lift Analyzer_headers.get_parameter static

  (*--------------------------------------------------------------------*)
  (** dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  let set_global_dynamic_information gdynamic dynamic =
    { dynamic with global = gdynamic }

  (**************************************************************************)
  (*implementations*)

  let initialize static dynamic error =
    let init_global_dynamic_information = { local = (); global = dynamic } in
    let parameter = Analyzer_headers.get_parameter static in
    let kappa_handler = Analyzer_headers.get_kappa_handler static in
    let init_global_static_information =
      { global_static_information = static }
    in
    let error, event_list =
      Ckappa_sig
      .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
      .fold parameter error
        (fun _ error (i, (j, k)) (i', j', k') event_list ->
          ( error,
            Communication.See_a_new_bond ((i, j, k), (i', j', k')) :: event_list
          ))
        kappa_handler.Cckappa_sig.dual []
    in
    ( error,
      init_global_static_information,
      init_global_dynamic_information,
      event_list )

  let complete_wake_up_relation _static error wake_up = error, wake_up

  (**************************************************************************)

  type 'a zeroary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    Exception.method_handler * dynamic_information * 'a

  type ('a, 'b) unary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    Exception.method_handler * dynamic_information * 'b

  type ('a, 'b, 'c) binary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    'b ->
    Exception.method_handler * dynamic_information * 'c

  type ('a, 'b, 'c, 'd) ternary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    'b ->
    'c ->
    Exception.method_handler * dynamic_information * 'd

  (**************************************************************************)
  (*Implementation*)

  let add_initial_state _static dynamic error _species = error, dynamic, []

  (**************************************************************************)

  let is_enabled _static dynamic error _rule_id precondition =
    error, dynamic, Some precondition

  (***********************************************************)

  let maybe_reachable _static dynamic error _flag _pattern precondition =
    error, dynamic, Some precondition

  (**************************************************************************)

  let apply_rule _static dynamic error _rule_id precondition =
    error, dynamic, (precondition, [])
  (* this domain ignores rule application *)

  let apply_one_side_effect _static dynamic error _ _ precondition =
    error, dynamic, (precondition, [])
  (* this domain ignores side effects *)

  let apply_event_list _static dynamic error _event_list =
    let event_list = [] in
    error, dynamic, event_list

  let stabilize _static dynamic error = error, dynamic, ()
  let export _static dynamic error kasa_state = error, dynamic, kasa_state

  let print ?dead_rules _static dynamic error _loggers =
    let _ = dead_rules in
    error, dynamic, ()

  let get_dead_rules _static _dynamic = Analyzer_headers.dummy_dead_rules
  let get_side_effects _static _dynamic = Analyzer_headers.dummy_side_effects
end
