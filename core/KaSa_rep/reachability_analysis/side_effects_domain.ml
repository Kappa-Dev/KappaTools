(**
   * rule_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
   *
   * Creation: 2018, the 21th of August
   * Last modification: Time-stamp: <Dec 04 2018>
   *
   * Abstract domain to record effective side effects
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016,2017,2018
   * Institut National de Recherche en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false
let _ = local_trace

module Domain = struct
  (* the type of the struct that contains all static information as in the
     previous version of the analysis *)

  type static_information = {
    global_static_information: Analyzer_headers.global_static_information;
    domain_static_information: unit; (* no domain-specific static information *)
  }

  (* This array is statically allocated *)
  (* Why do you use extensive arrays ? *)

  type local_dynamic_information =
    Ckappa_sig.side_effects Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.t

  type dynamic_information = {
    local: local_dynamic_information;
    global: Analyzer_headers.global_dynamic_information;
  }

  (*--------------------------------------------------------------------*)
  (** global static information.
      explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)

  let get_global_static_information static = static.global_static_information
  let lift f x = f (get_global_static_information x)
  let get_parameter static = lift Analyzer_headers.get_parameter static

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  let set_global_dynamic_information gdynamic dynamic =
    { dynamic with global = gdynamic }

  (** dead rule local dynamic information*)
  let get_side_effects dynamic = dynamic.local

  let set_side_effects side_effects dynamic =
    { dynamic with local = side_effects }

  (*--------------------------------------------------------------------*)

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
  (** [get_scan_rule_set static] *)

  let initialize static dynamic error =
    let init_global_static_information =
      { global_static_information = static; domain_static_information = () }
    in
    let kappa_handler = Analyzer_headers.get_kappa_handler static in
    let parameters = Analyzer_headers.get_parameter static in
    let nrules = Handler.nrules parameters error kappa_handler in
    let potential_side_effects =
      Analyzer_headers.get_potential_side_effects_per_rule static
    in
    let error, init_dead_rule_array =
      if nrules = 0 then
        Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.create parameters error
          0
      else
        Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.init parameters error
          (nrules - 1) (fun _ error r_id ->
            let error, side_effects =
              Ckappa_sig.Rule_map_and_set.Map.find_default_without_logs
                parameters error [] r_id potential_side_effects
            in
            let error, map =
              List.fold_left
                (fun (error, map) (source, target) ->
                  Ckappa_sig.AgentsSiteState_map_and_set.Map.add_or_overwrite
                    parameters error source target map)
                (error, Ckappa_sig.AgentsSiteState_map_and_set.Map.empty)
                side_effects
            in
            ( error,
              {
                Ckappa_sig.not_seen_yet = map;
                Ckappa_sig.seen =
                  Ckappa_sig.AgentSiteState_map_and_set.Set.empty;
              } ))
    in
    let init_global_dynamic_information =
      { global = dynamic; local = init_dead_rule_array }
    in
    error, init_global_static_information, init_global_dynamic_information, []

  let complete_wake_up_relation _static error wake_up = error, wake_up

  let add_initial_state _static dynamic error _species =
    let event_list = [] in
    error, dynamic, event_list

  let is_enabled _static dynamic error _rule_id precondition =
    error, dynamic, Some precondition

  (***********************************************************)

  (* Nothing to do in this domain *)
  let maybe_reachable _static dynamic error _flag _pattern precondition =
    error, dynamic, Some precondition

  (* Nothing to do in this domain *)
  let apply_rule _static dynamic error _rule_id precondition =
    error, dynamic, (precondition, [])

  let apply_one_side_effect static dynamic error r_id (source_opt, target)
      precondition =
    match source_opt with
    | None -> error, dynamic, (precondition, [])
    | Some source ->
      let parameter = get_parameter static in
      let side_effects = get_side_effects dynamic in
      let error, side_effects_r_id =
        match
          Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameter error
            r_id side_effects
        with
        | error, None ->
          Exception.warn parameter error __POS__ Exit
            Ckappa_sig.empty_side_effects
        | error, Some side_effects -> error, side_effects
      in
      let error, seen =
        Ckappa_sig.AgentSiteState_map_and_set.Set.add_when_not_in parameter
          error target side_effects_r_id.Ckappa_sig.seen
      in
      if seen == side_effects_r_id.Ckappa_sig.seen then
        error, dynamic, (precondition, [])
      else (
        let error, not_seen_yet =
          Ckappa_sig.AgentsSiteState_map_and_set.Map.remove parameter error
            source side_effects_r_id.Ckappa_sig.not_seen_yet
        in
        let error, side_effects =
          Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.set parameter error
            r_id
            { Ckappa_sig.seen; Ckappa_sig.not_seen_yet }
            side_effects
        in
        let dynamic = set_side_effects side_effects dynamic in
        error, dynamic, (precondition, [])
      )

  let apply_event_list _static dynamic error _event_list =
    let event_list = [] in
    error, dynamic, event_list

  let stabilize _static dynamic error = error, dynamic, ()
  let export _static dynamic error kasa_state = error, dynamic, kasa_state
  (**************************************************************************)

  let print ?dead_rules _static dynamic error _loggers =
    let _ = dead_rules in
    error, dynamic, ()

  let get_dead_rules _static _dynamic = Analyzer_headers.dummy_dead_rules

  let get_side_effects _static dynamic parameters error r_id =
    match
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameters error r_id
        (get_side_effects dynamic)
    with
    | error, None -> Exception.warn parameters error __POS__ Exit None
    | error, Some b -> error, Some b
end
