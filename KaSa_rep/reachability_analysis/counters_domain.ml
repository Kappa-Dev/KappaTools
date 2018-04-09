(*
  * views_domain.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2016, the 30th of January
  * Last modification: Time-stamp: <Apr 09 2018>
  *
  * A monolitich domain to deal with all concepts in reachability analysis
  * This module is temporary and will be split according to different concepts
  * thanks to the functor Product
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

(** Abstract domain to over-approximate the set of reachable views *)

let local_trace = false

module Functor =
  functor
    (MI: Mat_inter.Mat_inter)
    ->
    struct

      type static_information =
        {
          global_static_information :
            Analyzer_headers.global_static_information;
          local_static_information  :
            Counters_domain_type.static
    }

  (*--------------------------------------------------------------*)
  (* One map: for each tuple: Yes, No, Maybe.
    - Yes: to say that when the sites x and y are bound with sites of
     the good type, then they are bound to the same B.
    - No: to say that when the sites x and y are bound with sites of the good
     type, then they are never bound to the same B.
    - Maybe: both cases may happen.*)

  type local_dynamic_information =
    {
      dummy: unit;
      store_value:
        MI.prod
          Ckappa_sig.Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t
    }

  type dynamic_information =
    {
      local  : local_dynamic_information ;
      global : Analyzer_headers.global_dynamic_information;
    }

  (** Static information:
      Explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)

  (*global static information*)

  let get_global_static_information static = static.global_static_information

  let lift f x = f (get_global_static_information x)

  let get_parameter static = lift Analyzer_headers.get_parameter static

  let get_wake_up_relation static =
    lift Analyzer_headers.get_wake_up_relation static

  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_compil static = lift Analyzer_headers.get_cc_code static

  let get_views_rhs static = lift Analyzer_headers.get_views_rhs static

  let get_local_static_information static = static.local_static_information

  let set_local_static_information local static =
    {
      static with
      local_static_information = local
    }

  let get_rule parameter error static r_id =
    let compil = get_compil static in
    let error, rule  =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get
        parameter
        error
        r_id
        compil.Cckappa_sig.rules
    in
    error, rule

  (*static information*)

  let get_rule_restriction static =
        (get_local_static_information
           static).Counters_domain_type.rule_restrictions

  let get_rule_creation static =
    (get_local_static_information
       static).Counters_domain_type.rule_creation

  let get_packs static =
         (get_local_static_information
            static).Counters_domain_type.packs

  let get_backqard_pointers static =
  (get_local_static_information
     static).Counters_domain_type.backward_pointers


  (*global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  let get_local_dynamic_information dynamic = dynamic.local

  let set_local_dynamic_information local dynamic =
    {
      dynamic with local = local
    }

  let set_global_dynamic_information global dynamic =
    {
      dynamic with global = global
    }

(*dynamic information*)

  let get_value dynamic =
    (get_local_dynamic_information dynamic).store_value

  let set_value value dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
        store_value = value
      } dynamic

  (*--------------------------------------------------------------*)

  type 'a zeroary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> Exception.method_handler * dynamic_information * 'a

  type ('a, 'b) unary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> Exception.method_handler * dynamic_information * 'b

  type ('a, 'b, 'c) binary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> 'b
    -> Exception.method_handler * dynamic_information * 'c

  type ('a, 'b, 'c, 'd) ternary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> 'b
    -> 'c
    -> Exception.method_handler * dynamic_information * 'd

  (****************************************************************)
  (*rule*)
  (*****************************************************************)

  let compute_local_static_information global_static_information _dynamic error =
    let parameters = Analyzer_headers.get_parameter global_static_information in
    let compil = Analyzer_headers.get_cc_code global_static_information in
    let kappa_handler = Analyzer_headers.get_kappa_handler global_static_information in
    let error, local_static_information =
      Counters_domain_static.compute_static
        parameters error kappa_handler compil
    in
    let static = {global_static_information; local_static_information} in
    error, static

(****************************************************************)
(*rules*)
(****************************************************************)


  let initialize static dynamic error =
    let parameters = Analyzer_headers.get_parameter static in
    let error, static =
      compute_local_static_information static dynamic error
    in
    let error, store_value =
      Ckappa_sig.Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.create parameters error (0,0)
    in
    let init_local_dynamic_information =
      {
        dummy = ();
        store_value
      }
    in
    let dynamic =
      {
        global = dynamic;
        local = init_local_dynamic_information ;
      }
    in
    error, static, dynamic, []

  (* fold over all the rules, all the tuples of interest, all the sites in
   these tuples, and apply the function Common_static.add_dependency_site_rule
   to update the wake_up relation *)
  let complete_wake_up_relation static error wake_up = error, wake_up
(* to do *)


  let compute_value_init static dynamic error init_state =
    error, dynamic

  (*************************************************************)

  let add_initial_state static dynamic error species =
    let event_list = [] in
    (*parallel bonds in the initial states*)
    let error, dynamic =
      compute_value_init static dynamic error species
    in
    error, dynamic, event_list

  (*************************************************************)
  (* if a parallel bound occurs on the lhs, check that this is possible *)

    (*************************************************************)

  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id)
      precondition =
    let _parameters = get_parameter static in
    (*-----------------------------------------------------------*)
    let _store_value = get_value dynamic in
    let error, bool = error, true (* todo *)
    in
    if bool
    then error, dynamic, Some precondition
    else error, dynamic, None

  (***********************************************************)

  let maybe_reachable static dynamic error flag pattern precondition =
  (* non parallel bonds in a pattern can be maps to parallel ones through morphisms *)
  (* thus when the flag is Morphisms with ignore non parallel bonds *)
    let _parameters = get_parameter static in
    let _store_value = get_value dynamic in
    let error, bool = error, true in (* to do *)
    if bool
    then error, dynamic, Some precondition
    else
    error, dynamic, None

  let discover_a_new_pair_of_modify_sites
      parameters error packs backward list_of_sites modified_sites =
    List.fold_left
      (fun (error, modified_sites) (agent_type,site)  ->
         match
             Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.unsafe_get parameters error (agent_type,site) backward
         with
         | error, None -> error, modified_sites
         | error, Some counter_set ->
           begin
             match
               Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif
                 .unsafe_get parameters error agent_type packs
               with
               | error, None -> error, modified_sites
               | error, Some agent_pack ->
                 begin
                   Ckappa_sig.Site_map_and_set.Set.fold
                     (fun counter (error, modified_sites) ->
                          match
                            Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.unsafe_get
                            parameters error counter agent_pack
                        with
                        | error, None -> error, modified_sites
                        | error, Some site_set ->
                          Ckappa_sig.Site_map_and_set.Set.fold
                            (fun site (error, modified_sites) ->
                               Communication.add_site parameters error agent_type site modified_sites)
                            site_set
                            (error, modified_sites))
                     counter_set
                     (error, modified_sites)
                 end
             end)
      (error, modified_sites)
      list_of_sites

(*if it is not the first time it is apply then do not apply *)

  let can_we_prove_this_is_not_the_first_application precondition =
    match
      Communication.is_the_rule_applied_for_the_first_time precondition
    with
    | Usual_domains.Sure_value b ->
      if b
      then true
      else false
    | Usual_domains.Maybe -> false

  let apply_rule static dynamic error rule_id precondition =
    (*--------------------------------------------------------------*)
    let parameters = get_parameter static in
    let event_list = [] in
    let error, modified_sites =
      Communication.init_sites_working_list parameters error
    in
    (*-----------------------------------------------------------*)
    let kappa_handler = get_kappa_handler static in
    let error, rule = get_rule parameters error static rule_id in
    match rule with
    | None ->
      let error, () =
        Exception.warn parameters error __POS__ Exit ()
      in
      error, dynamic, (precondition, event_list)
    | Some rule ->
      let parameters =
        Remanent_parameters.update_prefix parameters "\t\t"
      in
      let dump_title () =
        if local_trace ||
           Remanent_parameters.get_dump_reachability_analysis_diff parameters
        then
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%sUpdate information about counters"
              (Remanent_parameters.get_prefix parameters)
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        else
          ()
      in
      (* TODO -> regular *)
      (* TODO -> creation *)
      (* TODO -> side effect *)
      error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let add_rule static error rule_id event_list =
    let parameters = get_parameter static in
    let compiled = get_compil static in
    let kappa_handler = get_kappa_handler static in
    Communication.add_rule ~local_trace
      parameters compiled kappa_handler error
      rule_id event_list

  (*-----------------------------------------------------------*)

  let apply_event_list _static dynamic error _event_list =
    error, dynamic, []

  (****************************************************************)

  let stabilize _static dynamic error = error, dynamic, ()

  let print ?dead_rules static dynamic (error:Exception.method_handler) loggers =
    let _ = dead_rules in
    let kappa_handler = get_kappa_handler static in
    let parameters = get_parameter static in
    let log = loggers in
    (*-------------------------------------------------------*)
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_result
          parameters
      then
        let () =
          Loggers.fprintf log
            "------------------------------------------------------------\n";
          Loggers.fprintf log "* Properties of counters\n";
          Loggers.fprintf log
            "------------------------------------------------------------\n"
        in
        let store_value = get_value dynamic in
        (* TODO *)
        error
      else
        error
    in
    error, dynamic, ()

  (***********************************************************)

  let export static dynamic error kasa_state =
      let parameters = get_parameter static in
      let kappa_handler = get_kappa_handler static in
      let store_value = get_value dynamic in
      let domain_name = "Counters" in
      (*string * 'site_graph lemma list : head*)
      let current_list = [] in
      (*------------------------------------------------------------------*)
      (*internal constraint list*)
      let internal_constraints_list =
        Remanent_state.get_internal_constraints_list kasa_state
      in
      let error, internal_constraints_list =
        match internal_constraints_list with
        | None ->
          Exception.warn parameters error __POS__ Exit []
        | Some l -> error, l
      in
      let pair_list =
        (domain_name, List.rev current_list) :: internal_constraints_list in
      let kasa_state =
        Remanent_state.set_internal_constraints_list pair_list kasa_state in
      error, dynamic, kasa_state

  let lkappa_mixture_is_reachable _static dynamic error _lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable _static dynamic error _ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let get_dead_rules _static _dynamic  =
    Analyzer_headers.dummy_dead_rules
end

module Domain_affine_equalities_and_intervalles = Functor(Mat_inter.Mat_int)
module Domain_octagons = Functor(Mat_inter.Mat_int) (* to do *)
module Domain_non_relational = Functor(Mat_inter.Mat_int) (* to do *)
module Domain_abstract_multisets = Functor(Mat_inter.Mat_int) (* to do *)
