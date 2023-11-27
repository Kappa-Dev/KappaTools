(*
  * views_domain.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2016, the 30th of January
  * Last modification: Time-stamp: <Dec 22 2018>
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
  (MI : Mat_inter.Mat_inter with type var = Occu1.trans)
  ->
  struct
    type static_information = {
      global_static_information: Analyzer_headers.global_static_information;
      local_static_information: Counters_domain_type.static;
    }

    (*--------------------------------------------------------------*)
    (* One map: for each tuple: Yes, No, Maybe.
       - Yes: to say that when the sites x and y are bound with sites of
        the good type, then they are bound to the same B.
       - No: to say that when the sites x and y are bound with sites of the good
        type, then they are never bound to the same B.
       - Maybe: both cases may happen.*)

    type local_dynamic_information = {
      dummy: unit;
      store_value:
        MI.prod
        Ckappa_sig
        .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
        .t;
    }

    type dynamic_information = {
      local: local_dynamic_information;
      global: Analyzer_headers.global_dynamic_information;
    }

    (** Static information:
      Explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)

    (*global static information*)

    let get_global_static_information static = static.global_static_information
    let lift f x = f (get_global_static_information x)
    let get_parameter static = lift Analyzer_headers.get_parameter static

    let get_kappa_handler static =
      lift Analyzer_headers.get_kappa_handler static

    let get_compil static = lift Analyzer_headers.get_cc_code static
    let get_local_static_information static = static.local_static_information

    let get_rule parameter error static r_id =
      let compil = get_compil static in
      let error, rule =
        Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameter error
          r_id compil.Cckappa_sig.rules
      in
      error, rule

    (*static information*)

    let get_counters_set static =
      (get_local_static_information static).Counters_domain_type.counters

    let get_rule_restriction static =
      (get_local_static_information static)
        .Counters_domain_type.rule_restrictions

    let get_rule_creation static =
      (get_local_static_information static).Counters_domain_type.rule_creation

    let get_packs static =
      (get_local_static_information static).Counters_domain_type.packs

    let get_backward_pointers static =
      (get_local_static_information static)
        .Counters_domain_type.backward_pointers

    (*global dynamic information*)

    let get_global_dynamic_information dynamic = dynamic.global
    let get_local_dynamic_information dynamic = dynamic.local
    let set_local_dynamic_information local dynamic = { dynamic with local }
    let set_global_dynamic_information global dynamic = { dynamic with global }

    (*dynamic information*)

    let get_value dynamic = (get_local_dynamic_information dynamic).store_value

    let set_value value dynamic =
      set_local_dynamic_information
        { (get_local_dynamic_information dynamic) with store_value = value }
        dynamic

    (*--------------------------------------------------------------*)

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

    (****************************************************************)
    (*rule*)
    (*****************************************************************)

    let compute_local_static_information global_static_information _dynamic
        error =
      let parameters =
        Analyzer_headers.get_parameter global_static_information
      in
      let compil = Analyzer_headers.get_cc_code global_static_information in
      let kappa_handler =
        Analyzer_headers.get_kappa_handler global_static_information
      in
      let error, local_static_information =
        Counters_domain_static.compute_static parameters error kappa_handler
          compil
      in
      let static = { global_static_information; local_static_information } in
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
        Ckappa_sig
        .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
        .create parameters error (0, 0)
      in
      let init_local_dynamic_information = { dummy = (); store_value } in
      let dynamic =
        { global = dynamic; local = init_local_dynamic_information }
      in
      error, static, dynamic, []

    (* fold over all the rules, all the tuples of interest, all the sites in
       these tuples, and apply the function Common_static.add_dependency_site_rule
       to update the wake_up relation *)
    let complete_wake_up_relation static error wake_up =
      let parameters = get_parameter static in
      let rule_restriction = get_rule_restriction static in
      let packs = get_packs static in
      Ckappa_sig.Rule_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
        error
        (fun parameters error rule_id agent_map wake_up ->
          let error, rule = get_rule parameters error static rule_id in
          match rule with
          | None ->
            let error, () = Exception.warn parameters error __POS__ Exit () in
            error, wake_up
          | Some rule ->
            let lhs =
              rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs
                .Cckappa_sig.views
            in
            Ckappa_sig.Agent_id_nearly_Inf_Int_storage_Imperatif.fold parameters
              error
              (fun parameters error agent_id counter_map wake_up ->
                let error, agent =
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameters error agent_id lhs
                in
                let error, agent_type =
                  match agent with
                  | Some (Cckappa_sig.Agent ag) ->
                    error, ag.Cckappa_sig.agent_name
                  | None
                  | Some
                      ( Cckappa_sig.Ghost | Cckappa_sig.Dead_agent _
                      | Cckappa_sig.Unknown_agent _ ) ->
                    Exception.warn parameters error __POS__ Exit
                      Ckappa_sig.dummy_agent_name
                in
                match
                  Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif
                  .unsafe_get parameters error agent_type packs
                with
                | error, None -> error, wake_up
                | error, Some pack_map ->
                  Ckappa_sig.Site_type_quick_nearly_Inf_Int_storage_Imperatif
                  .fold parameters error
                    (fun parameters error counter _ wake_up ->
                      match
                        Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif
                        .get parameters error counter pack_map
                      with
                      | error, None -> error, wake_up
                      | error, Some site_set ->
                        Ckappa_sig.Site_map_and_set.Set.fold
                          (fun site (error, wake_up) ->
                            Common_static.add_dependency_site_rule parameters
                              error agent_type site rule_id wake_up)
                          site_set (error, wake_up))
                    counter_map wake_up)
              agent_map wake_up)
        rule_restriction wake_up

    let update_event_list static error agent_type counter event_list =
      let parameters = get_parameter static in
      let packs = get_packs static in
      match
        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get parameters
          error agent_type packs
      with
      | error, None -> Exception.warn parameters error __POS__ Exit event_list
      | error, Some a ->
        (match
           Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.get parameters
             error counter a
         with
        | error, None -> Exception.warn parameters error __POS__ Exit event_list
        | error, Some a ->
          Ckappa_sig.Site_map_and_set.Set.fold
            (fun site (error, event_list) ->
              ( error,
                Communication.Modified_sites (agent_type, site) :: event_list ))
            a (error, event_list))

    let new_prod_gen bin static dynamic error dump_title agent_type counter prod
        event_list =
      let local = dynamic.local in
      let store_value = local.store_value in
      let kappa_handler = get_kappa_handler static in
      let parameters = get_parameter static in
      let (error, store_value), event_list =
        match
          Ckappa_sig
          .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
          .unsafe_get parameters error (agent_type, counter) store_value
        with
        | error, None ->
          let error, completed_event_list =
            update_event_list static error agent_type counter event_list
          in
          let error =
            if
              Remanent_parameters.get_dump_reachability_analysis_diff parameters
            then (
              let parameters =
                Remanent_parameters.update_prefix parameters "\t\t"
              in
              let () = dump_title () in
              let error, agent_string =
                Handler.translate_agent parameters error kappa_handler
                  agent_type
              in
              let error, counter_string =
                match
                  Handler.translate_site parameters error kappa_handler
                    agent_type counter
                with
                | error, Ckappa_sig.Counter x -> error, x
                | error, (Ckappa_sig.Internal x | Ckappa_sig.Binding x) ->
                  Exception.warn parameters error __POS__ Exit x
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "%s Agent: %s ; Counter: %s"
                  (Remanent_parameters.get_prefix parameters)
                  agent_string counter_string
              in
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              let error = MI.affiche_mat parameters error prod in
              error
            ) else
              error
          in
          ( Ckappa_sig
            .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
            .set parameters error (agent_type, counter) prod store_value,
            completed_event_list )
        | error, Some old_prod ->
          let error, old = MI.copy parameters error old_prod in
          let error, (new_prod, var_list) = bin parameters error old prod in
          if var_list = false then
            (error, store_value), event_list
          else (
            let error =
              if
                Remanent_parameters.get_dump_reachability_analysis_diff
                  parameters
              then (
                let parameters =
                  Remanent_parameters.update_prefix parameters "\t\t"
                in
                let () = dump_title () in
                let error, agent_string =
                  Handler.translate_agent parameters error kappa_handler
                    agent_type
                in
                let error, counter_string =
                  match
                    Handler.translate_site parameters error kappa_handler
                      agent_type counter
                  with
                  | error, Ckappa_sig.Counter x -> error, x
                  | error, (Ckappa_sig.Internal x | Ckappa_sig.Binding x) ->
                    Exception.warn parameters error __POS__ Exit x
                in
                let () =
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameters)
                    "%s Agent: %s ; Counter: %s"
                    (Remanent_parameters.get_prefix parameters)
                    agent_string counter_string
                in
                let () =
                  Loggers.print_newline
                    (Remanent_parameters.get_logger parameters)
                in
                let error = MI.affiche_mat parameters error new_prod in
                error
              ) else
                error
            in

            let error, completed_event_list =
              update_event_list static error agent_type counter event_list
            in
            ( Ckappa_sig
              .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
              .set parameters error (agent_type, counter) new_prod store_value,
              completed_event_list )
          )
      in
      let local = { local with store_value } in
      error, { dynamic with local }, event_list

    let new_union static dynamic error agent_type counter prod event_list =
      new_prod_gen MI.union_incr static dynamic error agent_type counter prod
        event_list

    let new_widen static dynamic error agent_type counter prod event_list =
      new_prod_gen MI.widen static dynamic error agent_type counter prod
        event_list
    (*************************************************************)

    let prod_of_assignement parameters error assignement =
      let list = List.rev_map fst assignement in
      let error, prod = MI.compt_of_var_list parameters error list in
      let error, prod =
        List.fold_left
          (fun (error, prod) (v, delta) ->
            MI.push parameters error prod v
              { Fraction.num = delta - 1; Fraction.den = 1 })
          (error, prod) assignement
      in
      error, prod

    let add_initial_state static dynamic error species =
      let parameters = get_parameter static in
      let compil = get_compil static in
      let kappa_handler = get_kappa_handler static in
      let packs = get_packs static in
      let event_list = [] in
      let dump_title () =
        if
          local_trace
          || Remanent_parameters.get_dump_reachability_analysis_diff parameters
        then (
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
        ) else
          ()
      in
      (*parallel bonds in the initial states*)
      let error, (dynamic, event_list) =
        let enriched_init = species.Cckappa_sig.e_init_c_mixture in
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
          parameters error
          (fun parameters error _ag_id ag (dynamic, event_list) ->
            match ag with
            | Cckappa_sig.Ghost | Cckappa_sig.Unknown_agent _
            | Cckappa_sig.Dead_agent _ ->
              Exception.warn parameters error __POS__ Exit (dynamic, event_list)
            | Cckappa_sig.Agent ag ->
              let agent_type = ag.Cckappa_sig.agent_name in
              let error, assignements =
                Counters_domain_static.convert_view parameters error
                  kappa_handler compil packs agent_type
                  (Some (Cckappa_sig.Agent ag))
              in
              let error, dynamic, event_list =
                List.fold_left
                  (fun (error, dynamic, event_list)
                       ((agent_type, counter), assignement) ->
                    let error, prod =
                      prod_of_assignement parameters error assignement
                    in
                    new_union static dynamic error dump_title agent_type counter
                      prod event_list)
                  (error, dynamic, event_list)
                  assignements
              in
              error, (dynamic, event_list))
          enriched_init.Cckappa_sig.views (dynamic, event_list)
      in
      error, dynamic, event_list

    (*************************************************************)
    (* if a parallel bound occurs on the lhs, check that this is possible *)

    (*************************************************************)

    let restrict parameters error x test = MI.guard parameters error x test

    let is_enabled static dynamic error (rule_id : Ckappa_sig.c_rule_id)
        precondition =
      let parameters = get_parameter static in
      let rule_restriction = get_rule_restriction static in
      (*-----------------------------------------------------------*)
      let error, rule = get_rule parameters error static rule_id in
      match rule with
      | None ->
        let error, () = Exception.warn parameters error __POS__ Exit () in
        error, dynamic, None
      | Some rule ->
        let parameters = Remanent_parameters.update_prefix parameters "\t\t" in
        let dump_title () =
          if
            local_trace
            || Remanent_parameters.get_dump_reachability_analysis_diff
                 parameters
          then (
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
          ) else
            ()
        in
        let lhs =
          rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
        in
        (match
           Ckappa_sig.Rule_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
             parameters error rule_id rule_restriction
         with
        | error, None -> error, dynamic, None
        | error, Some map ->
          let store_value = get_value dynamic in
          let error, bool =
            Ckappa_sig.Agent_id_nearly_Inf_Int_storage_Imperatif.for_all
              parameters error
              (fun parameters error agent_id pack_map ->
                let error, agent =
                  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
                    parameters error agent_id lhs
                in
                let error, agent_type =
                  match agent with
                  | Some (Cckappa_sig.Agent ag) ->
                    error, ag.Cckappa_sig.agent_name
                  | None
                  | Some
                      ( Cckappa_sig.Ghost | Cckappa_sig.Dead_agent _
                      | Cckappa_sig.Unknown_agent _ ) ->
                    Exception.warn parameters error __POS__ Exit
                      Ckappa_sig.dummy_agent_name
                in
                Ckappa_sig.Site_type_quick_nearly_Inf_Int_storage_Imperatif
                .for_all parameters error
                  (fun parameters error counter restriction ->
                    match
                      Ckappa_sig
                      .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
                      .get parameters error (agent_type, counter) store_value
                    with
                    | error, None ->
                      Exception.warn parameters error __POS__ Exit true
                    | error, Some x ->
                      let error, x' = MI.copy parameters error x in
                      let error, x_opt =
                        restrict parameters error x'
                          restriction.Counters_domain_type.tests
                      in
                      (match x_opt with
                      | None ->
                        if
                          local_trace
                          || Remanent_parameters
                             .get_dump_reachability_analysis_diff parameters
                        then (
                          let () = dump_title () in
                          let () =
                            Loggers.fprintf
                              (Remanent_parameters.get_logger parameters)
                              "%sBot detected"
                              (Remanent_parameters.get_prefix parameters)
                          in
                          let error = MI.affiche_mat parameters error x' in
                          error, false
                        ) else
                          error, false
                      | Some _ -> error, true))
                  pack_map)
              map
          in
          if bool then
            error, dynamic, Some precondition
          else
            error, dynamic, None)

    (***********************************************************)

    let maybe_reachable static dynamic error _flag _pattern precondition =
      (* non parallel bonds in a pattern can be maps to parallel ones through morphisms *)
      (* thus when the flag is Morphisms with ignore non parallel bonds *)
      let _parameters = get_parameter static in
      let _store_value = get_value dynamic in
      let error, bool = error, true in
      (* to do *)
      if bool then
        error, dynamic, Some precondition
      else
        error, dynamic, None

    (*if it is not the first time it is apply then do not apply *)

    let can_we_prove_this_is_not_the_first_application precondition =
      match
        Communication.is_the_rule_applied_for_the_first_time precondition
      with
      | Usual_domains.Sure_value b ->
        if b then
          true
        else
          false
      | Usual_domains.Maybe -> false

    let abstract_away parameters error x list =
      MI.abstract_away parameters error x (List.rev_map fst (List.rev list))

    let set parameters error x list =
      let error, prod = prod_of_assignement parameters error list in
      match MI.merge parameters error x prod with
      | error, None -> Exception.warn parameters error __POS__ Exit x
      | error, Some a -> error, a

    let translate parameters error x list =
      List.fold_left
        (fun (error, x) (v, delta) ->
          try
            MI.push parameters error x v
              { Fraction.num = delta; Fraction.den = 1 }
          with Not_found -> Exception.warn parameters error __POS__ Exit x)
        (error, x) list

    let apply_rule static dynamic error rule_id precondition =
      (*--------------------------------------------------------------*)
      let parameters = get_parameter static in
      let event_list = [] in
      let rule_restriction = get_rule_restriction static in
      let rule_creation = get_rule_creation static in
      (*-----------------------------------------------------------*)
      let error, rule = get_rule parameters error static rule_id in
      let first_application =
        can_we_prove_this_is_not_the_first_application precondition
      in
      match rule with
      | None ->
        let error, () = Exception.warn parameters error __POS__ Exit () in
        error, dynamic, (precondition, event_list)
      | Some rule ->
        let parameters = Remanent_parameters.update_prefix parameters "\t\t" in
        let dump_title () =
          if
            local_trace
            || Remanent_parameters.get_dump_reachability_analysis_diff
                 parameters
          then (
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
          ) else
            ()
        in
        let lhs =
          rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
        in
        (* regular updates *)
        let error, dynamic, event_list =
          match
            Ckappa_sig.Rule_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
              parameters error rule_id rule_restriction
          with
          | error, None -> error, dynamic, event_list
          | error, Some map ->
            let error, (dynamic, event_list) =
              Ckappa_sig.Agent_id_nearly_Inf_Int_storage_Imperatif.fold
                parameters error
                (fun parameters error agent_id pack_map (dynamic, event_list) ->
                  let error, agent =
                    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif
                    .get parameters error agent_id lhs
                  in
                  let error, agent_type =
                    match agent with
                    | Some (Cckappa_sig.Agent ag) ->
                      error, ag.Cckappa_sig.agent_name
                    | None
                    | Some
                        ( Cckappa_sig.Ghost | Cckappa_sig.Dead_agent _
                        | Cckappa_sig.Unknown_agent _ ) ->
                      Exception.warn parameters error __POS__ Exit
                        Ckappa_sig.dummy_agent_name
                  in
                  Ckappa_sig.Site_type_quick_nearly_Inf_Int_storage_Imperatif
                  .fold parameters error
                    (fun parameters error counter restriction
                         (dynamic, event_list) ->
                      let store_value = get_value dynamic in
                      let error, x =
                        match
                          Ckappa_sig
                          .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
                          .get parameters error (agent_type, counter)
                            store_value
                        with
                        | error, None ->
                          Exception.warn parameters error __POS__ Exit
                            (MI.create parameters 0)
                        | error, Some x -> error, x
                      in
                      let error, x' = MI.copy parameters error x in
                      let error, x_opt =
                        restrict parameters error x'
                          restriction.Counters_domain_type.tests
                      in
                      let error, x =
                        match x_opt with
                        | None ->
                          Exception.warn parameters error __POS__ Exit x'
                        | Some x -> error, x
                      in
                      let error, x =
                        abstract_away parameters error x
                          restriction
                            .Counters_domain_type.non_invertible_assignments
                      in
                      let error, x =
                        set parameters error x
                          restriction
                            .Counters_domain_type.non_invertible_assignments
                      in
                      let error, x =
                        translate parameters error x
                          restriction
                            .Counters_domain_type.invertible_assignments
                      in
                      let error, dynamic, event_list =
                        if first_application then
                          new_union static dynamic error dump_title agent_type
                            counter x event_list
                        else
                          new_widen static dynamic error dump_title agent_type
                            counter x event_list
                      in
                      error, (dynamic, event_list))
                    pack_map (dynamic, event_list))
                map (dynamic, event_list)
            in
            error, dynamic, event_list
        in
        (* creation *)
        let error, dynamic, event_list =
          if first_application then (
            match
              Ckappa_sig.Rule_id_quick_nearly_Inf_Int_storage_Imperatif
              .unsafe_get parameters error rule_id rule_creation
            with
            | error, None -> error, dynamic, event_list
            | error, Some map ->
              let error, (dynamic, event_list) =
                Ckappa_sig
                .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
                .fold parameters error
                  (fun parameters error (agent_type, counter) assignement_list
                       (dynamic, event_list) ->
                    List.fold_left
                      (fun (error, (dynamic, event_list)) assignement ->
                        let error, prod =
                          prod_of_assignement parameters error assignement
                        in
                        let error, dynamic, event_list =
                          new_union static dynamic error dump_title agent_type
                            counter prod event_list
                        in
                        error, (dynamic, event_list))
                      (error, (dynamic, event_list))
                      assignement_list)
                  map (dynamic, event_list)
              in
              error, dynamic, event_list
          ) else
            error, dynamic, event_list
        in
        error, dynamic, (precondition, event_list)

    let apply_one_side_effect static dynamic error _rule_id
        (_, (agent, site, state)) precondition =
      let parameters = get_parameter static in
      let backward = get_backward_pointers static in
      let dump_title () =
        if
          local_trace
          || Remanent_parameters.get_dump_reachability_analysis_diff parameters
        then (
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
        ) else
          ()
      in
      let event_list = [] in
      let error, dynamic, event_list =
        match
          Ckappa_sig
          .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
          .unsafe_get parameters error (agent, site) backward
        with
        | error, None -> error, dynamic, event_list
        | error, Some set ->
          Ckappa_sig.Site_map_and_set.Set.fold
            (fun counter (error, dynamic, event_list) ->
              let value = get_value dynamic in
              let guard =
                [ Occu1.Bool (site, state), Counters_domain_type.EQ, 1 ]
              in
              let update =
                [
                  Occu1.Bool (site, state), -1;
                  Occu1.Bool (site, Ckappa_sig.state_index_of_int 0), 1;
                ]
              in
              let error, x =
                match
                  Ckappa_sig
                  .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
                  .get parameters error (agent, counter) value
                with
                | error, None ->
                  Exception.warn parameters error __POS__ Exit
                    (MI.create parameters 0)
                | error, Some x -> error, x
              in
              let error, x' = MI.copy parameters error x in
              let error, x_opt = restrict parameters error x' guard in
              let error, x =
                match x_opt with
                | None -> Exception.warn parameters error __POS__ Exit x'
                | Some x -> error, x
              in
              let error, x = translate parameters error x update in
              let error, dynamic, event_list =
                new_union static dynamic error dump_title agent counter x
                  event_list
              in
              error, dynamic, event_list)
            set
            (error, dynamic, event_list)
      in
      error, dynamic, (precondition, event_list)

    (*-----------------------------------------------------------*)

    let apply_event_list _static dynamic error _event_list = error, dynamic, []

    (****************************************************************)

    let stabilize static dynamic error =
      let store_value = get_value dynamic in
      let parameters = get_parameter static in
      let error, store_value =
        Ckappa_sig
        .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
        .fold parameters error
          (fun parameters error k prod store ->
            let error, prod_opt = MI.solve_all parameters error prod in
            match prod_opt with
            | None ->
              Ckappa_sig
              .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
              .free parameters error k store
            | Some a ->
              Ckappa_sig
              .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
              .set parameters error k a store)
          store_value store_value
      in
      let dynamic = set_value store_value dynamic in
      error, dynamic, ()

    let get_interval static error (agent_type, site) store_value =
      let back = get_backward_pointers static in
      let parameters = get_parameter static in
      let error, back_site_opt =
        Ckappa_sig
        .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
        .get parameters error (agent_type, site) back
      in
      let error, back_site =
        match back_site_opt with
        | None ->
          Exception.warn parameters error __POS__ Exit
            Ckappa_sig.Site_map_and_set.Set.empty
        | Some set -> error, set
      in
      Ckappa_sig.Site_map_and_set.Set.fold
        (fun counter (error, intervalle_opt) ->
          let error, mi_opt =
            Ckappa_sig
            .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
            .get parameters error (agent_type, counter) store_value
          in
          match mi_opt with
          | Some new_mi ->
            let error, interv =
              MI.interval_of_pro parameters error new_mi (Occu1.Counter site)
            in
            (match intervalle_opt, interv with
            | None, _ -> error, interv
            | _, None -> error, intervalle_opt
            | Some (a, b), Some (c, d) ->
              error, Some (Fraction.ffmax a c, Fraction.ffmin b d))
          | None -> Exception.warn parameters error __POS__ Exit intervalle_opt)
        back_site (error, None)

    let print ?dead_rules static dynamic (error : Exception.method_handler)
        loggers =
      let _ = dead_rules in
      let kappa_handler = get_kappa_handler static in
      let counter_set = get_counters_set static in
      let parameters = get_parameter static in
      let log = loggers in
      (*-------------------------------------------------------*)
      let error =
        if Remanent_parameters.get_dump_reachability_analysis_result parameters
        then (
          let () =
            Loggers.fprintf log
              "------------------------------------------------------------\n";
            Loggers.fprintf log "* Properties of counters\n";
            Loggers.fprintf log
              "------------------------------------------------------------\n"
          in
          let store_value = get_value dynamic in
          Ckappa_sig.AgentSite_map_and_set.Set.fold
            (fun (agent_type, site) error ->
              let error, intervalle_opt =
                get_interval static error (agent_type, site) store_value
              in
              let error, agent_string =
                Handler.translate_agent parameters error kappa_handler
                  agent_type
              in
              let error, site_string =
                Handler.translate_site parameters error kappa_handler agent_type
                  site
              in
              let error, site_string =
                match site_string with
                | Ckappa_sig.Counter x -> error, x
                | Ckappa_sig.Internal _ | Ckappa_sig.Binding _ ->
                  Exception.warn parameters error __POS__ Exit ""
              in
              let error, () =
                match intervalle_opt with
                | None | Some (Fraction.Minfinity, Fraction.Infinity) ->
                  error, ()
                | Some (Fraction.Minfinity, Fraction.Frac f) ->
                  let () =
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters)
                      "%s%s%s => %s%s%s%s%s%s%s%s%s%s%s" agent_string
                      (Remanent_parameters.get_agent_open_symbol parameters)
                      (Remanent_parameters.get_agent_close_symbol parameters)
                      agent_string
                      (Remanent_parameters.get_agent_open_symbol parameters)
                      site_string
                      (Remanent_parameters.get_open_counter_state parameters)
                      (Remanent_parameters.get_open_int_interval_infinity_symbol
                         parameters)
                      (Remanent_parameters.get_minus_infinity_symbol parameters)
                      (Remanent_parameters.get_int_interval_separator_symbol
                         parameters)
                      (Fraction.string_of f)
                      (Remanent_parameters
                       .get_close_int_interval_inclusive_symbol parameters)
                      (Remanent_parameters.get_close_counter_state parameters)
                      (Remanent_parameters.get_agent_close_symbol parameters)
                  in
                  ( error,
                    Loggers.print_newline
                      (Remanent_parameters.get_logger parameters) )
                | Some (Fraction.Frac f, Fraction.Infinity) ->
                  let () =
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters)
                      "%s%s%s => %s%s%s%s%s%s%s%s%s%s%s" agent_string
                      (Remanent_parameters.get_agent_open_symbol parameters)
                      (Remanent_parameters.get_agent_close_symbol parameters)
                      agent_string
                      (Remanent_parameters.get_agent_open_symbol parameters)
                      site_string
                      (Remanent_parameters.get_open_counter_state parameters)
                      (Remanent_parameters
                       .get_open_int_interval_inclusive_symbol parameters)
                      (Fraction.string_of f)
                      (Remanent_parameters.get_int_interval_separator_symbol
                         parameters)
                      (Remanent_parameters.get_plus_infinity_symbol parameters)
                      (Remanent_parameters
                       .get_close_int_interval_infinity_symbol parameters)
                      (Remanent_parameters.get_close_counter_state parameters)
                      (Remanent_parameters.get_agent_close_symbol parameters)
                  in
                  ( error,
                    Loggers.print_newline
                      (Remanent_parameters.get_logger parameters) )
                | Some (Fraction.Frac f1, Fraction.Frac f2) ->
                  let () =
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters)
                      "%s%s%s => %s%s%s%s%s%s%s%s%s%s%s" agent_string
                      (Remanent_parameters.get_agent_open_symbol parameters)
                      (Remanent_parameters.get_agent_close_symbol parameters)
                      agent_string
                      (Remanent_parameters.get_agent_open_symbol parameters)
                      site_string
                      (Remanent_parameters.get_open_counter_state parameters)
                      (Remanent_parameters
                       .get_open_int_interval_inclusive_symbol parameters)
                      (Fraction.string_of f1)
                      (Remanent_parameters.get_int_interval_separator_symbol
                         parameters)
                      (Fraction.string_of f2)
                      (Remanent_parameters
                       .get_close_int_interval_inclusive_symbol parameters)
                      (Remanent_parameters.get_close_counter_state parameters)
                      (Remanent_parameters.get_agent_close_symbol parameters)
                  in
                  ( error,
                    Loggers.print_newline
                      (Remanent_parameters.get_logger parameters) )
                | Some (Fraction.Unknown, _)
                | Some (_, Fraction.Unknown)
                | Some (Fraction.Infinity, _)
                | Some (_, Fraction.Minfinity) ->
                  Exception.warn parameters error __POS__ Exit ()
              in
              error)
            counter_set error
        ) else
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
      let counters_set = get_counters_set static in
      (*------------------------------------------------------------------*)
      (*internal constraint list*)
      let internal_constraints_list =
        Remanent_state.get_internal_constraints_list kasa_state
      in
      let error, internal_constraints_list =
        match internal_constraints_list with
        | None -> Exception.warn parameters error __POS__ Exit []
        | Some l -> error, l
      in
      let error, current_list =
        Ckappa_sig.AgentSite_map_and_set.Set.fold
          (fun (agent_type, site) (error, current_list) ->
            let error, intervalle_opt =
              get_interval static error (agent_type, site) store_value
            in
            match intervalle_opt with
            | None
            | Some
                ( Fraction.Infinity, _
                | _, Fraction.Minfinity
                | Fraction.Unknown, _
                | _, Fraction.Unknown ) ->
              Exception.warn parameters error __POS__ Exit current_list
            | Some (Fraction.Minfinity, Fraction.Infinity) ->
              error, current_list
            | Some
                ( ((Fraction.Frac _ | Fraction.Minfinity) as inf),
                  ((Fraction.Frac _ | Fraction.Infinity) as sup) ) ->
              let t = Site_graphs.KaSa_site_graph.empty in
              let error', agent_id, t =
                Site_graphs.KaSa_site_graph.add_agent parameters error
                  kappa_handler agent_type t
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let error, inf =
                match inf with
                | Fraction.Minfinity -> error, None
                | Fraction.Infinity | Fraction.Unknown ->
                  Exception.warn parameters error __POS__ Exit None
                | Fraction.Frac _ -> error, Some (Fraction.floor_int inf)
              in
              let error, sup =
                match sup with
                | Fraction.Infinity -> error, None
                | Fraction.Minfinity | Fraction.Unknown ->
                  Exception.warn parameters error __POS__ Exit None
                | Fraction.Frac _ -> error, Some (Fraction.cell_int sup)
              in
              let error', t' =
                Site_graphs.KaSa_site_graph.add_counter_range parameters error
                  kappa_handler agent_id site ?inf ?sup t
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              ( error,
                { Public_data.hyp = t; Public_data.refinement = [ t' ] }
                :: current_list ))
          counters_set (error, current_list)
      in
      let pair_list =
        (domain_name, List.rev current_list) :: internal_constraints_list
      in
      let kasa_state =
        Remanent_state.set_internal_constraints_list pair_list kasa_state
      in
      error, dynamic, kasa_state

    let get_dead_rules _static _dynamic = Analyzer_headers.dummy_dead_rules
    let get_side_effects _static _dynamic = Analyzer_headers.dummy_side_effects
  end

module Domain_affine_equalities_and_intervalles = Functor (Mat_inter.Mat_int)
module Domain_octagons = Functor (Octo.Octo)
module Domain_non_relational = Functor (Non_rel.Non_rel)
module Domain_abstract_multisets = Functor (Mat_inter.Mat_int) (* to do *)
