(**
 * translation_in_natural_language.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: 2016
 * Last modification: Time-stamp: <Dec 22 2018>
 * *
 * Signature for prepreprocessing language ckappa
 *
 * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

let trace = false
let _ = trace

type token =
  | Range of
      Ckappa_sig.c_mvbdu_var
      * (Ckappa_sig.c_state * Ckappa_sig.Views_bdu.mvbdu option) list
  | Equiv of
      (Ckappa_sig.c_mvbdu_var * Ckappa_sig.c_state)
      * (Ckappa_sig.c_mvbdu_var * Ckappa_sig.c_state)
  | Imply of
      (Ckappa_sig.c_mvbdu_var * Ckappa_sig.c_state)
      * (Ckappa_sig.c_mvbdu_var * Ckappa_sig.c_state)
  | Partition of
      (Ckappa_sig.c_mvbdu_var * (Ckappa_sig.c_state * token list) list)
  | No_known_translation of
      ((Ckappa_sig.c_mvbdu_var * Ckappa_sig.c_state) list
      * Ckappa_sig.Views_bdu.mvbdu option)
      list

type rename_sites =
  Remanent_parameters_sig.parameters ->
  Exception.exceptions_caught_and_uncaught ->
  Ckappa_sig.c_mvbdu_var ->
  Exception.exceptions_caught_and_uncaught * Ckappa_sig.c_mvbdu_var

(****************************************************************************)

let non_relational parameters handler error mvbdu =
  let error, handler, list =
    Ckappa_sig.Views_bdu.mvbdu_cartesian_abstraction parameters handler error
      mvbdu
  in
  let error, handler, mvbdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
  in
  let error, handler, recomposition =
    List.fold_left
      (fun (error, handler, conjunct) term ->
        Ckappa_sig.Views_bdu.mvbdu_and parameters handler error conjunct term)
      (error, handler, mvbdu_true)
      list
  in
  error, handler, Ckappa_sig.Views_bdu.equal mvbdu recomposition

let non_relational_with_threshold parameters handler error ~threshold mvbdu =
  let error, handler, list =
    Ckappa_sig.Views_bdu.mvbdu_cartesian_abstraction_with_threshold parameters
      handler error ~threshold mvbdu
  in
  let error, handler, mvbdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
  in
  let error, handler, recomposition =
    List.fold_left
      (fun (error, handler, conjunct) term ->
        Ckappa_sig.Views_bdu.mvbdu_and parameters handler error conjunct term)
      (error, handler, mvbdu_true)
      list
  in
  error, handler, Ckappa_sig.Views_bdu.equal mvbdu recomposition

let try_partitioning parameters handler error handler_kappa
    (rename_site_inverse : rename_sites) mvbdu =
  let i =
    Ckappa_sig.int_of_guard_parameter
      (Handler.get_nr_guard_parameters handler_kappa)
  in
  if i = 0 then (
    let error, handler, mvbdu_true =
      Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
    in
    let error, handler, var_hconsed_list =
      Ckappa_sig.Views_bdu.variables_list_of_mvbdu parameters handler error
        mvbdu
    in
    let error, handler, var_list =
      Ckappa_sig.Views_bdu.extensional_of_variables_list parameters handler
        error var_hconsed_list
    in
    let rec aux l (error, handler) =
      match l with
      | [] -> error, handler, None
      | head :: tail ->
        let error', handler, singleton =
          Ckappa_sig.Views_bdu.build_variables_list parameters handler error
            [ head ]
        in
        let error =
          Exception.check_point Exception.warn parameters error error' __POS__
            Exit
        in
        let error_2, handler, mvbdu_ref =
          Ckappa_sig.Views_bdu.mvbdu_project_abstract_away parameters handler
            error mvbdu singleton
        in
        let error =
          Exception.check_point Exception.warn parameters error error_2 __POS__
            Exit
        in
        let error_3, handler, proj_in =
          Ckappa_sig.Views_bdu.mvbdu_project_keep_only parameters handler error
            mvbdu singleton
        in
        let error =
          Exception.check_point Exception.warn parameters error error_3 __POS__
            Exit
        in
        let error_4, handler, list_asso =
          Ckappa_sig.Views_bdu.extensional_of_mvbdu parameters handler error
            proj_in
        in
        let error =
          Exception.check_point Exception.warn parameters error error_4 __POS__
            Exit
        in
        let error_5, range =
          let rec aux2 list (error, output) =
            match list with
            | [] -> error, output
            | [ (x, i) ] :: tail when x = head -> aux2 tail (error, i :: output)
            | _ :: tail ->
              (*TODO: bdu_ex*)
              aux2 tail (Exception.warn parameters error __POS__ Exit output)
          in
          aux2 list_asso (error, [])
        in
        let error =
          Exception.check_point Exception.warn parameters error error_5 __POS__
            Exit
        in
        let rec aux3 list (error, handler, output) =
          match list with
          | [] -> error, handler, Some output
          | h :: t ->
            let error_6, handler, select =
              Ckappa_sig.Views_bdu.build_association_list parameters handler
                error
                [ head, h ]
            in
            let error =
              Exception.check_point Exception.warn parameters error error_6
                __POS__ Exit
            in
            let error_7, handler, mvbdu_case =
              Ckappa_sig.Views_bdu.mvbdu_redefine parameters handler error
                mvbdu_true select
            in
            let error =
              Exception.check_point Exception.warn parameters error error_7
                __POS__ Exit
            in
            let error_8, handler, case =
              Ckappa_sig.Views_bdu.mvbdu_and parameters handler error mvbdu_case
                mvbdu
            in
            let error =
              Exception.check_point Exception.warn parameters error error_8
                __POS__ Exit
            in
            let error_9, handler, bool =
              non_relational parameters handler error case
            in
            let error =
              Exception.check_point Exception.warn parameters error error_9
                __POS__ Exit
            in
            if bool then (
              let error_10, handler, away =
                Ckappa_sig.Views_bdu.mvbdu_project_abstract_away parameters
                  handler error case singleton
              in
              let error =
                Exception.check_point Exception.warn parameters error error_10
                  __POS__ Exit
              in
              if Ckappa_sig.Views_bdu.equal away mvbdu_ref then
                aux3 t (error, handler, output)
              else (
                let error_11, handler, list =
                  Ckappa_sig.Views_bdu.mvbdu_cartesian_abstraction parameters
                    handler error away
                in
                let error =
                  Exception.check_point Exception.warn parameters error error_11
                    __POS__ Exit
                in
                let error, handler, list =
                  List.fold_left
                    (fun (error, handler, list) elt ->
                      let error, handler, mvbdu_test =
                        Ckappa_sig.Views_bdu.mvbdu_and parameters handler error
                          mvbdu_ref elt
                      in
                      if Ckappa_sig.Views_bdu.equal mvbdu_test mvbdu_ref then
                        error, handler, list
                      else (
                        let error_12, handler, elt =
                          Ckappa_sig.Views_bdu.extensional_of_mvbdu parameters
                            handler error elt
                        in
                        let error =
                          Exception.check_point Exception.warn parameters error
                            error_12 __POS__ Exit
                        in
                        let error, var_list_opt =
                          match elt with
                          | [] | [] :: _ | ((_, _) :: _ :: _) :: _ ->
                            error, None
                          | [ (a, b) ] :: q ->
                            let rec aux4 q output =
                              match q with
                              | [] -> error, Some (a, output)
                              | [ (c, d) ] :: q when c = a ->
                                aux4 q ((d, None) :: output)
                              | _ -> error, None
                            in
                            aux4 q [ b, None ]
                        in
                        match var_list_opt with
                        | None ->
                          let error, () =
                            Exception.warn parameters error __POS__ Exit ()
                          in
                          error, handler, list
                        | Some (a, l) ->
                          let error', a' =
                            rename_site_inverse parameters error a
                          in
                          let error =
                            Exception.check_point Exception.warn parameters
                              error error' __POS__ Exit
                          in
                          error, handler, Range (a', l) :: list
                      ))
                    (error, handler, []) (List.rev list)
                in
                aux3 t (error, handler, (h, list) :: output)
              )
            ) else
              error, handler, None
        in
        let error_13, handler, output = aux3 range (error, handler, []) in
        let error =
          Exception.check_point Exception.warn parameters error error_13 __POS__
            Exit
        in
        (match output with
        | None -> aux tail (error, handler)
        | Some l ->
          let error_14, head = rename_site_inverse parameters error head in
          let error =
            Exception.check_point Exception.warn parameters error error_14
              __POS__ Exit
          in
          error, handler, Some (head, l))
    in
    aux var_list (error, handler)
  ) else (
    let nsites = Handler.get_nsites handler_kappa in
    let threshold = Ckappa_sig.int_of_site_name nsites - 1 in
    let error, handler, mvbdu_true =
      Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
    in
    let error, handler, var_hconsed_list =
      Ckappa_sig.Views_bdu.variables_list_of_mvbdu_with_threshold parameters
        handler error ~threshold mvbdu
    in
    let error, handler, var_list =
      Ckappa_sig.Views_bdu.extensional_of_variables_list parameters handler
        error var_hconsed_list
    in
    let rec aux l (error, handler) =
      match l with
      | [] -> error, handler, None
      | head :: tail ->
        let error', handler, singleton =
          Ckappa_sig.Views_bdu.build_variables_list parameters handler error
            [ head ]
        in
        let error =
          Exception.check_point Exception.warn parameters error error' __POS__
            Exit
        in
        let error_2, handler, mvbdu_ref =
          Ckappa_sig.Views_bdu.mvbdu_project_abstract_away parameters handler
            error mvbdu singleton
        in
        let error =
          Exception.check_point Exception.warn parameters error error_2 __POS__
            Exit
        in
        let error_3, handler, proj_in =
          Ckappa_sig.Views_bdu.mvbdu_project_keep_only_with_threshold parameters
            handler error ~threshold mvbdu singleton
        in
        let error =
          Exception.check_point Exception.warn parameters error error_3 __POS__
            Exit
        in
        let error_4, handler, list_asso =
          Ckappa_sig.Views_bdu.parametric_conditions_of_mvbdu parameters handler
            error ~threshold proj_in
        in
        let error =
          Exception.check_point Exception.warn parameters error error_4 __POS__
            Exit
        in
        let error_5, range =
          let rec aux2 list (error, output) =
            match list with
            | [] -> error, output
            | ([ (x, i) ], m) :: tail when x = head ->
              aux2 tail (error, (i, m) :: output)
            | _ :: tail ->
              (*TODO: bdu_ex*)
              aux2 tail (Exception.warn parameters error __POS__ Exit output)
          in
          aux2 list_asso (error, [])
        in
        let error =
          Exception.check_point Exception.warn parameters error error_5 __POS__
            Exit
        in
        let rec aux3 list (error, handler, output) =
          match list with
          | [] -> error, handler, Some output
          | (h, m) :: t ->
            let error_6, handler, select =
              Ckappa_sig.Views_bdu.build_association_list parameters handler
                error
                [ head, h ]
            in
            let error =
              Exception.check_point Exception.warn parameters error error_6
                __POS__ Exit
            in
            let error_7, handler, mvbdu_case =
              Ckappa_sig.Views_bdu.mvbdu_redefine parameters handler error
                mvbdu_true select
            in
            let error =
              Exception.check_point Exception.warn parameters error error_7
                __POS__ Exit
            in
            let error_8, handler, mvbdu_case_with_param =
              Ckappa_sig.Views_bdu.mvbdu_and parameters handler error mvbdu_case
                m
            in
            let error =
              Exception.check_point Exception.warn parameters error error_8
                __POS__ Exit
            in
            let error_9, handler, case_with_param =
              Ckappa_sig.Views_bdu.mvbdu_and parameters handler error
                mvbdu_case_with_param mvbdu
            in
            let error =
              Exception.check_point Exception.warn parameters error error_9
                __POS__ Exit
            in
            let error_10, handler, bool =
              non_relational_with_threshold parameters handler error ~threshold
                case_with_param
            in
            let error =
              Exception.check_point Exception.warn parameters error error_10
                __POS__ Exit
            in
            if bool then (
              let error_11, handler, away =
                Ckappa_sig.Views_bdu.mvbdu_project_abstract_away parameters
                  handler error case_with_param singleton
              in
              let error =
                Exception.check_point Exception.warn parameters error error_11
                  __POS__ Exit
              in
              if Ckappa_sig.Views_bdu.equal away mvbdu_ref then
                aux3 t (error, handler, output)
              else (
                let error_12, handler, list =
                  Ckappa_sig.Views_bdu
                  .mvbdu_cartesian_abstraction_with_threshold parameters handler
                    error ~threshold away
                in
                let error =
                  Exception.check_point Exception.warn parameters error error_12
                    __POS__ Exit
                in
                let error, handler, list =
                  List.fold_left
                    (fun (error, handler, list) elt ->
                      let error, handler, mvbdu_test =
                        Ckappa_sig.Views_bdu.mvbdu_and parameters handler error
                          mvbdu_ref elt
                      in
                      if Ckappa_sig.Views_bdu.equal mvbdu_test mvbdu_ref then
                        error, handler, list
                      else (
                        let error_13, handler, elt =
                          Ckappa_sig.Views_bdu.extensional_of_mvbdu parameters
                            handler error elt
                        in
                        let error =
                          Exception.check_point Exception.warn parameters error
                            error_13 __POS__ Exit
                        in
                        let error, var_list_opt =
                          match elt with
                          | [] | [] :: _ | ((_, _) :: _ :: _) :: _ ->
                            error, None
                          | [ (a, b) ] :: q ->
                            let rec aux4 q output =
                              match q with
                              | [] -> error, Some (a, output)
                              | [ (c, d) ] :: q when c = a ->
                                aux4 q ((d, None) :: output)
                              | _ -> error, None
                            in
                            aux4 q [ b, None ]
                        in
                        match var_list_opt with
                        | None ->
                          let error, () =
                            Exception.warn parameters error __POS__ Exit ()
                          in
                          error, handler, list
                        | Some (a, l) ->
                          let error', a' =
                            rename_site_inverse parameters error a
                          in
                          let error =
                            Exception.check_point Exception.warn parameters
                              error error' __POS__ Exit
                          in
                          error, handler, Range (a', l) :: list
                      ))
                    (error, handler, []) (List.rev list)
                in
                aux3 t (error, handler, (h, list) :: output)
              )
            ) else
              error, handler, None
        in
        let error_14, handler, output = aux3 range (error, handler, []) in
        let error =
          Exception.check_point Exception.warn parameters error error_14 __POS__
            Exit
        in
        (match output with
        | None -> aux tail (error, handler)
        | Some l ->
          let error_15, head = rename_site_inverse parameters error head in
          let error =
            Exception.check_point Exception.warn parameters error error_15
              __POS__ Exit
          in
          error, handler, Some (head, l))
    in
    aux var_list (error, handler)
  )

(****************************************************************************)

let translate parameters handler error kappa_handler
    (rename_site_inverse : rename_sites) mvbdu nsites restriction_bdu =
  let threshold = Ckappa_sig.int_of_site_name nsites - 1 in
  let error, handler, list =
    Ckappa_sig.Views_bdu.parametric_conditions_of_mvbdu parameters handler error
      ~threshold mvbdu
  in
  let error, handler, list, list_with_mvbdu, all_mvbdu_are_true =
    List.fold_left
      (fun (error, handler, list, list_with_mvbdu, all_mvbdu_are_true)
           (elt1, mvbdu) ->
        let error, handler, mvbdu_is_true =
          Ckappa_sig.mvbdu_is_true_for_guards parameters handler error mvbdu
            restriction_bdu
        in
        let mvbdu_opt =
          if mvbdu_is_true then
            None
          else
            Some mvbdu
        in
        let error, elt1 =
          List.fold_left
            (fun (error, list) (elt2, asso) ->
              let error, elt2 = rename_site_inverse parameters error elt2 in
              error, (elt2, asso) :: list)
            (error, []) (List.rev elt1)
        in
        ( error,
          handler,
          elt1 :: list,
          (elt1, mvbdu_opt) :: list_with_mvbdu,
          all_mvbdu_are_true && mvbdu_is_true ))
      (error, handler, [], [], true)
      (List.rev list)
  in
  if Remanent_parameters.get_post_processing parameters then (
    let error, handler, vars =
      Ckappa_sig.Views_bdu.variables_list_of_mvbdu parameters handler error
        mvbdu
    in
    let error, handler, var_list =
      Ckappa_sig.Views_bdu.extensional_of_variables_list parameters handler
        error vars
    in
    let error, var_list, guard_list =
      List.fold_left
        (fun (error, list, guard_list) elt ->
          let error, elt = rename_site_inverse parameters error elt in
          (* keep only the sites and not the guards, because the guards are contained in the mvbdu *)
          match Ckappa_sig.site_or_guard_p_of_mvbdu_var elt nsites with
          | Site _ -> error, elt :: list, guard_list
          | Guard_p _ -> error, list, elt :: guard_list)
        (error, [], []) (List.rev var_list)
    in
    match var_list with
    | [] ->
      error, (handler, No_known_translation list_with_mvbdu)
      (* indirectly checks if the mvbdu is true *)
    | [ x ] ->
      let error, list_with_mvbdu =
        List.fold_left
          (fun (error, list) (elt, mvbdu) ->
            match elt with
            | [ (a, b) ] when a = x -> error, (b, mvbdu) :: list
            | _ -> Exception.warn parameters error __POS__ Exit list)
          (error, []) list_with_mvbdu
      in
      error, (handler, Range (x, list_with_mvbdu))
    | [ _; _ ] ->
      (match list with
      | [] | [ _ ] ->
        Exception.warn parameters error __POS__ Exit
          (handler, No_known_translation list_with_mvbdu)
      | [
       [ (site1, state1); (site2, state2) ];
       [ (site1', state1'); (site2', state2') ];
      ]
        when all_mvbdu_are_true ->
        if site1 = site1' && site2 = site2' then
          if state1 > state1' then
            error, (handler, Equiv ((site1, state1), (site2, state2)))
          else
            error, (handler, Equiv ((site1, state1'), (site2, state2')))
        else
          Exception.warn parameters error __POS__ Exit
            (handler, No_known_translation list_with_mvbdu)
      | [
       [ (site1, state1); (site2, state2) ];
       [ (site1', state1'); (site2', state2') ];
       [ (site1'', state1''); (site2'', state2'') ];
      ]
        when all_mvbdu_are_true ->
        if
          site1 = site1' && site1 = site1'' && site2 = site2' && site2 = site2''
        then
          if state1 = state1' then
            if state1 < state1'' then
              error, (handler, Imply ((site1, state1''), (site2, state2'')))
            else
              ( error,
                ( handler,
                  Imply
                    ( ( site2,
                        if state2 = state2'' then
                          state2'
                        else
                          state2 ),
                      (site1, state1) ) ) )
          else if state1 = state1'' then
            if state1 < state1' then
              error, (handler, Imply ((site1, state1'), (site2, state2')))
            else
              ( error,
                ( handler,
                  Imply
                    ( ( site2,
                        if state2 = state2' then
                          state2''
                        else
                          state2 ),
                      (site1, state1) ) ) )
          else if state1' = state1'' then
            if state1' < state1 then
              error, (handler, Imply ((site1, state1), (site2, state2)))
            else
              ( error,
                ( handler,
                  Imply
                    ( ( site2,
                        if state2 = state2'' then
                          state2'
                        else
                          state2'' ),
                      (site1, state1') ) ) )
          else
            error, (handler, No_known_translation list_with_mvbdu)
        else
          Exception.warn parameters error __POS__ Exit
            (handler, No_known_translation list_with_mvbdu)
      | _ when all_mvbdu_are_true ->
        let error, handler, guards_list_consed =
          Ckappa_sig.Views_bdu.build_variables_list parameters handler error
            guard_list
        in
        let error, handler, mvbdu =
          Ckappa_sig.Views_bdu.mvbdu_project_abstract_away parameters handler
            error mvbdu guards_list_consed
        in
        let error, handler, output =
          try_partitioning parameters handler error kappa_handler
            rename_site_inverse mvbdu
        in
        (match output with
        | None -> error, (handler, No_known_translation list_with_mvbdu)
        | Some (var, l) -> error, (handler, Partition (var, l)))
      | _ -> error, (handler, No_known_translation list_with_mvbdu))
    | _ when all_mvbdu_are_true ->
      let error, handler, guards_list_consed =
        Ckappa_sig.Views_bdu.build_variables_list parameters handler error
          guard_list
      in
      let error, handler, mvbdu =
        Ckappa_sig.Views_bdu.mvbdu_project_abstract_away parameters handler
          error mvbdu guards_list_consed
      in
      let error, handler, output =
        try_partitioning parameters handler error kappa_handler
          rename_site_inverse mvbdu
      in
      (match output with
      | None -> error, (handler, No_known_translation list_with_mvbdu)
      | Some (var, l) -> error, (handler, Partition (var, l)))
    | _ -> error, (handler, No_known_translation list_with_mvbdu)
  ) else
    error, (handler, No_known_translation list_with_mvbdu)

(*****************************************************************************)

let rec print ?beginning_of_sentence:(beggining = true)
    ?(prompt_agent_type = true) ?(html_mode = false)
    ~show_dep_with_dimmension_higher_than:dim_min parameters handler_kappa
    bdu_handler restriction_bdu error agent_string agent_type agent_id
    translation t =
  let tab =
    if html_mode then
      "<PRE>         </PRE>"
    else
      "\t"
  in
  let endofline =
    if html_mode then
      "<Br>\n"
    else
      "\n"
  in
  let beginenumeration =
    if html_mode then
      "<UL>\n"
    else
      ""
  in
  let endenumeration =
    if html_mode then
      "</UL>\n"
    else
      ""
  in
  let beginenum =
    if html_mode then
      "<LI>"
    else
      "+"
  in
  let endenum =
    if html_mode then
      "</LI>\n"
    else
      ""
  in
  let cap s =
    if beggining then
      Tools.capitalize s
    else
      s
  in
  let in_agent s =
    if prompt_agent_type then
      "in agent " ^ s ^ " "
    else
      ""
  in
  let in_agent_comma s =
    if prompt_agent_type then
      "in agent " ^ s ^ ", "
    else
      ""
  in
  let log = Remanent_parameters.get_logger parameters in
  let nsites = Handler.get_nsites handler_kappa in
  let error, bdu_handler =
    match translation with
    | Range (site_type, state_list) ->
      if dim_min <= 1 then (
        match Remanent_parameters.get_backend_mode parameters with
        | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
          let error, t =
            Site_graphs.KaSa_site_graph.add_site_or_guard parameters error
              handler_kappa agent_id site_type t
          in
          let error =
            Site_graphs.KaSa_site_graph.print log parameters error t
          in
          let () = Loggers.fprintf log " => " in
          let should_use_bracket =
            match state_list with
            | [] | [ _ ] -> false
            | _ :: _ -> true
          in
          let () = if should_use_bracket then Loggers.fprintf log "[ " in
          let error, bdu_handler, _bool =
            List.fold_left
              (fun (error, bdu_handler, bool) (state, mvbdu) ->
                let () = if bool then Loggers.fprintf log " v " in
                let error, t =
                  Site_graphs.KaSa_site_graph.add_state_or_guard parameters
                    error handler_kappa agent_id site_type state t
                in
                let error =
                  Site_graphs.KaSa_site_graph.print log parameters error t
                in
                let error, bdu_handler =
                  Handler.print_guard_option parameters error handler_kappa
                    bdu_handler mvbdu
                in
                error, bdu_handler, true)
              (error, bdu_handler, false)
              state_list
          in
          let () = if should_use_bracket then Loggers.fprintf log " ]" in
          let () = Loggers.print_newline log in
          error, bdu_handler
        | Remanent_parameters_sig.Natural_language ->
          let error', site_string =
            Handler.string_of_site_or_guard_in_natural_language parameters error
              handler_kappa agent_type site_type
          in
          let error =
            Exception.check_point Exception.warn parameters error error' __POS__
              Exit
          in
          let rec aux list error bdu_handler =
            match list with
            | [] -> Exception.warn parameters error __POS__ Exit bdu_handler
            | [ (state, mvbdu) ] ->
              let error', state_string =
                Handler.string_of_state_fully_deciphered_with_guard parameters
                  error handler_kappa agent_type site_type state
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let () = Loggers.fprintf log " and %s" state_string in
              let error, bdu_handler =
                Handler.print_guard_option parameters error handler_kappa
                  bdu_handler mvbdu
              in
              let () = Loggers.fprintf log ".%s" endofline in
              error, bdu_handler
            | (state, mvbdu) :: tail ->
              let error', state_string =
                Handler.string_of_state_fully_deciphered_with_guard parameters
                  error handler_kappa agent_type site_type state
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let () = Loggers.fprintf log " %s" state_string in
              let error, bdu_handler =
                Handler.print_guard_option parameters error handler_kappa
                  bdu_handler mvbdu
              in
              let () = Loggers.fprintf log "," in
              aux tail error bdu_handler
          in
          let error, bdu_handler =
            match state_list with
            | [] -> Exception.warn parameters error __POS__ Exit bdu_handler
            | [ (state, mvbdu) ] ->
              let error', state_string =
                Handler.string_of_state_fully_deciphered_with_guard parameters
                  error handler_kappa agent_type site_type state
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let error, bdu_handler =
                match mvbdu with
                | None ->
                  let () =
                    Loggers.fprintf log "%s%s %sis always %s"
                      (Remanent_parameters.get_prefix parameters)
                      (cap site_string) (in_agent agent_string) state_string
                  in
                  error, bdu_handler
                | Some mvbdu ->
                  let () =
                    Loggers.fprintf log "%s%s %scan only be %s but only if "
                      (Remanent_parameters.get_prefix parameters)
                      (cap site_string) (in_agent agent_string) state_string
                  in
                  Handler.print_guard_mvbdu parameters error handler_kappa
                    bdu_handler mvbdu
              in
              let () = Loggers.fprintf log ".%s" endofline in
              error, bdu_handler
            | [ (state1, mvbdu1); (state2, mvbdu2) ] ->
              let error', state_string1 =
                Handler.string_of_state_fully_deciphered_with_guard parameters
                  error handler_kappa agent_type site_type state1
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let error', state_string2 =
                Handler.string_of_state_fully_deciphered_with_guard parameters
                  error handler_kappa agent_type site_type state2
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let () =
                Loggers.fprintf log "%s%s %sranges over %s"
                  (Remanent_parameters.get_prefix parameters)
                  (cap site_string) (in_agent agent_string) state_string1
              in
              let error, bdu_handler =
                Handler.print_guard_option parameters error handler_kappa
                  bdu_handler mvbdu1
              in
              let () =
                Loggers.fprintf log "\n             and %s" state_string2
              in
              let error, bdu_handler =
                Handler.print_guard_option parameters error handler_kappa
                  bdu_handler mvbdu2
              in
              let () = Loggers.fprintf log ".%s" endofline in
              error, bdu_handler
            | list ->
              let () =
                Loggers.fprintf log "%s%s %sranges over"
                  (Remanent_parameters.get_prefix parameters)
                  (cap site_string) (in_agent agent_string)
              in
              aux list error bdu_handler
          in
          error, bdu_handler
      ) else
        error, bdu_handler
    | Equiv ((site1, state1), (site2, state2)) ->
      if dim_min <= 2 then (
        let error, () =
          match Remanent_parameters.get_backend_mode parameters with
          | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
            let error, t' =
              Site_graphs.KaSa_site_graph.add_state_or_guard parameters error
                handler_kappa agent_id site1 state1 t
            in
            let error, t'' =
              Site_graphs.KaSa_site_graph.add_state_or_guard parameters error
                handler_kappa agent_id site2 state2 t
            in
            let error =
              Site_graphs.KaSa_site_graph.print
                (Remanent_parameters.get_logger parameters)
                parameters error t'
            in
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                " <=> "
            in
            let error =
              Site_graphs.KaSa_site_graph.print
                (Remanent_parameters.get_logger parameters)
                parameters error t''
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            error, ()
          | Remanent_parameters_sig.Natural_language ->
            let error', site_string1 =
              Handler.string_of_site_or_guard_in_natural_language parameters
                error handler_kappa agent_type site1
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            let error', state_string1 =
              Handler.string_of_state_fully_deciphered_with_guard parameters
                error handler_kappa agent_type site1 state1
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            let error', site_string2 =
              Handler.string_of_site_or_guard_in_natural_language parameters
                error handler_kappa agent_type site2
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            let error', state_string2 =
              Handler.string_of_state_fully_deciphered_with_guard parameters
                error handler_kappa agent_type site2 state2
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            ( error,
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%s%s%s is %s, if and only if, %s is %s.%s"
                (Remanent_parameters.get_prefix parameters)
                (cap (in_agent_comma agent_string))
                site_string1 state_string1 site_string2 state_string2 endofline
            )
        in
        error, bdu_handler
      ) else
        error, bdu_handler
    | Imply ((site1, state1), (site2, state2)) ->
      if dim_min <= 2 then (
        let error, () =
          match Remanent_parameters.get_backend_mode parameters with
          | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
            let error, t =
              Site_graphs.KaSa_site_graph.add_state_or_guard parameters error
                handler_kappa agent_id site1 state1 t
            in
            let error, t' =
              Site_graphs.KaSa_site_graph.add_state_or_guard parameters error
                handler_kappa agent_id site2 state2 t
            in
            let error =
              Site_graphs.KaSa_site_graph.print
                (Remanent_parameters.get_logger parameters)
                parameters error t
            in
            let () =
              Loggers.fprintf (Remanent_parameters.get_logger parameters) " => "
            in
            let error =
              Site_graphs.KaSa_site_graph.print
                (Remanent_parameters.get_logger parameters)
                parameters error t'
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            error, ()
          | Remanent_parameters_sig.Natural_language ->
            let error', site_string1 =
              Handler.string_of_site_or_guard_in_natural_language parameters
                error handler_kappa agent_type site1
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            let error', state_string1 =
              Handler.string_of_state_fully_deciphered_with_guard parameters
                error handler_kappa agent_type site1 state1
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            let error', site_string2 =
              Handler.string_of_site_or_guard_in_natural_language parameters
                error handler_kappa agent_type site2
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            let error', state_string2 =
              Handler.string_of_state_fully_deciphered_with_guard parameters
                error handler_kappa agent_type site2 state2
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            ( error,
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%s%s%s is %s whenever %s is %s.%s"
                (Remanent_parameters.get_prefix parameters)
                (cap (in_agent_comma agent_string))
                site_string2 state_string2 site_string1 state_string1 endofline
            )
        in
        error, bdu_handler
      ) else
        error, bdu_handler
    | Partition (v, list) ->
      (*let () =
        Loggers.fprintf log
          "%s%s%s" (Remanent_parameters.get_prefix parameters)
          (cap (in_agent_colon agent_string)) endofline
        in*)
      let error, site_string =
        Handler.string_of_site_or_guard_in_natural_language parameters error
          handler_kappa agent_type v
      in
      let parameters = Remanent_parameters.update_prefix parameters tab in
      let error, bdu_handler =
        List.fold_left
          (fun (error, bdu_handler) (a, list) ->
            let error, parameters =
              match Remanent_parameters.get_backend_mode parameters with
              | Remanent_parameters_sig.Natural_language ->
                let error, state_string =
                  Handler.string_of_state_fully_deciphered_with_guard parameters
                    error handler_kappa agent_type v a
                in
                let () =
                  Loggers.fprintf log "%swhen %s is equal to %s, then:%s%s"
                    (Remanent_parameters.get_prefix parameters)
                    site_string state_string endofline beginenumeration
                in
                let parameters =
                  Remanent_parameters.update_prefix parameters
                    (tab ^ beginenum ^ " ")
                in
                error, parameters
              | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
                error, parameters
            in
            let error, t' =
              Site_graphs.KaSa_site_graph.add_state_or_guard parameters error
                handler_kappa agent_id v a t
            in
            let error, bdu_handler =
              List.fold_left
                (fun (error, bdu_handler) token ->
                  let error, bdu_handler =
                    print ~beginning_of_sentence:false ~prompt_agent_type:false
                      ~html_mode ~show_dep_with_dimmension_higher_than:0
                      parameters handler_kappa bdu_handler restriction_bdu error
                      agent_string agent_type agent_id token t'
                  in
                  let () = Loggers.fprintf log "%s" endenum in
                  error, bdu_handler)
                (error, bdu_handler) list
            in
            let () = Loggers.fprintf log "%s" endenumeration in
            error, bdu_handler)
          (error, bdu_handler) list
      in
      error, bdu_handler
    | No_known_translation list ->
      (match Remanent_parameters.get_backend_mode parameters with
      | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
        let error = Site_graphs.KaSa_site_graph.print log parameters error t in
        let prefix = "   " in
        let () = Loggers.fprintf log " =>\n%s[\n" prefix in
        let prefix' = prefix in
        let prefix = "\t" in
        let error, bdu_handler, _bool =
          List.fold_left
            (fun (error, bdu_handler, bool) (state_list, mvbdu) ->
              let () =
                Loggers.fprintf log "%s%s" prefix
                  (if bool then
                     "v "
                   else
                     "  ")
              in
              let error, t' =
                List.fold_left
                  (fun (error, t) (site, state) ->
                    Site_graphs.KaSa_site_graph.add_state_or_guard parameters
                      error handler_kappa agent_id site state t)
                  (error, t) state_list
              in
              let error =
                Site_graphs.KaSa_site_graph.print log parameters error t'
              in
              let error, bdu_handler =
                Handler.print_guard_option parameters error handler_kappa
                  bdu_handler mvbdu
              in
              let () = Loggers.print_newline log in
              error, bdu_handler, true)
            (error, bdu_handler, false)
            list
        in
        let () = Loggers.fprintf log "%s]\n" prefix' in
        error, bdu_handler
      | Remanent_parameters_sig.Natural_language ->
        (match list with
        | [] -> error, bdu_handler
        | (head, _) :: _ ->
          let n = List.length head in
          if n >= dim_min then (
            let () =
              Loggers.fprintf log "%s%s"
                (Remanent_parameters.get_prefix parameters)
                (cap (in_agent_comma agent_string))
            in
            let error, () =
              let rec aux l error =
                match l with
                | [ (a, _) ] ->
                  let error, string =
                    Handler.string_of_site_or_guard_in_natural_language
                      parameters error handler_kappa agent_type a
                  in
                  let () = Loggers.fprintf log ", and %s, " string in
                  error, ()
                | [] -> Exception.warn parameters error __POS__ Exit ()
                | (a, _) :: b ->
                  let error, string =
                    Handler.string_of_site_or_guard_in_natural_language
                      parameters error handler_kappa agent_type a
                  in
                  let () = Loggers.fprintf log ", %s" string in
                  aux b error
              in
              match head with
              | [] | [ _ ] -> Exception.warn parameters error __POS__ Exit ()
              | (a, _) :: b ->
                let error, string =
                  Handler.string_of_site_or_guard_in_natural_language parameters
                    error handler_kappa agent_type a
                in
                let () = Loggers.fprintf log "%s" string in
                aux b error
            in
            let () =
              Loggers.fprintf log
                "are entangled by the following %i-d relationship:%s" n
                endofline
            in
            let parameters =
              Remanent_parameters.update_prefix parameters "\t"
            in
            List.fold_left
              (fun (error, bdu_handler) (l, mvbdu) ->
                let error, bdu_handler, bool =
                  List.fold_left
                    (fun (error, bdu_handler, bool) (site_type, state) ->
                      let site_or_guard =
                        Ckappa_sig.site_or_guard_p_of_mvbdu_var site_type nsites
                      in
                      let error', site_string =
                        match site_or_guard with
                        | Ckappa_sig.Site s ->
                          Handler.string_of_site parameters error handler_kappa
                            agent_type s
                        | Ckappa_sig.Guard_p _ ->
                          Exception.warn parameters error __POS__ ~message:""
                            Exit ""
                      in
                      let error =
                        Exception.check_point Exception.warn parameters error
                          error' __POS__ Exit
                      in
                      let error', state_string =
                        Handler.string_of_state_fully_deciphered_with_guard
                          parameters error handler_kappa agent_type site_type
                          state
                      in
                      let error =
                        Exception.check_point Exception.warn parameters error
                          error' __POS__ Exit
                      in
                      (*---------------------------------------------*)
                      let () =
                        if bool then
                          Loggers.fprintf log ","
                        else
                          Loggers.fprintf log "%s%s("
                            (Remanent_parameters.get_prefix parameters)
                            agent_string
                      in
                      let () =
                        Loggers.fprintf log "%s%s" site_string state_string
                      in
                      error, bdu_handler, true)
                    (error, bdu_handler, false)
                    l
                in
                (*----------------------------------------------------*)
                let () = if bool then Loggers.fprintf log ")" in
                let error, bdu_handler =
                  Handler.print_guard_option parameters error handler_kappa
                    bdu_handler mvbdu
                in
                let () = if bool then Loggers.fprintf log "%s" endofline in
                error, bdu_handler)
              (error, bdu_handler) list
          ) else
            error, bdu_handler))
  in
  error, bdu_handler

(*****************************************************************************)
(*TODO:convert views to json*)

let rec convert_views_internal_constraints_list_aux
    ~show_dep_with_dimmension_higher_than:dim_min parameters handler_kappa error
    agent_string agent_type agent_id translation t current_list bdu_handler =
  let error, bdu_handler, current_list =
    match translation with
    | Range (site_type, state_list) ->
      if dim_min <= 1 then (
        match Remanent_parameters.get_backend_mode parameters with
        | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
          (*hyp*)
          (*-----------------------------------------------------*)
          let error, t =
            Site_graphs.KaSa_site_graph.add_site_or_guard parameters error
              handler_kappa agent_id site_type t
          in
          let error'', bdu_handler, refinement =
            List.fold_left
              (fun (error, bdu_handler, c_list) (state, mvbdu) ->
                let error', t' =
                  Site_graphs.KaSa_site_graph.add_state_or_guard parameters
                    error handler_kappa agent_id site_type state t
                in
                let error =
                  Exception.check_point Exception.warn parameters error error'
                    __POS__ Exit
                in
                let error, bdu_handler, formula =
                  Handler.mvbdu_to_string_formula_option parameters error
                    handler_kappa bdu_handler mvbdu
                in
                error, bdu_handler, (t', formula) :: c_list)
              (error, bdu_handler, []) state_list
          in
          let lemma = { Public_data.hyp = t; Public_data.refinement } in
          let current_list = lemma :: current_list in
          let error =
            Exception.check_point Exception.warn parameters error error''
              __POS__ Exit
          in
          error, bdu_handler, current_list
        | Remanent_parameters_sig.Natural_language ->
          error, bdu_handler, current_list
      ) else
        error, bdu_handler, current_list
    | Equiv ((site1, state1), (site2, state2)) ->
      if dim_min <= 2 then (
        match Remanent_parameters.get_backend_mode parameters with
        | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
          let error', t' =
            Site_graphs.KaSa_site_graph.add_state_or_guard parameters error
              handler_kappa agent_id site1 state1 t
          in
          let error =
            Exception.check_point Exception.warn parameters error error' __POS__
              Exit
          in
          (*--------------------------------------------------*)
          let error''', t'' =
            Site_graphs.KaSa_site_graph.add_state_or_guard parameters error
              handler_kappa agent_id site2 state2 t
          in
          let error =
            Exception.check_point Exception.warn parameters error error'''
              __POS__ Exit
          in
          (*--------------------------------------------------*)
          let lemma =
            { Public_data.hyp = t'; Public_data.refinement = [ t'', None ] }
          in
          let current_list = lemma :: current_list in
          error, bdu_handler, List.rev current_list
        | Remanent_parameters_sig.Natural_language ->
          error, bdu_handler, current_list
      ) else
        error, bdu_handler, current_list
    | Imply ((site1, state1), (site2, state2)) ->
      if dim_min <= 2 then (
        match Remanent_parameters.get_backend_mode parameters with
        | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
          let error', t =
            Site_graphs.KaSa_site_graph.add_state_or_guard parameters error
              handler_kappa agent_id site1 state1 t
          in
          let error =
            Exception.check_point Exception.warn parameters error error' __POS__
              Exit
          in
          (*--------------------------------------------------*)
          let error''', t' =
            Site_graphs.KaSa_site_graph.add_state_or_guard parameters error
              handler_kappa agent_id site2 state2 t
          in
          let error =
            Exception.check_point Exception.warn parameters error error'''
              __POS__ Exit
          in
          (*--------------------------------------------------*)
          let lemma =
            { Public_data.hyp = t; Public_data.refinement = [ t', None ] }
          in
          let current_list = lemma :: current_list in
          error, bdu_handler, List.rev current_list
        | Remanent_parameters_sig.Natural_language ->
          error, bdu_handler, current_list
      ) else
        error, bdu_handler, current_list
    | Partition (site_type, list) ->
      let error, bdu_handler, current_list =
        List.fold_left
          (fun (error, bdu_handler, current_list) (state, list) ->
            let error', t' =
              Site_graphs.KaSa_site_graph.add_state_or_guard parameters error
                handler_kappa agent_id site_type state t
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            let error'', bdu_handler, current_list =
              List.fold_left
                (fun (error, bdu_handler, current_list) token ->
                  convert_views_internal_constraints_list_aux
                    ~show_dep_with_dimmension_higher_than:0 parameters
                    handler_kappa error agent_string agent_type agent_id token
                    t' current_list bdu_handler)
                (error, bdu_handler, current_list)
                list
            in
            let error =
              Exception.check_point Exception.warn parameters error error''
                __POS__ Exit
            in
            error, bdu_handler, current_list)
          (error, bdu_handler, current_list)
          list
      in
      error, bdu_handler, current_list
    | No_known_translation list ->
      (match Remanent_parameters.get_backend_mode parameters with
      | Remanent_parameters_sig.Kappa | Remanent_parameters_sig.Raw ->
        let error, bdu_handler, current_list =
          let error'', bdu_handler, refinement =
            List.fold_left
              (fun (error, bdu_handler, current_list) (state_list, mvbdu) ->
                let error, t' =
                  List.fold_left
                    (fun (error, t') (site, state) ->
                      let error', t' =
                        Site_graphs.KaSa_site_graph.add_state_or_guard
                          parameters error handler_kappa agent_id site state t'
                      in
                      let error =
                        Exception.check_point Exception.warn parameters error
                          error' __POS__ Exit
                      in
                      error, t')
                    (error, t) state_list
                in
                let error, bdu_handler, formula =
                  Handler.mvbdu_to_string_formula_option parameters error
                    handler_kappa bdu_handler mvbdu
                in
                let refinement = (t', formula) :: current_list in
                error, bdu_handler, refinement)
              (error, bdu_handler, []) list
          in
          let error =
            Exception.check_point Exception.warn parameters error error''
              __POS__ Exit
          in
          (*----------------------------------------*)
          let lemma = { Public_data.hyp = t; Public_data.refinement } in
          let current_list = lemma :: current_list in
          error, bdu_handler, current_list
        in
        error, bdu_handler, current_list
      | Remanent_parameters_sig.Natural_language ->
        error, bdu_handler, current_list)
  in
  error, bdu_handler, current_list

let convert_views_internal_constraints_list
    ~show_dep_with_dimmension_higher_than:dim_min parameters handler_kappa
    bdu_handler error agent_string agent_type translation current_list =
  let t = Site_graphs.KaSa_site_graph.empty in
  let error', agent_id, t =
    Site_graphs.KaSa_site_graph.add_agent parameters error handler_kappa
      agent_type t
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  let error, bdu_handler, current_list =
    match translation with
    | Range (site, _) ->
      let error, t =
        Site_graphs.KaSa_site_graph.add_site_or_guard parameters error
          handler_kappa agent_id site t
      in
      let error, bdu_handler, current_list =
        convert_views_internal_constraints_list_aux
          ~show_dep_with_dimmension_higher_than:dim_min parameters handler_kappa
          error agent_string agent_type agent_id translation t current_list
          bdu_handler
      in
      error, bdu_handler, current_list
    | Equiv _ | Imply _ | Partition _ | No_known_translation _ ->
      let error, bdu_handler, current_list =
        convert_views_internal_constraints_list_aux
          ~show_dep_with_dimmension_higher_than:dim_min parameters handler_kappa
          error agent_string agent_type agent_id translation t current_list
          bdu_handler
      in
      error, bdu_handler, current_list
  in
  error, bdu_handler, current_list

(*****************************************************************************)

let print ?beginning_of_sentence:(beggining = true) ?(prompt_agent_type = true)
    ?(html_mode = false) ~show_dep_with_dimmension_higher_than:dim_min
    parameters handler_kappa bdu_handler restriction_bdu error agent_string
    agent_type translation =
  let t = Site_graphs.KaSa_site_graph.empty in
  let error, id, t =
    Site_graphs.KaSa_site_graph.add_agent parameters error handler_kappa
      agent_type t
  in
  print ~beginning_of_sentence:beggining ~prompt_agent_type ~html_mode
    ~show_dep_with_dimmension_higher_than:dim_min parameters handler_kappa
    bdu_handler restriction_bdu error agent_string agent_type id translation t
