(**
 * translation_in_natural_language.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: 2016
 * Last modification: Time-stamp: <Oct 18 2016>
 * *
 * Signature for prepreprocessing language ckappa
 *
 * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

let trace = false

type token =
  | Range of Ckappa_sig.c_site_name * Ckappa_sig.c_state list
  | Equiv of (Ckappa_sig.c_site_name * Ckappa_sig.c_state) * (Ckappa_sig.c_site_name * Ckappa_sig.c_state)
  | Imply of (Ckappa_sig.c_site_name * Ckappa_sig.c_state) * (Ckappa_sig.c_site_name * Ckappa_sig.c_state)
  | Partition of (Ckappa_sig.c_site_name * (Ckappa_sig.c_state * token list) list)
  | No_known_translation of (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list list

type rename_sites =
  (Remanent_parameters_sig.parameters ->
   Exception.method_handler ->
   Ckappa_sig.Site_map_and_set.Map.elt ->
   Exception.method_handler * Ckappa_sig.Site_map_and_set.Map.elt)


module Triple_pair_list_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = Ckappa_backend.Ckappa_backend.triple_pair_list
         let compare = compare
         let print _ _ = ()
       end))

(****************************************************************************)

let non_relational parameters handler error mvbdu =
  let error, handler, list =
    Ckappa_sig.Views_bdu.mvbdu_cartesian_abstraction parameters handler error mvbdu
  in
  let error, handler, mvbdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
  in
  let error, handler, recomposition =
    List.fold_left
      (fun (error,handler,conjunct) term ->
         Ckappa_sig.Views_bdu.mvbdu_and parameters handler error conjunct term
      )
      (error, handler, mvbdu_true) list
  in
  error, handler,
  Ckappa_sig.Views_bdu.equal mvbdu recomposition

let try_partitioning parameters handler error (rename_site_inverse:rename_sites) mvbdu =
  let error, handler, mvbdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true parameters handler error
  in
  let error, handler, var_hconsed_list =
    Ckappa_sig.Views_bdu.variables_list_of_mvbdu parameters handler error mvbdu
  in
  let error, handler, var_list =
    Ckappa_sig.Views_bdu.extensional_of_variables_list
      parameters handler error var_hconsed_list
  in
  let rec aux l (error, handler) =
    match
      l
    with
    | [] -> error, handler, None
    | head :: tail ->
      let error, handler, singleton =
        Ckappa_sig.Views_bdu.build_variables_list
          parameters handler error [head]
      in
      let error, handler, mvbdu_ref =
        Ckappa_sig.Views_bdu.mvbdu_project_abstract_away
          parameters handler error mvbdu singleton
      in
      let error, handler, proj_in =
        Ckappa_sig.Views_bdu.mvbdu_project_keep_only
          parameters handler error mvbdu singleton
      in
      let error, handler, list_asso =
        Ckappa_sig.Views_bdu.extensional_of_mvbdu
          parameters handler error proj_in
      in
      let error, range =
        let rec aux2 list (error, output) =
          match
            list
          with
          | [] -> (error, output)
          | [(x, i)] :: tail when x = head ->
            aux2 tail (error, (i :: output))
          | _ :: tail ->
            aux2 tail
              (Exception.warn parameters error __POS__ Exit output)
        in aux2 list_asso (error, [])
      in
      let rec aux3 list (error,handler,output) =
        match
          list
        with
        | [] -> error, handler, Some output
        | h :: t ->
          begin
            let error, handler, select =
              Ckappa_sig.Views_bdu.build_association_list
                parameters handler error [head, h]
            in
            let error, handler, mvbdu_case =
              Ckappa_sig.Views_bdu.mvbdu_redefine
                parameters handler error mvbdu_true select
            in
            let error, handler, case =
              Ckappa_sig.Views_bdu.mvbdu_and
                parameters handler error mvbdu_case mvbdu
            in
            let error, handler, bool =
              non_relational parameters handler error case
            in
            if bool
            then
              let error, handler, away =
                Ckappa_sig.Views_bdu.mvbdu_project_abstract_away
                  parameters handler error case singleton
              in
              if
                Ckappa_sig.Views_bdu.equal
                  away mvbdu_ref
              then
                aux3 t (error, handler, output)
              else
                let error, handler, list =
                  Ckappa_sig.Views_bdu.mvbdu_cartesian_abstraction
                    parameters handler error away
                in
                let error, handler, list =
                  List.fold_left
                    (fun (error, handler, list) elt ->
                       let error, handler, elt =
                         Ckappa_sig.Views_bdu.extensional_of_mvbdu
                           parameters handler error elt
                       in
                       begin
                         let error, var_list_opt =
                           match
                             elt
                           with
                           | [] | [] :: _ | ((_, _) :: _ :: _) :: _ -> error, None
                           | [(a, b)] :: q ->
                             begin
                               let rec aux4 q output =
                                 match q with
                                 | [] -> error, Some (a, output)
                                 | [(c, d)] :: q when c = a ->
                                   aux4 q (d :: output)
                                 | _ -> error, None
                               in aux4 q [b]
                             end
                         in
                         match var_list_opt with
                         | None ->
                           let error, () =
                             Exception.warn
                               parameters error __POS__ Exit ()
                           in
                           error, handler, list
                         | Some (a, l) ->
                           let error, a' = rename_site_inverse parameters error a in
                           (error, handler,
                            (
                              (Range
                                 (a',
                                  l))
                              :: list))
                       end)
                    (error, handler, [])
                    (List.rev list)
                in
                aux3 t (error, handler, ((h, list) :: output))
            else
              error, handler, None
          end
      in
      let error, handler, output = aux3 range (error, handler, []) in
      match output with
      | None -> aux tail (error, handler)
      | Some l ->
        let error, head = rename_site_inverse parameters error head in
        error, handler, Some (head, l)
  in
  aux var_list (error, handler)

(****************************************************************************)

let translate parameters handler error (rename_site_inverse: rename_sites)
    mvbdu =
  let error, handler, list =
    Ckappa_sig.Views_bdu.extensional_of_mvbdu
      parameters handler error mvbdu
  in
  let error, list =
    List.fold_left
      (fun (error, list) elt1 ->
         let error, elt1 =
           List.fold_left
             (fun (error, list) (elt2, asso) ->
                let error, elt2 = rename_site_inverse parameters error elt2 in
                error, (elt2, asso) :: list
             )
             (error, []) (List.rev elt1)
         in
         error, elt1::list)
      (error, [])
      (List.rev list)
  in
  if Remanent_parameters.get_post_processing  parameters
  then
    begin
      let error, handler, vars =
        Ckappa_sig.Views_bdu.variables_list_of_mvbdu
          parameters handler error mvbdu
      in
      let error, handler, var_list =
        Ckappa_sig.Views_bdu.extensional_of_variables_list
          parameters handler error vars
      in
      let error, var_list =
        List.fold_left
          (fun (error, list) elt ->
             let error, elt = rename_site_inverse parameters error elt in
             error, elt::list)
          (error, [])
          (List.rev var_list)
      in
      match var_list with
      | [] -> error, (handler, No_known_translation list) (* OK if the agent has no sites *)
      | [x] ->
        let error, list =
          List.fold_left
            (fun (error, list) elt ->
               match elt with
               | [a, b] when a = x -> error, b :: list
               | _ ->
                 Exception.warn
                   parameters error __POS__ Exit list)
            (error, [])
            list
        in
        error, (handler, Range (x, list))
      | [_; _] ->
        begin
          match list with
          | [] | [_] ->
            Exception.warn
              parameters error __POS__ Exit
              (handler, No_known_translation list)
          | [[site1,state1; site2,state2]; [site1',state1'; site2',state2']] ->
            begin
              if site1 = site1' && site2 = site2'
              then
                if state1 > state1'
                then
                  error, (handler, Equiv ((site1, state1), (site2, state2)))
                else
                  error, (handler, Equiv ((site1, state1'), (site2, state2')))
              else
                Exception.warn
                  parameters error __POS__ Exit
                  (handler, No_known_translation list)
            end
          | [[site1,state1; site2,state2];
             [site1',state1'; site2',state2'];
             [site1'',state1'';site2'',state2'']] ->
            begin
              if site1 = site1' && site1 = site1'' && site2 = site2' && site2 = site2''
              then
                if state1 = state1'
                then
                  if state1 < state1''
                  then error,
                       (handler, Imply ((site1, state1''), (site2, state2'')))
                  else error,
                       (handler, Imply ((site2,
                                         if state2 = state2''
                                         then state2'
                                         else state2)
                                       , ((site1, state1))))
                else if state1 = state1''
                then
                  if state1 < state1'
                  then error,
                       (handler, Imply ((site1, state1'),(site2, state2')))
                  else error,
                       (handler, Imply ((site2,
                                         if state2 = state2'
                                         then state2''
                                         else state2)
                                       , (site1, state1)))
                else if state1' = state1''
                then
                  if state1' < state1
                  then
                    error, (handler, Imply ((site1, state1),(site2, state2)))
                  else
                    error,
                    (handler, Imply ((site2,
                                      if state2 = state2''
                                      then state2'
                                      else state2'')
                                    , (site1, state1')))
                else error, (handler, No_known_translation list)
              else
                Exception.warn
                  parameters error __POS__ Exit
                  (handler, No_known_translation list)
            end
          | _ ->
            begin
              let error, handler, output =
                try_partitioning
                  parameters
                  handler
                  error
                  rename_site_inverse
                  mvbdu
              in
              match
                output
              with
              | None -> error, (handler, No_known_translation list)
              | Some (var, l) ->
                error,
                (handler,
                 Partition (
                   var,
                   l))
            end
        end
      | _ ->
        begin
          let error, handler, output =
            try_partitioning parameters handler error rename_site_inverse mvbdu
          in
          match
            output
          with
          | None -> error, (handler, No_known_translation list)
          | Some (var, l) ->
            error,
            (handler,
             Partition (var, l))
        end
    end
  else
    error, (handler, No_known_translation list)

(*****************************************************************************)

let rec print ?beginning_of_sentence:(beggining=true)
    ?prompt_agent_type:(prompt_agent_type=true) ?html_mode:(html_mode=false)
    ~show_dep_with_dimmension_higher_than:dim_min parameters handler_kappa error
    agent_string agent_type agent_id translation t =
  let tab = if html_mode then "<PRE>         </PRE>" else "\t" in
  let endofline = if html_mode then "<Br>\n" else "\n" in
  let beginenumeration = if html_mode then "<UL>\n" else "" in
  let endenumeration = if html_mode then "</UL>\n" else "" in
  let beginenum = if html_mode then "<LI>" else "+" in
  let endenum = if html_mode then "</LI>\n" else "" in
  let cap s = if beggining then String.capitalize s else s in
  let in_agent s = if prompt_agent_type then "in agent "^s^" " else "" in
  let in_agent_comma s = if prompt_agent_type then "in agent "^s^", " else "" in
  let in_agent_colon s = if prompt_agent_type then "in agent "^s^": " else "" in
  let error, () =
    match
      translation
    with
    | Range (site_type, state_list) ->
      begin
        if dim_min <= 1
        then
          begin
            match Remanent_parameters.get_backend_mode parameters
            with
            | Remanent_parameters_sig.Kappa
            | Remanent_parameters_sig.Raw ->
              let error =
                Ckappa_backend.Ckappa_backend.print
                  (Remanent_parameters.get_logger parameters) parameters error handler_kappa
                  t
              in
              let () =
                Loggers.fprintf (Remanent_parameters.get_logger parameters) " => [ " in
              let error, _bool =
                List.fold_left
                  (fun (error, bool) state ->
                     let () =
                       if bool then
                         Loggers.fprintf (Remanent_parameters.get_logger parameters)
                           " v "
                     in
                     let error, t =
                       Ckappa_backend.Ckappa_backend.add_state
                         parameters error handler_kappa
                         agent_id site_type state t
                     in
                     let error =
                       Ckappa_backend.Ckappa_backend.print
                         (Remanent_parameters.get_logger parameters) parameters error handler_kappa
                         t
                     in
                     error, true)
                  (error, false) state_list
              in
              let () =
                Loggers.fprintf (Remanent_parameters.get_logger parameters) " ]" in
              let () =
                Loggers.print_newline (Remanent_parameters.get_logger parameters)
              in
              error, ()
            | Remanent_parameters_sig.Natural_language ->
              let error', site_string =
                Handler.string_of_site_in_natural_language parameters error handler_kappa
                  agent_type
                  site_type
              in
              let error =
                Exception.check_point
                  Exception.warn parameters error error' __POS__ Exit
              in
              let rec aux list error =
                match list
                with
                | [] ->
                  Exception.warn parameters error __POS__ Exit ()
                | [state] ->
                  let error', state_string =
                    Handler.string_of_state_fully_deciphered parameters error
                      handler_kappa agent_type
                      site_type
                      state
                  in
                  let error =
                    Exception.check_point
                      Exception.warn parameters error error'
                      __POS__ Exit
                  in
                  error, Loggers.fprintf (Remanent_parameters.get_logger parameters)
                    " and %s.%s" state_string endofline
                | state :: tail ->
                  let error', state_string =
                    Handler.string_of_state_fully_deciphered parameters error
                      handler_kappa agent_type
                      site_type
                      state
                  in
                  let error =
                    Exception.check_point
                      Exception.warn  parameters error error'
                      __POS__ Exit
                  in
                  let () =
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters)
                      " %s," state_string
                  in
                  aux tail error
              in
              match
                state_list
              with
              | [] -> Exception.warn  parameters error __POS__ Exit ()
              | [state] ->
                let error', state_string =
                  Handler.string_of_state_fully_deciphered parameters error
                    handler_kappa agent_type
                    site_type
                    state
                in
                let error =
                  Exception.check_point
                    Exception.warn  parameters error error'
                    __POS__ Exit
                in
                error,
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "%s%s %sis always %s.%s"
                  (Remanent_parameters.get_prefix parameters)
                  (cap site_string) (in_agent agent_string) state_string endofline
              | [state1; state2] ->
                let error', state_string1 =
                  Handler.string_of_state_fully_deciphered parameters error
                    handler_kappa agent_type
                    site_type
                    state1
                in
                let error =
                  Exception.check_point
                    Exception.warn  parameters error error'
                    __POS__ Exit
                in
                let error', state_string2 =
                  Handler.string_of_state_fully_deciphered parameters error
                    handler_kappa agent_type
                    site_type
                    state2
                in
                let error =
                  Exception.check_point
                    Exception.warn parameters error error' __POS__ Exit
                in
                error, Loggers.fprintf (Remanent_parameters.get_logger parameters)
                  "%s%s %sranges over %s and %s.%s"
                  (Remanent_parameters.get_prefix parameters)
                  (cap site_string)
                  (in_agent agent_string) state_string1 state_string2 endofline
              | list ->
                let () = Loggers.fprintf (Remanent_parameters.get_logger parameters)
                    "%s%s %sranges over"
                    (Remanent_parameters.get_prefix parameters)
                    (cap site_string)  (in_agent agent_string)
                in
                aux list error
          end
        else error,()
      end
    | Equiv ((site1,state1),(site2,state2)) ->
      if dim_min <= 2
      then
        begin
          match Remanent_parameters.get_backend_mode parameters
          with
          | Remanent_parameters_sig.Kappa
          | Remanent_parameters_sig.Raw ->
            let error, t' =
              Ckappa_backend.Ckappa_backend.add_state
                parameters error handler_kappa
                agent_id site1 state1 t
            in
            let error, t'' =
              Ckappa_backend.Ckappa_backend.add_state
                parameters error handler_kappa
                agent_id site2 state2 t
            in
            let error =
              Ckappa_backend.Ckappa_backend.print
                (Remanent_parameters.get_logger parameters) parameters error handler_kappa
                t'
            in
            let () =
              Loggers.fprintf (Remanent_parameters.get_logger parameters) " <=> " in
            let error =
              Ckappa_backend.Ckappa_backend.print
                (Remanent_parameters.get_logger parameters) parameters error handler_kappa
                t''
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            error, ()
          | Remanent_parameters_sig.Natural_language ->
            let error', site_string1 =
              Handler.string_of_site_in_natural_language parameters error handler_kappa
                agent_type
                site1
            in
            let error =
              Exception.check_point
                Exception.warn parameters error error' __POS__ Exit
            in
            let error', state_string1 =
              Handler.string_of_state_fully_deciphered parameters error
                handler_kappa agent_type
                site1
                state1
            in
            let error =
              Exception.check_point
                Exception.warn  parameters error error' __POS__ Exit
            in
            let error', site_string2 =
              Handler.string_of_site_in_natural_language
                parameters error handler_kappa
                agent_type site2
            in
            let error =
              Exception.check_point
                Exception.warn  parameters error error' __POS__ Exit
            in
            let error', state_string2 =
              Handler.string_of_state_fully_deciphered parameters error
                handler_kappa agent_type
                site2
                state2
            in
            let error =
              Exception.check_point
                Exception.warn parameters error error' __POS__ Exit in
            error,
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%s%s%s is %s, if and only if, %s is %s.%s"
              (Remanent_parameters.get_prefix parameters)
              (cap (in_agent_comma agent_string))
              site_string1 state_string1 site_string2 state_string2 endofline
        end
      else
        error,()
    | Imply ((site1,state1),(site2,state2)) ->
      if dim_min <= 2
      then
        begin
          match Remanent_parameters.get_backend_mode parameters
          with
          | Remanent_parameters_sig.Kappa
          | Remanent_parameters_sig.Raw ->
            let error, t =
              Ckappa_backend.Ckappa_backend.add_state
                parameters error handler_kappa
                agent_id site1 state1 t
            in
            let error, t' =
              Ckappa_backend.Ckappa_backend.add_state
                parameters error handler_kappa
                agent_id site2 state2 t
            in
            let error =
              Ckappa_backend.Ckappa_backend.print
                (Remanent_parameters.get_logger parameters) parameters error handler_kappa
                t
            in
            let () =
              Loggers.fprintf (Remanent_parameters.get_logger parameters) " => " in
            let error =
              Ckappa_backend.Ckappa_backend.print
                (Remanent_parameters.get_logger parameters) parameters error handler_kappa
                t'
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            error, ()
          | Remanent_parameters_sig.Natural_language ->
            let error', site_string1 =
              Handler.string_of_site_in_natural_language
                parameters error handler_kappa agent_type
                site1
            in
            let error =
              Exception.check_point
                Exception.warn  parameters error error' __POS__ Exit
            in
            let error', state_string1 =
              Handler.string_of_state_fully_deciphered parameters error
                handler_kappa agent_type
                site1
                state1
            in
            let error =
              Exception.check_point
                Exception.warn parameters error error' __POS__ Exit
            in
            let error', site_string2 =
              Handler.string_of_site_in_natural_language
                parameters error handler_kappa agent_type
                site2
            in
            let error =
              Exception.check_point
                Exception.warn parameters error error' __POS__ Exit
            in
            let error', state_string2 =
              Handler.string_of_state_fully_deciphered parameters error
                handler_kappa agent_type
                site2
                state2
            in
            let error =
              Exception.check_point
                Exception.warn parameters error error' __POS__ Exit
            in
            error,
            Loggers.fprintf (Remanent_parameters.get_logger parameters)
              "%s%s%s is %s whenever %s is %s.%s"
              (Remanent_parameters.get_prefix parameters)
              (cap (in_agent_comma agent_string))
              site_string2 state_string2 site_string1 state_string1 endofline
        end
      else
        error,()
    | Partition (v, list) ->
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters)
          "%s%s%s" (Remanent_parameters.get_prefix parameters)
          (cap (in_agent_colon agent_string)) endofline
      in
      let error, site_string =
        Handler.string_of_site_in_natural_language parameters error handler_kappa agent_type
          v
      in
      let parameters = Remanent_parameters.update_prefix parameters tab in
      let error =
        List.fold_left
          (fun error (a,list) ->
             let error, parameters  =
               match Remanent_parameters.get_backend_mode parameters
               with
               | Remanent_parameters_sig.Natural_language ->
                 let error, state_string =
                   Handler.string_of_state_fully_deciphered
                     parameters error handler_kappa agent_type
                     v a
                 in
                 let () = Loggers.fprintf (Remanent_parameters.get_logger parameters)
                     "%swhen %s is equal to %s, then:%s%s"
                     (Remanent_parameters.get_prefix parameters)
                     site_string state_string endofline beginenumeration
                 in
                 let parameters =
                   Remanent_parameters.update_prefix parameters (tab ^ beginenum ^ " ")
                 in
                 error, parameters
               | Remanent_parameters_sig.Kappa
               | Remanent_parameters_sig.Raw ->
                 error, parameters
             in
             let error, t' =
               Ckappa_backend.Ckappa_backend.add_state
                 parameters error handler_kappa
                 agent_id v a t
             in
             let error =
               List.fold_left
                 (fun error token ->
                    let error =
                      print
                        ~beginning_of_sentence:false
                        ~prompt_agent_type:false
                        ~html_mode
                        ~show_dep_with_dimmension_higher_than:0
                        parameters
                        handler_kappa
                        error
                        agent_string
                        agent_type
                        agent_id
                        token
                        t'
                    in
                    let () = Loggers.fprintf (Remanent_parameters.get_logger parameters)
                        "%s" endenum
                    in
                    error)
                 error list
             in
             let () =
               Loggers.fprintf (Remanent_parameters.get_logger parameters)
                 "%s" endenumeration
             in
             error)
          error list
      in error,()
    | No_known_translation list ->
      begin
        match Remanent_parameters.get_backend_mode parameters
        with
        | Remanent_parameters_sig.Kappa
        | Remanent_parameters_sig.Raw ->
          begin
            let error =
              Ckappa_backend.Ckappa_backend.print
                (Remanent_parameters.get_logger parameters) parameters error handler_kappa
                t
            in
            let prefix ="   " in
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                " =>\n%s[\n" prefix
            in
            let prefix' = prefix in
            let prefix ="\t" in
            let error, bool =
              List.fold_left
                (fun (error, bool) state_list ->
                   let () =
                     Loggers.fprintf
                       (Remanent_parameters.get_logger parameters)
                       "%s%s" prefix (if bool then "v " else "  ")
                   in
                   let error, t' =
                     List.fold_left
                       (fun (error,t) (site,state) ->
                          Ckappa_backend.Ckappa_backend.add_state
                            parameters error handler_kappa
                            agent_id site state t)
                       (error, t)
                       state_list
                   in
                   let error =
                     Ckappa_backend.Ckappa_backend.print
                       (Remanent_parameters.get_logger parameters) parameters error handler_kappa
                       t'
                   in
                   let () =
                     Loggers.print_newline
                       (Remanent_parameters.get_logger parameters)
                   in
                   (error,true)
                )
                (error, false)
                list
            in
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%s]\n" prefix'
            in error, ()
          end
        | Remanent_parameters_sig.Natural_language ->
          begin
            match list with
            | [] -> error, ()
            | head :: _ ->
              let n = List.length head in
              if n >= dim_min
              then
                let () =
                  Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s%s"
                    (Remanent_parameters.get_prefix parameters)
                    (cap (in_agent_comma agent_string))
                in
                let error,() =
                  let rec aux l error =
                    match
                      l
                    with
                    | [a, _] ->
                      let error, string =
                        Handler.string_of_site_in_natural_language
                          parameters error handler_kappa agent_type a
                      in
                      let () =
                        Loggers.fprintf (Remanent_parameters.get_logger parameters)
                          ", and %s, " string
                      in
                      error,()
                    | [] ->
                      Exception.warn parameters error __POS__ Exit ()
                    | (a, _) :: b ->
                      let error, string =
                        Handler.string_of_site_in_natural_language
                          parameters error handler_kappa agent_type a
                      in
                      let () =
                        Loggers.fprintf (Remanent_parameters.get_logger parameters)
                          ", %s" string
                      in
                      aux b error
                  in
                  match
                    head
                  with
                  | [] | [_] ->
                    Exception.warn parameters error __POS__ Exit ()
                  | (a , _) :: b ->
                    let error, string =
                      Handler.string_of_site_in_natural_language
                        parameters error handler_kappa agent_type
                        a
                    in
                    let () =
                      Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" string
                    in
                    aux
                      b
                      error
                in
                let () = Loggers.fprintf (Remanent_parameters.get_logger parameters)
                    "are entangled by the following %i-d relationship:%s" n endofline
                in
                let parameters = Remanent_parameters.update_prefix parameters "\t" in
                List.fold_left
                  (fun error l ->
                     let error, bool =
                       List.fold_left
                         (fun (error, bool) (site_type, state) ->
                            let error', site_string =
                              Handler.string_of_site parameters error handler_kappa
                                agent_type site_type
                            in
                            let error =
                              Exception.check_point
                                Exception.warn  parameters error error'
                                __POS__ Exit
                            in
                            let error', state_string =
                              Handler.string_of_state_fully_deciphered parameters error
                                handler_kappa agent_type site_type state
                            in
                            let error =
                              Exception.check_point
                                Exception.warn  parameters error error'
                                __POS__ Exit in
                            (*---------------------------------------------*)
                            let () =
                              if bool
                              then Loggers.fprintf (Remanent_parameters.get_logger parameters) ","
                              else Loggers.fprintf (Remanent_parameters.get_logger parameters)
                                  "%s%s(" (Remanent_parameters.get_prefix parameters) agent_string
                            in
                            let () = Loggers.fprintf (Remanent_parameters.get_logger parameters)
                                "%s%s" site_string state_string
                            in
                            error,true
                         )
                         (error,false) l
                     in
                     (*----------------------------------------------------*)
                     let () =
                       if bool
                       then Loggers.fprintf (Remanent_parameters.get_logger parameters)
                           ")%s" endofline
                     in error)
                  error
                  list,
                ()
              else
                error,()
          end
      end
  in
  error

(*****************************************************************************)
(*store the print function*)

let add_state_into_t parameters error handler_kappa agent_id site_type state
  t store_set =
  let error, t' =
    Ckappa_backend.Ckappa_backend.add_state
      parameters error handler_kappa
      agent_id site_type state
      t
  in
  let error, (internal_list, bound_to_list, binding_list) =
    Ckappa_backend.Ckappa_backend.print_store_views
      error handler_kappa t
  in
  let error, store_set =
    Triple_pair_list_map_and_set.Set.add_when_not_in
      parameters error
      (internal_list, bound_to_list, binding_list)
      store_set
  in
  error, store_set

let add_two_states_into_t parameters error handler_kappa agent_id site1 state1
    site2 state2 t store_set1 store_set2 =
let error, store_set1 =
  add_state_into_t parameters error
    handler_kappa
    agent_id site1 state1 t
    store_set1
in
let error, store_set2 =
  add_state_into_t parameters error handler_kappa
    agent_id site2 state2 t
    store_set2
in
let error, store_set =
  Triple_pair_list_map_and_set.Set.union
    parameters error
    store_set1
    store_set2
in
error, store_set

let rec print_store_views
    ~show_dep_with_dimmension_higher_than:dim_min
    parameters handler_kappa error agent_string agent_type
    agent_id translation t =
  let error, store_result =
    match translation with
    | Range (site_type, state_list) ->
      begin
        if dim_min <= 1
        then
          begin
            match Remanent_parameters.get_backend_mode parameters with
            | Remanent_parameters_sig.Kappa
            | Remanent_parameters_sig.Raw ->
              (*return the information store inside t from the beginning*)
              let error, (internal_list, bound_to_list, binding_list) =
                Ckappa_backend.Ckappa_backend.print_store_views
                  error handler_kappa
                  t
              in
              (*store the information into set*)
              let error, store_set =
                Triple_pair_list_map_and_set.Set.add_when_not_in
                parameters error
                (internal_list, bound_to_list, binding_list)
                Triple_pair_list_map_and_set.Set.empty (*empty*)
              in
              (*store this information into agent_id set*)
              let error, store_set =
                List.fold_left (fun (error, store_set) state ->
                    (*add state inside t*)
                    let error, store_set =
                      add_state_into_t parameters error
                        handler_kappa
                        agent_id site_type state
                        t
                        store_set
                    in error, store_set
                  ) (error, store_set) state_list
              in
              error, store_set
            | Remanent_parameters_sig.Natural_language ->
              let rec aux list store_set =
                match list with
                | [] -> error, store_set
                | [state] ->
                  let error, store_set =
                    add_state_into_t parameters error
                      handler_kappa
                      agent_id site_type state t
                      store_set
                  in
                  error, store_set
                | state :: tail ->
                let error, store_set =
                  add_state_into_t parameters error
                    handler_kappa
                    agent_id site_type state t
                    store_set
                in
                aux tail store_set
              in
              match state_list with
              | [] -> Exception.warn parameters error __POS__ Exit
                        Triple_pair_list_map_and_set.Set.empty
              | [state] ->
                (*add state into t*)
                let error, store_set =
                  add_state_into_t parameters error
                    handler_kappa
                    agent_id site_type state
                    t
                    Triple_pair_list_map_and_set.Set.empty
                in
                error, store_set
              | [state1; state2] ->
                let error, store_set =
                  add_two_states_into_t
                    parameters error handler_kappa
                    agent_id site_type state1
                    site_type state2
                    t
                    Triple_pair_list_map_and_set.Set.empty
                    Triple_pair_list_map_and_set.Set.empty
                in
                error, store_set
              | list ->
                let error, store_set =
                  aux list Triple_pair_list_map_and_set.Set.empty
                in
                aux list store_set
          end
        else
          error, Triple_pair_list_map_and_set.Set.empty
      end
      | Equiv ((site1,state1),(site2,state2)) ->
        if dim_min <= 2
        then
          begin
            match Remanent_parameters.get_backend_mode parameters with
            | Remanent_parameters_sig.Kappa
            | Remanent_parameters_sig.Raw ->
              let error, store_set =
                add_two_states_into_t
                  parameters error handler_kappa
                  agent_id site1 state1
                  site2 state2
                  t
                  Triple_pair_list_map_and_set.Set.empty
                  Triple_pair_list_map_and_set.Set.empty
              in
              error, store_set
            | Remanent_parameters_sig.Natural_language ->
              let error, store_set =
                add_two_states_into_t
                  parameters error handler_kappa
                  agent_id site1 state1
                  site2 state2
                  t
                  Triple_pair_list_map_and_set.Set.empty
                  Triple_pair_list_map_and_set.Set.empty
              in
              error, store_set
          end
        else error, Triple_pair_list_map_and_set.Set.empty
      | Imply ((site1,state1),(site2,state2)) ->
        if dim_min <= 2
        then
          begin
            match Remanent_parameters.get_backend_mode parameters with
            | Remanent_parameters_sig.Kappa
            | Remanent_parameters_sig.Raw ->
              let error, store_set =
                add_two_states_into_t parameters error
                  handler_kappa
                  agent_id site1 state1
                  site2 state2
                  t
                  Triple_pair_list_map_and_set.Set.empty
                  Triple_pair_list_map_and_set.Set.empty
              in
              error, store_set
            | Remanent_parameters_sig.Natural_language ->
              let error, store_set =
                add_two_states_into_t parameters error
                  handler_kappa
                  agent_id site1 state1
                  site2 state2
                  t
                  Triple_pair_list_map_and_set.Set.empty
                  Triple_pair_list_map_and_set.Set.empty
              in
              error, store_set
          end
        else error, Triple_pair_list_map_and_set.Set.empty
      | Partition (v, list) ->
        let error, store_set =
          List.fold_left (fun (error, store_set) (a, list) ->
              let error, store_set =
                match Remanent_parameters.get_backend_mode parameters with
                | Remanent_parameters_sig.Natural_language ->
                  (*add state*)
                  let error, store_set =
                    add_state_into_t parameters error handler_kappa
                      agent_id v a t
                      Triple_pair_list_map_and_set.Set.empty
                  in
                  error, store_set
                | Remanent_parameters_sig.Kappa
                | Remanent_parameters_sig.Raw ->
                  error, Triple_pair_list_map_and_set.Set.empty
              in
              let error, t' =
                Ckappa_backend.Ckappa_backend.add_state
                  parameters error
                  handler_kappa
                  agent_id
                  v
                  a
                  t
              in
              let error, store_set =
                List.fold_left (fun (error, store_set) token ->
                    let error, store_set =
                      print_store_views
                        ~show_dep_with_dimmension_higher_than:0
                        parameters
                        handler_kappa
                        error
                        agent_string
                        agent_type
                        agent_id
                        token
                        t'
                    in
                    error, store_set
                  ) (error, store_set) list
              in
              error, store_set
            ) (error, Triple_pair_list_map_and_set.Set.empty) list
        in
        error, store_set
      | No_known_translation list ->
        begin
          match Remanent_parameters.get_backend_mode parameters with
          | Remanent_parameters_sig.Kappa
          | Remanent_parameters_sig.Raw ->
            let error, store_set =
              List.fold_left (fun (error, store_set) state_list ->
                  let error, store_set =
                    List.fold_left (fun (error, store_set) (site, state) ->
                        let error, store_set =
                          add_state_into_t parameters error handler_kappa
                            agent_id site state t
                            store_set
                        in
                        error, store_set
                      ) (error, store_set)
                      state_list
                  in
                  error, store_set
                ) (error, Triple_pair_list_map_and_set.Set.empty) list
            in
            error, store_set
          | Remanent_parameters_sig.Natural_language ->
            begin
              match list with
              | [] -> error, Triple_pair_list_map_and_set.Set.empty
              | head :: _ ->
                let n = List.length head in
                if n >= dim_min
                then
                  let error, store_set =
                    let rec aux l store_set =
                      match l with
                      | [] -> error, store_set
                      | [site, state] ->
                        let error, store_set =
                          add_state_into_t
                            parameters error handler_kappa
                            agent_id site state t
                            store_set
                        in
                        error, store_set
                      | (site, state) :: tl ->
                      let error, store_set =
                        add_state_into_t
                          parameters error handler_kappa
                          agent_id site state t
                          store_set
                      in
                      aux tl store_set
                    in
                    match head with
                    | [] | [_] -> Exception.warn parameters error __POS__ Exit
                                    Triple_pair_list_map_and_set.Set.empty
                    | (_site, _state) :: tl ->
                      aux tl Triple_pair_list_map_and_set.Set.empty
                  in
                  List.fold_left
                    (fun (error, store_set) l ->
                       List.fold_left
                         (fun (error, store_set) (site_type, state) ->
                           let error, store_set =
                             add_state_into_t parameters error handler_kappa
                               agent_id site_type state t
                               store_set
                           in error, store_set
                         ) (error, store_set) l
                    ) (error, store_set) list
                else
                  error,
                  Triple_pair_list_map_and_set.Set.empty
            end
        end
  in
  error, store_result

let store_views
    ~show_dep_with_dimmension_higher_than:dim_min
    parameters handler_kappa error agent_string agent_type
    translation =
  let t = Ckappa_backend.Ckappa_backend.empty in
  let error, id, t =
  Ckappa_backend.Ckappa_backend.add_agent
    parameters error
    handler_kappa
    agent_type
    t
  in
  print_store_views
    ~show_dep_with_dimmension_higher_than:dim_min
    parameters handler_kappa error agent_string agent_type id translation t

(*****************************************************************************)

let print ?beginning_of_sentence:(beggining=true) ?prompt_agent_type:(prompt_agent_type=true) ?html_mode:(html_mode=false)
    ~show_dep_with_dimmension_higher_than:dim_min
    parameters handler_kappa error
    agent_string agent_type translation
  =
  let t = Ckappa_backend.Ckappa_backend.empty in
  let error, id, t =
    Ckappa_backend.Ckappa_backend.add_agent
      parameters error handler_kappa
      agent_type t
  in
  print
    ~beginning_of_sentence:beggining
    ~prompt_agent_type ~html_mode
    ~show_dep_with_dimmension_higher_than:dim_min
    parameters handler_kappa error agent_string agent_type id translation t
