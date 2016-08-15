(**
 * translation_in_natural_language.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: 2016
 * Last modification: Time-stamp: <Aug 15 2016>
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

(****************************************************************************************)

let non_relational parameter handler error mvbdu =
  let error, handler, list =
    Ckappa_sig.Views_bdu.mvbdu_cartesian_abstraction parameter handler error mvbdu
  in
  let error, handler, mvbdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true parameter handler error
  in
  let error, handler, recomposition =
    List.fold_left
      (fun (error,handler,conjunct) term ->
         Ckappa_sig.Views_bdu.mvbdu_and parameter handler error conjunct term
      )
      (error, handler, mvbdu_true) list
  in
  error, handler,
  Ckappa_sig.Views_bdu.equal mvbdu recomposition

let try_partitioning parameter handler error (rename_site_inverse:rename_sites) mvbdu =
  let error, handler, mvbdu_true =
    Ckappa_sig.Views_bdu.mvbdu_true parameter handler error
  in
  let error, handler, var_hconsed_list =
    Ckappa_sig.Views_bdu.variables_list_of_mvbdu parameter handler error mvbdu
  in
  let error, handler, var_list =
    Ckappa_sig.Views_bdu.extensional_of_variables_list
      parameter handler error var_hconsed_list
  in
  let rec aux l (error, handler) =
    match
      l
    with
    | [] -> error, handler, None
    | head :: tail ->
      let error, handler, singleton =
        Ckappa_sig.Views_bdu.build_variables_list
          parameter handler error [head]
      in
      let error, handler, mvbdu_ref =
        Ckappa_sig.Views_bdu.mvbdu_project_abstract_away
          parameter handler error mvbdu singleton
      in
      let error, handler, proj_in =
        Ckappa_sig.Views_bdu.mvbdu_project_keep_only
          parameter handler error mvbdu singleton
      in
      let error, handler, list_asso =
        Ckappa_sig.Views_bdu.extensional_of_mvbdu
          parameter handler error proj_in
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
              (Exception.warn parameter error __POS__ Exit output)
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
                parameter handler error [head, h]
            in
            let error, handler, mvbdu_case =
              Ckappa_sig.Views_bdu.mvbdu_redefine
                parameter handler error mvbdu_true select
            in
            let error, handler, case =
              Ckappa_sig.Views_bdu.mvbdu_and
                parameter handler error mvbdu_case mvbdu
            in
            let error, handler, bool =
              non_relational parameter handler error case
            in
            if bool
            then
              let error, handler, away =
                Ckappa_sig.Views_bdu.mvbdu_project_abstract_away
                  parameter handler error case singleton
              in
              if
                Ckappa_sig.Views_bdu.equal
                  away mvbdu_ref
              then
                aux3 t (error, handler, output)
              else
                let error, handler, list =
                  Ckappa_sig.Views_bdu.mvbdu_cartesian_abstraction
                    parameter handler error away
                in
                let error, handler, list =
                  List.fold_left
                    (fun (error, handler, list) elt ->
                       let error, handler, elt =
                         Ckappa_sig.Views_bdu.extensional_of_mvbdu
                           parameter handler error elt
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
                               parameter error __POS__ Exit ()
                           in
                           error, handler, list
                         | Some (a, l) ->
                           let error, a' = rename_site_inverse parameter error a in
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
        let error, head = rename_site_inverse parameter error head in
        error, handler, Some (head, l)
  in
  aux var_list (error, handler)

(****************************************************************************************)

let translate parameter handler error (rename_site_inverse: rename_sites) mvbdu =
  let error, handler, list =
    Ckappa_sig.Views_bdu.extensional_of_mvbdu
      parameter handler error mvbdu
  in
  let error, list =
    List.fold_left
      (fun (error, list) elt1 ->
         let error, elt1 =
           List.fold_left
             (fun (error, list) (elt2, asso) ->
                let error, elt2 = rename_site_inverse parameter error elt2 in
                error, (elt2, asso) :: list
             )
             (error, []) (List.rev elt1)
         in
         error, elt1::list)
      (error, [])
      (List.rev list)
  in
  if Remanent_parameters.get_post_processing  parameter
  then
    begin
      let error, handler, vars =
        Ckappa_sig.Views_bdu.variables_list_of_mvbdu
          parameter handler error mvbdu
      in
      let error, handler, var_list =
        Ckappa_sig.Views_bdu.extensional_of_variables_list
          parameter handler error vars
      in
      let error, var_list =
        List.fold_left
          (fun (error, list) elt ->
             let error, elt = rename_site_inverse parameter error elt in
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
                   parameter error __POS__ Exit list)
            (error, [])
            list
        in
        error, (handler, Range (x, list))
      | [_; _] ->
        begin
          match list with
          | [] | [_] ->
            Exception.warn
              parameter error __POS__ Exit
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
                  parameter error __POS__ Exit
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
                  parameter error __POS__ Exit
                  (handler, No_known_translation list)
            end
          | _ ->
            begin
              let error, handler, output =
                try_partitioning
                  parameter
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
            try_partitioning parameter handler error rename_site_inverse mvbdu
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

(****************************************************************************************)

let rec print ?beginning_of_sentence:(beggining=true) ?prompt_agent_type:(prompt_agent_type=true) ?html_mode:(html_mode=false)
    ~show_dep_with_dimmension_higher_than:dim_min parameter handler_kappa error agent_string agent_type translation =
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
          let error', site_string =
            Handler.string_of_site_in_natural_language parameter error handler_kappa
              agent_type
              site_type
          in
          let error =
            Exception.check_point
              Exception.warn parameter error error' __POS__ Exit
          in
          let rec aux list error =
            match list
            with
            | [] ->
              Exception.warn parameter error __POS__ Exit ()
            | [state] ->
              let error', state_string =
                Handler.string_of_state_fully_deciphered parameter error
                  handler_kappa agent_type
                  site_type
                  state
              in
              let error =
                Exception.check_point
                  Exception.warn parameter error error'
                  __POS__ Exit
              in
              error, Loggers.fprintf (Remanent_parameters.get_logger parameter)
                " and %s.%s" state_string endofline
            | state :: tail ->
              let error', state_string =
                Handler.string_of_state_fully_deciphered parameter error
                  handler_kappa agent_type
                  site_type
                  state
              in
              let error =
                Exception.check_point
                  Exception.warn  parameter error error'
                  __POS__ Exit
              in
              let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                  " %s," state_string
              in
              aux tail error
          in
          match
            state_list
          with
          | [] -> Exception.warn  parameter error __POS__ Exit ()
          | [state] ->
            let error', state_string =
              Handler.string_of_state_fully_deciphered parameter error
                handler_kappa agent_type
                site_type
                state
            in
            let error =
              Exception.check_point
                Exception.warn  parameter error error'
                __POS__ Exit
            in
            error,
            Loggers.fprintf
              (Remanent_parameters.get_logger parameter)
              "%s%s %sis always %s.%s"
              (Remanent_parameters.get_prefix parameter)
              (cap site_string) (in_agent agent_string) state_string endofline
          | [state1; state2] ->
            let error', state_string1 =
              Handler.string_of_state_fully_deciphered parameter error
                handler_kappa agent_type
                site_type
                state1
            in
            let error =
              Exception.check_point
                Exception.warn  parameter error error'
                __POS__ Exit
            in
            let error', state_string2 =
              Handler.string_of_state_fully_deciphered parameter error
                handler_kappa agent_type
                site_type
                state2
            in
            let error =
              Exception.check_point
                Exception.warn parameter error error' __POS__ Exit
            in
            error, Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "%s%s %sranges over %s and %s.%s"
              (Remanent_parameters.get_prefix parameter)
              (cap site_string)
              (in_agent agent_string) state_string1 state_string2 endofline
          | list ->
            let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "%s%s %sranges over"
                (Remanent_parameters.get_prefix parameter)
                (cap site_string)  (in_agent agent_string)
            in
            aux list error
        else error,()
      end
    | Equiv ((site1,state1),(site2,state2)) ->
      if dim_min <= 2
      then
        begin
          let error', site_string1 =
            Handler.string_of_site_in_natural_language parameter error handler_kappa
              agent_type
              site1
          in
          let error =
            Exception.check_point
              Exception.warn parameter error error' __POS__ Exit
          in
          let error', state_string1 =
            Handler.string_of_state_fully_deciphered parameter error
              handler_kappa agent_type
              site1
              state1
          in
          let error =
            Exception.check_point
              Exception.warn  parameter error error' __POS__ Exit
          in
          let error', site_string2 =
            Handler.string_of_site_in_natural_language
              parameter error handler_kappa
              agent_type site2
          in
          let error =
            Exception.check_point
              Exception.warn  parameter error error' __POS__ Exit
          in
          let error', state_string2 =
            Handler.string_of_state_fully_deciphered parameter error
              handler_kappa agent_type
              site2
              state2
          in
          let error =
            Exception.check_point
              Exception.warn parameter error error' __POS__ Exit in
          error,
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "%s%s%s is %s, if and only if, %s is %s.%s"
            (Remanent_parameters.get_prefix parameter)
            (cap (in_agent_comma agent_string))
            site_string1 state_string1 site_string2 state_string2 endofline
        end
      else
        error,()
    | Imply ((site1,state1),(site2,state2)) ->
      if dim_min <= 2
      then
        begin
          (* *)
          match Remanent_parameters.get_backend_mode parameter
          with
          | Remanent_parameters_sig.Kappa
          | Remanent_parameters_sig.Raw ->
            let t = Ckappa_backend.Ckappa_backend.empty in
            let error, id, t =
              Ckappa_backend.Ckappa_backend.add_agent
                parameter error handler_kappa
                agent_type t
            in
            (*  let error, t =
                Ckappa_backend.Ckappa_backend.add_site
                parameter error handler_kappa
                id site1 t
                in*)
            let error, t =
              Ckappa_backend.Ckappa_backend.add_state
                parameter error handler_kappa
                id site1 state1 t
            in
            (*  let error, t' =
                Ckappa_backend.Ckappa_backend.add_site
                parameter error handler_kappa
                id site2 t'
                in*)
            let error, t' =
              Ckappa_backend.Ckappa_backend.add_state
                parameter error handler_kappa
                id site2 state2 t
            in
            let error =
              Ckappa_backend.Ckappa_backend.print
                (Remanent_parameters.get_logger parameter) parameter error handler_kappa
                t
            in
            let () =
              Loggers.fprintf (Remanent_parameters.get_logger parameter) "=>" in
            let error =
              Ckappa_backend.Ckappa_backend.print
                (Remanent_parameters.get_logger parameter) parameter error handler_kappa
                t'
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameter)
            in
            error, ()
          | Remanent_parameters_sig.Natural_language ->
            (* *)
            let error', site_string1 =
              Handler.string_of_site_in_natural_language
                parameter error handler_kappa agent_type
                site1
            in
            let error =
              Exception.check_point
                Exception.warn  parameter error error' __POS__ Exit
            in
            let error', state_string1 =
              Handler.string_of_state_fully_deciphered parameter error
                handler_kappa agent_type
                site1
                state1
            in
            let error =
              Exception.check_point
                Exception.warn parameter error error' __POS__ Exit
            in
            let error', site_string2 =
              Handler.string_of_site_in_natural_language
                parameter error handler_kappa agent_type
                site2
            in
            let error =
              Exception.check_point
                Exception.warn parameter error error' __POS__ Exit
            in
            let error', state_string2 =
              Handler.string_of_state_fully_deciphered parameter error
                handler_kappa agent_type
                site2
                state2
            in
            let error =
              Exception.check_point
                Exception.warn parameter error error' __POS__ Exit
            in
            error,
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "%s%s%s is %s whenever %s is %s.%s"
              (Remanent_parameters.get_prefix parameter)
              (cap (in_agent_comma agent_string))
              site_string2 state_string2 site_string1 state_string1 endofline
        end
      else
        error,()
    | Partition (v, list) ->
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "%s%s%s" (Remanent_parameters.get_prefix parameter)
          (cap (in_agent_colon agent_string)) endofline
      in
      let error, site_string =
        Handler.string_of_site_in_natural_language parameter error handler_kappa agent_type
          v
      in
      let parameter = Remanent_parameters.update_prefix parameter tab in
      let error =
        List.fold_left
          (fun error (a,list) ->
             let error, state_string =
               Handler.string_of_state_fully_deciphered
                 parameter error handler_kappa agent_type
                 v
                 a
             in
             let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                 "%swhen %s is equal to %s, then:%s%s"
                 (Remanent_parameters.get_prefix parameter)
                 site_string state_string endofline beginenumeration
             in
             let parameter =
               Remanent_parameters.update_prefix parameter (tab ^ beginenum ^ " ")
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
                        parameter
                        handler_kappa
                        error
                        agent_string
                        agent_type
                        token
                    in
                    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                        "%s" endenum
                    in
                    error)
                 error list
             in
             let () =
               Loggers.fprintf (Remanent_parameters.get_logger parameter)
                 "%s" endenumeration
             in
             error)
          error list
      in error,()
    | No_known_translation list ->
      begin
        match list with
        | [] -> error, ()
        | head :: _ ->
          let n = List.length head in
          if List.length head >= dim_min
          then
            let () =
              Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s%s"
                (Remanent_parameters.get_prefix parameter)
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
                      parameter error handler_kappa agent_type a
                  in
                  let () =
                    Loggers.fprintf (Remanent_parameters.get_logger parameter)
                      ", and %s, " string
                  in
                  error,()
                | [] ->
                  Exception.warn parameter error __POS__ Exit ()
                | (a, _) :: b ->
                  let error, string =
                    Handler.string_of_site_in_natural_language
                      parameter error handler_kappa agent_type a
                  in
                  let () =
                    Loggers.fprintf (Remanent_parameters.get_logger parameter)
                      ", %s" string
                  in
                  aux b error
              in
              match
                head
              with
              | [] | [_] ->
                Exception.warn parameter error __POS__ Exit ()
              | (a , _) :: b ->
                let error, string =
                  Handler.string_of_site_in_natural_language
                    parameter error handler_kappa agent_type
                    a
                in
                let () =
                  Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" string
                in
                aux
                  b
                  error
            in
            let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "are entangled by the following %i-d relationship:%s" n endofline
            in
            let parameter = Remanent_parameters.update_prefix parameter "\t" in
            List.fold_left
              (fun error l ->
                 let error, bool =
                   List.fold_left
                     (fun (error, bool) (site_type, state) ->
                        let error', site_string =
                          Handler.string_of_site parameter error handler_kappa
                            agent_type site_type
                        in
                        let error =
                          Exception.check_point
                            Exception.warn  parameter error error'
                            __POS__ Exit
                        in
                        let error', state_string =
                          Handler.string_of_state_fully_deciphered parameter error
                            handler_kappa agent_type site_type state
                        in
                        let error =
                          Exception.check_point
                            Exception.warn  parameter error error'
                            __POS__ Exit in
                        (*-----------------------------------------------------------*)
                        let () =
                          if bool
                          then Loggers.fprintf (Remanent_parameters.get_logger parameter) ","
                          else Loggers.fprintf (Remanent_parameters.get_logger parameter)
                              "%s%s(" (Remanent_parameters.get_prefix parameter) agent_string
                        in
                        let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                            "%s%s" site_string state_string
                        in
                        error,true
                     )
                     (error,false) l
                 in
                 (*-----------------------------------------------------------*)
                 let () =
                   if bool
                   then Loggers.fprintf (Remanent_parameters.get_logger parameter)
                       ")%s" endofline
                 in error)

              error
              list,
            ()
          else
            error,()
      end
  in
  error
