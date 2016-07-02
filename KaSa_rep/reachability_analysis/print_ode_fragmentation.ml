(**
    * ode_fragmentation.ml
    * openkappa
    * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 2015, the 9th of Apirl
    * Last modification: Time-stamp: <Jul 02 2016>
    * *
    * ODE fragmentation
    *
    *
    * Copyright 2010,2011 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    *  under the terms of the GNU Library General Public License *)

let warn parameter mh message exn default =
  Exception.warn parameter mh (Some "print ODE fragmentation") message exn
    (fun () -> default)

let trace = false

(************************************************************************************)
(*Print sites that modified*)

let print_sites_modified_set parameter error handler_kappa result =
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.iter parameter error
    (fun parameter error agent_type site_set ->
       let error, agent_name =
         try
           Handler.string_of_agent parameter error handler_kappa agent_type
         with
           _ -> warn parameter error (Some "line 38") Exit
                  (Ckappa_sig.string_of_agent_name agent_type)
       in
       let () =
         Loggers.fprintf (Remanent_parameters.get_logger parameter)
           "agent_type:%s:%s"
           (Ckappa_sig.string_of_agent_name agent_type) agent_name
       in
       let () =
         Loggers.print_newline (Remanent_parameters.get_logger parameter)
       in
       (*convert site of type int to string*)
       let error =
         Ckappa_sig.Site_map_and_set.Set.fold (fun site_type error ->
             let error, site_string =
               try
                 Handler.string_of_site parameter error handler_kappa agent_type site_type
               with
                 _ -> warn parameter error (Some "line 50") Exit
                        (Ckappa_sig.string_of_site_name site_type)
             in
             let () = Loggers.fprintf
                 (Remanent_parameters.get_logger parameter)  "site_type:%s:%s"
                 (Ckappa_sig.string_of_site_name site_type)
                 site_string
             in
             let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
             error
           ) site_set error
       in
       error
    ) result

(************************************************************************************)

let cartesian_prod_eq i a b =
  let rec loop a acc =
    match a with
    | [] -> List.rev acc
    | x :: xs ->
      loop xs (List.rev_append (List.rev (List.fold_left (fun acc y ->
          if x <> y
          then (i, x, y) :: acc
          else acc
        ) [] b)) acc)
  in
  loop a []

let print_internal_flow parameter _error _handler_kappa result =
  let store_result1, _store_result2 = result in
  if Remanent_parameters.get_do_ODE_flow_of_information parameter
  then
    if Remanent_parameters.get_trace parameter
    then
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "Flow of information in the ODE semantics:internal flow (first case):"
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      Ode_fragmentation_type.Internal_flow_map.Map.iter
        (fun agent_type (site_list, modified_set) ->
           let modified_list =
             Ckappa_sig.Site_map_and_set.Set.elements modified_set
           in
           let cartesian_output =
             cartesian_prod_eq agent_type site_list modified_list
           in
           let _ =
             List.iter (fun (agent_type, site_type, site_modif) ->
                 let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                     "Flow of information in the ODE semantics:Internal flow\n-agent_type:%s:site_type:%s -> agent_type:%s:site_type_modified:%s"
                     (Ckappa_sig.string_of_agent_name agent_type)
                     (Ckappa_sig.string_of_site_name site_type)
                     (Ckappa_sig.string_of_agent_name agent_type)
                     (Ckappa_sig.string_of_site_name site_modif)
                 in
                 Loggers.print_newline (Remanent_parameters.get_logger parameter)
               ) cartesian_output
           in
           ()
        ) store_result1

(************************************************************************************)
(*MAIN*)

let print_result parameter error handler_kappa result =
  let _ =
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "Flow of information in the ODE semantics:Internal flow\n";
    let error =
      print_internal_flow
        parameter
        error
        handler_kappa
        result.Ode_fragmentation_type.store_internal_flow
    in
    error
  in
  error
