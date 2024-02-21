(**
   * ckappa_site_graph.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 17th of November
   * Last modification: Time-stamp: <Dec 22 2018>
   *
   * Site graph
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

(***************************************************************************)

let print_internal_pattern_aux ?logger parameters error _kappa_handler
    internal_constraints_list =
  let logger =
    match logger with
    | None -> Remanent_parameters.get_logger parameters
    | Some a -> a
  in
  let domain_name, lemma_list = internal_constraints_list in
  let () =
    Loggers.fprintf logger
      "------------------------------------------------------------\n";
    Loggers.fprintf logger "* Export %s to JSon (internal constraints_list):\n"
      domain_name;
    Loggers.fprintf logger
      "------------------------------------------------------------\n"
  in
  List.fold_left
    (fun (error, _) lemma ->
      let hyp = Public_data.get_hyp lemma in
      let refinement = Public_data.get_refinement lemma in
      let error =
        Site_graphs.KaSa_site_graph.print logger parameters error hyp
      in
      let () = Loggers.fprintf logger "=> [" in
      let error, b =
        match refinement with
        | [] -> error, false
        | [ hyp ] ->
          Site_graphs.KaSa_site_graph.print logger parameters error hyp, false
        | _ :: _ as l ->
          List.fold_left
            (fun (error, bool) hyp ->
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  (if bool then
                     "\t\tv "
                   else
                     "\t\t  ")
              in
              let error =
                Site_graphs.KaSa_site_graph.print logger parameters error hyp
              in
              error, true)
            (error, false) (List.rev l)
      in
      let () = Loggers.fprintf logger "]" in
      let () = Loggers.print_newline logger in
      error, b)
    (error, false) lemma_list

(*print the information as the output of non relational properties*)
let print_internal_pattern ?logger parameters error kappa_handler list =
  let logger' =
    match logger with
    | None -> Remanent_parameters.get_logger parameters
    | Some a -> a
  in
  let error =
    List.fold_left
      (fun error pattern ->
        let error, _ =
          print_internal_pattern_aux ?logger parameters error kappa_handler
            pattern
        in
        let () = Loggers.print_newline logger' in
        error)
      error list
  in
  error

(***************************************************************************)

let print_for_list logger parameter error t =
  let error, _ =
    List.fold_left
      (fun (error, bool) (agent_string, site_map) ->
        let error =
          Site_graphs.KaSa_site_graph.print_agent logger parameter error
            agent_string site_map bool
        in
        error, true)
      (error, false) t
  in
  let () = Loggers.fprintf logger " " in
  error

let print_pattern_aux ?logger parameters error constraints_list =
  let logger =
    match logger with
    | None -> Remanent_parameters.get_logger parameters
    | Some a -> a
  in
  let domain_name, lemma_list = constraints_list in
  let () =
    Loggers.fprintf logger
      "------------------------------------------------------------\n";
    Loggers.fprintf logger "* Export %s to JSon (constraints_list):\n"
      domain_name;
    Loggers.fprintf logger
      "------------------------------------------------------------\n"
  in
  List.fold_left
    (fun (error, _) lemma ->
      let hyp = Public_data.get_hyp lemma in
      let refinement = Public_data.get_refinement lemma in
      let error = print_for_list logger parameters error hyp in
      let () = Loggers.fprintf logger " => [" in
      (*refinement*)
      let error, b =
        List.fold_left
          (fun (error, bool) hyp ->
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                (if bool then
                   "\t\tv "
                 else
                   "\t\t  ")
            in
            let error = print_for_list logger parameters error hyp in
            error, true)
          (error, false) (List.rev refinement)
      in
      let () = Loggers.fprintf logger "]" in
      let () = Loggers.print_newline logger in
      error, b)
    (error, false) lemma_list

let _print_pattern ?logger parameters error _kappa_handler list =
  let logger' =
    match logger with
    | None -> Remanent_parameters.get_logger parameters
    | Some a -> a
  in
  let error =
    List.fold_left
      (fun error pattern ->
        let error, _ = print_pattern_aux ?logger parameters error pattern in
        let () = Loggers.print_newline logger' in
        error)
      error list
  in
  error

(*******************************************************************)

let site_graph_to_list error string_version =
  let error, current_list =
    Ckappa_sig.Agent_id_map_and_set.Map.fold
      (fun _ (agent_string, site_map) (error, current_list) ->
        (*-----------------------------------------*)
        let site_graph = (agent_string, site_map) :: current_list in
        error, site_graph)
      string_version (error, [])
  in
  error, List.rev current_list

let site_graph_list_to_list error list =
  List.fold_left
    (fun (error, current_list) t ->
      let string_version = Site_graphs.KaSa_site_graph.get_string_version t in
      let error, site_graph = site_graph_to_list error string_version in
      error, site_graph :: current_list)
    (error, []) list

let _pair_list_to_list parameters error kappa_handler pattern agent_id1
    site_type1' agent_id2 site_type2' pair_list =
  List.fold_left
    (fun (error, current_list) l ->
      match l with
      | [ (siteone, state1); (sitetwo, state2) ]
        when siteone == Ckappa_sig.fst_site && sitetwo == Ckappa_sig.snd_site ->
        let error, pattern =
          Site_graphs.KaSa_site_graph.add_state parameters error kappa_handler
            agent_id1 site_type1' state1 pattern
        in
        let error, pattern =
          Site_graphs.KaSa_site_graph.add_state parameters error kappa_handler
            agent_id2 site_type2' state2 pattern
        in
        let string_version =
          Site_graphs.KaSa_site_graph.get_string_version pattern
        in
        let error, site_graph = site_graph_to_list error string_version in
        error, site_graph :: current_list
      | _ -> Exception.warn parameters error __POS__ Exit [])
    (error, []) pair_list

let internal_pair_list_to_list parameters error kappa_handler pattern agent_id1
    site_type1' agent_id2 site_type2' pair_list =
  List.fold_left
    (fun (error, current_list) l ->
      match l with
      | [ (siteone, state1); (sitetwo, state2) ]
        when siteone == Ckappa_sig.fst_site && sitetwo == Ckappa_sig.snd_site ->
        let error, pattern =
          Site_graphs.KaSa_site_graph.add_state parameters error kappa_handler
            agent_id1 site_type1' state1 pattern
        in
        let error, pattern =
          Site_graphs.KaSa_site_graph.add_state parameters error kappa_handler
            agent_id2 site_type2' state2 pattern
        in
        error, pattern :: current_list
      | _ -> Exception.warn parameters error __POS__ Exit [])
    (error, []) pair_list

(******************************************************************)
