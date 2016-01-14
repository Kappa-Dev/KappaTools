(**
  * print_bdu_analysis.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 28th of October
  * Last modification: 
  * 
  * Print relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Printf
open Memo_sig
open Cckappa_sig
open Remanent_parameters_sig
open Bdu_analysis_type
open Print_bdu_analysis_static
open Print_bdu_analysis_dynamic
open Print_bdu_build
open Site_map_and_set

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Print bdu analysis") message exn (fun () -> default)  

let trace = false

(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error handler_kappa compiled result =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type_" in
  let _ =
    if true (*Remanent_parameters.get_dump_reachability_analysis_static parameter*)
    then
      let _ = Format.printf "\nReachability analysis static information ....@." in
      let parameters_cv =
        Remanent_parameters.update_prefix parameter ""
      in
      if (Remanent_parameters.get_trace parameters_cv)
      then 
        Printf.fprintf (Remanent_parameters.get_log parameters_cv) "\n";
      print_result_static
        parameter
        error
        handler_kappa
        compiled
        result.store_bdu_analysis_static
    else error
  in
  (*------------------------------------------------------------------------------*)
  let _ =
    if  Remanent_parameters.get_dump_reachability_analysis_dynamic parameter
    then
      let _ = Format.printf "\nReachability analysis dynamic information ....@." in
      let parameters_cv =
        Remanent_parameters.update_prefix parameter ""
      in
      if (Remanent_parameters.get_trace parameters_cv)
      then Printf.fprintf (Remanent_parameters.get_log parameters_cv) "\n";
      print_result_dynamic
        parameter
        error
        handler_kappa
        compiled
        result.store_bdu_analysis_dynamic
    else error
  in
  (*------------------------------------------------------------------------------*)
  (*print if one would like to test*)
  (*let _ =
    print_bdu_build
      parameter
      error
      result.store_bdu_build
  in*)
  error

(************************************************************************************)
(*main print of fixpoint*)

let print_bdu_update_map parameter error handler_kappa result =
  Map_bdu_update.Map.iter (fun (agent_type, cv_id) bdu_update ->
    let error, agent_string =
      try
        Handler.string_of_agent parameter error handler_kappa agent_type
      with
        _ -> warn parameter error (Some "line 89") Exit (string_of_int agent_type)
    in
    let _ =
      fprintf parameter.log "agent_type:%i:%s:cv_id:%i\n" 
        agent_type agent_string cv_id
    in
    Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_update
    ) result

let print_bdu_update_map_cartesian_decomposition
    parameter handler error handler_kappa site_correspondence result =
  Map_bdu_update.Map.fold 
    (fun (agent_type, cv_id) bdu_update (error,handler) ->
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 103") Exit (string_of_int agent_type)
      in
      let () =
	if trace || Remanent_parameters.get_trace parameter
	then
	  fprintf parameter.log "agent_type:%i:%s:cv_id:%i\n"
	    agent_type agent_string cv_id
      in
      let error, site_correspondence = 
        AgentMap.get parameter error agent_type site_correspondence 
      in
      let error, site_correspondence =
	match
	  site_correspondence
	with
	| None ->
	   warn parameter error (Some "line 58") Exit []
	| Some a -> error, a
      in
      let error, site_correspondence =
	let rec aux list =
	  match
	    list
	  with
	  | [] -> warn parameter error (Some "line 68") Exit []
	  | (h, list, _) :: _ when h = cv_id -> error, list
	  | _:: tail -> aux tail
	in aux site_correspondence
      in
      let error,(map1,map2) = 
        Bdu_build.new_index_pair_map parameter error site_correspondence 
      in
      let error, handler, list = 
        Mvbdu_wrapper.Mvbdu.mvbdu_full_cartesian_decomposition
          parameter handler error bdu_update 
      in 
      let error, handler =
	List.fold_left
	  (fun (error, handler) mvbdu ->	  
	   let error, handler =
	     if trace || Remanent_parameters.get_trace parameter
	     then
	       let () = Printf.fprintf parameter.log "INTENSIONAL DESCRIPTION:\n" in
	       let () = Mvbdu_wrapper.Mvbdu.print parameter.log "" mvbdu in
	       let () = Printf.fprintf parameter.log "EXTENSIONAL DESCRIPTION:\n" in
	       error,handler
	     else
	       error,handler
	   in
	   let error, handler, list = 
              Mvbdu_wrapper.Mvbdu.extensional_of_mvbdu parameter handler error mvbdu 
           in
	   let error =
	      List.fold_left
		(fun error l ->
	          let error, bool =
		    List.fold_left
		      (fun (error, bool) (site_type, state) ->
		       let error, site_type = 
                         Map.find_option parameter error site_type map2 
                       in 
		       let error, site_type =
			 match
			   site_type
			 with
			 | None -> warn parameter error (Some "line 139") Exit (-1)
			 | Some i -> error, i
		       in
		       let error, site_string =
			 try 
			   Handler.string_of_site parameter error handler_kappa
			     agent_type site_type
			 with
			   _ ->
                             warn parameter error (Some "line 147") Exit
                               (string_of_int site_type)
		       in
		       let error, state_string =
			 try
			   Handler.string_of_state_fully_deciphered parameter error
                             handler_kappa agent_type site_type state
			 with
			   _ -> warn parameter error (Some "line 146") Exit 
                             (string_of_int state)
		       in
		       let () =
			  if bool 
                          then Printf.fprintf parameter.log ","
			  else Printf.fprintf parameter.log "%s(" agent_string
                        in
		       let () = Printf.fprintf parameter.log 
                         "%s%s" site_string state_string
                       in
                       error,true
                      )
		      (error,false) l
		  in
		  let () = 
		    if bool then Printf.fprintf parameter.log ")\n"
		  in error)
		error list
	    in error, handler)
	  (error, handler)
	  list
      in 
      error, handler)
    result (error, handler)
  
			  
let print_bdu_update_map_cartesian_abstraction =
  print_bdu_update_map_cartesian_decomposition

(*parameter handler error handler_kappa result =
  Map_bdu_update.Map.fold 
    (fun (agent_type, cv_id) bdu_update (error,handler) ->
      let error, agent_string =
        Handler.string_of_agent parameter error handler_kappa agent_type
      in
      let _ = fprintf parameter.log "agent_type:%i:%s:cv_id:%i\n" 
        agent_type agent_string cv_id 
      in 
      let error, handler, list = 
        Mvbdu_wrapper.Mvbdu.mvbdu_cartesian_abstraction parameter handler error bdu_update 
      in 
      let _ = 
	List.iter 
	  (Mvbdu_wrapper.Mvbdu.print parameter.log "")
	  list
      in 
      error,handler)
    result (error,handler)*)

let print_result_dead_rule parameter error handler compiled result =
  if Remanent_parameters.get_dump_reachability_analysis_result parameter
  then
    let _ = Format.fprintf (Remanent_parameters.get_formatter parameter) 
      "\nReachability analysis result ....@." 
    in
    let parameter =
      Remanent_parameters.update_prefix parameter ""
    in
    let _ =
      fprintf (Remanent_parameters.get_log parameter)
        "\n------------------------------------------------------------\n";
      fprintf (Remanent_parameters.get_log parameter)
        "* Dead rule :\n";
      fprintf (Remanent_parameters.get_log parameter)
        "------------------------------------------------------------\n";
    in
    Array.iteri (fun index bool ->
      let error, rule_string =
        try
          Handler.string_of_rule parameter error handler compiled index
        with
          _ -> warn parameter error (Some "line 253") Exit (string_of_int index)
      in
      if bool
      then
        ()
      else
        Printf.fprintf stdout "%s is dead.\n" rule_string
    ) result  

let print_result_fixpoint parameter handler error handler_kappa site_correspondence result =
  if Remanent_parameters.get_dump_reachability_analysis_result parameter
  then
    let _ = Format.printf "\nReachability analysis result ....@." in
    let parameters_cv =
      Remanent_parameters.update_prefix parameter ""
    in
    let () =
      if trace
	 || (Remanent_parameters.get_trace parameters_cv)
      then
	begin
	  let () = Printf.fprintf (Remanent_parameters.get_log parameters_cv) "" in
	  let () =
            fprintf (Remanent_parameters.get_log parameter)
	      "\n------------------------------------------------------------\n";
            fprintf (Remanent_parameters.get_log parameter)
	      "* Fixpoint iteration :\n";
            fprintf (Remanent_parameters.get_log parameter)
	      "------------------------------------------------------------\n";
	  in
	  let () =
            print_bdu_update_map
              parameter
              error
              handler_kappa
              result
	  in
	  ()
	end
    in 
    let () =
      fprintf (Remanent_parameters.get_log parameter)
        "\n------------------------------------------------------------\n";
      fprintf (Remanent_parameters.get_log parameter)
        "* Cartesian decomposition:\n";
      fprintf (Remanent_parameters.get_log parameter)
        "------------------------------------------------------------\n";
    in
    let error, handler =
      print_bdu_update_map_cartesian_decomposition
        parameter
        handler 
        error
        handler_kappa
	site_correspondence
        result
    in
    let () =
      fprintf (Remanent_parameters.get_log parameter)
        "\n------------------------------------------------------------\n";
      fprintf (Remanent_parameters.get_log parameter)
        "* Cartesian abstraction:\n";
      fprintf (Remanent_parameters.get_log parameter)
        "------------------------------------------------------------\n";
    in
    let error,handler =
      print_bdu_update_map_cartesian_decomposition
        parameter
        handler 
        error
        handler_kappa
	site_correspondence
        result
    in
    error, handler
   else error, handler
