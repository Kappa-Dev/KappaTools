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

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Print bdu analysis") message exn (fun () -> default)  

let trace = false

(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error handler_kappa compiled result =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type_" in
  let _ =
    if Remanent_parameters.get_dump_reachability_analysis_static parameter
    then
      let _ = Format.printf "\nReachability analysis static information ....@." in
      let parameters_cv =
        Remanent_parameters.update_prefix parameter ""
      in
      if (Remanent_parameters.get_trace parameters_cv)
      then Printf.fprintf (Remanent_parameters.get_log parameters_cv) "\n";
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
      Handler.string_of_agent parameter error handler_kappa agent_type
    in
    let _ =
      fprintf parameter.log "agent_type:%i:%s:cv_id:%i\n" 
        agent_type agent_string cv_id
    in
    Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_update
    ) result

let print_bdu_update_map_cartesian_decomposition parameter handler error handler_kappa result = 
  Map_bdu_update.Map.fold 
    (fun (agent_type, cv_id) bdu_update (error,handler) ->
      let error, agent_string =
        Handler.string_of_agent parameter error handler_kappa agent_type
      in
      let _ = fprintf parameter.log "agent_type:%i:%s:cv_id:%i\n" 
        agent_type agent_string cv_id 
      in 
      let error, handler, list = 
        Mvbdu_wrapper.Mvbdu.mvbdu_full_cartesian_decomposition
          parameter handler error bdu_update 
      in 
      let error, handler =
	List.fold_left
	  (fun (error, handler) mvbdu ->
	    let () = Mvbdu_wrapper.Mvbdu.print parameter.log "" mvbdu in
	    let error, handler, list = 
              Mvbdu_wrapper.Mvbdu.extensional_of_mvbdu parameter handler error mvbdu 
            in
	    let () = Printf.fprintf parameter.log "EXTENSIONAL DESCRIPTION:\n" in 
	    let () =
	      List.iter
		(fun l ->
	          let () =
		    List.iter
		      (fun (site_type, state) ->
                        let error, site_string =
                          Handler.string_of_site parameter error handler_kappa
                            agent_type (site_type-1)
                        (*NOTE: minus 1, because using a new indexes for
                          site_type which is increased by 1*)
                        in
                        let error, state_string =
                          Handler.string_of_state parameter error handler_kappa
                          agent_type (site_type-1) state
                        in
                        (*Print with string*)
			(*let _ =
                          Printf.fprintf parameter.log 
                            "agent_type:%i:%s:(site_type:%i:%s:state:%i:%s);" 
                            agent_type agent_string 
                            site_type site_string
                            state state_string
                        in*)
                        let _ =
                          Printf.fprintf parameter.log 
                            "%s(%s;%s);" agent_string site_string state_string
                        in
                        ()
                      )
		      l in
		  Printf.fprintf parameter.log "\n")
		list
	    in error, handler)
	  (error, handler)
	  list
      in 
      error,handler)
    result (error,handler)
			  
let print_bdu_update_map_cartesian_abstraction parameter handler error handler_kappa result =
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
    result (error,handler)

let print_result_dead_rule parameter error handler compiled result =
  if Remanent_parameters.get_dump_reachability_analysis_result parameter
  then
    let _ = Format.printf "\nReachability analysis result ....@." in
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
        Handler.string_of_rule parameter error handler compiled index
      in
      if bool
      then
        ()
        (*Printf.fprintf stdout "%s: %b.\n" rule_string bool*)
      else
        Printf.fprintf stdout "%s is dead.\n" rule_string
    ) result  

let print_result_fixpoint parameter handler error handler_kappa result =
  if Remanent_parameters.get_dump_reachability_analysis_result parameter
  then
    let _ = Format.printf "\nReachability analysis result ....@." in
    let parameters_cv =
      Remanent_parameters.update_prefix parameter ""
    in
    let _ =
      if (Remanent_parameters.get_trace parameters_cv)
      then Printf.fprintf (Remanent_parameters.get_log parameters_cv) "";
      let _ =
        fprintf (Remanent_parameters.get_log parameter)
          "\n------------------------------------------------------------\n";
        fprintf (Remanent_parameters.get_log parameter)
          "* Fixpoint iteration :\n";
        fprintf (Remanent_parameters.get_log parameter)
          "------------------------------------------------------------\n";
      in
      let _ =
        print_bdu_update_map
          parameter
          error
          handler_kappa
          result
      in
      let _ =
        fprintf (Remanent_parameters.get_log parameter)
          "\n------------------------------------------------------------\n";
        fprintf (Remanent_parameters.get_log parameter)
          "* Cartesian decomposition:\n";
        fprintf (Remanent_parameters.get_log parameter)
          "------------------------------------------------------------\n";
      in
      let _ =
        print_bdu_update_map_cartesian_decomposition
          parameter
          handler 
          error
          handler_kappa
          result
      in
      let _ =
        fprintf (Remanent_parameters.get_log parameter)
          "\n------------------------------------------------------------\n";
        fprintf (Remanent_parameters.get_log parameter)
          "* Cartesian abstraction:\n";
        fprintf (Remanent_parameters.get_log parameter)
          "------------------------------------------------------------\n";
      in
      let _ =
        print_bdu_update_map_cartesian_decomposition
          parameter
          handler 
          error
          handler_kappa
          result
      in
      error, handler
    in
    error, handler
  else error, handler
