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
    if Remanent_parameters.get_do_reachability_analysis_static parameter
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
    if  Remanent_parameters.get_do_reachability_analysis_dynamic parameter
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
(*print only working list*)

let print_result_wl parameter error result =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type_" in
  let _ =
    print_bdu_build_wl
      parameter
      error
      result.store_bdu_build
  in
  error

(************************************************************************************)
(*main print of fixpoint*)

let print_bdu_update_map parameter error result =
  Map_bdu_update.Map.iter (fun (agent_type, cv_id) bdu_update ->
    let _ =
      fprintf parameter.log "agent_type:%i:cv_id:%i\n" agent_type cv_id
    in
    Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_update
    ) result

let print_bdu_update_map_cartesian parameter handler error result = 
  Map_bdu_update.Map.fold 
    (fun (agent_type, cv_id) bdu_update (error,handler) -> 
      let _ = fprintf parameter.log "agent_type:%i:cv_id:%i\n" agent_type cv_id in 
      let error,handler,list = 
        Mvbdu_wrapper.Mvbdu.mvbdu_cartesian_abstraction parameter handler error bdu_update 
      in 
      let _ = 
	List.iter 
	  (Mvbdu_wrapper.Mvbdu.print parameter.log "")
	  list
      in 
      error,handler)
    result (error,handler)
	
let print_result_fixpoint parameter handler error result =
  if Remanent_parameters.get_do_reachability_analysis_result parameter
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
        print_bdu_update_map_cartesian
          parameter
          handler 
          error
          result
      in
      error, handler
    in
    error, handler
  else error, handler
