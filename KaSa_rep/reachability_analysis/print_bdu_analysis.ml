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
  let error =
    if trace
      || Remanent_parameters.get_trace parameter
      || Remanent_parameters.get_dump_reachability_analysis_static parameter
    then
      let _ = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) 
        "Reachability analysis static information ...." 
      in
      let _ = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let parameters_cv =
        Remanent_parameters.update_prefix parameter ""
      in
      if (Remanent_parameters.get_trace parameters_cv)
      then
	let _ =
          Loggers.print_newline (Remanent_parameters.get_logger parameters_cv)
	in
	print_result_static
          parameter
          error
          handler_kappa
          compiled
          result.store_bdu_analysis_static
      else error
    else
      error
  in
  (*------------------------------------------------------------------------------*)
  let error =
    if  Remanent_parameters.get_dump_reachability_analysis_dynamic parameter
    then
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) 
        "Reachability analysis dynamic information ...." 
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      let parameters_cv =
        Remanent_parameters.update_prefix parameter ""
      in
      if (Remanent_parameters.get_trace parameters_cv)
      then
	let () = Loggers.print_newline  (Remanent_parameters.get_logger parameters_cv) in
	print_result_dynamic
          parameter
          error
          handler_kappa
          compiled
          result.store_bdu_analysis_dynamic
      else error
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
  Map_bdu_update.Map.fold (fun (agent_type, cv_id) bdu_update error ->
    let error', agent_string =
        Handler.string_of_agent parameter error handler_kappa agent_type
    in
    let error = Exception.check warn parameter error error' (Some "line 95") Exit in
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter) "agent_type:%i:%s:cv_id:%i"
        agent_type agent_string cv_id
    in
    let () =
      Loggers.print_newline (Remanent_parameters.get_logger parameter)
    in
    let () =
      Mvbdu_wrapper.Mvbdu.print parameter bdu_update
    in
    error)
    result error

(************************************************************************************)

let smash_map decomposition
    ~show_dep_with_dimmension_higher_than:dim_min
    parameter handler error handler_kappa site_correspondence result =
  let error,handler,mvbdu_true =
    Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error
  in
  Map_bdu_update.Map.fold
    (fun (agent_type, cv_id) bdu (error,handler,output) ->
      let error, handler, list =
        decomposition parameter handler error bdu
      in
      let error, site_correspondence =
        AgentMap.get parameter error agent_type site_correspondence
      in
      let error, site_correspondence =
	match site_correspondence with
	| None -> warn parameter error (Some "line 58") Exit []
	| Some a -> error, a
      in
      let error, site_correspondence =
	let rec aux list =
	  match list with
	  | [] -> warn parameter error (Some "line 68") Exit []
	  | (h, list, _) :: _ when h = cv_id -> error, list
	  | _ :: tail -> aux tail
	in aux site_correspondence
      in
      let error, (map1, map2) =
        Bdu_build.new_index_pair_map parameter error site_correspondence
      in
      let rename_site parameter error site_type =
        let error, site_type =
          match Map.find_option parameter error site_type map2 with
          | error, None -> warn parameter error (Some "line 165") Exit (-1)
          | error, Some i -> error, i
        in
        error, site_type
       in
      List.fold_left
	(fun (error, handler, output) bdu ->
	  begin
	    let error, handler, lvar =
	      Mvbdu_wrapper.Mvbdu.variables_list_of_mvbdu
		parameter handler error
		bdu
	    in
	    let error, handler, list =
	      Mvbdu_wrapper.Mvbdu.extensional_of_variables_list
		parameter handler error
		lvar
	    in
	    let error,asso =
	      List.fold_left
		(fun (error, list) i ->
		  let error, new_name =
		    rename_site parameter error i
		  in
		  error,(i, new_name) :: list)
		(error, [])
		(List.rev list)
	    in
	    let error,handler,hconsed_asso =
	      Mvbdu_wrapper.Mvbdu.build_association_list parameter handler error asso
	    in
	    let error,handler,renamed_mvbdu =
	      Mvbdu_wrapper.Mvbdu.mvbdu_rename parameter handler error bdu hconsed_asso
	    in
	    let error,handler,hconsed_vars =
	      Mvbdu_wrapper.Mvbdu.variables_list_of_mvbdu 
                parameter handler error renamed_mvbdu
	    in
            let error,cv_map_opt =
	      AgentMap.unsafe_get parameter error agent_type output
	    in
	    let error,cv_map =
	      match
		cv_map_opt
	      with
	      | None ->
		error, Wrapped_modules.LoggedIntMap.empty
	      | Some map -> error, map
	    in
	    let error, handler, cv_map' =
	      Mvbdu_wrapper.Mvbdu.store_by_variables_list
       		Wrapped_modules.LoggedIntMap.find_default_without_logs
		Wrapped_modules.LoggedIntMap.add_or_overwrite
		mvbdu_true
		Mvbdu_wrapper.Mvbdu.mvbdu_and
		parameter
		handler
		error
		hconsed_vars
		renamed_mvbdu
		cv_map
	    in
	    let error,output =
	      AgentMap.set parameter error agent_type cv_map'
		output
	    in
	    error, handler, output
	  end)
	(error, handler, output)
	list)
    result
    (let error, agent_map =
       AgentMap.create parameter error 0
     in
     (error, handler, agent_map))

  (**************************************************************************)

  let print_bdu_update_map_gen_decomposition decomposition
      ~smash:smash ~show_dep_with_dimmension_higher_than:dim_min
    parameter handler error handler_kappa site_correspondence result =
  if
    smash
  then
   let error,handler,output =
      smash_map 
        decomposition 
        ~show_dep_with_dimmension_higher_than:dim_min parameter handler error handler_kappa site_correspondence result
   in
   AgentMap.fold
     parameter
     error
     (fun parameter error agent_type map (handler:Mvbdu_wrapper.Mvbdu.handler) ->
	let error', agent_string =
          try
            Handler.string_of_agent parameter error handler_kappa agent_type
          with
            _ -> warn parameter error (Some "line 111") Exit (string_of_int agent_type)
	in
	let error = Exception.check warn parameter error error' (Some "line 110") Exit in
	(*-----------------------------------------------------------------------*)
	Wrapped_modules.LoggedIntMap.fold
	  (fun _ mvbdu (error,handler)
	  ->
	    let error, handler =
	      if trace || Remanent_parameters.get_trace parameter
	      then
		let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                  "INTENSIONAL DESCRIPTION:" 
                in
		let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
		let () = Mvbdu_wrapper.Mvbdu.print parameter mvbdu in
		let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                  "EXTENSIONAL DESCRIPTION:" 
                in
		let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
		error, handler
	      else
		error, handler
	    in
	    let error, (handler, translation) =
	      Translation_in_natural_language.translate
		parameter handler error (fun _ e i -> e, i) mvbdu
	    in
	    (*-----------------------------------------------------------------------*)
	    let error =
	      Translation_in_natural_language.print
		~show_dep_with_dimmension_higher_than:dim_min parameter
		handler_kappa error agent_string agent_type translation
	    in
	    error, handler
	  )
	  map
	  (error, handler))
      output handler
  else
    begin
      Map_bdu_update.Map.fold
	(fun (agent_type, cv_id) bdu_update (error,handler) ->
	  let error', agent_string =
            try
              Handler.string_of_agent parameter error handler_kappa agent_type
            with
              _ -> warn parameter error (Some "line 111") Exit (string_of_int agent_type)
	  in
	  let error = Exception.check warn parameter error error' (Some "line 110") Exit in
          (*-----------------------------------------------------------------------*)
	  let () =
	    if trace || Remanent_parameters.get_trace parameter
	    then
	      let () =
		Loggers.fprintf (Remanent_parameters.get_logger parameter) 
                  "agent_type:%i:%s:cv_id:%i"
		  agent_type agent_string cv_id
	      in
	      Loggers.print_newline (Remanent_parameters.get_logger parameter)
	  in
          (*-----------------------------------------------------------------------*)
	  let error, site_correspondence =
            AgentMap.get parameter error agent_type site_correspondence
	  in
	  let error, site_correspondence =
	    match site_correspondence with
	    | None -> warn parameter error (Some "line 58") Exit []
	    | Some a -> error, a
	  in
	  let error, site_correspondence =
	    let rec aux list =
	      match list with
	      | [] -> warn parameter error (Some "line 68") Exit []
	      | (h, list, _) :: _ when h = cv_id -> error, list
	      | _ :: tail -> aux tail
	    in aux site_correspondence
	  in
          (*-----------------------------------------------------------------------*)
	  let error,(map1, map2) =
            Bdu_build.new_index_pair_map parameter error site_correspondence
	  in
          (*-----------------------------------------------------------------------*)
	  let error, handler, list =
            decomposition parameter handler error bdu_update
	  in
          (*-----------------------------------------------------------------------*)
	  let error, handler =
	    List.fold_left
	      (fun (error, handler) mvbdu ->
		let error, handler =
		  if trace || Remanent_parameters.get_trace parameter
		  then
		    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
                      "INTENSIONAL DESCRIPTION:" in
		    let () = Loggers.print_newline 
                      (Remanent_parameters.get_logger parameter) 
                    in
		    let () = Mvbdu_wrapper.Mvbdu.print parameter mvbdu in
		    let () = Loggers.fprintf 
                      (Remanent_parameters.get_logger parameter) 
                      "EXTENSIONAL DESCRIPTION:" 
                    in
		    let () = Loggers.print_newline 
                      (Remanent_parameters.get_logger parameter) 
                    in
		    error, handler
		  else
		    error, handler
		in
		let rename_site parameter error site_type =
		  let error, site_type =
		    match Map.find_option parameter error site_type map2 with
		    | error, None -> warn parameter error (Some "line 165") Exit (-1)
		    | error, Some i -> error, i
		  in
		  error, site_type
		in
		let error, (handler, translation) =
		  Translation_in_natural_language.translate
		    parameter handler error rename_site mvbdu
		in
	        (*-----------------------------------------------------------------------*)
		let error =
		  Translation_in_natural_language.print
		    ~show_dep_with_dimmension_higher_than:dim_min parameter
		    handler_kappa error agent_string agent_type translation
		in error, handler
	      )
	      (error, handler)
	      list
	  in
	  error, handler)
	result (error, handler)
    end
(************************************************************************************)
(*non relational properties*)

let print_bdu_update_map_cartesian_abstraction a b c d =
  print_bdu_update_map_gen_decomposition
    ~smash:true
    ~show_dep_with_dimmension_higher_than:1
    Mvbdu_wrapper.Mvbdu.mvbdu_cartesian_abstraction a b c d

(************************************************************************************)
(*relational properties*)

let print_bdu_update_map_cartesian_decomposition a b c d =
  print_bdu_update_map_gen_decomposition
    ~smash:true
    ~show_dep_with_dimmension_higher_than:
    (if Remanent_parameters.get_hide_one_d_relations_from_cartesian_decomposition a 
     then 2 
     else 1
    )
    Mvbdu_wrapper.Mvbdu.mvbdu_full_cartesian_decomposition a b c d

(************************************************************************************)

let print_result_dead_rule parameter error handler compiled result =
  if Remanent_parameters.get_dump_reachability_analysis_result parameter
  then
    let parameter =
      Remanent_parameters.update_prefix parameter ""
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "------------------------------------------------------------" in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "* Dead rule :"
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "------------------------------------------------------------" in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let size = Array.length result in
    let rec aux k error =
      if k = size then error
      else
	let bool = Array.get result k in
	let error =
	  if bool
	  then
	    error
	  else
	    let error', rule_string =
              try
                Handler.string_of_rule parameter error handler compiled k
              with
              _ -> warn parameter error (Some "line 238") Exit (string_of_int k)
	    in
	    let error = Exception.check warn parameter error error' (Some "line 234") Exit in
            let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "%s will never be applied." rule_string
	    in
	    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
	    error
	in aux (k+1) error
    in aux 0 error
  else
    error

(************************************************************************************)

let print_result_fixpoint parameter handler error handler_kappa site_correspondence result =
  if Remanent_parameters.get_dump_reachability_analysis_result parameter
  then
    let error =
      if trace
	|| (Remanent_parameters.get_trace parameter)
      then
	begin
	  let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "" in
	  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
	  let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
	      "------------------------------------------------------------" in
	  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
          let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "* Fixpoint iteration :"
          in
	  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
	  let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "------------------------------------------------------------"
          in
	  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
	  let error =
            print_bdu_update_map
              parameter
              error
              handler_kappa
              result
	  in
	  error
	end
      else error
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "------------------------------------------------------------" in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "* Relational properties:" 
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "------------------------------------------------------------" 
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let error, handler =
      print_bdu_update_map_cartesian_decomposition
        parameter
        handler
        error
        handler_kappa
	site_correspondence
        result
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "------------------------------------------------------------" in
    let () =
      Loggers.print_newline (Remanent_parameters.get_logger parameter)
    in
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) 
      "* Non relational properties:" 
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) 
      "------------------------------------------------------------" 
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let error, handler =
      print_bdu_update_map_cartesian_abstraction
        parameter
        handler
        error
        handler_kappa
	site_correspondence
        result
    in
    error, handler
   else error, handler
