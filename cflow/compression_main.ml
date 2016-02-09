(**
  * compression_main.ml 
  *
  * Creation:                      <2011-10-19 16:52:55 feret>
  * Last modification: Time-stamp: <2016-01-05 16:21:22 feret> 
  * 
  * Causal flow compression: a module for KaSim 
  * Jerome Feret, projet Antique, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Universite Paris-Diderot, CNRS 
  *  
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011,2012,2013 Institut National de Recherche en Informatique 
  * et en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module U = Utilities 
module S = U.S
	     
type secret_log_info = StoryProfiling.StoryStats.log_info
type secret_step = S.PH.B.PB.CI.Po.K.refined_step
let init_secret_log_info = StoryProfiling.StoryStats.init_log_info
let secret_store_event = S.PH.B.PB.CI.Po.K.store_event
let secret_store_obs = S.PH.B.PB.CI.Po.K.store_obs

let log_step = true
let debug_mode = false
let dump_profiling_info = true

let bucket_sort = true 
let get_all_stories = false (** false -> only the first story per observable hit; true -> all stories per obs hit *)
			
let th_of_int n =
  match n mod 10 with
  | 1 -> (string_of_int n)^"st"
  | 2 -> (string_of_int n)^"nd"
  | 3 -> (string_of_int n)^"rd"
  | _ -> (string_of_int n)^"th"

let max_number_of_itterations = None

let never = (fun _ -> false) 
let always = (fun _ -> true)
let do_not_log parameter = (S.PH.B.PB.CI.Po.K.H.set_log_step parameter false)

										
let compress_and_print logger env log_info step_list =
  let parameter = S.PH.B.PB.CI.Po.K.H.build_parameter () in
  let parameter = S.PH.B.PB.CI.Po.K.H.set_log_step parameter log_step in
  let parameter = S.PH.B.PB.CI.Po.K.H.set_debugging_mode parameter debug_mode in 
  let parameter = S.PH.B.PB.CI.Po.K.H.set_logger parameter logger in 
  let parameter =
    match
      max_number_of_itterations
    with
    | None -> S.PH.B.PB.CI.Po.K.H.do_not_bound_itterations parameter
    | Some i -> S.PH.B.PB.CI.Po.K.H.set_itteration_bound parameter i
  in 
  let parameter =
    if get_all_stories
    then S.PH.B.PB.CI.Po.K.H.set_all_stories_per_obs parameter 
    else parameter 
  in
  let parameter =
    if bucket_sort
    then
      S.PH.B.PB.CI.Po.K.H.use_bucket_sort parameter
    else
      S.PH.B.PB.CI.Po.K.H.use_fusion_sort parameter
  in 
  let mode = parameter.S.PH.B.PB.CI.Po.K.H.compression_mode in
  let causal_trace_on = Parameter.get_causal_trace mode in
  let weak_compression_on = Parameter.get_weak_compression mode in
  let strong_compression_on = Parameter.get_strong_compression mode in
  let error = U.error_init in       
  let handler = S.PH.B.PB.CI.Po.K.H.init_handler env in
  let error,log_info,table1 = U.create_story_table parameter handler log_info error in
  let error,log_info,table2 = U.create_story_table parameter handler log_info error in
  let error,log_info,table3 = U.create_story_table parameter handler log_info error in
  let error,log_info,table4 = U.create_story_table parameter handler log_info error in 
  let empty_compression = table1,table2,table3,table4 in 
  let step_list = U.trace_of_pretrace step_list in 
  let causal,trivial,weak,strong =
    if (not causal_trace_on)
       && (not weak_compression_on)
       && (not strong_compression_on)
    then empty_compression
    else
      begin 
	let parameter = S.PH.B.PB.CI.Po.K.H.set_compression_none parameter in
	let error,log_info,step_list = U.remove_events_after_last_obs parameter handler log_info error step_list in 
	if not (U.has_obs step_list) 
        then
          let () = Debug.tag logger "+ No causal flow found" in
          empty_compression
        else
          let () =
            if (weak_compression_on || strong_compression_on)
            then Debug.tag logger "+ Producing causal compressions"
            else Debug.tag logger "+ Producing causal traces"
          in
	  let error,log_info,step_list = U.split_init parameter handler log_info error step_list in 
	  (* causal compression without any simplification (just partial order compression)*)
	  (* this is very costly, and mainly for teaching purpose *)
	  let error,log_info,causal_table = 
            if causal_trace_on 
            then 
              let () = 
		if log_step 
		then 
                  Debug.tag logger "\t - blackboard generation"
              in 
	      let error,log_info,step_list = U.make_unambiguous parameter handler log_info error step_list in
	      let error,log_info,blackboard = U.convert_trace_into_musical_notation parameter handler log_info error step_list in
              let () = 
		if debug_mode && log_step  
		then
		  Debug.tag logger "\t - pretty printing the grid"
              in 
              let log_info,error = 
		if debug_mode 
		then 
                  let error,log_info,() = U.export_musical_grid_to_xls parameter handler log_info error "a" 0 0 blackboard in 
                  let error,log_info,() = U.print_musical_grid parameter handler log_info error blackboard in
		  log_info,error
		else 
                  log_info,error 
              in  
              let error,log_info,list = U.extract_observable_hits_from_musical_notation parameter handler log_info error blackboard in 
              let n_stories = List.length list in 
              let () =
		if log_step 
		then 
		  Format.fprintf logger "\t - computing causal past of each observed events (%i)@." n_stories 
	      in
	      let error,log_info,causal_story_list = 
		let () = 
                  if debug_mode
                  then 
                    Debug.tag logger "\t\t * causal compression "
		in 
		(*		let log_info = U.S.PH.B.PB.CI.Po.K.P.set_start_compression log_info in *)
		(* We use the grid to get the causal precedence (pred* ) of each observable *)
		let grid = U.convert_trace_into_grid step_list handler in
		let error,log_info,enriched_grid = U.enrich_grid_with_transitive_past_of_each_node_without_a_progress_bar parameter handler log_info error grid in 
		let _ = 
                  if Parameter.log_number_of_causal_flows
                  then 
                    Causal.print_stat logger parameter handler enriched_grid 
		in 
		let () =
		  if log_step 
		  then 
		    Format.fprintf logger "\t - causal flow compression (%i)@." n_stories 
		in
		(* we fold the list of obervable hit, and for each one collect the causal past *)
		U.fold_left_with_progress_bar
		  parameter
		  handler
		  log_info
		  error
		  ~event:StoryProfiling.Collect_traces  
		  (fun
		      parameter 
		      ?(shall_we_compute=always) 
		      ?(shall_we_compute_profiling_information=always) 
		      handler log_info error story_list observable_id -> 
		   (*		    let log_info = S.PH.B.PB.CI.Po.K.P.reset_log log_info in *)
		    let () = 
                      if debug_mode
                      then 
			Debug.tag logger "\t\t * causal compression "
                    in 
		    let error,log_info,trace_before_compression = U.causal_prefix_of_an_observable_hit parameter handler log_info error "compression_main, line 2014" blackboard enriched_grid observable_id in 
                    let info = 
                      match U.get_runtime_info_from_observable_hit observable_id 
                      with 
                      | None -> []
                      | Some info -> 
			let info = {info with Mods.story_id = U.get_counter story_list} in 
			let info = Mods.update_profiling_info log_info  info 
			in 
			[info]
                    in
		    let error,log_info,causal_story_array = U.store_trace parameter handler log_info error trace_before_compression info story_list in 
		    error,log_info,causal_story_array  
		  )
	          table1
                  (List.rev list)
            in 
	    let error,log_info,causal_story_list = U.flatten_story_table  parameter handler log_info error causal_story_list in 
            error,log_info,causal_story_list 
	    else 
              error,log_info,table1 
          in
	  (* Now causal compression, with detection of siphons & detection of pseudo inverse events *)
	  let one_iteration_of_compression (log_info,error,event_list) = 
	    let error,log_info,event_list = 
	      if Graph_closure.ignore_flow_from_outgoing_siphon
	      then
		U.fill_siphon parameter ~shall_we_compute:always ~shall_we_compute_profiling_information:always handler log_info error event_list 
	      else
		error,log_info,event_list 
	    in
	    let () =
	      if debug_mode then
		U.print_trace parameter handler event_list
	    in
	    let error,log_info,event_list = 
	      if  Parameter.do_global_cut
	      then
		U.cut parameter ~shall_we_compute:always ~shall_we_compute_profiling_information:always handler log_info error event_list
	    else
	      error,log_info,event_list
	    in 
	    if Parameter.cut_pseudo_inverse_event 
	    then
	      U.remove_pseudo_inverse_events parameter ~shall_we_compute:always ~shall_we_compute_profiling_information:always handler log_info error event_list
	    else 
              error,log_info,event_list				      
          in
	  (* This fonction iter the causal compression until a fixpoint is reached *)
	  let rec aux k (error,log_info,event_list) = 
	    match 
	      S.PH.B.PB.CI.Po.K.H.get_bound_on_itteration_number parameter
	    with 
	      Some k' when k>=k' -> error,log_info,event_list 
	    | Some _ | None -> 
	      let output_opt = 
		try 
		  Some (one_iteration_of_compression (log_info,error,event_list))
		with 
		  ExceptionDefn.UserInterrupted _ -> None 
	      in 
	      match 
		output_opt 
	      with 
	      | None -> error,log_info,event_list 
	      | Some (error,log_info,event_list') -> 
		if U.size_of_pretrace event_list' < U.size_of_pretrace event_list
		then 
		  aux (k+1) (error,log_info,event_list')
		else 
		  error,log_info,event_list'
	  in 
	  let error,log_info,causal_story_table = 
            if weak_compression_on || strong_compression_on 
            then
	      let error,log_info,causal_story_list = 
		Utilities_expert.fold_over_the_causal_past_of_observables_with_a_progress_bar_while_reshaking_the_trace
		  parameter ~shall_we_compute:always ~shall_we_compute_profiling_information:always 
		  handler log_info error
		  always never		  
		  Utilities_expert.parameters
		  aux
		  (fun
		      parameter  (*?(shall_we_compute=always) ?(shall_we_compute_profiling_information=always)*)
		      handler log_info error trace  -> 
		   (* we remove pseudo inverse events *)
		   let error,log_info,trace = 
                     U.remove_pseudo_inverse_events (do_not_log parameter)  ~shall_we_compute:always ~shall_we_compute_profiling_information:always handler log_info error trace
		   in
		   (* we compute causal compression *)
		     U.cut (do_not_log parameter) ~shall_we_compute:always ~shall_we_compute_profiling_information:always handler log_info error trace
		   )
		  U.store_trace
		  step_list
		  table2
	      in 
	      let error,log_info,causal_story_list = 
		U.flatten_story_table  parameter handler log_info error causal_story_list 
	      in 
              error,log_info,causal_story_list 
	    else 
	      error,log_info,table2 
          in
          let _ = print_newline () in 
          let _ = print_newline () in 
	  let n_causal_stories = U.count_stories causal_story_table in 
          let error,log_info,weakly_story_table =
            if weak_compression_on || strong_compression_on 
            then 
              begin
		let () = Format.fprintf logger "\t - weak flow compression (%i)@." n_causal_stories in 
		let parameter = S.PH.B.PB.CI.Po.K.H.set_compression_weak parameter in 
		let error,log_info,weakly_story_table = 
		  U.fold_story_table_with_progress_bar parameter handler log_info error "weak compression" 
						       (fun parameter ?(shall_we_compute=always) ?(shall_we_compute_profiling_information=always) handler log_info error trace list_info story_list ->
							let error,log_info,list = U.weakly_compress parameter handler log_info error trace in 
							let error,log_info,story_list =
							  List.fold_left
							    (fun (error,log_info,story_list) trace -> 
							     U.store_trace parameter handler log_info error trace list_info story_list)
							    (error,log_info,story_list)
							    list
							in error,log_info,story_list)
						       causal_story_table 
						       table3
		in 
		U.flatten_story_table parameter handler log_info error weakly_story_table
	      end
	    else 
              error,log_info,table3
	  in 
          let n_weak_stories = U.count_stories weakly_story_table in 
          let error,log_info,strongly_story_table = 
            if strong_compression_on 
            then 
              begin 
                let parameter = S.PH.B.PB.CI.Po.K.H.set_compression_strong parameter in 
                let () = Format.fprintf logger "\t - strong flow compression (%i)@." n_weak_stories in
		let error,log_info,strongly_story_table =
		  U.fold_story_table_with_progress_bar
		    parameter handler log_info error 
		    "strong_compression" 
		    (fun parameter ?(shall_we_compute=always) ?(shall_we_compute_profiling_information=always) handler log_info error refined_event_list list_info strongly_story_table -> 
                        let error,log_info,list = U.compress parameter handler log_info error refined_event_list in
			let error,log_info,strongly_story_table = 
                          match 
                            list
                          with 
                          | [] -> 
                             error,log_info,strongly_story_table
                          | _ -> 
			     List.fold_left
			       (fun (error,log_info,strong_story_table) list -> 
				U.store_trace parameter handler log_info error list list_info strongly_story_table) 
			       (error,log_info,strongly_story_table)
			       list 
			in 			
			error,log_info,strongly_story_table)
		    weakly_story_table 
		    table4
		in 	
		U.flatten_story_table parameter handler log_info error strongly_story_table
	      end
            else 
              error,log_info,table4
          in 
	  causal_table,
	  causal_story_table,
	  weakly_story_table,
	  strongly_story_table 
        end 
  in 
  let error,log_info =
    if causal_trace_on then
      let error,log_info,export = U.export_story_table parameter handler log_info error causal in 
      let error,log_info = Causal.pretty_print (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) handler log_info error  env Graph_closure.config_small_graph "" "" export in
      error,log_info
    else error,log_info
  in
  let error,log_info =
    if weak_compression_on then
      let error,log_info,export = U.export_story_table parameter handler log_info error weak in
      let error,log_info = Causal.pretty_print (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) handler log_info error  env Graph_closure.config_small_graph "Weakly" "weakly " export in
      error,log_info
    else error,log_info
  in
  let error,log_info = 
    if strong_compression_on then
      let error,log_info,export = U.export_story_table parameter handler log_info error strong in
      let error,log_info = Causal.pretty_print (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) handler log_info error env Graph_closure.config_small_graph "Strongly" "strongly " export in
      error,log_info
    else
      error,log_info
  in
  let _ = StoryProfiling.StoryStats.close_logger (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) in 
  let _ =
    Exception.print_for_KaSim (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error
  in
  ()
    
