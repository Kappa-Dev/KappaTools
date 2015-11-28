(**
  * compression_main.ml 
  *
  * Creation:                      <2011-10-19 16:52:55 feret>
  * Last modification: Time-stamp: <2015-11-20 21:59:26 feret> 
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
module D = U.D

type secret_log_info = U.D.S.PH.B.PB.CI.Po.K.P.log_info
type secret_step = D.S.PH.B.PB.CI.Po.K.refined_step
let init_secret_log_info = U.D.S.PH.B.PB.CI.Po.K.P.init_log_info
let secret_store_event = D.S.PH.B.PB.CI.Po.K.store_event
let secret_store_obs = D.S.PH.B.PB.CI.Po.K.store_obs

let old_version = false
let log_step = true
let debug_mode = false
let dump_profiling_info = true

let store_uncompressed_stories = false

let get_all_stories = false (** false -> only the first story per observable hit; true -> all stories per obs hit *)
			
let th_of_int n =
  match n mod 10 with
  | 1 -> (string_of_int n)^"st"
  | 2 -> (string_of_int n)^"nd"
  | 3 -> (string_of_int n)^"rd"
  | _ -> (string_of_int n)^"th"

let always = (fun _ -> true)
let do_not_log parameter = (D.S.PH.B.PB.CI.Po.K.H.set_log_step parameter false)
			     
let compress_and_print logger env log_info step_list =
  let parameter = D.S.PH.B.PB.CI.Po.K.H.build_parameter () in
  let parameter = D.S.PH.B.PB.CI.Po.K.H.set_log_step parameter log_step in
  let parameter = D.S.PH.B.PB.CI.Po.K.H.set_debugging_mode parameter debug_mode in 
  let parameter =
    if get_all_stories
    then D.S.PH.B.PB.CI.Po.K.H.set_all_stories_per_obs parameter 
    else parameter 
  in 
  let mode = parameter.D.S.PH.B.PB.CI.Po.K.H.compression_mode in
  let causal_trace_on = Parameter.get_causal_trace mode in
  let weak_compression_on = Parameter.get_weak_compression mode in
  let strong_compression_on = Parameter.get_strong_compression mode in
  let error = U.error_init in       
  let handler =
    {
      D.S.PH.B.PB.CI.Po.K.H.env = env ;
    } in
  let causal,trivial,weak,strong =
    if (not causal_trace_on)
       && (not weak_compression_on)
       && (not strong_compression_on)
    then [],[],[],[]
    else
      begin (* causal compression *)
        let parameter = D.S.PH.B.PB.CI.Po.K.H.set_compression_none parameter in
	let error,log_info,step_list = U.remove_events_after_last_obs parameter always handler log_info error step_list in 
        if not @@ List.exists D.S.PH.B.PB.CI.Po.K.is_obs_of_refined_step step_list
        then
          let () = Debug.tag logger "+ No causal flow found" in
          [],[],[],[]
        else
          let () =
            if (weak_compression_on || strong_compression_on)
            then Debug.tag logger "+ Producing causal compressions"
            else Debug.tag logger "+ Producing causal traces"
          in
	  let error,log_info,step_list_with_more_init = U.split_init parameter  always handler log_info error step_list in 
          let error,log_info,refined_event_list = U.disambiguate parameter always handler log_info error step_list_with_more_init in
	  let error,log_info,refined_event_list_wo_siphon =
	    if Graph_closure.ignore_flow_from_outgoing_siphon
	    then
	      let error,log_info,step_list = U.fill_siphon parameter always handler log_info error refined_event_list in
	      U.disambiguate parameter always handler log_info error step_list
	    else
	      error,log_info,refined_event_list 
	  in
	  let () =
            if debug_mode then
	      U.print_trace parameter handler refined_event_list_wo_siphon
	  in
	  let error,log_info,refined_event_list_cut =
            if (weak_compression_on || strong_compression_on)
	       && Parameter.do_global_cut
	    then
              U.cut parameter always handler log_info error refined_event_list_wo_siphon 
	    else
	      error,log_info,refined_event_list_wo_siphon
          in
          
          let deal_with error cut log_info = 
            let error,log_info,refined_event_list_without_pseudo_inverse = 
              if cut && Parameter.cut_pseudo_inverse_event 
              then
		U.remove_pseudo_inverse_events parameter always handler log_info error refined_event_list_cut  
	      else 
                error,log_info,refined_event_list				      
            in 
            let () = 
              if log_step 
              then 
                Debug.tag logger "\t - blackboard generation"
            in 
            let error,log_info,blackboard = U.convert_trace_into_musical_notation parameter handler error log_info refined_event_list_without_pseudo_inverse in           
            let () = 
              if debug_mode && log_step  
              then 
                Debug.tag logger "\t - pretty printing the grid"
            in 
            let error = 
              if debug_mode 
              then 
                let error = U.export_musical_grid_to_xls parameter handler error "a" 0 0 blackboard in 
                let error = U.print_musical_grid parameter handler error blackboard in 
		error
              else 
                error 
            in  
            let error,list = U.extract_observable_hits_from_musical_notation parameter handler error blackboard in 
            let n_stories = List.length list in 
            let () =
	      if log_step 
	      then 
	      Format.fprintf logger "\t - computing causal past of each observed events (%i)@." n_stories 
	    in
	    (* generation of uncompressed stories *)
	    let error,log_info,causal_story_list = 
              let () = 
                if debug_mode
                then 
                  Debug.tag logger "\t\t * causal compression "
              in 
              let log_info = U.D.S.PH.B.PB.CI.Po.K.P.set_start_compression log_info in 

	      (* We use the grid to get the causal precedence (pred* ) of each observable *)
	      let grid = U.convert_trace_into_grid_while_trusting_side_effects refined_event_list_without_pseudo_inverse handler in
              let enriched_grid =
		if cut
		then
		  U.enrich_big_grid_with_transitive_closure logger grid
		else
	          U.enrich_std_grid_with_transitive_closure logger grid
	      in 
	      let _ = 
                if Parameter.log_number_of_causal_flows
                then 
                  Causal.print_stat logger parameter handler enriched_grid 
              in 
              let () =
		if log_step 
		then 
		  Format.fprintf logger "\t - %s (%i)@." 
		    (if store_uncompressed_stories
		     then
			"causal flow compression"
		     else
		        "causal & weak flow compression") 
		    n_stories 
	      in
	      let story_list = U.empty_story_table_with_progress_bar logger n_stories in 
              List.fold_left 
                (fun (error,log_info,story_list) observable_id -> 
		 let log_info = D.S.PH.B.PB.CI.Po.K.P.reset_log log_info in 
		 let () = 
                    if debug_mode
                    then 
                      Debug.tag logger "\t\t * causal compression "
                  in 
		  let error,trace_before_compression = U.causal_prefix_of_an_observable_hit "compression_main, line 2014" parameter handler error log_info blackboard enriched_grid observable_id in 
                  let info = 
                    match U.get_runtime_info_from_observable_hit observable_id 
                    with 
                    | None -> []
                    | Some info -> 
		       let info = 
                         {info with Mods.story_id = U.get_counter story_list}
		       in 
		       let info = Mods.update_profiling_info log_info  info 
		       in 
                       [info]
                  in
		  if
		    store_uncompressed_stories || not cut
		  then
		    let error,log_info,trace_without_pseudo_inverse_events = 
                      if cut 
                      then 
			U.remove_pseudo_inverse_events (do_not_log parameter) always handler log_info error trace_before_compression  
                      else 
			error,log_info,trace_before_compression
                    in 
                    let error,log_info,blackboard_cflow = U.convert_trace_into_musical_notation parameter handler error log_info trace_without_pseudo_inverse_events in 
                    let error,observable_hit = U.extract_observable_hit_from_musical_notation "compression_main.ml, line 214, " parameter handler error blackboard_cflow in 		 
		    let grid = U.convert_trace_into_grid_while_trusting_side_effects trace_without_pseudo_inverse_events handler in 
                    let enriched_grid = U.enrich_small_grid_with_transitive_closure logger grid in 
		    let error,event_list = U.causal_prefix_of_an_observable_hit "" parameter handler error log_info blackboard_cflow enriched_grid observable_hit in 
		    let event_list_with_side_effects = U.extend_trace_with_dummy_side_effects event_list in 
		    let error,causal_story_array,log_info = 
		      U.store_trace_while_trusting_side_effects_with_progress_bar parameter handler error info log_info event_list_with_side_effects  event_list story_list 
		    in 
		    error,log_info,causal_story_array  
		  else
		    U.from_none_to_weak_with_progress_bar  parameter handler  logger (error,log_info,story_list) (trace_before_compression,info)
		)
                (error,log_info,story_list)
                (List.rev list)
            in 
	    let error,causal_story_list = 
              U.flatten_story_table  parameter handler error causal_story_list 
	    in 
            error,log_info,causal_story_list 
          in

	  let error,log_info,causal_table = 
            if causal_trace_on 
            then 
              deal_with error false log_info 
            else 
	      error,log_info,U.empty_story_table ()
          in 
          let error,log_info,causal_story_table = 
            if weak_compression_on || strong_compression_on 
            then 
              deal_with error true log_info 
            else
              error,log_info,U.empty_story_table ()
          in 
          let _ = print_newline () in 
          let _ = print_newline () in 
	  let n_causal_stories = U.count_stories causal_story_table in 
          let error,weakly_story_table =
            if weak_compression_on || strong_compression_on 
            then 
              if
		store_uncompressed_stories
	      then
		begin 
                  let () = Format.fprintf logger "\t - weak flow compression (%i)@." n_causal_stories in 
                  let parameter = D.S.PH.B.PB.CI.Po.K.H.set_compression_weak parameter in 
                  let weak_stories_table =  U.empty_story_table_with_progress_bar logger n_causal_stories in 
                  let error,log_info,weakly_story_table = 
                    List.fold_left 
                      (fun x (_,a) ->
                       List.fold_left 
                         (U.from_none_to_weak_with_progress_bar_ext parameter handler  logger)
			 x
			 a)
                    (error,log_info,weak_stories_table)
                    (List.rev (U.get_stories causal_story_table))
                  in 
	          let error,weakly_story_table = U.flatten_story_table  parameter handler error weakly_story_table in
                  error,weakly_story_table
		end
	      else
		error,causal_story_table
            else 
              error,U.empty_story_table ()
          in 
          let n_weak_stories = U.count_stories weakly_story_table in 
          let _ = print_newline () in 
          let n_fail = U.count_faillure weakly_story_table in 
	  let _ = 
            match 
              n_fail 
            with 
            | 0 -> ()
            | 1 -> Format.fprintf logger "@.\t 1 weak compression has failed@."
            | n -> Format.fprintf logger "@.\t %i weak compressions have failed@." n_fail 
          in
          let error,strong_story_table = 
            if strong_compression_on 
            then 
              begin 
                let parameter = D.S.PH.B.PB.CI.Po.K.H.set_compression_strong parameter in 
                let () = Format.fprintf logger "\t - strong flow compression (%i)@." n_weak_stories in
		let strong_story_table = U.empty_story_table_with_progress_bar logger n_weak_stories in 
                let (error,strong_story_table,log_info) = 
                  List.fold_left 
                    (fun (error,strong_story_table,log_info) (_,a) -> 
		      List.fold_left 
                        (fun (error,strong_story_table,log_info) (_,_,_,refined_event_list,list_info) -> 
                         let error,log_info,list = U.compress logger parameter handler error log_info refined_event_list in
			  let error,strong_story_table,log_info = 
                            match 
                              list
                            with 
                              | [] -> 
                                error,U.inc_faillure strong_story_table,log_info
                              | _ -> 
				 List.fold_left
				   (fun (error,strong_story_table,log_info) list -> 
				     let strong_event_list = D.S.translate_result list in 
				     let strong_event_list = D.S.PH.B.PB.CI.Po.K.clean_events strong_event_list in 
				     let list_info = List.map (Mods.update_profiling_info (D.S.PH.B.PB.CI.Po.K.P.copy log_info)) list_info in  
				     U.store_trace_while_rebuilding_side_effects_with_progress_bar parameter handler error list_info log_info list strong_event_list strong_story_table) (* TO DO, there will be one tick per story altough there should be one tick per list of stories *)
				   (error,strong_story_table,log_info)
				   list 
			  in 
	             
			  error,strong_story_table,log_info)
			(error,strong_story_table,log_info) 
			a)
			(error,strong_story_table,log_info)
		        (List.rev (U.get_stories weakly_story_table))
                in 
	        U.flatten_story_table parameter handler error strong_story_table 
	      end
            else 
              error,U.empty_story_table ()
          in 
	  let _ = 
            match 
              U.count_faillure strong_story_table 
            with 
            | 0 -> ()
            | 1 -> Format.fprintf logger "@.\t 1 strong compression has failed"
            | n -> Format.fprintf logger "@.\t %i strong compressions have failed" n
          in 
	  let _ =
            List.iter 
              (D.S.PH.B.PB.CI.Po.K.H.dump_error parameter handler error)
              error
          in 
          U.get_stories causal_table,
	  U.get_stories causal_story_table,
	  U.get_stories weakly_story_table,
	  U.get_stories strong_story_table 
        end 
  in 
  let () =
    if causal_trace_on then
      Causal.pretty_print logger env Graph_closure.config_std "" ""
			  (D.sort_list causal) in
  let () =
    if weak_compression_on then
      Causal.pretty_print logger env Graph_closure.config_std "Weakly" "weakly "
	(D.sort_list weak) in
  if strong_compression_on then
    Causal.pretty_print logger env Graph_closure.config_std "Strongly" "strongly "
      (D.sort_list strong)
