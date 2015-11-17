(**
  * compression_main.ml 
  *
  * Causal flow compression: a module for KaSim 
  * Jerome Feret, projet Antique, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Universite Paris-Diderot, CNRS 
  *  
  * Creation: 19/10/2011
  * Last modification: 10/11/2015
  * * 
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

let dummy_weak = false

let compress_and_print logger env log_info step_list =
  let parameter = D.S.PH.B.PB.CI.Po.K.H.build_parameter () in
  let parameter =
    if get_all_stories
    then D.S.PH.B.PB.CI.Po.K.H.set_all_stories_per_obs parameter 
    else parameter 
  in 
  let mode = parameter.D.S.PH.B.PB.CI.Po.K.H.compression_mode in
  let causal_trace_on = Parameter.get_causal_trace mode in
  let weak_compression_on = Parameter.get_weak_compression mode in
  let strong_compression_on = Parameter.get_strong_compression mode in
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
          let error = U.error_init in
          let refined_event_list = U.disambiguate (U.split_init step_list) in
          let () = if log_step then Debug.tag logger"\t - refining events" in
	  let refined_event_list_wo_siphon =
	    if Graph_closure.ignore_flow_from_outgoing_siphon
	    then
	      let () =
		if log_step then Debug.tag logger"\t - detecting siphons"
	      in
	      U.disambiguate (U.fill_siphon refined_event_list) 
            else refined_event_list
	  in
          let () =
            if debug_mode then
	      U.print_trace parameter handler refined_event_list_wo_siphon
	  in
	  let refined_event_list_cut,int =
            if (weak_compression_on || strong_compression_on)
	       && Parameter.do_global_cut then
              let () =
                if log_step then
                  Debug.tag logger "\t - cutting concurrent events" in
              let error,(refined_event_list_cut,int) =
		U.cut parameter handler error refined_event_list_wo_siphon in
	      let () =
		if debug_mode then
		  U.print_trace parameter handler refined_event_list_cut in
              refined_event_list_cut,int
            else refined_event_list_wo_siphon,0
          in
          
          let deal_with error cut log_info = 
            let refined_event_list_without_pseudo_inverse,int_pseudo_inverse = 
              if cut && Parameter.cut_pseudo_inverse_event 
              then 
                begin 
                  let _ = 
                    if log_step
                    then 
                      Debug.tag logger "\t - detecting pseudo inverse events" 
                  in 
                  let error,(refined_event_list_without_pseudo_inverse,int_pseudo_inverse)  = U.remove_pseudo_inverse_events parameter handler error refined_event_list_cut  in
		  let () =
		    if debug_mode then
		      U.print_trace parameter handler refined_event_list_without_pseudo_inverse
		  in
                  refined_event_list_without_pseudo_inverse,int_pseudo_inverse 
                end
              else 
                refined_event_list,0				      
            in 
            let () = 
              if log_step 
              then 
                Debug.tag logger "\t - blackboard generation"
            in 
            let error,log_info,blackboard = U.convert_trace_into_musical_notation parameter handler error log_info refined_event_list_without_pseudo_inverse in 
            let log_info = U.D.S.PH.B.PB.CI.Po.K.P.set_global_cut int log_info in 
            let log_info = U.D.S.PH.B.PB.CI.Po.K.P.set_pseudo_inv int_pseudo_inverse log_info in 
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
	    (* Partial order reduction: generation of uncompressed stories *)
	    let error,_,_,_,causal_story_array,causal_story_faillure = 
              let () = 
                if debug_mode
                then 
                  Debug.tag logger "\t\t * causal compression "
              in 
              let log_info = U.D.S.PH.B.PB.CI.Po.K.P.set_start_compression log_info in 

	      (* We use the grid to get the causal precedence (pred* ) of each observable *)
	      let grid = U.convert_trace_into_grid_while_trusting_side_effects refined_event_list_without_pseudo_inverse handler in
	      let config_init = 
                if cut
                then 
                  Graph_closure.config_init
                else 
                  Graph_closure.config_std 
              in 
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
	      
	      let tick = 
                if n_stories > 0 
                then Mods.tick_stories logger n_stories (false,0,0) 
                else (false,0,0)
              in
	      let blackboard_cflow = blackboard in 
              List.fold_left 
                (fun (error,counter,tick,blackboard,causal_story_array,causal_story_faillure) observable_id
 -> 
                  let () = 
                    if debug_mode
                    then 
                      Debug.tag logger "\t\t * causal compression "
                  in 
		  let error,trace_before_compression = U.causal_prefix_of_an_observable_hit "compression_main, line 2014" parameter handler error log_info blackboard enriched_grid observable_id in 
                  let error,(trace_without_pseudo_inverse_events,int_pseudo_inverse)  = 
                    if cut 
                    then 
                      U.remove_pseudo_inverse_events  parameter handler error trace_before_compression
                    else 
                      error,(trace_before_compression,0)
                  in 
                  let error,log_info,blackboard_cflow = U.convert_trace_into_musical_notation parameter handler error log_info trace_without_pseudo_inverse_events in 
                  let error,observable_hit = U.extract_observable_hit_from_musical_notation "compression_main.ml, line 214, " parameter handler error blackboard_cflow in 
		  let grid = U.convert_trace_into_grid_while_trusting_side_effects trace_without_pseudo_inverse_events  handler in 
	      	  let enriched_grid = U.enrich_small_grid_with_transitive_closure logger grid in 
		  let eid = 
                    match U.get_event_list_from_observable_hit observable_hit 
		    with 
		    | [a] -> a 
                    | _ -> failwith "wrong list, compression_main, 325" 
                  in 

                  let refined_list = 
                    if cut && Parameter.do_detect_separable_components 
                    then 
                      (List.rev_map (fun x -> (x,[],(*bool*) dummy_weak)) (List.rev trace_without_pseudo_inverse_events))
                    else 
                      (List.rev_map (fun x -> (x,[],dummy_weak)) (List.rev trace_without_pseudo_inverse_events))
                  in 
		  let grid = D.S.PH.B.PB.CI.Po.K.build_grid refined_list true handler in
                  let enriched_grid = Causal.enrich_grid logger Graph_closure.config_intermediary grid in 
                  let event_id_list_rev = ((eid+1)::(enriched_grid.Causal.prec_star.(eid+1))) in 
                  let event_id_list = List.rev_map pred (event_id_list_rev) in 
		  let info = 
                    match U.get_runtime_info_from_observable_hit observable_hit 
                    with 
                    | None -> None 
                    | Some info -> 
		       let info = 
                         {info with Mods.story_id = counter }
		       in 
		       let info = Mods.update_profiling_info (D.S.PH.B.PB.CI.Po.K.P.copy log_info)  info 
		       in 
                       Some info
                  in
		  let list_order = U.get_list_order observable_hit in 
		  if
		    store_uncompressed_stories || not cut
		  then
		    let error,event_list,result_wo_compression = D.S.translate parameter handler error blackboard_cflow event_id_list in 
                    let grid = D.S.PH.B.PB.CI.Po.K.build_grid (List.rev_map (fun (x,y) -> x,y,dummy_weak) (List.rev result_wo_compression)) true handler in
                    let error,graph = D.graph_of_grid parameter handler error grid in
                    let error,prehash = D.prehash parameter handler error graph in
		    let tick = Mods.tick_stories logger n_stories tick in
                    let causal_story_array =
		      (prehash,[grid,graph,None,event_list,[info]])::causal_story_array in 
                    error,counter+1,tick,blackboard,causal_story_array,causal_story_faillure
		  else
		    U.from_none_to_weak_with_tick  parameter handler log_info logger n_stories
		      (error,counter,tick,blackboard,causal_story_array,causal_story_faillure)
		      (trace_before_compression,[info])
		)
                (error,1,tick,blackboard,[],0) 
                (List.rev list)
            in 
	    let error,causal_story_array = 
              D.hash_list parameter handler error 
                (List.rev causal_story_array) 
            in 
            let n_stories = 
              List.fold_left 
                (fun n l -> n + List.length (snd l))
                0 
                causal_story_array
            in 
            error,log_info,causal_story_array,causal_story_faillure,n_stories,blackboard
          in

	  let error,log_info,causal,_,_,_ = 
            if causal_trace_on 
            then 
              deal_with error false log_info 
            else 
              let error,log_info,blackboard = D.S.PH.B.import parameter handler error log_info [] in 
              error,log_info,[],0,0,blackboard 
          in 
          let error,log_info,causal_story_array,causal_story_faillure,n_stories,blackboard = 
            if weak_compression_on || strong_compression_on 
            then 
              deal_with error true log_info 
            else
               let error,log_info,blackboard = D.S.PH.B.import parameter handler error log_info [] in 
              error,log_info,[],0,0,blackboard 
          in 
          let _ = Priority.n_story := 1 in 
          let _ = print_newline () in 
          let _ = print_newline () in 
          let error,weakly_compression_faillure,weakly_compressed_story_array = 
            if weak_compression_on || strong_compression_on 
            then 
              if
		store_uncompressed_stories
	      then
		begin 
                  let () = Format.fprintf logger "\t - weak flow compression (%i)@." n_stories in
                  let parameter = D.S.PH.B.PB.CI.Po.K.H.set_compression_weak parameter in 
                  let tick = 
                    if n_stories > 0 
                    then Mods.tick_stories logger n_stories (false,0,0) 
                    else (false,0,0)
                  in 
                  let error,_,_,_,weakly_compressed_story_array,weakly_compression_faillure = 
                    List.fold_left 
                      (fun x (_,a) ->
                       List.fold_left 
                         (U.from_none_to_weak_with_tick_ext parameter handler log_info logger n_stories)
			 x
			 a)
                    (error,1,tick,blackboard,[],0) 
                    (List.rev causal_story_array)
                  in 
	          let error,weakly_compressed_story_array = D.hash_list parameter handler error   (List.rev weakly_compressed_story_array) in
                  error,weakly_compression_faillure,weakly_compressed_story_array
		end
	      else
		error,causal_story_faillure,causal_story_array
            else 
              error,0,[]
          in 
          let n_stories = 
            List.fold_left 
              (fun n l -> n + List.length (snd l))
              0 
              weakly_compressed_story_array
          in 
          let _ = print_newline () in 
          let _ = 
            match 
              weakly_compression_faillure 
            with 
            | 0 -> ()
            | 1 -> Format.fprintf logger "@.\t 1 weak compression has failed@."
            | _ -> Format.fprintf logger "@.\t %i weak compressions have failed@."
				  weakly_compression_faillure
          in
          let error,strong_compression_faillure,strongly_compressed_story_array = 
            if strong_compression_on 
            then 
              begin 
                let parameter = D.S.PH.B.PB.CI.Po.K.H.set_compression_strong parameter in 
                let () = Format.fprintf logger "\t - strong flow compression (%i)@." n_stories in
                let tick = 
                  if n_stories > 0 
                  then Mods.tick_stories logger n_stories (false,0,0)
                  else (false,0,0)
                in 
                let error,_,_,_,strong_compression_faillure,strongly_compressed_story_array = 
                  List.fold_left 
                    (fun (error,counter,tick,blackboard,strong_compression_faillure,strongly_compressed_story_array) (_,a) ->
                      List.fold_left 
                        (fun (error,counter,tick,blackboard,strong_compression_faillure,strongly_compressed_story_array) (_,grid,graph,refined_event_list,list_info) -> 
                          let info = List.hd list_info in 
                          let error,log_info,blackboard_tmp = D.S.PH.B.import parameter handler error log_info refined_event_list in 
                          let error,list = D.S.PH.forced_events parameter handler error blackboard_tmp in     
                          let list_order = 
                            match list 
                            with 
                            | [] -> []
                            | (list_order,_,_)::_ -> list_order
                          in 
                          let error,log_info,blackboard_tmp,output,list = 
                            D.S.compress parameter handler error log_info blackboard_tmp list_order 
                          in
                          let log_info = D.S.PH.B.PB.CI.Po.K.P.set_story_research_time log_info in 
                          let error = 
                            if debug_mode
                            then 
                              let _ =  Debug.tag logger "\t\t * result"  in
                              let _ =
                                if D.S.PH.B.is_failed output 
                                then 
                                  let _ = Format.fprintf parameter.D.S.PH.B.PB.CI.Po.K.H.out_channel_err "Fail_to_compress" in  error
                                else 
                                  let _ = Format.fprintf parameter.D.S.PH.B.PB.CI.Po.K.H.out_channel_err "Succeed_to_compress" in 
                                  error
                              in 
                              error 
                            else 
                              error
                          in 
                          let error,strong_compression_faillure,strongly_compressed_story_array,info = 
                            match 
                              list
                            with 
                              | [] -> 
                                error,strong_compression_faillure+1,strongly_compressed_story_array,None
                              | _ -> 
				 List.fold_left
				   (fun (error,strong_compression_faillure,strongly_compressed_story_array,info) list -> 
				    let grid = D.S.PH.B.PB.CI.Po.K.build_grid (List.rev_map (fun (x,y) -> x,y,dummy_weak) (List.rev list)) false handler in
				    let log_info  = D.S.PH.B.PB.CI.Po.K.P.set_grid_generation  log_info in 
				    let error,graph = D.graph_of_grid parameter handler error grid in 
				    let error,prehash = D.prehash parameter handler error graph in 
				    let log_info = D.S.PH.B.PB.CI.Po.K.P.set_canonicalisation log_info in 
				    let info = 
				      match info 
				      with 
				      | None -> None 
				      | Some info -> 
					     let info = 
					       {info with Mods.story_id = counter }
                                             in 
					     let info = Mods.update_profiling_info (D.S.PH.B.PB.CI.Po.K.P.copy log_info)  info 
					     in 
					     Some info
				    in 
				    error,strong_compression_faillure,(prehash,[grid,graph,None,[](* TO DO PROVIDE TRACE, IN CASE OF FURTHER COMPRESSION *),list_info])::strongly_compressed_story_array,info)
				   (error,strong_compression_faillure,strongly_compressed_story_array,info)
				   list 
			  in 
                          let error,log_info,blackboard = D.S.PH.B.reset_init parameter handler error log_info blackboard in 
                          let tick = Mods.tick_stories logger n_stories tick in
                          error,counter+1,tick,blackboard,strong_compression_faillure,strongly_compressed_story_array)
                        (error,counter,tick,blackboard,strong_compression_faillure,strongly_compressed_story_array) a) 
                    (error,1,tick,blackboard,0,[]) 
                    (List.rev weakly_compressed_story_array)
                in 
	        let error,strongly_compressed_story_array = D.hash_list parameter handler error   (List.rev strongly_compressed_story_array) in
                error,strong_compression_faillure,strongly_compressed_story_array 
              end
            else 
              error,0,[]
          in 
          let _ = 
            match 
              strong_compression_faillure 
            with 
            | 0 -> ()
            | 1 -> Format.fprintf logger "@.\t 1 strong compression has failed"
            | _ -> Format.fprintf logger "@.\t %i strong compressions have failed"
				  strong_compression_faillure
          in 
            
          let _ = 
            List.iter 
              (D.S.PH.B.PB.CI.Po.K.H.dump_error parameter handler error)
              error
          in 
          causal,causal_story_array,weakly_compressed_story_array,strongly_compressed_story_array
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
