(**
  * compression_main.ml 
  *
  * Causal flow compression: a module for KaSim 
  * Jerome Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Universite Paris-Diderot, CNRS 
  *  
  * Creation: 19/10/2011
  * Last modification: 21/11/2013
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011,2012,2013 Institut National de Recherche en Informatique 
  * et en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module D = Dag.Dag 

let old_version = false
let log_step = true
let debug_mode = false
let dump_profiling_info = false

let th_of_int n = 
  match n mod 10  
  with 
    | 1 -> (string_of_int n)^"st"
    | 2 -> (string_of_int n)^"nd"
    | 3 -> (string_of_int n)^"rd"
    | _ -> (string_of_int n)^"th"

let dummy_weak = false

let compress env state log_info step_list =  
  let parameter = D.S.PH.B.PB.CI.Po.K.H.build_parameter () in 
  let mode = parameter.D.S.PH.B.PB.CI.Po.K.H.compression_mode in 
  let causal_trace_on = Parameter.get_causal_trace mode in 
  let weak_compression_on = Parameter.get_weak_compression mode in 
  let strong_compression_on = Parameter.get_strong_compression mode in 
  let handler = 
    {
      D.S.PH.B.PB.CI.Po.K.H.env = env ;
      D.S.PH.B.PB.CI.Po.K.H.state = state 
    }
  in 
  let causal,trivial,weak,strong = 
    if (not causal_trace_on)
      && (not weak_compression_on)  
      && (not strong_compression_on)
    then 
      [],[],[],[]
    else
      let _ = print_newline () in 
      begin (* causal compression *)
        let parameter = D.S.PH.B.PB.CI.Po.K.H.set_compression_none parameter in 
        if D.S.PH.B.PB.CI.Po.K.no_obs_found step_list 
        then 
          let _ = Debug.tag "+ No causal flow found" in 
          [],[],[],[]
        else 
          begin
            let _ = 
              if (weak_compression_on || strong_compression_on)
            then 
                let _ = Debug.tag "+ Producing causal compressions" in ()
              else 
                let _ = Debug.tag "+ Producing causal traces" in ()
            in 
          let error = D.S.PH.B.PB.CI.Po.K.H.error_init in 
          let step_list = D.S.PH.B.PB.CI.Po.K.disambiguate step_list handler in 
          let _ = 
            if log_step
            then 
              Debug.tag "\t - refining events" 
          in 
          let refined_event_list = 
            List.rev_map 
              (fun x -> 
                snd (D.S.PH.B.PB.CI.Po.K.refine_step parameter handler error x))
              step_list 
          in       
          let _ = 
            if debug_mode
            then 
              let _ = 
                List.iter 
                  (fun x -> let _ = D.S.PH.B.PB.CI.Po.K.print_refined_step parameter handler error x in ()) 
                  refined_event_list  
              in flush parameter.D.S.PH.B.PB.CI.Po.K.H.out_channel
          in 
          
          let refined_event_list_cut,int = 
            if false && Parameter.do_global_cut 
            then 
              begin 
                let _ = 
                  if log_step
                  then 
                    Debug.tag "\t - cutting concurrent events" 
                in 
                let error,(refined_event_list_cut,int) = D.S.PH.B.PB.CI.Po.cut parameter handler error refined_event_list in 
                let _ = 
                  if debug_mode
                  then 
                    let _ = 
                      List.iter 
                        (fun x -> 
                          let _ = D.S.PH.B.PB.CI.Po.K.print_refined_step parameter handler error x in ()) 
                        refined_event_list_cut  
                    in flush parameter.D.S.PH.B.PB.CI.Po.K.H.out_channel_err
                in 
                refined_event_list_cut,int 
              end
            else 
              refined_event_list,0 
          in 
          
          let deal_with error cut  log_info = 
            let _ = 
              if log_step 
              then 
                if cut
                then 
                  Debug.tag "\t - Causal flow (with trivial simplifications) computation"
                else
                  Debug.tag "\t - Causal flow (without simplification) computation" 
            in 
            let refined_event_list_without_pseudo_inverse,int_pseudo_inverse = 
              if cut && Parameter.cut_pseudo_inverse_event 
              then 
                begin 
                  let _ = 
                    if log_step
                    then 
                      Debug.tag "\t\t * detecting pseudo inverse events" 
                  in 
                  let error,refined_event_list_without_pseudo_inverse,int_pseudo_inverse  = D.S.PH.B.PB.CI.cut parameter handler error refined_event_list_cut  in 
                  let _ = 
                    if debug_mode
                    then 
                      let _ = 
                        List.iter 
                          (fun (x,bool) -> 
                            let _ = D.S.PH.B.PB.CI.Po.K.print_refined_step parameter handler error x in 
                            let _ = 
                              if bool 
                              then 
                                let _ = Printf.fprintf stderr "Weak event \n" in
                                let _ = Printf.fprintf stderr "\n" in 
                                ()
                            in 
                            ()) 
                          refined_event_list_without_pseudo_inverse
                      in flush parameter.D.S.PH.B.PB.CI.Po.K.H.out_channel_err
                  in 
                  refined_event_list_without_pseudo_inverse,int_pseudo_inverse 
                end
              else 
                List.rev_map (fun x -> x,dummy_weak) (List.rev refined_event_list_cut),0 
            in 
            let _ = 
              if log_step 
              then 
                Debug.tag "\t\t * blackboard generation"
            in 
            let error,log_info,blackboard = D.S.PH.B.import parameter handler error log_info (List.rev_map (fun (x,_)->x) (List.rev refined_event_list_without_pseudo_inverse)) in 
           
            let log_info = D.S.PH.B.PB.CI.Po.K.P.set_global_cut int log_info in 
            let log_info = D.S.PH.B.PB.CI.Po.K.P.set_pseudo_inv int_pseudo_inverse log_info in 
            let _ = 
              if debug_mode && log_step  
              then 
                Debug.tag "\t\t * pretty printing the grid"
            in 
            let error = 
              if debug_mode 
              then 
                let error = D.S.PH.B.export_blackboard_to_xls parameter handler error "a" 0 0 blackboard in 
                let error = D.S.PH.B.print_blackboard parameter handler error blackboard in error
              else 
                error 
            in  
            let error,list = D.S.PH.forced_events parameter handler error blackboard in 
            let n_stories = List.length list in 
            let _ = Debug.tag ("\t\t * Causal flow computation ("^(string_of_int n_stories)^")") in           
            let error,_,_,causal_story_array = 
              let _ = 
                if debug_mode
                then 
                  Debug.tag "\t\t * causal compression "
              in 
              let log_info = D.S.PH.B.PB.CI.Po.K.P.set_start_compression log_info in 
              let refined_list = 
                if cut && Parameter.do_detect_separable_components 
                then 
                  (List.rev_map (fun (x,bool) -> (x,D.S.PH.B.PB.CI.Po.K.empty_side_effect,bool)) (List.rev refined_event_list_without_pseudo_inverse))
                else 
                  (List.rev_map (fun (x,_) -> (x,D.S.PH.B.PB.CI.Po.K.empty_side_effect,dummy_weak)) (List.rev refined_event_list_without_pseudo_inverse))
              in 
              let grid = D.S.PH.B.PB.CI.Po.K.build_grid refined_list true handler in
              let config_init = 
                if cut
                then 
                  Graph_closure.config_init
                else 
                  Graph_closure.config_std 
              in 
              let enriched_grid = Causal.enrich_grid config_init grid in 
              let _ = 
                if Parameter.log_number_of_causal_flows
                then 
                  Causal.print_stat parameter handler enriched_grid 
              in 
              let tick = 
                if n_stories > 0 
                then Mods.tick_stories n_stories (false,0,0) 
                else (false,0,0)
              in 
              List.fold_left 
                (fun (error,counter,tick,causal_story_array) (list_order,list_eid,info) -> 
                  let _ = 
                    if debug_mode
                    then 
                      Debug.tag "\t\t * causal compression "
                  in 
                  let eid = 
                    match list_eid with [a] -> a 
                    | _ -> raise Exit
                  in 
                  let log_info = D.S.PH.B.PB.CI.Po.K.P.set_start_compression log_info in 
                  let event_id_list_rev = ((eid+1)::(enriched_grid.Causal.prec_star.(eid+1))) in 
                  let event_id_list = List.rev_map pred (event_id_list_rev) in 
                  let error,event_list,result_wo_compression = D.S.translate parameter handler error blackboard event_id_list in 
                  let result_wo_compression = List.rev_map fst (List.rev result_wo_compression) in 
                  let error,refined_event_list_without_pseudo_inverse,int_pseudo_inverse  = 
                    if cut 
                    then 
                      D.S.PH.B.PB.CI.cut parameter handler error result_wo_compression   
                    else 
                      D.S.PH.B.PB.CI.do_not_cut parameter handler error result_wo_compression 
                  in 
                  
                  let error,log_info,blackboard = D.S.PH.B.import parameter handler error log_info (List.rev_map (fun (x,_)->x) (List.rev refined_event_list_without_pseudo_inverse)) in 
                  let error,list = D.S.PH.forced_events parameter handler error blackboard in 
                  let list_order,list_eid,info = 
                    match list with [a,b,c] -> a,b,c
                    | _ -> raise Exit 
                  in 
                  let eid = 
                    match list_eid with [a] -> a 
                    | _ -> raise Exit 
                  in 
                  let refined_list = 
                    if cut && Parameter.do_detect_separable_components 
                    then 
                      (List.rev_map (fun (x,bool) -> (x,D.S.PH.B.PB.CI.Po.K.empty_side_effect,bool)) (List.rev refined_event_list_without_pseudo_inverse))
                    else 
                      (List.rev_map (fun (x,_) -> (x,D.S.PH.B.PB.CI.Po.K.empty_side_effect,dummy_weak)) (List.rev refined_event_list_without_pseudo_inverse))
                  in 
                  let grid = D.S.PH.B.PB.CI.Po.K.build_grid refined_list true handler in
                  let enriched_grid = Causal.enrich_grid Graph_closure.config_intermediary grid in 
                  let event_id_list_rev = ((eid+1)::(enriched_grid.Causal.prec_star.(eid+1))) in 
                  let event_id_list = List.rev_map pred (event_id_list_rev) in 
                  let error,event_list,result_wo_compression = D.S.translate parameter handler error blackboard event_id_list in 
                  let grid = D.S.PH.B.PB.CI.Po.K.build_grid (List.rev_map (fun (x,y) -> x,y,dummy_weak) (List.rev result_wo_compression)) true handler in
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
                  let tick = Mods.tick_stories n_stories tick in 
                  let causal_story_array = (prehash,[grid,graph,None,(event_id_list,list_order,event_list),[],[info]])::causal_story_array in 
                  error,counter+1,tick,causal_story_array
                )
                (error,1,tick,[]) 
                (List.rev list)
            in 
            let _ = 
              if log_step 
              then 
                Debug.tag "\n" 
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
            error,log_info,causal_story_array,n_stories,blackboard
          in
          let error,log_info,causal,_,_ = 
            if causal_trace_on 
            then 
              deal_with error false log_info 
            else 
              let error,log_info,blackboard = D.S.PH.B.import parameter handler error log_info [] in 
              error,log_info,[],0,blackboard 
          in 
          let error,log_info,causal_story_array,n_stories,blackboard = 
            if weak_compression_on || strong_compression_on 
            then 
              deal_with error true log_info 
            else
               let error,log_info,blackboard = D.S.PH.B.import parameter handler error log_info [] in 
              error,log_info,[],0,blackboard 
          in 
          let _ = Priority.n_story := 1 in 
          let _ = print_newline () in 
          let _ = print_newline () in 
          let error,weakly_compression_faillure,weakly_compressed_story_array = 
            if weak_compression_on || strong_compression_on 
            then 
              begin 
                let _ = Debug.tag ("\t - Weak flow compression ("^(string_of_int n_stories)^")") in 
                let parameter = D.S.PH.B.PB.CI.Po.K.H.set_compression_weak parameter in 
                let tick = 
                  if n_stories > 0 
                  then Mods.tick_stories n_stories (false,0,0) 
                  else (false,0,0)
                in 
                let error,_,_,_,weakly_compressed_story_array,weakly_compression_faillure = 
                  List.fold_left 
                    (fun (error,counter,tick,blackboard,weakly_compressed_story_array,weakly_compression_faillure) (_,a) ->
                      List.fold_left 
                        (fun (error,counter,tick,blackboard,weakly_compressed_story_array,weakly_compression_faillure) (_,grid,graph,(event_id_list,list_order,event_list),step_list,list_info) -> 
                          let info = List.hd list_info in 
                          let error,log_info,blackboard_tmp,list_order = 
                            let error,log_info,blackboard_tmp = D.S.sub parameter handler error log_info blackboard event_list in 
                            let error,list = D.S.PH.forced_events parameter handler error blackboard_tmp in 
                            let list_order = 
                              match list 
                              with 
                              | (list_order,_,_)::q -> list_order 
                              | _ -> [] 
                            in 
                            error,log_info,blackboard_tmp,list_order
                          in 
                          let error,log_info,blackboard_tmp,output,list = 
                            D.S.compress parameter handler error log_info blackboard_tmp list_order 
                          in
                          let log_info = D.S.PH.B.PB.CI.Po.K.P.set_story_research_time log_info in 
                          let error = 
                            if debug_mode
                            then 
                              let _ =  Debug.tag "\t\t * result"  in 
                              let _ =
                                if D.S.PH.B.is_failed output 
                                then 
                                  let _ = Printf.fprintf parameter.D.S.PH.B.PB.CI.Po.K.H.out_channel_err "Fail_to_compress" in  error
                                else 
                                  let _ = Printf.fprintf parameter.D.S.PH.B.PB.CI.Po.K.H.out_channel_err "Succeed_to_compress" in 
                                  error
                              in 
                              error 
                            else 
                              error
                          in 
                          let error,weakly_compressed_story_array,weakly_compression_faillure,info = 
                            match 
                              list
                            with 
                              | None -> 
                                error,weakly_compressed_story_array,weakly_compression_faillure+1,None
                              | Some list -> 
                                if weak_compression_on || strong_compression_on 
                                then 
                                  let weak_event_list = D.S.translate_result list in 
                                  let error,weak_event_list = D.S.PH.B.PB.CI.Po.K.clean_events parameter handler error weak_event_list in 
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
                                  error,(prehash,[grid,graph,None,(event_id_list,list_order,event_list),weak_event_list,list_info])::weakly_compressed_story_array,weakly_compression_faillure,info
                                else 
                                  error,weakly_compressed_story_array,weakly_compression_faillure,None
                          in 
                          let error,log_info,blackboard = D.S.PH.B.reset_init parameter handler error log_info blackboard in 
                          let tick = Mods.tick_stories n_stories tick in 
                          error,counter+1,tick,blackboard,weakly_compressed_story_array,weakly_compression_faillure)
                        (error,counter,tick,blackboard,weakly_compressed_story_array,weakly_compression_faillure) a) 
                    (error,1,tick,blackboard,[],0) 
                    (List.rev causal_story_array)
                in 
	        let error,weakly_compressed_story_array = D.hash_list parameter handler error   (List.rev weakly_compressed_story_array) in
                error,weakly_compression_faillure,weakly_compressed_story_array 
              end
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
          let _ = print_newline () in 
          let _ = 
            match 
              weakly_compression_faillure 
            with 
            | 0 -> ()
            | 1 -> Debug.tag "\n\t 1 weak compression has failed" 
            | _ -> Debug.tag ("\n\t "^(string_of_int weakly_compression_faillure)^" weak compressions have failed")
          in 
          let error,strong_compression_faillure,strongly_compressed_story_array = 
            if strong_compression_on 
            then 
              begin 
                let parameter = D.S.PH.B.PB.CI.Po.K.H.set_compression_strong parameter in 
                let _ = Debug.tag ("\t - Strong flow compression ("^(string_of_int n_stories)^")") in 
                let tick = 
                  if n_stories > 0 
                  then Mods.tick_stories n_stories (false,0,0) 
                  else (false,0,0)
                in 
                let error,_,_,_,strong_compression_faillure,strongly_compressed_story_array = 
                  List.fold_left 
                    (fun (error,counter,tick,blackboard,strong_compression_faillure,strongly_compressed_story_array) (_,a) ->
                      List.fold_left 
                        (fun (error,counter,tick,blackboard,strong_compression_faillure,strongly_compressed_story_array) (_,grid,graph,(event_id_list,list_order,event_list),step_list,list_info) -> 
                          let info = List.hd list_info in 
                          let refined_event_list = 
                            List.rev_map (fun x -> snd (D.S.PH.B.PB.CI.Po.K.refine_step parameter handler error x)) step_list 
                          in       
                          let error,log_info,blackboard_tmp = D.S.PH.B.import parameter handler error log_info refined_event_list in 
                          let error,list = D.S.PH.forced_events parameter handler error blackboard_tmp in     
                          let list_order = 
                            match list 
                            with 
                            | [] -> []
                            | (list_order,_,_)::q -> list_order
                          in 
                          let error,log_info,blackboard_tmp,output,list = 
                            D.S.compress parameter handler error log_info blackboard_tmp list_order 
                          in
                          let log_info = D.S.PH.B.PB.CI.Po.K.P.set_story_research_time log_info in 
                          let error = 
                            if debug_mode
                            then 
                              let _ =  Debug.tag "\t\t * result"  in 
                              let _ =
                                if D.S.PH.B.is_failed output 
                                then 
                                  let _ = Printf.fprintf parameter.D.S.PH.B.PB.CI.Po.K.H.out_channel_err "Fail_to_compress" in  error
                                else 
                                  let _ = Printf.fprintf parameter.D.S.PH.B.PB.CI.Po.K.H.out_channel_err "Succeed_to_compress" in 
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
                              | None -> 
                                error,strong_compression_faillure+1,strongly_compressed_story_array,None
                              | Some list -> 
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
                                error,strong_compression_faillure,(prehash,[grid,graph,None,(event_id_list,list_order,event_list),[],list_info])::strongly_compressed_story_array,info
                          in 
                          let error,log_info,blackboard = D.S.PH.B.reset_init parameter handler error log_info blackboard in 
                          let tick = Mods.tick_stories n_stories tick in 
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
            | 1 -> Debug.tag "\n\t 1 strong compression has failed" 
            | _ -> Debug.tag ("\n\t "^(string_of_int strong_compression_faillure)^" strong compressions have failed")
          in 
            
          let _ = 
            List.iter 
              (D.S.PH.B.PB.CI.Po.K.H.dump_error parameter handler error)
              error
          in 
          causal,causal_story_array,weakly_compressed_story_array,strongly_compressed_story_array
        end 
    end 
  in 
  let lift bool x = 
    if bool 
    then Some (D.sort_list x)
    else None
  in 
  lift causal_trace_on causal,
  lift weak_compression_on weak,
  lift strong_compression_on strong 
