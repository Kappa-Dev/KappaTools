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
  * Last modification: 01/08/2013
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module D = Dag.Dag 

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
  let causal,weak,strong = 
    if (not causal_trace_on)
      && (not weak_compression_on)  
      && (not strong_compression_on)
    then 
      [],[],[]
    else
      let _ = print_newline () in 
      begin (* causal compression *)
        let parameter =  D.S.PH.B.PB.CI.Po.K.H.set_compression_weak parameter in 
        if D.S.PH.B.PB.CI.Po.K.no_obs_found step_list 
        then 
          let _ = Debug.tag "+ No causal flow found" in 
          [],[],[]
        else 
          begin
            let _ = 
              if (weak_compression_on or strong_compression_on)
            then 
                let _ = Debug.tag "+ Producing causal compressions" in ()
              else 
                let _ = Debug.tag "+ Producing causal traces" in ()
            in 
          let _ = Debug.tag "\t - blackboard generation" in 
          let error = D.S.PH.B.PB.CI.Po.K.H.error_init in 
          let step_list = D.S.PH.B.PB.CI.Po.K.disambiguate step_list handler in 
          let _ = 
            if log_step
            then 
              Debug.tag "\t\t * refining events" 
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
            if Parameter.do_global_cut 
            then 
              begin 
                let _ = 
                  if log_step
                  then 
                    Debug.tag "\t\t * cutting concurrent events" 
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
          let refined_event_list_without_pseudo_inverse,int_pseudo_inverse = 
            if Parameter.cut_pseudo_inverse_event
            then 
              begin 
                let _ = 
                  if log_step
                  then 
                    Debug.tag "\t\t * detecting pseudo inverse events" 
                in 
                let error,refined_event_list_without_pseudo_inverse,int_pseudo_inverse = D.S.PH.B.PB.CI.cut parameter handler error refined_event_list_cut  in 
                let _ = 
                  if debug_mode
                  then 
                    let _ = 
                      List.iter 
                        (fun x -> 
                          let _ = D.S.PH.B.PB.CI.Po.K.print_refined_step parameter handler error x in ()) 
                        refined_event_list_without_pseudo_inverse
                    in flush parameter.D.S.PH.B.PB.CI.Po.K.H.out_channel_err
                in 
                refined_event_list_without_pseudo_inverse,int_pseudo_inverse 
              end
            else 
              refined_event_list_cut,0 
          in 
          let error,log_info,blackboard = D.S.PH.B.import parameter handler error log_info refined_event_list_without_pseudo_inverse in 
          let _ = 
            if log_step 
            then 
              Debug.tag "\t\t * blackboard generation" 
          in 
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
              D.S.PH.B.print_blackboard parameter handler error blackboard 
            else 
              error 
          in  
          let error,list = D.S.PH.forced_events parameter handler error blackboard in 
          let n_stories = List.length list in 
          let _ = Debug.tag ("\t - Causal flow computation ("^(string_of_int n_stories)^")") in 
          let tick = 
            if n_stories > 0 
            then Mods.tick_stories n_stories (false,0,0) 
            else (false,0,0)
          in 
          let error,_,_,causal_story_array = 
            List.fold_left 
              (fun (error,counter,tick,causal_story_array) (list_order,list_eid,info) -> 
                let _ = 
                  if debug_mode
                  then 
                    Debug.tag ("\t\t * causal compression "^(string_of_int (List.length list_eid)))
                in 
                let log_info = D.S.PH.B.PB.CI.Po.K.P.set_start_compression log_info in 
                let error,log_info,event_id_list = D.S.detect_independent_events parameter handler error log_info blackboard list_eid in 
                let error,event_list,result_wo_compression = D.S.translate parameter handler error blackboard event_id_list in 
                let grid = D.S.PH.B.PB.CI.Po.K.build_grid result_wo_compression true handler in
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
                error,counter+1,tick,causal_story_array)
              (error,1,tick,[]) 
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
          let _ = Priority.n_story := 1 in 
          let _ = print_newline () in 
          let _ = print_newline () in 
          let error,weakly_compression_faillure,weakly_compressed_story_array = 
            if weak_compression_on or strong_compression_on 
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
                                if weak_compression_on
                                then 
                                  let weak_event_list = D.S.translate_result list in 
                                  let error,weak_event_list = D.S.PH.B.PB.CI.Po.K.clean_events parameter handler error weak_event_list in 
                                  let grid = D.S.PH.B.PB.CI.Po.K.build_grid list false handler in
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
                                let grid = D.S.PH.B.PB.CI.Po.K.build_grid list false handler in
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
          causal_story_array,weakly_compressed_story_array,strongly_compressed_story_array
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
