(**
  * utilities.ml  
  *
  * Causal flow compression: a module for KaSim 
  * Jerome Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Universite Paris-Diderot, CNRS 
  *  
  * Creation: 10/08/2015
  * Last modification: 10/08/2015
  * * 
  * Some functionalities for story compression
  *  
  * Copyright 2011,2012,2013 Institut National de Recherche en Informatique 
  * et en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let debug_mode = false
let dummy_weak = false
		   
module D=Dag.Dag 
type ('a,'b,'c) remanent =  
  D.S.PH.B.PB.CI.Po.K.H.error_channel * int * (bool * int * int) *
    D.S.PH.B.blackboard *
      (D.prehash *
         (Causal.grid * D.graph * 'a option *
            ('b * D.S.PH.update_order list *
               D.S.PH.B.PB.CI.Po.K.refined_step list) *
              D.S.PH.B.PB.CI.Po.K.step list *
		'c Mods.simulation_info option list)
           list)
        list * int
		 
let from_none_to_weak parameter handler log_info logger (error,counter,tick,blackboard,weakly_compressed_story_array,weakly_compression_faillure) ((event_id_list,list_order,event_list),step_list,list_info) = 
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
  let error,weakly_compressed_story_array,weakly_compression_faillure,info = 
    match 
      list
    with 
    | None -> 
       error,weakly_compressed_story_array,weakly_compression_faillure+1,None
    | Some list -> 
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
  in 
  let error,log_info,blackboard = D.S.PH.B.reset_init parameter handler error log_info blackboard in 
  error,counter,tick,blackboard,weakly_compressed_story_array,weakly_compression_faillure

let from_none_to_weak_with_tick parameter handler log_info logger n_stories x y =
  let error,counter,tick,blackboard,w1,w2 = from_none_to_weak parameter handler log_info logger x y in
  let tick = Mods.tick_stories logger n_stories tick in 
  error,counter+1,tick,blackboard,w1,w2

let from_none_to_weak_with_tick_ext parameter handler log_info logger n_stories x (_,_,_,y,z,t) =
  let error,counter,tick,blackboard,w1,w2 = from_none_to_weak parameter handler log_info logger x (y,z,t) in
  let tick = Mods.tick_stories logger n_stories tick in 
  error,counter+1,tick,blackboard,w1,w2
				       
