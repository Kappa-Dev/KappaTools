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

type error_log =  D.S.PH.B.PB.CI.Po.K.H.error_channel
type parameter =  D.S.PH.B.PB.CI.Po.K.H.parameter
type kappa_handler = D.S.PH.B.PB.CI.Po.K.H.handler 
type profiling_info = D.S.PH.B.PB.CI.Po.K.P.log_info
		       
type refined_trace = D.S.PH.B.PB.CI.Po.K.refined_step list
type refined_trace_with_weak_events = (D.S.PH.B.PB.CI.Po.K.refined_step * bool) list
type step_with_side_effects = D.S.PH.B.PB.CI.Po.K.refined_step * D.S.PH.B.PB.CI.Po.K.side_effect										
type refined_trace_with_side_effect = step_with_side_effects list
type step_id = D.S.PH.B.PB.step_id

		 
type cflow_grid = Causal.grid  
type enriched_cflow_grid = Causal.enriched_grid
type musical_grid =  D.S.PH.B.blackboard 
type transitive_closure_config = Graph_closure.config 
		       
(* dag: internal representation for cflows *)
type dag = D.graph
(* cannonical form for cflows (completely capture isomorphisms) *)
type dag_connonical_form = D.canonical_form
(* prehashform for cflows, if two cflows are isomorphic, they have the same prehash form *) 
type dag_prehash = D.prehash 

(* I need to investigate further, what I know is that:
   for each hash, there is a list of stories having this hash, for each one, we have the grid, the dag, I do not remember the three following components, and then a list of timestamp that indicated when the observables have been hit *) 
type ('a,'b) story_list =
  dag_prehash * (cflow_grid * dag  * 'a option * refined_trace * 'b Mods.simulation_info option list) list
			     
type observable_hit = 
  {
    list_of_actions: D.S.PH.update_order list ;
    list_of_events: step_id  list ; 
    runtime_info:  unit Mods.simulation_info option}

let get_event_list_from_observable_hit a = a.list_of_events 
let get_runtime_info_from_observable_hit a = a.runtime_info 
let get_list_order a = a.list_of_actions 

type ('a,'b) remanent =  error_log * int * (bool * int * int) * D.S.PH.B.blackboard * (('a,'b) story_list) list * int

module Profiling = D.S.PH.B.PB.CI.Po.K.P
		     
let error_init = D.S.PH.B.PB.CI.Po.K.H.error_init
let split_init = D.S.PH.B.PB.CI.Po.K.split_init
let disambiguate = D.S.PH.B.PB.CI.Po.K.disambiguate
let fill_siphon = D.S.PH.B.PB.CI.Po.K.fill_siphon 
let cut = D.S.PH.B.PB.CI.Po.cut

let remove_weak_events_annotation l = 
  List.rev_map fst (List.rev l) 

let remove_pseudo_inverse_events a b c d =
  let a,b,c = D.S.PH.B.PB.CI.cut a b c d in
  a,(remove_weak_events_annotation b,c)
	    
(*let remove_pseudo_inverse_events_and_tag_weak_events a b c d = 
  let a,b,c = D.S.PH.B.PB.CI.cut a b c d in 
  a,(b,c)

let tag_weak_events a b c d = 
  let a,b,c = D.S.PH.B.PB.CI.do_not_cut a b c d in 
  a,b
*)




let extract_observable_hits_from_musical_notation a b c d = 
  let error,l = D.S.PH.forced_events a b c d in 
  error,
  List.rev_map
    (fun (a,b,c) -> 
      {
      list_of_actions = a;
      list_of_events = b ;
      runtime_info = c
      })
    (List.rev l)

let extract_observable_hit_from_musical_notation string a b c d = 
  let error,l = D.S.PH.forced_events a b c d in 
  match l with [a,b,c] -> 
    error,{
      list_of_actions = a;
      list_of_events = b; 
      runtime_info = c}
  | [] -> failwith (string^" no story")
  | _::_ -> failwith (string^" several stories")
      
    
let causal_prefix_of_an_observable_hit string parameter handler error log_info blackboard (enriched_grid:enriched_cflow_grid) observable_id = 
  let eid = 
    match 
      get_event_list_from_observable_hit observable_id  
    with 
    | [a] -> a 
    | [] -> failwith ("no observable in that story"^string)
    | _ -> failwith  ("several observables in that story"^string)
  in 
  let event_id_list_rev = ((eid+1)::(enriched_grid.Causal.prec_star.(eid+1))) in 
  let event_id_list = List.rev_map pred (event_id_list_rev) in 
  let error,list_eid,_ = D.S.translate parameter handler error blackboard event_id_list in 
  error,list_eid 

  

let print_trace parameter handler =
      Format.fprintf
	parameter.D.S.PH.B.PB.CI.Po.K.H.out_channel "@[<v>%a@]@."
	(Pp.list Pp.space (D.S.PH.B.PB.CI.Po.K.print_refined_step ~handler))


let export_musical_grid_to_xls = D.S.PH.B.export_blackboard_to_xls
  
let print_musical_grid = D.S.PH.B.print_blackboard 


let from_none_to_weak parameter handler log_info logger ((error,counter,tick,blackboard,weakly_compressed_story_array,weakly_compression_faillure):(D.canonical_form,D.S.PH.B.PB.CI.Po.K.P.log_info) remanent) ((*(event_id_list,list_order,event_list),*)step_list,list_info) = 
  let info = List.hd list_info in 
  let event_list = step_list in 
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
    | [] -> 
       error,weakly_compressed_story_array,weakly_compression_faillure+1,None
    | _ ->  
       List.fold_left
	 (fun (error,weakly_compressed_story_array,weakly_compression_faillure,info) list -> 
	  let weak_event_list = D.S.translate_result list in 
          let weak_event_list = D.S.PH.B.PB.CI.Po.K.clean_events weak_event_list in 
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
          error,(prehash,[grid,graph,None,weak_event_list,list_info])::weakly_compressed_story_array,weakly_compression_faillure,info)
	 (error,weakly_compressed_story_array,weakly_compression_faillure,info)
	 list 
  in 
  let error,log_info,blackboard = D.S.PH.B.reset_init parameter handler error log_info blackboard in 
  ((error,counter,tick,blackboard,weakly_compressed_story_array,weakly_compression_faillure):(D.canonical_form,D.S.PH.B.PB.CI.Po.K.P.log_info) remanent)

let convert_trace_into_grid_while_trusting_side_effects trace handler = 
  let refined_list = 
    List.rev_map (fun (x) -> (x,[],dummy_weak)) (List.rev trace)
  in 
  D.S.PH.B.PB.CI.Po.K.build_grid refined_list true handler 
    
let convert_trace_into_musical_notation = D.S.PH.B.import

let enrich_grid_with_transitive_closure = Causal.enrich_grid
let enrich_big_grid_with_transitive_closure f = Causal.enrich_grid f Graph_closure.config_init
let enrich_small_grid_with_transitive_closure f = Causal.enrich_grid f Graph_closure.config_intermediary
let enrich_std_grid_with_transitive_closure f = Causal.enrich_grid f Graph_closure.config_std
					    
let from_none_to_weak_with_tick parameter handler log_info logger n_stories x y =
  let error,counter,tick,blackboard,w1,w2 = from_none_to_weak parameter handler log_info logger x y in
  let tick = Mods.tick_stories logger n_stories tick in 
  error,counter+1,tick,blackboard,w1,w2

let from_none_to_weak_with_tick_ext parameter handler log_info logger n_stories x (_,_,_,z,t) =
  let error,counter,tick,blackboard,w1,w2 = from_none_to_weak parameter handler log_info logger x (z,t) in
  let tick = Mods.tick_stories logger n_stories tick in 
  error,counter+1,tick,blackboard,w1,w2
				       
