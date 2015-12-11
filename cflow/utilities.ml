(**
 * utilities.ml 
 *      
 * Creation:                      <2015-03-28 feret>
 * Last modification: Time-stamp: <2015-12-11 09:51:29 feret>
 * 
 * API for causal compression
 * Jerome Feret, projet Abstraction, INRIA Paris-Rocquencourt
 * Jean Krivine, Université Paris-Diderot, CNRS   
 *  
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.  
 * This file is distributed under the terms of the 
 * GNU Library General Public License *)

let debug_mode = false
		   
module D=Dag.StoryTable
module S=Generic_branch_and_cut_solver.Solver

type error_log =  Exception.method_handler 
type parameter =  S.PH.B.PB.CI.Po.K.H.parameter
type kappa_handler = S.PH.B.PB.CI.Po.K.H.handler 
type profiling_info = S.PH.B.PB.CI.Po.K.P.log_info
type progress_bar = bool * int * int 

type step = S.PH.B.PB.CI.Po.K.refined_step				   
type step_with_side_effects = S.PH.B.PB.CI.Po.K.refined_step * S.PH.B.PB.CI.Po.K.side_effect										
type step_id = S.PH.B.PB.step_id

type trace =
  {
    compressed_trace: step_with_side_effects list option ;
    pretrace: step list ; 
    with_potential_ambiguity: bool 
  }


let get_pretrace_of_trace trace = trace.pretrace 
let size_of_pretrace trace = List.length (get_pretrace_of_trace trace)
let may_initial_sites_be_ambiguous trace = trace.with_potential_ambiguity					     
let set_ambiguity_level trace x = {trace with with_potential_ambiguity=x}
let set_pretrace trace x = {trace with pretrace = x}
let set_compressed_trace trace x = {trace with compressed_trace = x}				     
let get_compressed_trace trace =
  match
    trace.compressed_trace
  with
  | Some x -> x
  | None -> List.rev_map (fun x -> x,[]) (List.rev (get_pretrace_of_trace trace)) 					     
let is_compressed_trace trace = trace.compressed_trace != None 

let trace_of_pretrace_with_ambiguity with_ambiguity pretrace =
  {
    pretrace =  pretrace ;
    compressed_trace = None ; 
    with_potential_ambiguity = with_ambiguity ;
  }

let trace_of_pretrace = trace_of_pretrace_with_ambiguity true
							 
let build_compressed_trace x y =
  {
    compressed_trace = Some y;
    pretrace = x;
    with_potential_ambiguity = false
  }
    
let get_log_step = S.PH.B.PB.CI.Po.K.H.get_log_step
let get_debugging_mode = S.PH.B.PB.CI.Po.K.H.get_debugging_mode
let get_logger = S.PH.B.PB.CI.Po.K.H.get_logger
let dummy_log = fun p -> p

let extend_trace_with_dummy_side_effects l = List.rev_map (fun a -> a,[]) (List.rev l)
			   
let print_pretrace parameter handler =
      Format.fprintf
	(S.PH.B.PB.CI.Po.K.H.get_out_channel parameter)
	"@[<v>%a@]@."
	(Pp.list Pp.space (S.PH.B.PB.CI.Po.K.print_refined_step ~handler))

(** operations over traces *) 

let print_trace parameter handler trace = print_pretrace parameter handler (get_pretrace_of_trace trace)
							       
			     
let transform_trace_gen f log_message debug_message log =
  (fun parameters p kappa_handler profiling_info error trace ->
   if p parameters
   then 
     let bool =
       if
	 S.PH.B.PB.CI.Po.K.H.get_log_step parameters
       then
	 match log_message
	 with
	 | Some log_message ->
	    let () = Debug.tag_begin_n (S.PH.B.PB.CI.Po.K.H.get_logger parameters) log_message in
	    true
	 | None -> false
       else
	 false 
     in
     let pretrace = get_pretrace_of_trace trace in 
     let error,(pretrace',n) = f parameters kappa_handler error pretrace in
     let trace' = trace_of_pretrace pretrace' in
     let trace =
       if trace == trace'
       then trace
       else 
	 set_ambiguity_level trace' (may_initial_sites_be_ambiguous trace)
     in 
     let () =
       if
	 bool
       then
	 Debug.tag_end_n (S.PH.B.PB.CI.Po.K.H.get_logger parameters) n
     in 
     let () =
       if
	 S.PH.B.PB.CI.Po.K.H.get_debugging_mode parameters
       then
	 let _ = Debug.tag (S.PH.B.PB.CI.Po.K.H.get_debugging_channel parameters) debug_message in 
	 print_trace parameters kappa_handler trace
     in
     let profiling_info = log n profiling_info  in 
     error,profiling_info,trace 
   else
     error,profiling_info,trace)


type kind =
  | Solve_ambiguities
  | May_add_ambiguities
  | Neutral


let handle_ambiguities kind b =
  match kind
  with
  | Solve_ambiguities -> false
  | Neutral -> b
  | May_add_ambiguities -> true 

type ambiquities_precondition = Do_not_care | Require_the_abscence_of_ambiguity | Better_when_no_ambiguity

let must_we_solve_ambiguity parameters x =
  match
    x
  with
  | Do_not_care -> false
  | Require_the_abscence_of_ambiguity -> true
  | Better_when_no_ambiguity -> S.PH.B.PB.CI.Po.K.H.always_disambiguate parameters
								      
let monadic_lift f = (fun _ _ e t ->
		      let t' = f t in
		      e,(t',List.length t - List.length t'))
let dummy_profiling = (fun _ p -> p)

let disambiguate =
  transform_trace_gen
    (monadic_lift
       S.PH.B.PB.CI.Po.K.disambiguate)
    None
    "Trace after having renames agents:\n"
    dummy_profiling

let make_unambiguous parameters p kappa_handler profiling_info error trace =
  if may_initial_sites_be_ambiguous trace
  then 
    let error,_,trace' = disambiguate parameters (fun parameters -> may_initial_sites_be_ambiguous trace && p parameters) kappa_handler profiling_info error trace in
    error,
    if trace'==trace   
    then trace
    else set_ambiguity_level trace' false
  else
    error,trace 


 

 
let lift_to_care_about_ambiguities f requirement effect =
  (fun parameters p kappa_handler profiling_info error trace ->
   if p parameters
   then 
     let error,trace =
       if must_we_solve_ambiguity parameters requirement
       then
	 make_unambiguous parameters p kappa_handler profiling_info error trace
       else
	 error,trace
     in
     let error,log_info,trace = f parameters p kappa_handler profiling_info error trace in
     let trace = set_ambiguity_level trace (handle_ambiguities effect true) in
     error,log_info,trace 
   else
     error,profiling_info,trace)
	    
let split_init =
  lift_to_care_about_ambiguities
    (transform_trace_gen
       (monadic_lift S.PH.B.PB.CI.Po.K.split_init)
       (Some "\t - splitting initial events")
       "Trace after having split initial events:\n"
       dummy_profiling)
    Do_not_care
    Neutral 
       

    
let cut =
  lift_to_care_about_ambiguities
    (transform_trace_gen
       S.PH.B.PB.CI.Po.cut
       (Some "\t - cutting concurrent events")
       "Trace after having removed concurrent events:\n"
       S.PH.B.PB.CI.Po.K.P.set_global_cut)
    Require_the_abscence_of_ambiguity
    Neutral 
    
    
let fill_siphon =
  lift_to_care_about_ambiguities
    (transform_trace_gen
       (monadic_lift S.PH.B.PB.CI.Po.K.fill_siphon)
       (Some "\t - detecting siphons")
       "Trace after having detected siphons:\n"
       dummy_profiling)
    Require_the_abscence_of_ambiguity
    May_add_ambiguities
    
let remove_events_after_last_obs =
  lift_to_care_about_ambiguities
    (transform_trace_gen
       (monadic_lift ((List_utilities.remove_suffix_after_last_occurrence S.PH.B.PB.CI.Po.K.is_obs_of_refined_step)))
       (Some "\t - removing events occurring after the last observable:\n")
       "Trace after having removed the events after the last observable"
       dummy_profiling)
    Do_not_care
    Neutral 

let remove_pseudo_inverse_events =
  lift_to_care_about_ambiguities 
    (transform_trace_gen
       S.PH.B.PB.CI.cut
       (Some "\t - detecting pseudo inverse events")
       "Trace after having removed pseudo inverse events:\n"
       S.PH.B.PB.CI.Po.K.P.set_pseudo_inv)
    Do_not_care
    Neutral
						
  		 
type cflow_grid = Causal.grid  
type enriched_cflow_grid = Causal.enriched_grid
type musical_grid =  S.PH.B.blackboard 
type transitive_closure_config = Graph_closure.config
type story_table =  
  { 
    story_counter:int;
    story_list: D.table ;
   }
    
let count_stories story_table = D.count_stories story_table.story_list 
    
type observable_hit = 
  {
    list_of_actions: S.PH.update_order list ;
    list_of_events: step_id  list ; 
    runtime_info:  unit Mods.simulation_info option}

let get_event_list_from_observable_hit a = a.list_of_events 
let get_runtime_info_from_observable_hit a = a.runtime_info 
let get_list_order a = a.list_of_actions 

module Profiling = S.PH.B.PB.CI.Po.K.P

		     
let error_init = Exception.empty_error_handler 
	    
let extract_observable_hits_from_musical_notation a b c d = 
  let error,l = S.PH.forced_events a b c d in 
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
  let error,l = S.PH.forced_events a b c d in 
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
  let event_id_list = Graph_closure.get_list_in_increasing_order_with_last_event (eid+1) enriched_grid.Causal.prec_star in 
  let error,list_eid,_ = S.translate parameter handler error blackboard event_id_list in 
  error,trace_of_pretrace list_eid 



let export_musical_grid_to_xls = S.PH.B.export_blackboard_to_xls
  
let print_musical_grid = S.PH.B.print_blackboard 


let create_story_table parameters handler error  =
  let error,init = D.init_table parameters handler error in
  error,
  { 
    story_counter=1;
    story_list= init ;
  }  
  
let get_trace_of_story (_,_,_,y,_) = trace_of_pretrace y
let get_info_of_story (_,_,_,_,t) = t


let tick_opt bar = 
  match 
    bar
  with 
  | None -> bar
  | Some (logger,n,bar) -> Some (logger,n,Mods.tick_stories logger n bar)

let close_progress_bar_opt logger = 
  match 
    logger 
  with 
  | None -> ()
  | Some logger -> Format.pp_print_newline logger () 

 
let print_fails logger s n =
  match 
    n 
  with 
  | 0 -> ()
  | 1 -> Format.fprintf logger "@.\t 1 %s has failed@." s
  | _ -> Format.fprintf logger "@.\t %i %ss have failed@." n s 
 
let inc_fails a a' b = 
  if a==a'
  then succ b
  else b 
    
let fold_story_table_gen logger logger_fail parameter handler error s f l a =
  let n_stories_input = count_stories l in 
  let progress_bar = 
    match logger
    with None -> None
       | Some logger -> 
	  Some (logger,n_stories_input,Mods.tick_stories logger n_stories_input (false,0,0))
  in
  let g parameter handler error story info (progress_bar,a,n_fails) =
    let error,a' = f parameter handler error (trace_of_pretrace_with_ambiguity false story) info a in
    let progress_bar = tick_opt progress_bar in
    let n_fails = inc_fails a a' n_fails in 
    error,(progress_bar,a',n_fails)
  in
  let error,(_,a,n_fails) =   D.fold_table parameter handler error g l.story_list (progress_bar,a,0) in 
  let () = close_progress_bar_opt logger in 
  let () = print_fails logger_fail  s n_fails in 
  error,a 

let fold_story_table_with_progress_bar logger = fold_story_table_gen (Some logger) logger 
let fold_story_table_without_progress_bar a = fold_story_table_gen None a


let get_counter story_list = story_list.story_counter 
let get_stories story_list = story_list.story_list 
let inc_counter story_list =
  {
    story_list with 
      story_counter = succ story_list.story_counter
  }


let store_trace (parameter:parameter) (handler:kappa_handler) (error:error_log) obs_info  computation_info trace  (story_table:story_table) = 
  let pretrace = get_pretrace_of_trace trace in
  let trace2 = get_compressed_trace trace in 
  let bool = not (is_compressed_trace trace) in 
  let grid = S.PH.B.PB.CI.Po.K.build_grid trace2 bool  handler in
  let computation_info  = S.PH.B.PB.CI.Po.K.P.set_grid_generation  computation_info in 
  let story_info = 
    List.map 
      (Mods.update_profiling_info (S.PH.B.PB.CI.Po.K.P.copy computation_info))
      obs_info 
  in
  let error,story_list = D.add_story parameter handler error grid pretrace story_info story_table.story_list in 
  let story_table = 
    {
      story_list = story_list ;
      story_counter = story_table.story_counter +1 								       
    }
  in
  error,story_table,computation_info 

let fold_left_with_progress_bar logger string f a l =
  let n = List.length l in
  let progress_bar = Mods.tick_stories logger n (false,0,0) in
  let _,a,n_fail = 
    (List.fold_left
       (fun (bar,a,n_fail) x ->
	let a' = f a x in
	let bar = Mods.tick_stories logger n bar in
	let n_fail = inc_fails a a' n_fail in 
	(bar,a',n_fail))
       (progress_bar,a,0)
       l)
  in 
  let () = close_progress_bar_opt (Some logger) in 
  let () = print_fails logger string n_fail in 
  a 
   
let flatten_story_table parameter handler error story_table = 
  let error,list = D.hash_list parameter handler error story_table.story_list in 
  error,
  {story_table
   with 
     story_list = list}

let always = (fun _ -> true) 


let compress logger parameter handler error log_info trace =
  match
    parameter.S.PH.B.PB.CI.Po.K.H.current_compression_mode
  with
  | None -> error,log_info,[trace] 
  | Some Parameter.Causal ->
     let error,log_info,trace = cut parameter always handler log_info error trace
     in error,log_info,[trace] 
  | Some Parameter.Weak | Some Parameter.Strong -> 
    let event_list = get_pretrace_of_trace trace in 
    let error,log_info,blackboard = S.PH.B.import parameter handler error log_info event_list in 
    let error,list = S.PH.forced_events parameter handler error blackboard in     
    let list_order = 
      match list 
      with 
      | (list_order,_,_)::_ -> list_order
      | _ -> []
    in 
    let error,log_info,blackboard,output,list = 
      S.compress parameter handler error log_info blackboard list_order 
    in
    let list =
      List.rev_map
	(fun pretrace ->
	 let event_list = S.translate_result pretrace in 
	 let event_list = S.PH.B.PB.CI.Po.K.clean_events event_list in 
	 (build_compressed_trace event_list pretrace))
	list
    in 
    let log_info = S.PH.B.PB.CI.Po.K.P.set_story_research_time log_info in 
    let error = 
      if debug_mode
      then 
	let _ =  Debug.tag logger "\t\t * result"  in
	let _ =
          if S.PH.B.is_failed output 
          then 
            let _ = Format.fprintf parameter.S.PH.B.PB.CI.Po.K.H.out_channel_err "Fail_to_compress" in  error
          else 
            let _ = Format.fprintf parameter.S.PH.B.PB.CI.Po.K.H.out_channel_err "Succeed_to_compress" in 
            error
	in 
      error 
      else 
	error
    in error,log_info,list
			
let set_compression_mode p x =
  match
    x
  with
  | Parameter.Causal -> S.PH.B.PB.CI.Po.K.H.set_compression_none p
  | Parameter.Strong -> S.PH.B.PB.CI.Po.K.H.set_compression_strong p
  | Parameter.Weak -> S.PH.B.PB.CI.Po.K.H.set_compression_weak p
						       
let strongly_compress logger parameter = compress logger (set_compression_mode parameter Parameter.Strong)
let weakly_compress logger parameter = compress logger (set_compression_mode parameter Parameter.Weak)
								  
let convert_trace_into_grid trace handler = 
    let event_list = get_compressed_trace trace in
    S.PH.B.PB.CI.Po.K.build_grid event_list (not (is_compressed_trace trace)) handler 
    
let convert_trace_into_musical_notation p h e info x = S.PH.B.import p h e info (get_pretrace_of_trace x)

let enrich_grid_with_transitive_closure = Causal.enrich_grid

let enrich_grid_with_transitive_past_of_observables_with_a_progression_bar f = Causal.enrich_grid f Graph_closure.config_big_graph_with_progress_bar
let enrich_grid_with_transitive_past_of_observables_without_a_progress_bar f = Causal.enrich_grid f Graph_closure.config_big_graph_without_progress_bar
let enrich_grid_with_transitive_past_of_each_node_without_a_progress_bar f = Causal.enrich_grid f Graph_closure.config_small_graph
					    
let sort_story_list  = D.sort_list 
let export_story_table parameter handler error x = sort_story_list parameter handler error (get_stories x)
let has_obs x = List.exists S.PH.B.PB.CI.Po.K.is_obs_of_refined_step (get_pretrace_of_trace x)
				   
