(**
 * utilities.ml 
 *      
 * Creation:                      <2015-03-28 feret>
 * Last modification: Time-stamp: <2015-12-16 09:14:46 feret>
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
module P=StoryProfiling.StoryStats
	   
type error_log =  Exception.method_handler 
type parameter =  S.PH.B.PB.CI.Po.K.H.parameter
type kappa_handler = S.PH.B.PB.CI.Po.K.H.handler 
type profiling_info = P.log_info
type progress_bar = bool * int * int 
type shall_we = (parameter -> bool)
		  
type step = S.PH.B.PB.CI.Po.K.refined_step				   
type step_with_side_effects = S.PH.B.PB.CI.Po.K.refined_step * S.PH.B.PB.CI.Po.K.side_effect										
type step_id = S.PH.B.PB.step_id

type trace =
  {
    compressed_trace: step_with_side_effects list option ;
    pretrace: step list ; 
    with_potential_ambiguity: bool 
  }
type trace_runtime_info = profiling_info Mods.simulation_info

				 					 
type do_we_log = (parameter -> bool)			
type 'a with_handlers =
  parameter -> ?shall_we_compute:shall_we -> ?shall_we_compute_profiling_information:shall_we -> kappa_handler -> profiling_info -> error_log -> 'a
type 'a zeroary =
  (error_log * profiling_info * 'a) with_handlers
type ('a,'b) unary =
  ('a -> error_log * profiling_info * 'b) with_handlers 
type ('a,'b,'c) binary =
  ('a -> 'b -> error_log * profiling_info * 'c) with_handlers
type ('a,'b,'c,'d) ternary =
  ('a -> 'b -> 'c ->error_log * profiling_info * 'd) with_handlers
type ('a,'b,'c,'d,'e) quaternary =
  ('a -> 'b -> 'c -> 'd -> error_log * profiling_info * 'e) with_handlers
type ('a,'b,'c,'d,'e,'f) quinternary =
  ('a -> 'b -> 'c -> 'd -> 'e -> error_log * profiling_info * 'f) with_handlers 
							    
let (we_shall:shall_we) = (fun _ -> true)
let (we_shall_not:shall_we) = (fun _ -> false)
		     
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
							       
			     
let transform_trace_gen f log_message debug_message profiling_event =
  (fun parameters ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) kappa_handler profiling_info error trace ->
   if shall_we_compute parameters
   then 
     let profiling_info = StoryProfiling.StoryStats.add_event profiling_event (Some (fun () -> size_of_pretrace trace)) profiling_info in 
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
     let error,profiling_info,(pretrace',n) = f parameters kappa_handler profiling_info error pretrace in
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
     let error, profiling_info =
       StoryProfiling.StoryStats.close_event (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameters) error profiling_event (Some (fun () -> size_of_pretrace trace)) profiling_info
     in 
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
								      
let monadic_lift f = (fun _ _ log_info error t ->
		      let t' = f t in
		      error,log_info,(t',List.length t - List.length t'))
let dummy_profiling = (fun _ p -> p)

let disambiguate =
  transform_trace_gen
    (monadic_lift S.PH.B.PB.CI.Po.K.disambiguate)
    None
    "Trace after having renames agents:\n"
    StoryProfiling.Agent_ids_disambiguation
    
let make_unambiguous parameters ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall_not) kappa_handler profiling_info error trace =
  if may_initial_sites_be_ambiguous trace
  then 
    let error,profiling_info,trace' = disambiguate parameters
				      ~shall_we_compute:(fun parameters -> may_initial_sites_be_ambiguous trace && shall_we_compute parameters)
				      ~shall_we_compute_profiling_information:shall_we_compute_profiling_information
				      kappa_handler profiling_info error trace in
    error,profiling_info,
    if trace'==trace   
    then
      set_ambiguity_level trace false 
    else
	set_ambiguity_level trace' false
  else
    error,profiling_info,trace 

let lift_to_care_about_ambiguities f requirement effect =
  (fun parameters ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) kappa_handler profiling_info error trace ->
   if shall_we_compute parameters
   then 
     let error,profiling_info,trace =
       if must_we_solve_ambiguity parameters requirement
       then
	 make_unambiguous parameters ~shall_we_compute:shall_we_compute kappa_handler profiling_info error trace
       else
	 error,profiling_info,trace
     in
     let error,log_info,trace =
       (f: S.PH.B.PB.CI.Po.K.H.parameter -> ?shall_we_compute:(S.PH.B.PB.CI.Po.K.H.parameter -> bool) ->
	?shall_we_compute_profiling_information:(S.PH.B.PB.CI.Po.K.H.parameter -> bool) -> 
        S.PH.B.PB.CI.Po.K.H.handler ->
        StoryProfiling.StoryStats.log_info ->
        Exception.method_handler ->
        trace ->
        Exception.method_handler * StoryProfiling.StoryStats.log_info *
          trace)
	 parameters ~shall_we_compute:shall_we_compute ~shall_we_compute_profiling_information:shall_we_compute_profiling_information kappa_handler profiling_info error trace in
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
       StoryProfiling.Decompose_initial_state)
    Do_not_care
    Neutral 
    
let cut =
  lift_to_care_about_ambiguities
    (transform_trace_gen
       S.PH.B.PB.CI.Po.cut
       (Some "\t - cutting concurrent events")
       "Trace after having removed concurrent events:\n"
       StoryProfiling.Partial_order_reduction)
    Require_the_abscence_of_ambiguity    
    Neutral 
    
    
let fill_siphon =
  lift_to_care_about_ambiguities
    (transform_trace_gen
       (monadic_lift S.PH.B.PB.CI.Po.K.fill_siphon)
       (Some "\t - detecting siphons")
       "Trace after having detected siphons:\n"
       StoryProfiling.Siphon_detection)
    Require_the_abscence_of_ambiguity
    May_add_ambiguities
    
let  (remove_events_after_last_obs: (trace,trace) unary) =
  lift_to_care_about_ambiguities
    (transform_trace_gen
       (monadic_lift ((List_utilities.remove_suffix_after_last_occurrence S.PH.B.PB.CI.Po.K.is_obs_of_refined_step)))
       (Some "\t - removing events occurring after the last observable:\n")
       "Trace after having removed the events after the last observable"
       StoryProfiling.Remove_events_after_last_observable
    )
    Do_not_care
    Neutral 

let remove_pseudo_inverse_events =
  lift_to_care_about_ambiguities 
    (transform_trace_gen
       S.PH.B.PB.CI.cut
       (Some "\t - detecting pseudo inverse events")
       "Trace after having removed pseudo inverse events:\n"
       StoryProfiling.Pseudo_inverse_deletion)
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

		     
let error_init = Exception.empty_error_handler 
	    
let extract_observable_hits_from_musical_notation a  ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) b profiling_info c d = 
  let error,profiling_info,l = S.PH.forced_events a b profiling_info c d in 
  error,
  profiling_info, 
  List.rev_map
    (fun (a,b,c) -> 
      {
      list_of_actions = a;
      list_of_events = b ;
      runtime_info = c
      })
    (List.rev l)

let extract_observable_hit_from_musical_notation a ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) b profiling_info c string d = 
  let error,profiling_info,l = S.PH.forced_events a b profiling_info c d in 
  match l with
  | [a,b,c] -> 
     error,profiling_info,
     {
       list_of_actions = a;
       list_of_events = b; 
       runtime_info = c}
  | [] -> failwith (string^" no story")
  | _::_ -> failwith (string^" several stories")
		     
    
let translate p h profiling_info e b list = 
  let error,profiling_info,(list,_) = S.translate p h profiling_info e b list in 
  error,profiling_info,trace_of_pretrace list 

let causal_prefix_of_an_observable_hit parameter ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) handler profiling_info error string blackboard (enriched_grid:enriched_cflow_grid) observable_id = 
  let eid = 
    match 
      get_event_list_from_observable_hit observable_id  
    with 
    | [a] -> a 
    | [] -> failwith ("no observable in that story"^string)
    | _ -> failwith  ("several observables in that story"^string)
  in 
  let event_id_list = Graph_closure.get_list_in_increasing_order_with_last_event (eid+1) enriched_grid.Causal.prec_star in 
  let error,profiling_info,output = translate parameter handler profiling_info error blackboard event_id_list in
  error,profiling_info,output
  


let export_musical_grid_to_xls a ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) b (p:StoryProfiling.StoryStats.log_info) c d e f g=
  S.PH.B.export_blackboard_to_xls a b p c d e f g
  
let print_musical_grid a ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) b p c d = S.PH.B.print_blackboard a b p c d


let create_story_table parameters ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) handler profiling_info error  =
  let error,profiling_info,init = D.init_table parameters handler profiling_info error in
  error,
  profiling_info,
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
    
let fold_story_table_gen logger parameter ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) (handler:kappa_handler) (profiling_info:profiling_info) error s
			 (f:((trace, trace_runtime_info list, 'a, 'a) ternary)) l a =
  let n_stories_input = count_stories l in 
  let progress_bar = 
    match logger
    with None -> None
       | Some logger -> 
	  Some (logger,n_stories_input,Mods.tick_stories logger n_stories_input (false,0,0))
  in
  let g parameter handler profiling_info error story info (k,progress_bar,a,n_fails) =
    let event = StoryProfiling.Story k in 
    let profiling_info = P.add_event event None profiling_info in
    let error,profiling_info,a' = f parameter ~shall_we_compute:shall_we_compute ~shall_we_compute_profiling_information:shall_we_compute_profiling_information handler profiling_info error (trace_of_pretrace_with_ambiguity false story) info a in
    let progress_bar = tick_opt progress_bar in
    let n_fails = inc_fails a a' n_fails in 
    let error,profiling_info = P.close_event (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error event None profiling_info in
    error,profiling_info,(succ k,progress_bar,a',n_fails)
  in
  let error,profiling_info,(_,_,a,n_fails) =   D.fold_table parameter handler profiling_info error g l.story_list (1,progress_bar,a,0) in 
  let () = close_progress_bar_opt logger in 
  let () = print_fails parameter.S.PH.B.PB.CI.Po.K.H.out_channel_err s n_fails in 
  error,(profiling_info:profiling_info),a 

let fold_story_table_with_progress_bar parameter ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) (handler:kappa_handler) profiling_info error s
				        f l a =
  fold_story_table_gen
    (Some (S.PH.B.PB.CI.Po.K.H.get_logger parameter))
    parameter
    ~shall_we_compute:shall_we_compute
    ~shall_we_compute_profiling_information:shall_we_compute_profiling_information
    handler profiling_info error s f l a 
let fold_story_table_without_progress_bar parameter ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) handler profiling_info error s f l a =
  fold_story_table_gen
    None
    parameter
    ~shall_we_compute:shall_we_compute
    ~shall_we_compute_profiling_information:shall_we_compute_profiling_information
    handler profiling_info error s f l a 


let get_counter story_list = story_list.story_counter 
let get_stories story_list = story_list.story_list 
let inc_counter story_list = { story_list with story_counter = succ story_list.story_counter }


let store_trace
      (parameter:parameter) ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) handler
      computation_info error trace obs_info story_table = 
  let pretrace = get_pretrace_of_trace trace in
  let trace2 = get_compressed_trace trace in 
  let bool = not (is_compressed_trace trace) in 
  let grid = S.PH.B.PB.CI.Po.K.build_grid trace2 bool  handler in
  let computation_info  = P.set_grid_generation  computation_info in 
  let story_info = 
    List.map 
      (Mods.update_profiling_info (P.copy computation_info))
      obs_info 
  in
  let error,computation_info,story_list = D.add_story parameter handler computation_info error grid pretrace story_info story_table.story_list in 
  let story_table = 
    {
      story_list = story_list ;
      story_counter = story_table.story_counter +1 								       
    }
  in
  error,computation_info,story_table
   
let flatten_story_table parameter ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) handler log_info error story_table = 
  let error,log_info,list = D.hash_list parameter handler log_info error story_table.story_list in 
  error,
  log_info,
  {story_table
   with 
     story_list = list}

let always = (fun _ -> true) 


let compress parameter ?(shall_we_compute=always) ?(shall_we_compute_profiling_information=we_shall) handler log_info error trace =
  match
    parameter.S.PH.B.PB.CI.Po.K.H.current_compression_mode
  with
  | None -> error,log_info,[trace] 
  | Some Parameter.Causal ->
     let error,log_info,trace = cut parameter ~shall_we_compute:always handler log_info error trace
     in error,log_info,[trace] 
  | Some Parameter.Weak
  | Some Parameter.Strong -> 
     let event = match parameter.S.PH.B.PB.CI.Po.K.H.current_compression_mode
       with Some Parameter.Weak -> StoryProfiling.Weak_compression
	  | _ -> StoryProfiling.Strong_compression
     in
     let log_info = P.add_event event (Some (fun () -> size_of_pretrace trace)) log_info in 
    let event_list = get_pretrace_of_trace trace in 
     let error,log_info,blackboard = S.PH.B.import parameter handler log_info error event_list in 
     let error,log_info,list = S.PH.forced_events parameter handler log_info error blackboard in     
     let list_order = 
       match list 
       with 
       | (list_order,_,_)::_ -> list_order
       | _ -> []
     in 
     let error,log_info,(blackboard,output,list) = 
       S.compress parameter handler log_info error blackboard list_order 
     in
     let list =
       List.rev_map
	 (fun pretrace ->
	  let event_list = S.translate_result pretrace in 
	  let event_list = S.PH.B.PB.CI.Po.K.clean_events event_list in 
	  (build_compressed_trace event_list pretrace))
	 list
     in 
     let log_info = P.set_story_research_time log_info in 
     let error,log_info = P.close_event (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error event (Some  (fun () -> size_of_pretrace trace)) log_info in 
  
     let error = 
       if debug_mode
       then 
	 let _ =  Debug.tag parameter.S.PH.B.PB.CI.Po.K.H.out_channel_err "\t\t * result"  in
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
						       
let strongly_compress parameter = compress (set_compression_mode parameter Parameter.Strong)
let weakly_compress parameter = compress (set_compression_mode parameter Parameter.Weak)
								  
let convert_trace_into_grid trace handler =
    let event_list = get_compressed_trace trace in
    S.PH.B.PB.CI.Po.K.build_grid event_list (not (is_compressed_trace trace)) handler 
    
let convert_trace_into_musical_notation parameters  ?(shall_we_compute=always) ?(shall_we_compute_profiling_information=we_shall) kappa_handler profiling_info error trace =
  S.PH.B.import parameters kappa_handler profiling_info error (get_pretrace_of_trace trace)

let enrich_grid_with_transitive_closure logger config  ?(shall_we_compute=always) ?(shall_we_compute_profiling_information=we_shall) handler log_info error grid =
  let output = Causal.enrich_grid logger config grid in
  error,log_info,output

let enrich_grid_with_transitive_past_of_observables_with_a_progress_bar f = enrich_grid_with_transitive_closure  (S.PH.B.PB.CI.Po.K.H.get_logger f) Graph_closure.config_big_graph_with_progress_bar
let enrich_grid_with_transitive_past_of_observables_without_a_progress_bar f = enrich_grid_with_transitive_closure (S.PH.B.PB.CI.Po.K.H.get_logger f) Graph_closure.config_big_graph_without_progress_bar
let enrich_grid_with_transitive_past_of_each_node_without_a_progress_bar f = enrich_grid_with_transitive_closure (S.PH.B.PB.CI.Po.K.H.get_logger f) Graph_closure.config_big_graph_without_progress_bar
let enrich_grid_with_transitive_past_of_each_node_without_a_progress_bar f = enrich_grid_with_transitive_closure (S.PH.B.PB.CI.Po.K.H.get_logger f) Graph_closure.config_small_graph
					    
let sort_story_list  = D.sort_list 
let export_story_table parameter ?(shall_we_compute=always) ?(shall_we_compute_profiling_information=we_shall) handler log_info error x =
  let a,log_info,b = sort_story_list parameter handler log_info error (get_stories x) in
  a,log_info,b
let has_obs x = List.exists S.PH.B.PB.CI.Po.K.is_obs_of_refined_step (get_pretrace_of_trace x)
			    
let fold_left_with_progress_bar
      parameter ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall)
      handler profiling_information error (string:string) (f:('a,'b,'a) binary) a list =
  let n = List.length list in
  let progress_bar = Mods.tick_stories (S.PH.B.PB.CI.Po.K.H.get_logger parameter) n (false,0,0) in
  let error,profiling_information,_,_,a,n_fail =
    let rec aux list (error,profiling_information,bar,k,a,n_fail) =
      let event = StoryProfiling.Story k in 
      match
	list
      with
      | [] -> error,profiling_information,bar,k,a,n_fail
      | x::tail -> 
	 let profiling_information = P.add_event event None profiling_information in
 	 let output_opt  =
	   try 
	     let error,profiling_information,a' =
	       f
		 parameter 
		 ~shall_we_compute:shall_we_compute ~shall_we_compute_profiling_information:shall_we_compute_profiling_information
		 handler
		 profiling_information
		 error
		 a x
	     in
	     let bar = Mods.tick_stories (S.PH.B.PB.CI.Po.K.H.get_logger parameter) n bar in
	     let n_fail = inc_fails a a' n_fail in
	     let error,profiling_information = P.close_event (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error event None profiling_information in 
	     Some (error,profiling_information,bar,k+1,a',n_fail) 
	   with ExceptionDefn.UserInterrupted _  -> None
	 in
	 match output_opt
	 with
	   None ->
	   let error,profiling_information = P.close_event (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error event None profiling_information in 
	   (error,profiling_information,bar,k+1,a,n_fail)
	 | Some remanent -> 
	    aux tail remanent
    in
    aux list (error,profiling_information,progress_bar,1,a,0)
  in  
  let () = close_progress_bar_opt (Some (S.PH.B.PB.CI.Po.K.H.get_logger parameter)) in 
  let () = print_fails parameter.S.PH.B.PB.CI.Po.K.H.out_channel_err string n_fail in 
  error,profiling_information,a 			    

let fold_over_the_causal_past_of_observables_through_a_grid_with_a_progress_bar parameter handler log_info error f t a =
  let grid = convert_trace_into_grid t handler in 
  Causal.fold_over_causal_past_of_obs 
    (S.PH.B.PB.CI.Po.K.H.get_logger parameter)
    Graph_closure.config_big_graph_with_progress_bar
    grid 
    f (error,log_info,a)

    
let fold_over_the_causal_past_of_observables_with_a_progress_bar parameter  ?(shall_we_compute=we_shall) ?(shall_we_compute_profiling_information=we_shall) handler log_info error log_step debug_mode f t a =
  let () = 
    if log_step parameter 
    then 
      Debug.tag (S.PH.B.PB.CI.Po.K.H.get_logger parameter) "\t - blackboard generation"
  in 
  let error,log_info,blackboard = convert_trace_into_musical_notation parameter handler log_info error t in
  let error,log_info,list = extract_observable_hits_from_musical_notation parameter handler log_info error blackboard in 
  let n_stories = List.length list in 
  let () =
    if log_step parameter 
    then 
      Format.fprintf (S.PH.B.PB.CI.Po.K.H.get_logger parameter) "\t - computing causal past of each observed events (%i)@." n_stories 
  in
  (* generation of uncompressed stories *)
  let () = 
    if debug_mode parameter
    then 
      Debug.tag (S.PH.B.PB.CI.Po.K.H.get_logger parameter) "\t\t * causal compression "
  in 
  let log_info = P.set_start_compression log_info in 
  let grid = convert_trace_into_grid t handler in 
  let error,log_info,_,_,a =
    Causal.fold_over_causal_past_of_obs 
      (S.PH.B.PB.CI.Po.K.H.get_logger parameter)
      Graph_closure.config_big_graph_with_progress_bar
      grid
      (fun observable_hit causal_past (error,log_info,counter,list,a) ->
       match list with
       | [] -> error,log_info,counter,list,a
       | head::tail ->
	  let observable_id = head in 
	  let log_info = P.reset_log log_info in 
	  let () = 
	    if debug_mode parameter 
	    then 
	      Debug.tag (S.PH.B.PB.CI.Po.K.H.get_logger parameter) "\t\t * causal compression "
	  in
	  (* we translate the list of event ids into a trace thanks to the blackboad *)
	  let error,log_info,trace = 
	    translate parameter handler log_info error blackboard (List.rev (observable_hit::causal_past)) 
	  in
	  (* we collect run time info about the observable *)
	  let info = 
	    match get_runtime_info_from_observable_hit observable_id 
	    with 
	    | None -> []
	    | Some info -> 
	       let info = {info with Mods.story_id = counter} in 
	       let info = Mods.update_profiling_info log_info  info 
	       in 
	       [info]
	  in
	  let error,log_info,a = (f:('a,'b,'c,'d) ternary)  parameter handler log_info error trace info a in
	  error,log_info,counter+1,tail,a)
      (error,log_info,1,List.rev list,a)
  in
  error,log_info,a 

let copy_log_info = P.copy
