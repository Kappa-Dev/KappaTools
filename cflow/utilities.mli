(**
  * utilities.mli  
  *
  * Creation:                      <2015-08-10 09:21:53 feret>
  * Last modification: Time-stamp: <2015-11-20 21:39:37 feret>
  * 
  * Causal flow compression: a module for KaSim 
  * Jerome Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  *
  * KaSim
  * Jean Krivine, Universite Paris-Diderot, CNRS 
  *  
  * * 
  * Some functionalities for story compression
  *  
  * Copyright 2011,2012,2013,2014,2015 Institut National de Recherche en Informatique 
  * et en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module D:Dag.Dag
	   
type error_log = D.S.PH.B.PB.CI.Po.K.H.error list 
type parameter = D.S.PH.B.PB.CI.Po.K.H.parameter 
type kappa_handler = D.S.PH.B.PB.CI.Po.K.H.handler 
type profiling_info = D.S.PH.B.PB.CI.Po.K.P.log_info 

(** a pretrace is a subsequence of events, they are not necessarily playable *)
type pretrace = D.S.PH.B.PB.CI.Po.K.refined_step list

(** a trace is a playable pretrace *)
type trace = D.S.PH.B.PB.CI.Po.K.refined_step list

(** when an event is played in a different context, the sites that are made free by side_effects *) 					      
type trace_with_side_effect = (D.S.PH.B.PB.CI.Po.K.refined_step * D.S.PH.B.PB.CI.Po.K.side_effect) list

val extend_trace_with_dummy_side_effects: trace -> trace_with_side_effect
				      
type step_id = D.S.PH.B.PB.step_id
		 
type cflow_grid = Causal.grid  
type enriched_cflow_grid = Causal.enriched_grid 
type musical_grid = D.S.PH.B.blackboard 
type transitive_closure_config = Graph_closure.config
				   
(** Comparison among stories *)
(** three representations: dag -> hash -> canonical_form *)
(** two isomorphic dags have the same hash *)
(** two dags are isomorphic iff they have the same canonical form *)

type dag = D.graph
type canonical_form = D.canonical_form
type hash = D.prehash 

(** For each hash form, the list of stories with that hash. 
    For each such stories: the grid and the graph.
    If necessessary the cannonical form.
    The pretrace (to apply further compression later on) 
    Some information about the corresponding observable hits *)	       
type story_list =
  hash * (cflow_grid * dag  * canonical_form option * pretrace * profiling_info Mods.simulation_info list) list
			     
		      
type observable_hit 
val get_event_list_from_observable_hit: observable_hit -> step_id list     
val get_runtime_info_from_observable_hit: observable_hit -> unit  Mods.simulation_info option
			

type story_table  

(** error_init is an empty log of errors *)
val error_init: D.S.PH.B.PB.CI.Po.K.H.error_channel

(** Trace local simplification *)		  
val remove_events_after_last_obs: parameter -> (parameter -> bool) -> kappa_handler -> profiling_info -> error_log -> trace -> error_log * profiling_info * trace 
		
(** split_init split init event agent-wise *)
val split_init: parameter -> (parameter -> bool) -> kappa_handler -> profiling_info -> error_log -> trace -> error_log * profiling_info * trace 
			   
(** disambiguate ensures that agent id are used only once along traces *)
val disambiguate: parameter -> (parameter -> bool) -> kappa_handler -> profiling_info -> error_log -> trace -> error_log * profiling_info * trace (* try to hide this *)
				     
(** fill_siphon adds spurious init event, to break causal dependences; 
    Currently, it insert init event when an agent return to its initial state; 
    other heuristic may be considered;
    The output has to be  disanbiguated, otherwise it is useless (compression will remove the ficitious init events *)
val fill_siphon: parameter -> (parameter -> bool) -> kappa_handler -> profiling_info -> error_log -> trace -> error_log * profiling_info * trace
																	     
(** cut performs partial order reduction and remove orthogonal events *)
val cut: parameter -> (parameter -> bool) -> kappa_handler -> profiling_info -> error_log -> trace -> error_log * profiling_info * trace 

(** remove_pseudo_inverse_events removes pseudo inverse events *)
val remove_pseudo_inverse_events: parameter -> (parameter -> bool) -> kappa_handler -> profiling_info -> error_log -> trace -> error_log * profiling_info * trace 

(** causal flows *)
val convert_trace_into_grid_while_trusting_side_effects: trace -> kappa_handler -> cflow_grid 
val enrich_grid_with_transitive_closure:  Format.formatter -> transitive_closure_config -> cflow_grid -> enriched_cflow_grid
val enrich_big_grid_with_transitive_closure: Format.formatter -> cflow_grid -> enriched_cflow_grid
val enrich_small_grid_with_transitive_closure: Format.formatter -> cflow_grid -> enriched_cflow_grid 
val enrich_std_grid_with_transitive_closure: Format.formatter -> cflow_grid -> enriched_cflow_grid 
										   

(** Musical processing *)
val convert_trace_into_musical_notation: parameter -> kappa_handler -> error_log -> profiling_info -> pretrace -> error_log * profiling_info * musical_grid											     
val extract_observable_hits_from_musical_notation: parameter -> kappa_handler -> error_log ->  musical_grid -> error_log * observable_hit list 
val extract_observable_hit_from_musical_notation: string -> parameter -> kappa_handler -> error_log ->  musical_grid -> error_log * observable_hit  
val causal_prefix_of_an_observable_hit: string -> parameter -> kappa_handler -> error_log -> profiling_info -> musical_grid -> enriched_cflow_grid -> observable_hit -> error_log * trace 


(** Story table *)

(** the int argument is the number of stories to compress, it is used for setting the progress bar *)
val empty_story_table_with_progress_bar: Format.formatter -> int -> story_table 
val empty_story_table: unit -> story_table
		       
val get_counter: story_table -> int 		         (* try to hide this *)
val get_stories: story_table -> story_list list 
val count_faillure: story_table -> int 
val count_stories: story_table -> int 
val inc_faillure: story_table -> story_table             (* try to hide this *)

val store_trace_while_trusting_side_effects: parameter -> kappa_handler -> error_log ->  profiling_info Mods.simulation_info list -> profiling_info  -> trace_with_side_effect -> trace -> story_table -> error_log * story_table *  profiling_info 
val store_trace_while_rebuilding_side_effects: parameter -> kappa_handler -> error_log ->  profiling_info Mods.simulation_info list -> profiling_info  -> trace_with_side_effect -> trace -> story_table -> error_log * story_table *  profiling_info 
val store_trace_while_trusting_side_effects_with_progress_bar: parameter -> kappa_handler -> error_log ->  profiling_info Mods.simulation_info list -> profiling_info  -> trace_with_side_effect -> trace -> story_table -> error_log * story_table *  profiling_info 
val store_trace_while_rebuilding_side_effects_with_progress_bar: parameter -> kappa_handler -> error_log ->  profiling_info Mods.simulation_info list -> profiling_info  -> trace_with_side_effect -> trace -> story_table -> error_log * story_table *  profiling_info 

(** put together the stories having the same canonic form *) 
val flatten_story_table: parameter -> kappa_handler -> error_log -> story_table -> error_log * story_table 

 																				      
(** Print utilities *)													    
val print_trace: parameter -> kappa_handler -> trace -> unit  
val export_musical_grid_to_xls: parameter -> kappa_handler -> error_log  -> string -> int -> int -> musical_grid  -> error_log				
val print_musical_grid: parameter -> kappa_handler -> error_log  -> musical_grid  -> error_log					   


val compress: Format.formatter -> parameter -> kappa_handler -> error_log -> profiling_info -> trace -> error_log * profiling_info * trace_with_side_effect list 
														     

val from_none_to_weak_with_progress_bar:
  parameter -> kappa_handler -> Format.formatter -> (error_log * profiling_info * story_table) -> pretrace * profiling_info Mods.simulation_info list -> (error_log * profiling_info * story_table)
																			          
		      
val from_none_to_weak_with_progress_bar_ext:
  parameter -> kappa_handler -> Format.formatter -> (error_log * profiling_info * story_table) -> cflow_grid * dag * canonical_form option * pretrace * profiling_info Mods.simulation_info list -> (error_log * profiling_info * story_table )
