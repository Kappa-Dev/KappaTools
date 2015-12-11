(**
  * utilities.mli  
  *
  * Creation:                      <2015-08-10 09:21:53 feret>
  * Last modification: Time-stamp: <2015-12-11 09:51:17 feret>
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

module D:Dag.StoryTable
module S:Generic_branch_and_cut_solver.Solver
	   
type error_log = Exception.method_handler 

(** error_init is an empty log of errors *)
val error_init: error_log 

type parameter = S.PH.B.PB.CI.Po.K.H.parameter 
type kappa_handler = S.PH.B.PB.CI.Po.K.H.handler 
type profiling_info = S.PH.B.PB.CI.Po.K.P.log_info 
		    
(** traces *)			
type trace 

val size_of_pretrace: trace -> int 
val print_trace: parameter -> kappa_handler -> trace -> unit  

(** check wether there is an observable in a trace *)
val has_obs: trace -> bool 																								             
(** convert a list of refined step into a trace *)
val trace_of_pretrace: S.PH.B.PB.CI.Po.K.refined_step list -> trace   

(** remove the events after the last observable *)
val remove_events_after_last_obs: parameter -> (parameter -> bool) -> kappa_handler -> profiling_info -> error_log -> trace -> error_log * profiling_info * trace 
		
(** split_init split init event agent-wise *)
val split_init: parameter -> (parameter -> bool) -> kappa_handler -> profiling_info -> error_log -> trace -> error_log * profiling_info * trace 
			   				     
(** fill_siphon adds spurious init event, to break causal dependences; 
    Currently, it inserts an init event when an agent return to its initial state; 
    other heuristics may be considered;
    The output has to be  disanbiguated, otherwise it is useless (compression will remove the ficitious init events) 
    It should work in quasi linear time (I think)*)
val fill_siphon: parameter -> (parameter -> bool) -> kappa_handler -> profiling_info -> error_log -> trace -> error_log * profiling_info * trace
																	     
(** cut performs partial order reduction and remove orthogonal events *)
val cut: parameter -> (parameter -> bool) -> kappa_handler -> profiling_info -> error_log -> trace -> error_log * profiling_info * trace 

(** remove_pseudo_inverse_events removes pseudo inverse events *)
val remove_pseudo_inverse_events: parameter -> (parameter -> bool) -> kappa_handler -> profiling_info -> error_log -> trace -> error_log * profiling_info * trace 

(** compress a trace with the level of abstraction defined in the argument parameter *)
val compress: Format.formatter -> parameter -> kappa_handler -> error_log -> profiling_info -> trace -> error_log * profiling_info * trace list

(** change the default level of oabstraction for compression (when used with compress) *) 
val set_compression_mode: parameter -> Parameter.current_compression_mode -> parameter
									       															         
val weakly_compress:  Format.formatter -> parameter -> kappa_handler -> error_log -> profiling_info -> trace -> error_log * profiling_info * trace list
val strongly_compress: Format.formatter -> parameter -> kappa_handler -> error_log -> profiling_info -> trace -> error_log * profiling_info * trace list																	   
type story_table 
val fold_story_table_with_progress_bar: Format.formatter -> parameter -> kappa_handler -> error_log -> string -> (parameter -> kappa_handler -> error_log -> trace -> profiling_info Mods.simulation_info list -> 'a -> error_log * 'a) -> story_table -> 'a -> error_log * 'a
val fold_story_table_without_progress_bar: Format.formatter -> parameter -> kappa_handler -> error_log -> string -> (parameter -> kappa_handler -> error_log -> trace -> profiling_info Mods.simulation_info list -> 'a -> error_log * 'a) -> story_table -> 'a -> error_log * 'a
																					
(** put together the stories having the same canonic form, this has do be done explicitely on the moment, I will improve this soon*)
val flatten_story_table: parameter -> kappa_handler -> error_log -> story_table -> error_log * story_table 

       
type step_id = S.PH.B.PB.step_id
type observable_hit 
val get_event_list_from_observable_hit: observable_hit -> step_id list     
val get_runtime_info_from_observable_hit: observable_hit -> unit  Mods.simulation_info option


type cflow_grid = Causal.grid  
type enriched_cflow_grid = Causal.enriched_grid 

(** Blackbord with debugging utilities *)
type musical_grid       

(** Show the current status of the branch and cut assumptions in a libreoffice macro file *)
val export_musical_grid_to_xls: parameter -> kappa_handler -> error_log  -> string -> int -> int -> musical_grid  -> error_log				

(** Show the current status of the branch and cut assumptions in ASCII *)
val print_musical_grid: parameter -> kappa_handler -> error_log  -> musical_grid  -> error_log					   


(** causal flows *)
val convert_trace_into_grid: trace -> kappa_handler -> cflow_grid 

(** compute transitive closure with different parameters (progress_bar, gc) *)
(* change the names to more explicit ones *)
val enrich_grid_with_transitive_past_of_observables_with_a_progression_bar: Format.formatter -> cflow_grid -> enriched_cflow_grid
val enrich_grid_with_transitive_past_of_observables_without_a_progress_bar: Format.formatter -> cflow_grid -> enriched_cflow_grid 
val enrich_grid_with_transitive_past_of_each_node_without_a_progress_bar: Format.formatter -> cflow_grid -> enriched_cflow_grid 
										   

(** Musical processing *)
val convert_trace_into_musical_notation: parameter -> kappa_handler -> error_log -> profiling_info -> trace -> error_log * profiling_info * musical_grid
val extract_observable_hits_from_musical_notation: parameter -> kappa_handler -> error_log ->  musical_grid -> error_log * observable_hit list 
val extract_observable_hit_from_musical_notation: string -> parameter -> kappa_handler -> error_log ->  musical_grid -> error_log * observable_hit  
val causal_prefix_of_an_observable_hit: string -> parameter -> kappa_handler -> error_log -> profiling_info -> musical_grid -> enriched_cflow_grid -> observable_hit -> error_log * trace 


(** Story table *)

val create_story_table: parameter -> kappa_handler -> error_log -> error_log * story_table
val get_counter: story_table -> int 		      
val count_stories: story_table -> int 


(** Store trace in story table *)

val store_trace: parameter -> kappa_handler -> error_log ->  profiling_info Mods.simulation_info list -> profiling_info  -> trace -> story_table -> error_log * story_table *  profiling_info
val export_story_table: parameter -> kappa_handler -> error_log -> story_table -> error_log * (Causal.grid * S.PH.B.PB.CI.Po.K.P.log_info Mods.simulation_info list) list

																			          
val fold_left_with_progress_bar: Format.formatter -> string -> ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a 
