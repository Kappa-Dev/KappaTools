(**
  * utilities.mli  
  *
  * Creation:                      <2015-08-10 09:21:53 feret>
  * Last modification: Time-stamp: <2015-11-20 10:13:00 feret>
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
  * Copyright 2011,2012,2013 Institut National de Recherche en Informatique 
  * et en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module D:Dag.Dag
	   
type error_log = D.S.PH.B.PB.CI.Po.K.H.error_channel
type parameter = D.S.PH.B.PB.CI.Po.K.H.parameter 
type kappa_handler = D.S.PH.B.PB.CI.Po.K.H.handler 
type profiling_info = D.S.PH.B.PB.CI.Po.K.P.log_info
type progress_bar = bool * int * int 

type refined_trace = D.S.PH.B.PB.CI.Po.K.refined_step list
type refined_trace_with_side_effect = (D.S.PH.B.PB.CI.Po.K.refined_step * D.S.PH.B.PB.CI.Po.K.side_effect) list
type refined_trace_with_weak_events = (D.S.PH.B.PB.CI.Po.K.refined_step * bool) list
type step_id = D.S.PH.B.PB.step_id 
type cflow_grid = Causal.grid  
type enriched_cflow_grid = Causal.enriched_grid 
type musical_grid = D.S.PH.B.blackboard 
type transitive_closure_config = Graph_closure.config
				   
		      
(* dag: internal representation for cflows *)
type dag = D.graph
(* cannonical form for cflows (completely capture isomorphisms) *)
type dag_canonical_form = D.canonical_form
(* prehashform for cflows, if two cflows are isomorphic, they have the same prehash form *) 
type dag_prehash = D.prehash 

(* I need to investigate further, what I know is that:
   for each hash, there is a list of stories having this hash, for each one, we have the grid, the dag, then the canonical form, if it has been computed already, the compressed trace, then a list of timestamp that indicated when the observables have been hit *) 
type story_list =
  dag_prehash * (cflow_grid * dag  * dag_canonical_form  option * refined_trace * profiling_info Mods.simulation_info option list) list
			     
		      
type observable_hit 
			
type story_table (*=*)  
(*  { 
    story_counter:int;
    progress_bar:progress_bar;
    blackboard:musical_grid;
    story_list: story_list list;
    faillure:int}*)

(** error_init is an empty log of errors *)
val error_init: D.S.PH.B.PB.CI.Po.K.H.error_channel



(** Trace local simplification *)		  

val remove_events_after_last_obs: refined_trace -> refined_trace
(** split_init split init event agent-wise *)
val split_init: refined_trace -> refined_trace
			   
(** disambiguate ensures that agent id are used only once along traces *)
val disambiguate: refined_trace -> refined_trace
				     
(** fill_siphon adds spurious init event, to break causal dependences; 
    Currently, it insert init event when an agent return to its initial state; 
    other heuristic may be considered;
    The output has to be  disanbiguated, otherwise it is useless (compression will remove the ficitious init events *)
val fill_siphon: refined_trace -> refined_trace

(** cut performs partial order reduction and remove orthogonal events *)
val cut: parameter -> kappa_handler -> error_log -> refined_trace -> error_log * (refined_trace * int)

(** remove_pseudo_inverse_events removes pseudo inverse events *)
val remove_pseudo_inverse_events: parameter -> kappa_handler -> error_log -> refined_trace -> error_log * (refined_trace * int)


(** causal flows *)
val convert_trace_into_grid_while_trusting_side_effects: refined_trace -> kappa_handler -> cflow_grid 
val enrich_grid_with_transitive_closure:  Format.formatter -> transitive_closure_config -> cflow_grid -> enriched_cflow_grid
val enrich_big_grid_with_transitive_closure: Format.formatter -> cflow_grid -> enriched_cflow_grid
val enrich_small_grid_with_transitive_closure: Format.formatter -> cflow_grid -> enriched_cflow_grid 
val enrich_std_grid_with_transitive_closure: Format.formatter -> cflow_grid -> enriched_cflow_grid 
										   
(** Musical processing *)
val convert_trace_into_musical_notation: parameter -> kappa_handler -> error_log -> profiling_info -> refined_trace -> error_log * profiling_info * musical_grid

 
											     
val extract_observable_hits_from_musical_notation: parameter -> kappa_handler -> error_log ->  musical_grid -> error_log * observable_hit list 
val extract_observable_hit_from_musical_notation: string -> parameter -> kappa_handler -> error_log ->  musical_grid -> error_log * observable_hit  


val get_event_list_from_observable_hit: observable_hit -> step_id list     
val get_runtime_info_from_observable_hit: observable_hit -> unit  Mods.simulation_info option
val get_list_order: observable_hit -> D.S.PH.update_order list

val causal_prefix_of_an_observable_hit: string -> parameter -> kappa_handler -> error_log -> profiling_info -> musical_grid -> enriched_cflow_grid -> observable_hit -> error_log * refined_trace 

val empty_story_table_with_tick: Format.formatter -> (*musical_grid ->*) int -> story_table 
val empty_story_table: (*musical_grid ->*) int -> story_table 
val tick: Format.formatter -> story_table -> story_table 
val inc_counter: story_table -> story_table 
val get_counter: story_table -> int 		
val get_stories: story_table -> story_list list 
val count_faillure: story_table -> int 

val store_trace_gen: bool -> parameter -> kappa_handler -> error_log ->  profiling_info Mods.simulation_info option -> profiling_info  -> refined_trace_with_side_effect -> refined_trace -> story_table -> error_log * story_table *  profiling_info Mods.simulation_info option 

(** put together the stories having the same canonic form *) 
val flatten_story_table: parameter -> kappa_handler -> error_log -> story_table -> error_log * story_table 

val count_stories: story_table -> int  																				      
(** Print utilities *)													    
val print_trace: parameter -> kappa_handler -> refined_trace -> unit  
		
val export_musical_grid_to_xls: parameter -> kappa_handler -> error_log  -> string -> int -> int -> musical_grid  -> error_log				
val print_musical_grid: parameter -> kappa_handler -> error_log  -> musical_grid  -> error_log					   


										       

val from_none_to_weak_with_tick:
  D.S.PH.B.PB.CI.Po.K.H.parameter ->
  D.S.PH.B.PB.CI.Po.K.H.handler ->
  D.S.PH.B.PB.CI.Po.K.P.log_info ->
  Format.formatter ->  int -> (error_log * story_table)  ->
  D.S.PH.B.PB.CI.Po.K.refined_step list
  * D.S.PH.B.PB.CI.Po.K.P.log_info Mods.simulation_info option list ->
  (error_log * story_table)
    
val from_none_to_weak_with_tick_ext:
       D.S.PH.B.PB.CI.Po.K.H.parameter ->
           D.S.PH.B.PB.CI.Po.K.H.handler ->
           D.S.PH.B.PB.CI.Po.K.P.log_info ->
           Format.formatter ->
           int -> (error_log * story_table) ->
	   'd * 'e * 'f * D.S.PH.B.PB.CI.Po.K.refined_step list  * D.S.PH.B.PB.CI.Po.K.P.log_info Mods.simulation_info option list ->
           (error_log * story_table )
	     
		      
