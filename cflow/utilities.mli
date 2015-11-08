(**
  * utilities.mli  
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

module D:Dag.Dag
	   
type error_log = D.S.PH.B.PB.CI.Po.K.H.error_channel
type parameter = D.S.PH.B.PB.CI.Po.K.H.parameter 
type kappa_handler = D.S.PH.B.PB.CI.Po.K.H.handler 
type profiling_info = D.S.PH.B.PB.CI.Po.K.P.log_info

type refined_trace = D.S.PH.B.PB.CI.Po.K.refined_step list
type cflow_grid = Causal.grid  
type musical_grid = D.S.PH.B.blackboard 
			
type ('a,'b,'c) remanent =  
  error_log * int * (bool * int * int) *
    D.S.PH.B.blackboard *
      (D.prehash *
         (Causal.grid * D.graph * 'a option *
            ('b * D.S.PH.update_order list * refined_trace) *
              refined_trace * 'c Mods.simulation_info option list)
           list)
        list * int


(** error_init is an empty log of errors *)
val error_init: D.S.PH.B.PB.CI.Po.K.H.error_channel



(** Trace local simplification *)		  
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

(** remove_pseudo_inverse_events removes pseudo inverse *)
val remove_pseudo_inverse_events: parameter -> kappa_handler -> error_log -> refined_trace -> error_log * (refined_trace * int)

(** causal flows *)
val convert_trace_into_grid_while_trusting_side_effects: refined_trace -> kappa_handler -> cflow_grid 

													    
(** Musical processing *)
val convert_trace_into_musical_notation: parameter -> kappa_handler -> error_log -> profiling_info -> refined_trace -> error_log * profiling_info * musical_grid 													    
(** Profiling *)
																		      
module Profiling = D.S.PH.B.PB.CI.Po.K.P												    

(** Print utilities *)													    
val print_trace: parameter -> kappa_handler -> refined_trace -> unit  
										   
val from_none_to_weak_with_tick:
  D.S.PH.B.PB.CI.Po.K.H.parameter ->
  D.S.PH.B.PB.CI.Po.K.H.handler ->
  D.S.PH.B.PB.CI.Po.K.P.log_info ->
  Format.formatter ->
  int ->
  ('a,'b,'c) remanent ->
  ('b * 'd * D.S.PH.B.PB.CI.Po.K.refined_step list) * 'e * 'c Mods.simulation_info option list ->
  ('a,'b,'c) remanent
    
val from_none_to_weak_with_tick_ext:
       D.S.PH.B.PB.CI.Po.K.H.parameter ->
           D.S.PH.B.PB.CI.Po.K.H.handler ->
           D.S.PH.B.PB.CI.Po.K.P.log_info ->
           Format.formatter ->
           int ->
           ('a,'b,'c) remanent ->
	   'd * 'e * 'f * ('b * 'g * D.S.PH.B.PB.CI.Po.K.refined_step list) *
           'h * 'c Mods.simulation_info option list ->
           ('a,'b,'c) remanent
	     
		      
