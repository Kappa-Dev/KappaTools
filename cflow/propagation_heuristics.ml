(**
  * propagation_heuristic.ml
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS 
  *  
  * Creation: 05/09/2011
  * Last modification: 05/09/2011
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module type Blackboard = 
  sig
    module H:Cflow_handler.Cflow_handler
     
  end 

type agent 
type site 

type event = int 
type short_event = int 
type wire 

module EventMap = MapExt.Make (struct type t = event let compare = compare end)
type case_address =  wire * short_event

type wire_type = 
  | Here of agent 
  | Internal_state of agent * site 
  | Bound_or_not of agent * site 
  | Bound_to of agent * site 

type case_info = 
    { 
      wire_type: wire_type ; 
      event_before: event option ; 
      event_after: event option ;
      short_event_after : short_event option ;
      short_event_before : short_event option ;
    }

type case_value = 
  | Counter of int 
  | Internat_state of string 
  | Undefined 
  | Present
  | Free 
  | Bound 
  | Bound_to of wire * agent * site
  | Bound_to_type of agent * site
  | Defined

let is_equal pattern state = pattern = state 
let is_compatible pattern state = 
  match pattern
  with 
    | Undefined -> state=Undefined
    | Defined -> not (state=Undefined)
    | Bound_to_type (ag,site) -> 
	begin
	  match state 
	  with 
	    | Bound_to (_,ag',site') -> ag=ag' & site=site' 
	    | _ -> false
	end 
    | Bound -> 
	begin
	  match state
	  with 
	    | Bound_to (_) -> true
	    | _ -> false 
	end 
    | _ -> is_equal pattern state

let is_compatible pattern state = 
  match pattern
  with 
    | Undefined -> state=Undefined
    | Defined -> not (state=Undefined)
    | Bound_to_type (ag,site) -> 
	begin
	  match state 
	  with 
	    | Bound_to (_,ag',site') -> ag=ag' & site=site' 
	    | _ -> false
	end 
    | Bound -> 
	begin
	  match state
	  with 
	    | Bound_to (_) -> true
	    | _ -> false 
	end 
    | _ -> is_equal pattern state

type instruction = 
  | Keep_event of event
  | Discard_event of event 
  | Propagate_up of case_address * case_value 
  | Propagate_down of case_address * case_value 
  | Decrease_counter of case_address 

type blackboard = 
    {
      matrix: case_value array array; 
      event_to_quark_list: case_address list array;
      quark_to_mod_list: (case_value*case_value)  EventMap.t array;
      unsolved_events:int;
      unsolved_events_list:event list;

      
    }
