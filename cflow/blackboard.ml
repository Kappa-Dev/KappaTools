(**
  * blackboard.ml
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS 
  *  
  * Creation: 06/09/2011
  * Last modification: 19/10/2011
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

  (** blackboard matrix *)
  type case_info
  type case_value 
  type case_address  

  
  
  (** blackboard predicates*)

  type predicate_id
  type predicate_info

  (** propagation request *)

  type instruction 

  (** blackboard*)

  type pre_blackboard  (*blackboard during its construction*)
  type blackboard      (*blackboard, once finalized*)

  val set: ((case_address * case_value option) -> blackboard  -> H.error * blackboard) H.with_handler 
  val get: (case_address -> blackboard -> H.error * case_value option) H.with_handler 


  (** stack *)
  type stack

  val record_modif: ((case_address * case_value option) -> stack -> H.error * stack) H.with_handler
  val branch: (blackboard -> stack -> H.error *blackboard * stack) H.with_handler 
  val reset: (stack -> H.error * stack * (case_address * case_value option) list) H.with_handler 
 
  (** initialisation*)
  val init:  (H.error * pre_blackboard) H.with_handler 
  val add_case: (case_address -> pre_blackboard -> H.error * pre_blackboard) H.with_handler 
  val finalize: (pre_blackboard -> H.error * blackboard) H.with_handler 

  (** heuristics *)
 
  val next_choice: (blackboard -> H.error * instruction list) H.with_handler 
  val propagation_heuristic: (blackboard -> instruction  -> instruction list -> H.error * instruction list) H.with_handler 
  val apply_instruction: (blackboard -> instruction -> instruction list -> (H.error * (case_address*case_value) list * instruction list)) H.with_handler 


  (** output result*)
  type result 

  (** iteration*)
  val is_maximal_solution: (blackboard -> H.error * bool) H.with_handler 

  (** exporting result*)
  val translate_blackboard: (blackboard -> H.error * result) H.with_handler 

  (**pretty printing*)
  val print_blackboard:(blackboard -> H.error) H.with_handler 
  
end

module Blackboard = 
  (struct 
     module H = Cflow_handler.Cflow_handler

     (** blackboard matrix*) 

     type event_id = int 
     type event_short_id = int 

     (** blackboard predicates*)

     type agent = int 
     type site = int
     type predicate_id = int
     type predicate_info = 
       | Here of agent  
       | Bound_type of agent * site 
       | Bound_or_not of agent * site 
       | Bound_to of agent * site 
	 
     type case_address = 
	     {
	       column_id:predicate_id; 
	       row_id:   event_id;
	     }
	       
     type case_value = 
       | Counter of int 
       | Internat_state of string 
       | Undefined 
       | Present
       | Free 
       | Bound 
       | Bound_to of predicate_id * agent * site
       | Bound_to_type of agent * site
       | Defined

     type case_info = 
	 {
	   row_short_id: event_short_id;
	   state_before: case_value option;
	   state_after: case_value option
	 }

     let dummy_case_info = 
       {
	 row_short_id = -1 ;
	 state_before = None ;
	 state_after = None 
       }
     

     (** propagation request *)

     type instruction = unit

     (** maps and sets *)
     module PredicateMap = Map.Make (struct type t = predicate_info let compare = compare end)
     module PredicateIdSet = Set.Make (struct type t = predicate_id let compare = compare end)
     module PredicateIdMap = Map.Make (struct type t = predicate_id let compare = compare end)
     module EidMap = Map.Make (struct type t = event_id let compare = compare end)

     (** blackboard*)
	 

     type pre_blackboard = 
	 {
	   pre_sparse_matrix: case_info array array ;
	   pre_nevents_by_column: event_id array;
	   pre_nevents: event_id;
	   pre_ncolumn: predicate_id;
	   pre_column_map: predicate_id PredicateMap.t;
	   pre_columns_of_eid: predicate_id list array; 
	   pre_row_short_id_map : event_short_id EidMap.t array;
	   pre_previous_binding: PredicateIdSet.t array;
	   pre_state: case_value PredicateIdMap.t 
	 }

 (*blackboard during its construction*)
  type blackboard = pre_blackboard 

  let set parameter handler error (case_address,case_value_opt) blackboard = error,blackboard 
 
  let get parameter handler error case_address blackboard = error,None 

  (** stack *)
  type stack = (case_address*case_value option) list 

  let record_modif parameter handler error info stack = error,info::stack

  let branch parameter handler error blackboard stack = error,blackboard,[]

  let reset parameter handler error stack = error,[],stack 
 
  (** initialisation*)
  let init parameter handler error = 
    error, 
    {
      pre_sparse_matrix = Array.make 0 (Array.make 0 dummy_case_info);
      pre_nevents_by_column = Array.make 0 0 ;
      pre_nevents = 0 ;
      pre_ncolumn = 0 ;
      pre_column_map = PredicateMap.empty ; 
      pre_columns_of_eid = Array.make 0 [] ;
      pre_row_short_id_map = Array.make 0 (EidMap.empty);
      pre_previous_binding = Array.make 0 (PredicateIdSet.empty) ; 
      pre_state = PredicateIdMap.empty ;
    }


  let add_case parameter handler error case_address pre_blackboard = error,pre_blackboard 

  let finalize parameter handler error pre_blackboard = error,pre_blackboard 

  (** heuristics *)
 
  let next_choice parameter handler error blackboard = error,[]
    
  let propagation_heuristic parameter handler error blackboard instruction instruction_list = error,instruction_list 

  let apply_instruction parameter handler error blackboard instruction instruction_list = error,[],instruction_list 

  (** output result*)
  type result = ()

  (** iteration*)
  let is_maximal_solution parameter handler error blackboard = error,false 

  (** exporting result*)
  let translate_blackboard parameter handler error blackboard = error,()

  (**pretty printing*)
  let print_blackboard paramter handler error blackboard = error

   end:Blackboard)


      

