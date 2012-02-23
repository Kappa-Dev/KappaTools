(**
  * blackboard.ml
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS 
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
  module K:Kappa_instantiation.Cflow_signature

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

  val predicates_of_test: (pre_blackboard -> K.test -> H.error_channel * pre_blackboard * (predicate_id*case_value) list) H.with_handler 
     
val predicates_of_action: (pre_blackboard -> K.action -> H.error_channel * pre_blackboard * (predicate_id*case_value) list) H.with_handler  


  val set: ((case_address * case_value option) -> blackboard  -> H.error_channel * blackboard) H.with_handler 
  val get: (case_address -> blackboard -> H.error_channel * case_value option) H.with_handler 

  val strictly_more_refined: case_value -> case_value -> bool

  (** stack *)
  type stack

  val record_modif: ((case_address * case_value option) -> stack -> H.error_channel * stack) H.with_handler
  val branch: (blackboard -> stack -> H.error_channel *blackboard * stack) H.with_handler 
  val reset: (stack -> H.error_channel * stack * (case_address * case_value option) list) H.with_handler 
 
  (** initialisation*)
  val init:  (H.error_channel * pre_blackboard) H.with_handler 
  val add_event: (Kappa_instantiation.Cflow_linker.refined_event -> pre_blackboard -> H.error_channel * pre_blackboard) H.with_handler
  val add_case: (case_address -> pre_blackboard -> H.error_channel * pre_blackboard) H.with_handler 
  val finalize: (pre_blackboard -> H.error_channel * blackboard) H.with_handler 

  (** heuristics *)
 
  val next_choice: (blackboard -> H.error_channel * instruction list) H.with_handler 
  val propagation_heuristic: (blackboard -> instruction  -> instruction list -> H.error_channel * instruction list) H.with_handler 
  val apply_instruction: (blackboard -> instruction -> instruction list -> (H.error_channel * (case_address*case_value) list * instruction list)) H.with_handler 


  (** output result*)
  type result 

  (** iteration*)
  val is_maximal_solution: (blackboard -> H.error_channel * bool) H.with_handler 

  (** exporting result*)
  val translate_blackboard: (blackboard -> H.error_channel * result) H.with_handler 

  (**pretty printing*)
  val print_blackboard:(blackboard -> H.error_channel) H.with_handler 
  
end

module Blackboard = 
  (struct 
     module H = Cflow_handler.Cflow_handler
     module K = Kappa_instantiation.Cflow_linker 

     (** blackboard matrix*) 

     type event_id = int 
     type event_short_id = int 

     (** blackboard predicates*)

     
     type predicate_id = int
     type predicate_info = 
       | Here of K.agent_id  
       | Bound_site of K.agent_id * K.site_name
       | Internal_state of K.agent_id * K.site_name 
       | Fictitious of int 
	 
     type case_address = 
	     {
	       column_id:predicate_id; 
	       row_id:   event_id;
	     }
	       
     type case_value = 
       | Point_to of event_id 
       | Counter of int 
       | Internal_state_is of K.internal_state
       | Undefined 
       | Present
       | Free 
       | Bound
       | Bound_to of predicate_id * K.agent_id * K.site_name 
       | Bound_to_type of K.agent_id * K.site_name
       | Defined
       | Unknown 

     let strictly_more_refined x y = 
       match y  
       with 
         | Undefined 
         | Counter _ 
         | Internal_state_is _ 
         | Present 
         | Free 
         | Bound_to (_) -> false
         | Bound_to_type (ag,s) -> 
           begin 
             match x 
             with 
               | Bound_to(_,ag',s') when ag=ag' && s=s' -> true
               | _ -> false 
           end
         | Point_to _ -> 
           begin
             match x 
             with 
               | Point_to _ -> true
               | _ -> false 
             end
         | Bound -> 
           begin
             match x 
             with 
               | Bound_to _ | Bound_to_type _ -> true
               | _ -> false
           end
         | Defined -> 
             begin
               match x 
               with 
                 | Defined | Undefined -> false
                 | _ -> true
             end
         | Unknown -> 
           begin
             match x
             with 
               | Unknown -> false
               | _ -> true 
           end

     let conj parameter handler error x y = 
       if x=y or strictly_more_refined x y
       then error,x 
       else if strictly_more_refined y x  
       then error,y
       else 
         let error_list,error = 
              H.create_error parameter handler error (Some "blackbord.ml") None (Some "conj") (Some "174") (Some "Try to compare incompare site states") Exit
         in 
         H.raise_error 
           parameter 
           handler 
           error_list 
           stderr 
           error 
           x 
         


     type case_info_static  = 
	 {
	   row_short_id: event_short_id;
	   state_before: case_value;
           state_after: case_value
	 }

     type case_info_dynamic = 
         {
           pointer_previous: event_short_id;
           pointer_next: event_short_id;
         }
          
     type case_info = 
         {
           static: case_info_static;
           dynamic: case_info_dynamic
         }
       
     let null_pointer = -1 

     let dummy_case_info_static = 
       {
	 row_short_id = null_pointer ;
	 state_before = Undefined ;
	 state_after = Undefined
       }
     
     let dummy_case_info_dynamic = 
       { 
         pointer_previous = null_pointer;
         pointer_next = null_pointer
       }

     let dummy_case_info = 
       {
         static = dummy_case_info_static ;
         dynamic = dummy_case_info_dynamic
       }

     (** propagation request *)

     type instruction = unit

     (** maps and sets *)
     module PredicateMap = Map.Make (struct type t = predicate_info let compare = compare end)
     module PredicateSet = Set.Make (struct type t = predicate_info let compare = compare end)
     module PredicateIdSet = Set.Make (struct type t = predicate_id let compare = compare end)
     module PredicateIdMap = Map.Make (struct type t = predicate_id let compare = compare end)
     module EidMap = Map.Make (struct type t = event_id let compare = compare end)
       
    
     (** blackboard*)
	 

     type pre_blackboard = 
	 {
           pre_events_by_column: (event_id*case_value*(case_value*case_value) list) array;
	   pre_nevents: event_id;
	   pre_ncolumn: predicate_id;
	   pre_column_map: predicate_id PredicateMap.t;
	   pre_columns_of_eid: predicate_id list array;
	   pre_row_short_id_map : event_short_id EidMap.t array;
	   pre_previous_binding: PredicateIdSet.t array;
	   predicate_id_list_of_predicate_id: PredicateSet.t array;
         } 

           

 (*blackboard during its construction*)
  


     type blackboard = pre_blackboard 

     let rec bind parameter handler error blackboard predicate ag_id =
       let error,blackboard,eid = 
         allocate parameter handler error blackboard (Here ag_id) ag_id
       in 
       let old_set = 
         try 
           Array.get blackboard.predicate_id_list_of_predicate_id eid
         with 
             Not_found -> 
               PredicateSet.empty
       in 
       let new_set = 
         PredicateSet.add predicate old_set 
       in 
         try 
           let _ = Array.set blackboard.predicate_id_list_of_predicate_id eid new_set in 
           error,blackboard 
         with 
             Not_found -> raise Exit 
     and 
         allocate parameter handler error blackboard predicate ag_id = 
       let map = blackboard.pre_column_map in 
       try 
         let eid = PredicateMap.find predicate map in
         error,blackboard,eid
       with 
           Not_found -> 
             let eid'= blackboard.pre_ncolumn + 1 in 
             let map' = PredicateMap.add predicate eid' map in 
             let error,blackboard = 
               bind 
                 parameter 
                 handler 
                 error 
                 {blackboard with pre_ncolumn = eid' ; pre_column_map = map'}
                 predicate
                 ag_id 
             in 
               error,blackboard,eid' 
                 
     let free_agent parameter handler error blackboard agent_id = 
       let error,blackboard,predicate_id = 
         allocate parameter handler error blackboard (Here agent_id) agent_id in 
       let set = 
         try 
           Array.get blackboard.predicate_id_list_of_predicate_id predicate_id
         with 
             _ -> raise Exit
       in 
       let map = 
         PredicateSet.fold 
           (fun predicate map -> 
             PredicateMap.remove 
               predicate 
               map)
           set 
           blackboard.pre_column_map
       in 
       error,{blackboard with pre_column_map = map}

     
     let predicates_of_action parameter handler error blackboard action = 
       match action with 
         | K.Create (ag,interface) -> 
           let ag_id = K.agent_id_of_agent ag in
           let error,blackboard,predicate_id = allocate parameter handler error blackboard (Here ag_id) ag_id in   
           List.fold_left 
             (fun (error,blackboard,list) (s_id,opt) -> 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Bound_site(ag_id,s_id)) ag_id in 
               let list = (predicate_id,Free)::list in 
                 match opt 
                 with 
                   | None -> error,blackboard,list
                   | Some x -> 
                     let error,blackboard,predicate_id = allocate parameter handler error blackboard (Internal_state (ag_id,s_id)) ag_id in 
                     error,blackboard,(predicate_id,Internal_state_is x)::list
             )
             (error,blackboard,[predicate_id,Present])
             interface
         | K.Mod_internal (site,int)  -> 
           let error,blackboard,predicate_id = allocate parameter handler error blackboard (Internal_state (K.agent_id_of_site site,K.site_name_of_site site)) (K.agent_id_of_site site) in 
           error,blackboard,[predicate_id,Internal_state_is int]
         | K.Bind (s1,s2) -> 
           let ag_id1 = K.agent_id_of_site s1 in 
           let ag_id2 = K.agent_id_of_site s2 in 
           let site_id1 = K.site_name_of_site s1 in 
           let site_id2 = K.site_name_of_site s2 in 
           let error,blackboard,predicate_id1 = allocate parameter handler error blackboard (Bound_site (ag_id1,site_id1)) ag_id1 in 
           let error,blackboard,predicate_id2 = allocate parameter handler error blackboard (Bound_site (ag_id2,site_id2)) ag_id2 in 
           error,blackboard,
           [predicate_id1,Bound_to (predicate_id2,ag_id2,site_id2);
            predicate_id2,Bound_to (predicate_id1,ag_id1,site_id1)]
         | K.Unbind (s1,s2) ->
           let ag_id1 = K.agent_id_of_site s1 in 
           let ag_id2 = K.agent_id_of_site s2 in 
           let site_id1 = K.site_name_of_site s1 in 
           let site_id2 = K.site_name_of_site s2 in 
           let error,blackboard,predicate_id1 = allocate parameter handler error blackboard (Bound_site (ag_id1,site_id1)) ag_id1 in 
           let error,blackboard,predicate_id2 = allocate parameter handler error blackboard (Bound_site (ag_id2,site_id2)) ag_id2 in 
           error,blackboard,[predicate_id1,Free;predicate_id2,Free]
         | K.Free s -> 
           let ag_id = K.agent_id_of_site s in 
           let site_id = K.site_name_of_site s in 
           let error,blackboard,predicate_id = allocate parameter handler error blackboard (Bound_site (ag_id,site_id)) ag_id in     
           error,blackboard,[predicate_id,Free]
         | K.Remove ag -> 
           let ag_id = K.agent_id_of_agent ag in 
           let error,blackboard,predicate_id = allocate parameter handler error blackboard (Here ag_id) ag_id in 
           let error,blackboard = free_agent parameter handler error blackboard ag_id in 
           error,blackboard,[predicate_id,Undefined]
 

     let predicates_of_test  parameter handler error blackboard action = 
       error,blackboard,[]

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
      pre_events_by_column = Array.make 0 (0,Undefined,[]) ; 
      pre_nevents = 0 ;
      pre_ncolumn = 0 ;
      pre_column_map = PredicateMap.empty ; 
      pre_columns_of_eid = Array.make 0 [] ;
      pre_row_short_id_map = Array.make 0 (EidMap.empty);
      pre_previous_binding = Array.make 0 (PredicateIdSet.empty) ; 
      predicate_id_list_of_predicate_id = Array.make 0 (PredicateSet.empty) ;
    }

  let allocate parameter handler error event pre_blackboard = 
    error,pre_blackboard
 
  let add_event parameter handler error event blackboard = 
    let neid  = blackboard.pre_nevents+1 in 
    let test_list = K.tests_of_refined_event event in 
    let action_list = K.actions_of_refined_event event in 
    let blackboard = 
      { blackboard with 
        (*pre_events_by_column = Array.make 0 (0,Undefined,[]) ;*) 
        pre_nevents = neid+1; 
       (* pre_row_short_id_map = Array.make 0 (EidMap.empty);
        pre_previous_binding = Array.make 0 (PredicateIdSet.empty) ; 
        predicate_id_list_of_predicate_id = Array.make 0 (PredicateSet.empty) ;
       *) }
    in 
    error,blackboard 

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


      

