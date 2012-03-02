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
  module A:LargeArray.GenArray
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
  val add_step: (Kappa_instantiation.Cflow_linker.refined_step -> pre_blackboard -> H.error_channel * pre_blackboard) H.with_handler
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
  val print_preblackboard: (out_channel -> pre_blackboard -> H.error_channel) H.with_handler  
end

module Blackboard = 
  (struct 
     module H = Cflow_handler.Cflow_handler
     module K = Kappa_instantiation.Cflow_linker 
     module A = Mods.DynArray

     (** blackboard matrix*) 

     type step_id = int 
     type step_short_id = int 

     (** blackboard predicates*)

     
     type predicate_id = int
     type predicate_info = 
       | Here of K.agent_id  
       | Bound_site of K.agent_id * K.site_name
       | Internal_state of K.agent_id * K.site_name 
       | Fictitious of int 

     let print_predicate_info log x = 
       match x 
       with 
         | Here i -> Printf.fprintf log "Agent_Here %i \n" i
         | Bound_site (i,s) -> Printf.fprintf log "Binding_state (%i,%i) \n" i s 
         | Internal_state (i,s) -> Printf.fprintf log "Internal_state (%i,%i) \n" i s 
         | Fictitious (int) -> Printf.fprintf log "Fictitious %i \n" int 
    
     type case_address = 
	     {
	       column_id:predicate_id; 
	       row_id:   step_id;
	     }
	       
     type case_value = 
       | Point_to of step_id 
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

     let print_case_value log x = 
       match x 
       with 
         | Point_to step_id -> 
           Printf.fprintf log "Point_to %i \n" step_id 
         | Counter int ->  
           Printf.fprintf log "Counter %i \n" int
         | Internal_state_is internal_state -> 
           Printf.fprintf log "%i \n" internal_state  
         | Undefined -> 
           Printf.fprintf log "Undefined\n"
         | Present ->
           Printf.fprintf log "Present\n"
         | Free -> 
           Printf.fprintf log "Free\n" 
         | Bound -> 
           Printf.fprintf log "Bound\n" 
         | Bound_to (id,agent,site) ->
           Printf.fprintf log "Bound(%i,%i@%i)\n" id agent site
         | Bound_to_type (agent,site)-> 
           Printf.fprintf log "Bound(%i@%i)\n" agent site 
         | Defined -> 
           Printf.fprintf log "Defined\n" 
         | Unknown -> 
           Printf.fprintf log "Unknown\n" 

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
	   row_short_id: step_short_id;
	   state_before: case_value;
           state_after: case_value
	 }

     type case_info_dynamic = 
         {
           pointer_previous: step_short_id;
           pointer_next: step_short_id;
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
     module CaseValueSet = Set.Make (struct type t = case_value let compare = compare end)
     module PredicatsidSet = Set.Make (struct type t = predicate_id let compare = compare end)
     module PredicatsidMap = Map.Make (struct type t = predicate_id let compare = compare end)
     module SidMap = Map.Make (struct type t = step_id let compare = compare end)
       
    
     (** blackboard*)
	 

     type pre_blackboard = 
	 {
           pre_steps_by_column: (case_value*(case_value*case_value) list) A.t;
	   pre_nsteps: step_id;
	   pre_ncolumn: predicate_id;
	   pre_column_map: predicate_id PredicateMap.t;
	   pre_column_map_inv: predicate_info A.t;
           pre_columns_of_sid: predicate_id list A.t;
	   pre_row_short_id_map : step_short_id SidMap.t A.t;
	   predicate_id_list_related_to_predicate_id: PredicateSet.t A.t;
           history_of_case_values_to_predicate_id: CaseValueSet.t A.t;
         } 

     let print_predicate_id log blackboard i = 
         let predicate_info = A.get blackboard.pre_column_map_inv i in  
         let _ = Printf.fprintf log "Predicate: %i " i in 
         let _ = print_predicate_info log predicate_info in 
         ()
  
     let print_preblackboard parameter handler error log blackboard = 
       let _ = Printf.fprintf log "**\nPREBLACKBOARD\n**\n" in 
       let _ = Printf.fprintf log "*\n steps by column\n*\n" in 
       let _ = 
         A.iteri 
           (fun id (value,list) ->
             let _ = print_predicate_id log blackboard id  in
             let _ = print_case_value log value in 
             let _ = 
               List.iter 
                 (fun (test,action) -> 
                   let _ = Printf.fprintf log "TEST:   " in
                   let _ = print_case_value log test in 
                   let _ = Printf.fprintf log "ACTION: " in 
                   let _ = print_case_value log action in 
                   ())
                 list
             in 
             let _ = Printf.fprintf log "---\n" in 
             ())
           blackboard.pre_steps_by_column 
       in 
       let _ = Printf.fprintf log "*\n predicate_id related to the predicate \n*\n" in 
       let _ = 
         A.iteri 
           (fun i s -> 
             let _ = print_predicate_id log blackboard i in 
             let _ = 
               PredicateSet.iter
                 (fun s -> print_predicate_info log s)
                 s
             in 
             let _ = Printf.fprintf log "---\n" in 
             ()
           )
           blackboard.predicate_id_list_related_to_predicate_id 
       in 
       let _ = Printf.fprintf log "*\n past values of a predicate \n*\n" in 
       let _ = 
         A.iteri 
           (fun i s -> 
             let _ = print_predicate_id log blackboard i in 
             let _ = 
               CaseValueSet.iter 
                 (fun s -> print_case_value log s)
                 s
             in 
             let _ = Printf.fprintf log "---\n" in 
             ()
           )
           blackboard.history_of_case_values_to_predicate_id 
       in 
       let _ = Printf.fprintf log "**\n" in 
       error 

  


     type blackboard = pre_blackboard 

     let rec bind parameter handler error blackboard predicate ag_id =
       let error,blackboard,sid = 
         allocate parameter handler error blackboard (Here ag_id) ag_id
       in 
       let old_set = 
         try 
           A.get blackboard.predicate_id_list_related_to_predicate_id sid
         with 
             Not_found -> 
               PredicateSet.empty
       in 
       let new_set = 
         PredicateSet.add predicate old_set 
       in 
         try 
           let _ = A.set blackboard.predicate_id_list_related_to_predicate_id sid new_set in 
           error,blackboard 
         with 
             Not_found -> raise Exit 
     and 
         allocate parameter handler error blackboard predicate ag_id = 
       let map = blackboard.pre_column_map in 
       let map_inv = blackboard.pre_column_map_inv in 
       try 
         let sid = PredicateMap.find predicate map in
         error,blackboard,sid
       with 
           Not_found -> 
             let sid'= blackboard.pre_ncolumn + 1 in 
             let map' = PredicateMap.add predicate sid' map in 
             let _  = A.set map_inv sid' predicate in 
             let map_inv' = map_inv in 
             let error,blackboard = 
               bind 
                 parameter 
                 handler 
                 error 
                 {blackboard 
                  with 
                    pre_ncolumn = sid' ; 
                    pre_column_map = map' ;
                    pre_column_map_inv = map_inv' 
                 }
                 predicate
                 ag_id 
             in 
               error,blackboard,sid' 
                 
     let free_agent parameter handler error blackboard agent_id = 
       let error,blackboard,predicate_id = 
         allocate parameter handler error blackboard (Here agent_id) agent_id in 
       let set = 
         try 
           A.get blackboard.predicate_id_list_related_to_predicate_id predicate_id
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
      pre_steps_by_column = A.make 1 (Undefined,[]) ; 
      pre_nsteps = 0 ;
      pre_ncolumn = 0 ;
      pre_column_map = PredicateMap.empty ; 
      pre_column_map_inv = A.make 1 (Fictitious 0) ; 
      pre_columns_of_sid = A.make 1 [] ;
      pre_row_short_id_map = A.make 1 (SidMap.empty);
      history_of_case_values_to_predicate_id = A.make 1 CaseValueSet.empty;
      predicate_id_list_related_to_predicate_id = A.make 1 PredicateSet.empty ;
    }

  let allocate parameter handler error step pre_blackboard = 
    error,pre_blackboard
 
  let add_step parameter handler error step blackboard = 
    let nsid  = blackboard.pre_nsteps+1 in 
    let test_list = K.tests_of_refined_step step in 
    let action_list,_ = K.actions_of_refined_step step in 
    let build_map list map = 
      List.fold_left 
        (fun map (id,value) -> PredicatsidMap.add id value map)
        map 
        list 
    in 
    let fadd pid p map = 
      let old = A.get map pid in 
      A.set map pid (CaseValueSet.add p old) 
    in 
    let error,blackboard,test_map = 
      List.fold_left 
        (fun (error,blackboard,map) test -> 
          let error,blackboard,test_list = predicates_of_test parameter handler error blackboard test in
          error,blackboard,build_map test_list map)
        (error,blackboard,PredicatsidMap.empty)
        test_list in 
    let error,blackboard,action_map = 
      List.fold_left 
        (fun (error,blackboard,map) action -> 
          let error,blackboard,action_list = predicates_of_action parameter handler error blackboard action in 
          let _ = 
            List.iter 
              (fun (pid,p) -> 
                fadd pid p blackboard.history_of_case_values_to_predicate_id)
              action_list 
          in
              error,blackboard,build_map action_list map)
        (error,blackboard,PredicatsidMap.empty)
        action_list in 
    let g x = 
      match x 
      with 
        | None -> Undefined
        | Some x -> x
    in 
    let merged_map = 
      PredicatsidMap.merge 
        (fun _ test action -> Some(g test,g action))
        test_map
        action_map 
    in 
    let pre_steps_by_column = 
      PredicatsidMap.fold 
        (fun id (test,action) map -> 
          begin 
            let value,list = 
              A.get map id  
            in 
            let value' = 
              match action
              with 
                | Undefined -> value
                | x -> x
            in 
            let _ = A.set map id (value',(test,action)::list)
            in map
          end)
        merged_map
        blackboard.pre_steps_by_column 
    in 
    let blackboard = 
      { 
        blackboard with 
          pre_steps_by_column = pre_steps_by_column; 
          pre_nsteps = nsid+1; 
      }
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


      

