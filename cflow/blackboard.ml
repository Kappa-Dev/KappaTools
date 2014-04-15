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
  * Last modification: 03/08/2013
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011,2012,2013 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let debug_mode = false

module type Blackboard = 
sig 
  module PB:Blackboard_generation.PreBlackboard 

  (** blackboard matrix *)
  type event_case_address 
  type case_info
  type case_value 
  type case_address  
  type pointer
 
  (** blackboard*)

  type blackboard      (*blackboard, once finalized*)
  type assign_result (*= Fail | Success | Ignore *)

  val is_failed: assign_result -> bool
  val is_succeeded: assign_result -> bool
  val is_ignored: assign_result -> bool 
  val success: assign_result
  val ignore: assign_result
  val fail: assign_result 

  val predicate_id_of_case_address: event_case_address -> PB.predicate_id 
  val build_pointer: PB.step_short_id -> pointer 
  val is_before_blackboard: pointer -> bool 
  val get_event: blackboard -> int -> PB.CI.Po.K.refined_step 
  val get_n_eid: blackboard -> int 
  val get_npredicate_id: blackboard -> int 
  val get_n_unresolved_events_of_pid_by_level: blackboard -> PB.predicate_id -> Priority.level -> int 
  val get_n_unresolved_events_of_pid: blackboard -> PB.predicate_id  -> int 
    
  val get_n_unresolved_events: blackboard -> int 
  val get_first_linked_event: blackboard -> PB.predicate_id -> int option 
  val get_last_linked_event: blackboard -> PB.predicate_id -> int option 
  val get_stack_depth: blackboard -> int 
  val case_address_of_case_event_address : event_case_address -> case_address 
  val predicate_value_of_case_value: (case_value -> PB.CI.Po.K.H.error_channel * PB.predicate_value) PB.CI.Po.K.H.with_handler 
  val follow_pointer_up: (blackboard -> event_case_address -> PB.CI.Po.K.H.error_channel * event_case_address) PB.CI.Po.K.H.with_handler 
  val follow_pointer_down: (blackboard -> event_case_address -> PB.CI.Po.K.H.error_channel * event_case_address) PB.CI.Po.K.H.with_handler 
  val is_boundary: (blackboard -> event_case_address -> PB.CI.Po.K.H.error_channel * bool) PB.CI.Po.K.H.with_handler  

  val build_event_case_address: PB.predicate_id -> pointer -> event_case_address  
  val exist_case: (blackboard -> event_case_address -> PB.CI.Po.K.H.error_channel * bool option) PB.CI.Po.K.H.with_handler   
  val get_static: (blackboard -> event_case_address -> PB.CI.Po.K.H.error_channel * (PB.step_short_id * PB.step_id * PB.predicate_value * PB.predicate_value)) PB.CI.Po.K.H.with_handler 
            
  val set: (case_address -> case_value -> blackboard  -> PB.CI.Po.K.H.error_channel * blackboard) PB.CI.Po.K.H.with_handler 
  val get: (case_address -> blackboard -> PB.CI.Po.K.H.error_channel * case_value) PB.CI.Po.K.H.with_handler 
  val dec: (case_address -> blackboard -> PB.CI.Po.K.H.error_channel * blackboard) PB.CI.Po.K.H.with_handler
  val overwrite: (case_address -> case_value -> blackboard -> PB.CI.Po.K.H.error_channel * blackboard) PB.CI.Po.K.H.with_handler  
  val refine: (case_address -> case_value -> blackboard -> PB.CI.Po.K.H.error_channel * blackboard * assign_result) PB.CI.Po.K.H.with_handler  
  val branch: (PB.CI.Po.K.P.log_info -> blackboard -> PB.CI.Po.K.H.error_channel * PB.CI.Po.K.P.log_info * blackboard) PB.CI.Po.K.H.with_handler 
  val reset_last_branching: (PB.CI.Po.K.P.log_info -> blackboard -> PB.CI.Po.K.H.error_channel * PB.CI.Po.K.P.log_info * blackboard ) PB.CI.Po.K.H.with_handler 
  val reset_init: (PB.CI.Po.K.P.log_info -> blackboard -> PB.CI.Po.K.H.error_channel * PB.CI.Po.K.P.log_info * blackboard) PB.CI.Po.K.H.with_handler 

  (** initialisation*)
  val import:  (PB.CI.Po.K.P.log_info -> PB.CI.Po.K.refined_step list -> PB.CI.Po.K.H.error_channel * PB.CI.Po.K.P.log_info * blackboard) PB.CI.Po.K.H.with_handler 


  (** output result*)
  type result = (PB.CI.Po.K.refined_step * PB.CI.Po.K.side_effect) list  
     
  (** iteration*)
  val is_maximal_solution: (blackboard -> PB.CI.Po.K.H.error_channel * bool) PB.CI.Po.K.H.with_handler 

  (** exporting result*)
  val translate_blackboard: (blackboard -> PB.CI.Po.K.H.error_channel * result) PB.CI.Po.K.H.with_handler 

  (**pretty printing*)
  val print_blackboard:(blackboard -> PB.CI.Po.K.H.error_channel) PB.CI.Po.K.H.with_handler 
  val export_blackboard_to_xls: (string -> int -> int -> blackboard -> PB.CI.Po.K.H.error_channel) PB.CI.Po.K.H.with_handler 
  val print_event_case_address:(blackboard ->  event_case_address -> PB.CI.Po.K.H.error_channel) PB.CI.Po.K.H.with_handler 
  val print_stack: (blackboard -> PB.CI.Po.K.H.error_channel) PB.CI.Po.K.H.with_handler 
  val exist: event_case_address -> case_address 
  val boolean: bool option -> case_value 
  val pointer_to_previous: event_case_address -> case_address 
  val pointer_to_next: event_case_address -> case_address 
  val pointer: event_case_address -> case_value 
  val value_after: event_case_address -> case_address 
  val case_list_of_eid: (blackboard -> PB.step_id -> PB.CI.Po.K.H.error_channel * event_case_address list) PB.CI.Po.K.H.with_handler 
  val state: PB.predicate_value -> case_value 
  val is_exist_event: PB.step_id -> case_address 
  val n_unresolved_events_at_level: Priority.level -> case_address
  val n_unresolved_events: case_address 
  val n_unresolved_events_in_column_at_level: event_case_address -> Priority.level -> case_address 
  val n_unresolved_events_in_column: event_case_address -> case_address 
  val forced_events: blackboard -> (PB.step_id list * unit Mods.simulation_info option) list 
  val side_effect_of_event: blackboard -> PB.step_id -> PB.CI.Po.K.side_effect
(*  val cut_predicate_id: (blackboard -> PB.predicate_id -> PB.CI.Po.K.H.error_channel *   blackboard) PB.CI.Po.K.H.with_handler *)
  val cut: (PB.CI.Po.K.P.log_info -> blackboard -> PB.step_id list -> PB.CI.Po.K.H.error_channel * PB.CI.Po.K.P.log_info * blackboard * PB.step_id list) PB.CI.Po.K.H.with_handler 
  val tick: PB.CI.Po.K.P.log_info -> bool * PB.CI.Po.K.P.log_info (* to do: move to the module PB.CI.Po.K.P*)
  val level_of_event: (blackboard -> PB.step_id -> PB.CI.Po.K.H.error_channel * Priority.level) PB.CI.Po.K.H.with_handler 
end

module Blackboard = 
  (struct 
    module PB = Blackboard_generation.Preblackboard 
    (** blackboard matrix*) 

    type assign_result = Fail | Success | Ignore 
    type pointer = PB.step_short_id 
        
   
    let success = Success
    let ignore = Ignore
    let fail = Fail 
      
    let is_ignored x = 
      match x 
      with 
        | Ignore -> true
        | _ -> false

    let is_failed x = 
      match x 
      with 
        | Fail -> true
        | _ -> false     


    let is_succeeded x = 
      match x 
      with 
        | Success -> true
        | _ -> false

    let null_pointer = -1 (*Null_pointer*) 
    let is_null_pointer x = x=null_pointer 
    let pointer_before_blackboard = 0 
    let is_before_blackboard x = x=0 (*Before_blackboard*)
    let build_pointer i = i 

    type event_case_address = 
	{
	  column_predicate_id:PB.predicate_id; 
	  row_short_event_id: pointer; 
	}

    let predicate_id_of_case_address x = x.column_predicate_id 

    let is_boundary parameter handler error blackboard event_address = 
      error,is_before_blackboard event_address.row_short_event_id 

    let build_event_case_address pid seid = 
      {
        column_predicate_id = pid ;
        row_short_event_id = seid 
      }
	
    type case_address = 
    | N_unresolved_events_in_column_at_level of int*int
    | N_unresolved_events_in_column of int 
    | Pointer_to_next of event_case_address 
    | Value_after of event_case_address 
    | Value_before of event_case_address 
    | Pointer_to_previous of event_case_address
    | N_unresolved_events 
    | N_unresolved_events_at_level of int 
    | Exist of event_case_address 
    | Keep_event of PB.step_id 
        
    let is_exist_event i = Keep_event i 
    let n_unresolved_events_in_column i = N_unresolved_events_in_column (i.column_predicate_id) 
    let n_unresolved_events_in_column_at_level i j = N_unresolved_events_in_column_at_level ((i.column_predicate_id),j)
    let pointer_to_next e = Pointer_to_next e 
    let value_after e = Value_after e 
    let value_before e = Value_before e 
    let pointer_to_previous e = Pointer_to_previous e 
    let n_unresolved_events = N_unresolved_events 
    let n_unresolved_events_at_level i = N_unresolved_events_at_level i
    let exist e = Exist e


    type case_value = 
      | State of PB.predicate_value 
      | Counter of int 
      | Pointer of pointer 
      | Boolean of bool option 

    let print_case_value parameter x = 
      match x 
      with 
        | State x -> 
          begin 
            let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "State! " in 
            let _ = PB.print_predicate_value parameter.PB.CI.Po.K.H.out_channel_err x in 
            let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\n" in 
            () 
          end
        | Counter i -> Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "Counter %i\n" i
        | Pointer i -> Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "Pointer %i\n" i 
        | Boolean b -> Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "Boolean %s\n" (match b with None -> "?" | Some true -> "true" | _ -> "false")
 
    let string_of_pointer seid = "event seid "^(string_of_int seid)
    let print_pointer log seid = 
       Printf.fprintf log "%s" (string_of_pointer seid)

    let state predicate_value = State predicate_value 
    let counter i = Counter i 
    let pointer p = Pointer p.row_short_event_id  
    let boolean b = Boolean b 

    let case_address_of_case_event_address event_address = 
      Value_after (event_address)
          
    let predicate_value_of_case_value parameter handler error case_value = 
      match case_value 
      with 
        | State x -> error,x 
        | _ ->    
          let _ = print_case_value parameter case_value in 
          let error_list,error = PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "predicate_value_of_case_value") (Some "226") (Some "wrong kinf of case_value in predicate_value_of_case_value") (failwith "predicate_value_of_case_value") in 
          PB.CI.Po.K.H.raise_error parameter handler error_list error PB.unknown

    type assignment = (case_address * case_value) 
        
    let bool_strictly_more_refined x y = 
      match x,y 
      with 
        | Some _ , None -> true 
        | _,_ -> false 

    let g p p2 string parameter handler error x y = 
      match 
        x,y 
      with 
        | State x,State y -> error,p x y 
        | Boolean x,Boolean y -> error,p2 x y
        | _,_  ->  
          let error_list,error = PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some string) (Some "246") (Some "Counters and/or Pointers should not be compared") (failwith "strictly_more_refined") in 
          PB.CI.Po.K.H.raise_error parameter handler error_list error false
            
    let strictly_more_refined = g PB.strictly_more_refined bool_strictly_more_refined "strictly_more_refined"
      
    type case_info_static  = 
	{
	  row_short_id: PB.step_short_id;
          event_id: PB.step_id; 
	  test: PB.predicate_value;
          action: PB.predicate_value
	}
          
    type case_info_dynamic = 
        {
          pointer_previous: pointer;
          pointer_next: pointer;
          state_after: PB.predicate_value;
          selected: bool option;
        }
          
    type case_info = 
        {
          static: case_info_static;
          dynamic: case_info_dynamic
        }

    let dummy_case_info_static = 
      {
	row_short_id = -1 ;
        event_id = -1 ;
	test = PB.unknown ;
	action = PB.unknown ;
      }
        
    let dummy_case_info_dynamic = 
      { 
        pointer_previous = null_pointer ;
        pointer_next = null_pointer ;
        state_after = PB.unknown ;  
        selected = None ;  
      }
        
    let correct_pointer seid size = 
      if seid < 0 
      then pointer_before_blackboard
      else if seid>=size 
      then size 
      else seid 
        
    let init_info_dynamic seid size= 
      { 
        pointer_previous = correct_pointer (seid-1) size ;
        pointer_next = correct_pointer (seid+1) size ;
        state_after = PB.unknown ; 
        selected = None ;
      }
        
    let init_info_static p_id seid (eid,_,test,action) = 
      {
	row_short_id = seid ;
        event_id = eid ;
	test = test ;
	action = action ;
      }
        
    let get_eid_of_triple (x,_,_,_) = x
    let dummy_case_info = 
      {
        static = dummy_case_info_static ;
        dynamic = dummy_case_info_dynamic
      }
        
    let init_case_info = 
      {dummy_case_info 
       with dynamic = 
          { dummy_case_info_dynamic 
            with 
              state_after = PB.undefined ; 
              pointer_previous = pointer_before_blackboard ;
              pointer_next = 1 }}
        
        
     let init_info p_id seid size triple = 
       {
         static = init_info_static p_id seid triple ;
         dynamic = init_info_dynamic seid size
       }
         
     (** blackboard *)
     type stack = assignment list 
     type blackboard = 
         {
           event: PB.CI.Po.K.refined_step PB.A.t;
           pre_column_map_inv: PB.predicate_info PB.A.t; (** maps each wire id to its wire label *)
           forced_events: (int list * unit Mods.simulation_info option) list;
           n_predicate_id: int ;
           n_eid:int;
           n_seid: int PB.A.t;
           current_stack: stack ; 
           stack: stack list ;
           blackboard: case_info PB.A.t PB.A.t ;
           selected_events: bool option PB.A.t ;
           weigth_of_predicate_id: int PB.A.t;
           weigth_of_predicate_id_by_level: int PB.A.t Priority.LevelMap.t; 
           used_predicate_id: bool PB.A.t ;  
           n_unresolved_events: int ;
           n_unresolved_events_by_level: int Priority.LevelMap.t; 
           last_linked_event_of_predicate_id: int PB.A.t;
           event_case_list: event_case_address list PB.A.t;
           side_effect_of_event: PB.CI.Po.K.side_effect PB.A.t;
           fictitious_observable: PB.step_id option;
           level_of_event: Priority.level PB.A.t;
         }
           
     let tick profiling_info = PB.CI.Po.K.P.tick profiling_info 
     let level_of_event parameter handler error blackboard eid = 
       try 
         error,PB.A.get blackboard.level_of_event eid 
       with 
         Not_found -> 
           let error_list,error = PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "level_of_event") (Some "373") (Some "Unknown event id") (failwith "Unknown event id")  in 
           PB.CI.Po.K.H.raise_error parameter handler error_list error Priority.default 
             
     let get_event blackboard k = PB.A.get blackboard.event k 
     let get_n_eid blackboard = blackboard.n_eid 
     let get_stack_depth blackboard = List.length blackboard.stack 
     let forced_events blackboard = blackboard.forced_events 
     let side_effect_of_event blackboard i = PB.A.get blackboard.side_effect_of_event i 

     let case_list_of_eid parameter handler error blackboard eid = 
       try 
         error,PB.A.get blackboard.event_case_list eid 
       with 
         | _ -> 
           let error_list,error = PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "case_list_of_eid") (Some "366") (Some "Out of bound") (failwith "Dereferencing null pointer")
           in 
           PB.CI.Po.K.H.raise_error parameter handler error_list error [] 

     let get_case parameter handler error case_address blackboard = 
       try 
         error,PB.A.get 
             (PB.A.get blackboard.blackboard case_address.column_predicate_id)
             (case_address.row_short_event_id) 
       with 
         | _ -> 
           let error_list,error = PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "get_case") (Some "377") (Some "Dereferencing null pointer") (failwith "Dereferencing null pointer")
           in 
           PB.CI.Po.K.H.raise_error parameter handler error_list error dummy_case_info 

     let get_static parameter handler error blackboard address = 
       let error,case = get_case parameter handler error address blackboard in 
       let static = case.static in 
       error,(static.row_short_id,static.event_id,static.test,static.action)

     let print_event_case_address parameter handler error blackboard case = 
       let error,(_,eid,_,_) = get_static parameter handler error blackboard case in 
      let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel "Event: %i, Predicate: %i\n" eid (predicate_id_of_case_address case) in 
      error

     let print_case_address parameter handler error blackboard x = 
      match x
      with 
      | N_unresolved_events_in_column_at_level (i,j) -> 
        let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "n_unresolved_events_in_pred %i %i\n" i j in 
         error
       | N_unresolved_events_in_column i -> 
         let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "n_unresolved_events_in_pred %i \n" i in 
         error 
       | Pointer_to_next e -> 
         let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "Pointer" in 
         print_event_case_address parameter handler error blackboard e  
       | Value_after e -> 
        let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "Value_after  " in   
        print_event_case_address parameter handler error blackboard e 
      | Value_before e -> 
        let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "Value_before " in 
        print_event_case_address parameter handler error blackboard e 
      | Pointer_to_previous e -> 
        let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "Pointer_before " in 
        print_event_case_address parameter handler error blackboard e 
      | N_unresolved_events_at_level i -> 
        let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "Unresolved_events_at_level %i" i in 
        error   
      | N_unresolved_events -> 
        let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "Unresolved_events" in 
        error 
      | Exist e -> 
        let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "Exist " in 
        print_event_case_address parameter handler error blackboard e 
      | Keep_event i -> 
        let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "Keep %i" i in 
        error 
      
     let get_npredicate_id blackboard = blackboard.n_predicate_id 
     let get_n_unresolved_events_of_pid_by_level blackboard pid level = 
       try 
         PB.A.get 
           (Priority.LevelMap.find level blackboard.weigth_of_predicate_id_by_level)  
           pid 
       with 
       | Not_found -> 0
     let get_n_unresolved_events_of_pid blackboard pid = 
       PB.A.get 
         blackboard.weigth_of_predicate_id  
         pid 
         
     let get_n_unresolved_events blackboard = blackboard.n_unresolved_events 
     let get_pointer_next case = case.dynamic.pointer_next 
     
     let follow_pointer_down parameter handler error blackboard address = 
       let error,case = get_case parameter handler error address blackboard in 
       error,{address with row_short_event_id = case.dynamic.pointer_next}

     let follow_pointer_up parameter handler error blackboard address = 
       let error,case = get_case parameter handler error address blackboard in 
       error,{address with row_short_event_id = case.dynamic.pointer_previous}

     let get_first_linked_event blackboard pid = 
       if pid <0 || pid >= blackboard.n_predicate_id 
       then 
         None 
       else 
         Some 0 

     let get_last_linked_event blackboard pid = 
       if pid<0 || pid >= blackboard.n_predicate_id 
       then 
         None 
       else 
         Some 
           (PB.A.get blackboard.last_linked_event_of_predicate_id pid) 

   (**pretty printing*)
     let print_known_case log pref inf suf case =
        let _ = Printf.fprintf log "%stest:" pref in  
        let _ = PB.print_predicate_value log case.static.test in 
        let _ = Printf.fprintf log "/eid:%i/action:" case.static.event_id in 
        let _ = PB.print_predicate_value log case.static.action in 
        let _ = Printf.fprintf log "%s" inf in 
        let _ = PB.print_predicate_value log case.dynamic.state_after in 
        let _ = Printf.fprintf log "%s" suf in 
        () 

     let print_case log case =
       let status = case.dynamic.selected in 
       match status 
       with 
         | Some false -> ()
         | Some true -> 
           print_known_case log "" " " " " case 
         | None -> 
           print_known_case log "?(" ") " " " case 

     let print_address parameter handler error blackboard address = 
       let log = parameter.PB.CI.Po.K.H.out_channel_err in 
       match address 
       with 
         | Keep_event i -> 
           let _ = Printf.fprintf log "Is the event %i selected ? " i in 
           error
         | Exist i -> 
           let _ = Printf.fprintf log "Is the case " in 
           let _ = print_event_case_address parameter handler error blackboard i in 
           let _ = Printf.fprintf log "selected ? " in 
           error 
         | N_unresolved_events_in_column_at_level (i,j) -> 
           let _ = Printf.fprintf log "Number of unresolved events for the predicate %i at level %i" i j in 
           error
         | N_unresolved_events_in_column i -> 
           let _ = Printf.fprintf log "Number of unresolved events for the predicate %i" i in 
           error
         | Pointer_to_next i -> 
           let _ = Printf.fprintf log "Prochain événement agissant sur "  in 
           let _ = print_event_case_address parameter handler error blackboard i in 
           error
         | Value_after i -> 
           let _ = Printf.fprintf log "Valeur après "  in 
           let _ = print_event_case_address parameter handler error blackboard i in 
           error
         | Value_before i -> 
           let _ = Printf.fprintf log "Valeur avant "  in 
           let _ = print_event_case_address parameter handler error blackboard i in 
           error
         | Pointer_to_previous i -> 
           let _ = Printf.fprintf log "Evenement précésent agissant sur " in 
           let _ = print_event_case_address parameter handler error blackboard i in 
           error
         | N_unresolved_events -> 
           let _ = Printf.fprintf log "Nombre d'événements non résolu" in 
           error
         | N_unresolved_events_at_level i -> 
           let _ = Printf.fprintf log "Nombre d'événements non résolu at level %i" i in 
           error
  
    
     let string_of_value value = 
        match value 
        with 
         | State pb -> PB.string_of_predicate_value pb 
         | Counter i -> "Counter "^(string_of_int i)
         | Pointer i -> string_of_pointer i 
         | Boolean bool -> 
             (match bool 
              with 
                | None -> "?" 
                | Some true -> "Yes"
                | Some false -> "No")

     let print_value log value = 
       match value 
       with 
         | State pb -> PB.print_predicate_value log pb 
         | Counter i -> Printf.fprintf log "Counter %i" i 
         | Pointer i -> print_pointer log i 
         | Boolean bool -> 
           Printf.fprintf log "%s"
             (match bool 
              with 
                | None -> "?" 
                | Some true -> "Yes"
                | Some false -> "No")


     let print_assignment parameter handler error blackboard (address,value)  = 
       let error  = print_address parameter handler error blackboard address in 
       let _ = print_value parameter.PB.CI.Po.K.H.out_channel_err value in 
       error 

     let print_blackboard parameter handler error blackboard = 
       let log = parameter.PB.CI.Po.K.H.out_channel_err in  
       let _ = Printf.fprintf log "**\nBLACKBOARD\n**\n" in 
       let _ = Printf.fprintf log "%i wires, %i events\n" blackboard.n_predicate_id blackboard.n_eid in 
       let _ = Printf.fprintf log "*wires:*\n" in 
       let err = ref error in 
       let _ = 
         PB.A.iteri
           (fun i array -> 
             let _ = Printf.fprintf log "%i" i in 
             let _ = 
               if PB.A.get blackboard.used_predicate_id i 
               then 
                 let _ = Printf.fprintf log "*wires %i: " i in
                 let rec aux j error = 
                   let case = PB.A.get array j in 
                   let _ = print_case log case in
                   let j' = get_pointer_next case in 
                   if j=j' then error
                 else aux j' error 
                 in 
                 let error = aux pointer_before_blackboard (!err) in 
                 let _ = err := error in 
                 () 
               else 
                 ()
             in 
             let _ = Printf.fprintf log "\n" in ())
           blackboard.blackboard 
       in 
       let error = !err in 
       let _ = Printf.fprintf log "*stacks*\n" in 
       let error = List.fold_left (fun error -> print_assignment parameter handler error blackboard) error (List.rev blackboard.current_stack) in 
       let _ = Printf.fprintf log "\n" in 
       let _ = 
         List.fold_left 
           (fun error stack -> 
             let error = List.fold_left (fun error -> print_assignment parameter handler error blackboard) error stack in 
             let _ = Printf.fprintf log "\n" in 
             error)
           error blackboard.stack 
       in 
       let _ = Printf.fprintf log "*selected_events*\n" in 
       let _ = 
         PB.A.iteri 
           (fun i bool -> 
             match bool 
             with 
               | None -> ()
               | Some b -> 
                 Printf.fprintf log "  Event:%i (%s)\n" i (if b then "KEPT" else "REMOVED"))
           blackboard.selected_events
       in 
       let _ = Printf.fprintf log "*unsolved_events*\n" in 
       let _ = Printf.fprintf log " %i\n" blackboard.n_unresolved_events in 
       let _ = Printf.fprintf log "*weight of predicate_id*\n" in 
       let _ = 
         PB.A.iteri 
           (Printf.fprintf log " %i:%i\n")
           blackboard.weigth_of_predicate_id
       in 
       let _ = Printf.fprintf log "*weight of predicate_id_by_level*\n" in 
       let _ = 
         Priority.LevelMap.iter 
           (fun l  -> 
             let _ = Printf.fprintf log " Level:%i\n" l in 
             PB.A.iteri 
               (Printf.fprintf log " %i:%i\n")
           )
           blackboard.weigth_of_predicate_id_by_level  
       in 
       let _ = Printf.fprintf log "**\n" in 
       let _ = flush log in 
       error

    

     (** propagation request *)

     let add_event eid (pid,seid) array level unsolved = 
       let event_case_address = build_event_case_address pid seid in 
       let old = PB.A.get array eid in 
       let unsolved = 
         try 
           Priority.LevelMap.add level ((Priority.LevelMap.find level unsolved)+1) unsolved 
         with 
           Not_found -> Priority.LevelMap.add level 1 unsolved
       in 
       PB.A.set array eid (event_case_address::old),unsolved 

     let empty_stack = []

     let import parameter handler error log_info pre_blackboard = 
       let error,n_predicates = PB.n_predicates parameter handler error pre_blackboard in  
       let error,n_events = PB.n_events parameter handler error pre_blackboard in 
       let stack = [] in 
       let current_stack = empty_stack in
       let event_case_list = PB.A.make n_events [] in 
       let n_seid = PB.A.make n_predicates 0 in 
       let unsolved_by_level = Priority.LevelMap.empty in 
       let blackboard = PB.A.make n_predicates (PB.A.make 1 dummy_case_info) in
       let weigth_of_predicate_id_by_level = 
         let rec aux k map = 
           if k<0 then map 
           else 
             aux (k-1) (Priority.LevelMap.add k (PB.A.create 0 0) map)
         in 
         let error,priority_max = 
           match 
             PB.CI.Po.K.H.get_priorities parameter 
           with 
           | Some x -> error,x.Priority.max_level
           | None -> 
             let error_list,error = PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "import") (Some "694") (Some "Compression mode has to been selected") (failwith "Compression mode has not been selected") in 
          PB.CI.Po.K.H.raise_error parameter handler error_list error Priority.zero

         in 
         aux priority_max Priority.LevelMap.empty
       in 
       let inc_depth level p_id = 
         try 
           let a = 
             Priority.LevelMap.find level weigth_of_predicate_id_by_level 
           in 
           let old = 
             try 
               PB.A.get a p_id
             with 
             | Not_found -> 0 
           in 
           PB.A.set a  p_id (old+1) 
         with 
         | Not_found -> ()
       in 

       let weigth_of_predicate_id = PB.A.create 0 0 in 
       let last_linked_event_of_predicate_id = PB.A.make n_predicates 0 in 
       let error = 
         let rec aux1 p_id error = 
           if p_id < 0 
           then error
           else 
             let error,size = PB.n_events_per_predicate parameter handler error pre_blackboard p_id in 
             let size = size + 1 in 
             let _ = PB.A.set last_linked_event_of_predicate_id p_id (size-1) in 
             let _ = PB.A.set weigth_of_predicate_id p_id (size-2) in 
             let _ = PB.A.set n_seid p_id size in 
             let error,list = PB.event_list_of_predicate parameter handler error pre_blackboard p_id in 
             let array = PB.A.make size dummy_case_info in 
             let _ = PB.A.set blackboard p_id array in 
             let rec aux2 seid l = 
               match l 
               with 
                 | [] -> 
                   let info = 
                     {dynamic = 
                         { 
                           pointer_previous = 0 ;
                           pointer_next = 1 ;
                           state_after = PB.undefined ;
                           selected = Some true 
                         };
                      static = 
                           {
                           row_short_id = 0 ; 
                           event_id = -1 ;
                           test = PB.unknown ;
                           action = PB.unknown ;
                           }}
                   in 
                   let _ = PB.A.set array 0 info in 
                   let info = 
                     {dynamic = 
                         {
                           pointer_previous = size-1 ;
                           pointer_next = size-1 ;
                           state_after = PB.unknown ;
                           selected = Some true 
                         } ;
                      static = 
                         {
                           row_short_id = size-1 ; 
                           event_id = -1 ;
                           test = PB.unknown ;
                           action = PB.unknown ;
                         }}     
                   in 
                   let _ = PB.A.set array (size-1)  info in 
                   () 
                 | triple::q -> 
                   let info = init_info p_id seid (size-1) triple in 
                   let eid = get_eid_of_triple triple in 
                   let error,events = 
                     PB.get_pre_event parameter handler error pre_blackboard  in 
                   let error,level = PB.get_level_of_event parameter handler error pre_blackboard eid in 
                   let _ = inc_depth level p_id in 
                   let _ = add_event eid (p_id,seid) event_case_list level Priority.LevelMap.empty in 
                   let _ = PB.A.set array seid info in 
                   aux2 (seid-1) q 
             in 
             let _ = aux2 (size-2) list in 
             aux1 (p_id-1) error 
         in aux1 (n_predicates-1) error 
       in 
       let error,forced_events = PB.mandatory_events parameter handler error pre_blackboard in 
       let error,event = PB.get_pre_event parameter handler error pre_blackboard in 
       let unsolved_by_level = 
         let rec aux k map = 
           if k=0 
           then 
             map
           else
             let error,level = PB.get_level_of_event parameter handler error pre_blackboard k in 
             let map = 
               try 
                 Priority.LevelMap.add level ((Priority.LevelMap.find level map)+1) map 
               with 
               | Not_found ->
                 Priority.LevelMap.add level 1 map
             in aux (k-1) map 
         in aux n_events unsolved_by_level 
       in 
       let error,side_effects = PB.get_side_effect parameter handler error pre_blackboard in 
       let error,fictitious_obs = PB.get_fictitious_observable parameter handler error pre_blackboard in 
       error,
       log_info,
       {
         event = event ;
         side_effect_of_event = side_effects ; 
         pre_column_map_inv = PB.get_pre_column_map_inv pre_blackboard; 
         level_of_event = PB.levels pre_blackboard;
         forced_events = forced_events;
         n_eid = n_events;
         n_seid = n_seid;
         event_case_list = event_case_list;
         last_linked_event_of_predicate_id = last_linked_event_of_predicate_id ;
         n_predicate_id = n_predicates; 
         current_stack=current_stack; 
         stack=stack;
         blackboard=blackboard;
         selected_events= PB.A.make n_events None; 
         weigth_of_predicate_id = weigth_of_predicate_id;
         weigth_of_predicate_id_by_level= weigth_of_predicate_id_by_level; 
         used_predicate_id = PB.A.make n_predicates true; 
         n_unresolved_events = n_events ; 
         n_unresolved_events_by_level = unsolved_by_level ;
         fictitious_observable = fictitious_obs ;
       }
         
   
     let exist_case parameter handler error blackboard case_address = 
       let error,info = get_case parameter handler error case_address blackboard in 
       error,info.dynamic.selected

     let set_case parameter handler error case_address case_value blackboard = 
       try 
         let _ = PB.A.set (PB.A.get blackboard.blackboard case_address.column_predicate_id) (case_address.row_short_event_id) case_value 
         in error,blackboard 
       with 
         | _ -> 
           let error_list,error = PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set_case") (Some "680") (Some "Dereferencing null pointer") (failwith "Dereferencing null pointer")
           in 
           PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard


     let set parameter handler error case_address case_value blackboard = 
       match 
         case_address 
       with 
          | N_unresolved_events_in_column_at_level (int,level) -> 
           begin 
             match case_value
             with 
               | Counter int2 -> 
                 begin 
                   try 
                     let a = Priority.LevelMap.find level blackboard.weigth_of_predicate_id_by_level in 
                     let _ = 
                       PB.A.set a int int2 in 
                     error,blackboard 
                   with 
                     Not_found -> 
                       begin
                         let error_list,error = 
                           PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "698") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                         in 
                         PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard 
                       end 
                 end
               | _ -> 
                 let error_list,error = 
                   PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "698") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard 
           end 
       | N_unresolved_events_in_column int -> 
           begin 
             match case_value
             with 
               | Counter int2 -> 
                 let _ = PB.A.set blackboard.weigth_of_predicate_id int int2 in 
                 error,blackboard 
               | _ -> 
                 let error_list,error = 
                   PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "698") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard 
           end 
         | Pointer_to_next case_address -> 
              begin 
                match case_value
                with 
                  | Pointer int2 -> 
                    let error,old = get_case parameter handler error case_address blackboard in 
                    let case_value = {old with dynamic = {old.dynamic with pointer_next = int2}} in 
                    let error,blackboard = set_case parameter handler error case_address case_value blackboard in 
                    error,blackboard 
                  | _ -> 
                    let error_list,error = 
                   PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "713") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard 
              end 
         | Value_after case_address -> 
           begin 
             match case_value
             with 
               | State state -> 
                 let error,old = get_case parameter handler error case_address blackboard in 
                 let case_value = {old with dynamic = {old.dynamic with state_after = state}} in 
                 let error,blackboard = set_case parameter handler error case_address case_value blackboard in 
                 error,blackboard 
               | _ -> 
                 let error_list,error = 
                   PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "728") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard 
           end 
         | Value_before case_address -> 
           let error_list,error = 
             PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "734") (Some "Blackboard.set should not be called with value_before") (failwith "Incompatible address in function Blackboard.set")
           in 
           PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard 
         | Pointer_to_previous case_address -> 
               begin 
                match case_value
                with 
                  | Pointer int2 -> 
                    let error,old = get_case parameter handler error case_address blackboard in 
                    let case_value = {old with dynamic = {old.dynamic with pointer_previous = int2}} in 
                    let error,blackboard = set_case parameter handler error case_address case_value blackboard in 
                    error,blackboard 
                  | _ -> 
                    let error_list,error = 
                   PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "748") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard 
              end 
         | N_unresolved_events -> 
           begin 
             match case_value
             with 
               | Counter int -> 
                 error,{blackboard with n_unresolved_events = int}
               | _ -> 
                 let error_list,error = 
                   PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "760") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard 
           end 
         | N_unresolved_events_at_level level  -> 
           begin 
             match case_value
             with 
               | Counter int -> 
                 error,{blackboard 
                        with n_unresolved_events_by_level = 
                     Priority.LevelMap.add level int blackboard.n_unresolved_events_by_level}
               | _ -> 
                 let error_list,error = 
                   PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "760") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard 
           end 
         | Keep_event step_id -> 
           begin 
            match case_value
             with 
               | Boolean b -> 
                 let _ = PB.A.set blackboard.selected_events step_id b in 
                 error,blackboard 
               | _ -> 
                 let error_list,error = 
                   PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "773") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard 
           end 
         | Exist case_address -> 
           begin 
             match case_value
             with 
               | Boolean b -> 
                 let error,old = get_case parameter handler error case_address blackboard in 
                 let case_value = {old with dynamic = {old.dynamic with selected = b}} in 
                 let error,blackboard = set_case parameter handler error case_address case_value blackboard in 
                 error,blackboard 
               | _ -> 
                 let error_list,error = 
                   PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "788") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard 
           end 
             
     let rec get parameter handler error case_address blackboard = 
       match 
         case_address 
       with 
         | Keep_event step_id -> error,Boolean (PB.A.get blackboard.selected_events step_id) 
         | N_unresolved_events_in_column_at_level (int,level) -> 
           let n = 
             try 
               let a = Priority.LevelMap.find level blackboard.weigth_of_predicate_id_by_level in
               PB.A.get a int 
             with 
               Not_found -> 0 
               in 
               error,Counter n
         | N_unresolved_events_in_column int -> error,Counter (PB.A.get blackboard.weigth_of_predicate_id int)
         | Exist case_address -> 
           let error,case = get_case parameter handler error case_address blackboard in 
           error,Boolean case.dynamic.selected 
         | Pointer_to_next case_address -> 
           let error,case = get_case parameter handler error case_address blackboard in 
           error,Pointer case.dynamic.pointer_next 
         | Value_after case_address -> 
           let error,case = get_case parameter handler error case_address blackboard in 
           error,State case.dynamic.state_after 
         | Value_before case_address -> 
           let error,case = get_case parameter handler error case_address blackboard in 
           let pointer = case.dynamic.pointer_previous in 
           if is_null_pointer pointer 
           then 
              let error_list,error = 
                PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "get") (Some "814") (Some "Value before an unexisting element requested ") (failwith "Value before an unexisting element requested")  
              in 
              PB.CI.Po.K.H.raise_error parameter handler error_list error (State PB.undefined)
           else 
             get parameter handler error (Value_after {case_address with row_short_event_id = pointer}) blackboard 
         | Pointer_to_previous case_address -> 
           let error,case = get_case parameter handler error case_address blackboard in 
           error,Pointer case.dynamic.pointer_previous 
         | N_unresolved_events -> error,Counter blackboard.n_unresolved_events
         | N_unresolved_events_at_level lvl -> error,
           Counter(try 
           Priority.LevelMap.find lvl blackboard.n_unresolved_events_by_level
             with Not_found -> 0)


     let export_blackboard_to_xls parameter handler error prefix int int2 blackboard = 
        let file_name = prefix^"_"^(string_of_int int)^"_"^(string_of_int int2)^".sxw" in 
        let desc = open_out file_name in 
        let parameter = 
          {parameter with PB.CI.Po.K.H.out_channel = desc }
        in 
        let ncolumns_left = 3 in 
        let nrows_head = 2 in 
        let row_of_precondition eid = nrows_head + 3*eid in 
        let row_of_postcondition eid = 1+(row_of_precondition eid) in 
        let column_of_pid pid = pid + ncolumns_left in 
        let _ = Printf.fprintf desc "REM  *****  BASIC  *****\n" in 
        let colors = PB.A.create blackboard.n_eid None in 
        let backcolor log color = 
          match 
            color 
          with 
          | Some color -> 
            let r,g,b=Color.triple_of_color color in 
            Printf.fprintf log "C.CellBackColor = RGB(%i,%i,%i)\n" r g b 
          | None -> ()
        in 
        let textcolor log color  = 
          match 
            color 
          with 
          | Some color -> 
            let r,g,b=Color.triple_of_color color in 
            Printf.fprintf log "C.CharColor = RGB(%i,%i,%i)\n" r g b 
          | None -> ()
        in 
        let getcell log row col = 
          Printf.fprintf log "C = S.getCellByPosition(%i,%i)\n" col row 
        in 
        let overline_case log row col color = 
          let _ = Printf.fprintf desc "R=S.Rows(%i)\n" row in 
          let _ = Printf.fprintf desc "R.TopBorder = withBord\n" in 
          ()
        in 
        let print_case log row col color_font color_back string = 
          if string <> ""
          then 
            let _ = getcell log row col in 
            let _ = textcolor log color_font in 
            let _ = backcolor log color_back in 
            let _ = Printf.fprintf log "C.setFormula(\"%s\")\n" string
            in () 
        in 
        let print_case_fun log row col color_font color_back f error = 
          let _ = getcell log row col in 
          let _ = textcolor log color_font in 
          let _ = backcolor log color_back in 
          let _ = Printf.fprintf log "C.setFormula(\""in 
          let error = f error in 
          let _ = Printf.fprintf log "\")\n" in 
          error
        in 
        let _ = Printf.fprintf desc "Sub Main\n\n" in 
        let r,g,b = Color.triple_of_color Color.Black in 
        let _ = Printf.fprintf desc "Dim withBord As New com.sun.star.table.BorderLine\n" in 
        let _ = Printf.fprintf desc "With withBord withBord.Color = RGB(%i,%i,%i)\n" r g b in 
        let _ = Printf.fprintf desc "withBord.OuterLineWidth = 60\n" in 
        let _ = Printf.fprintf desc "End With\n" in 
        let _ = Printf.fprintf desc "S = ThisComponent.Sheets(0)\n" in 
        let _ = 
          match forced_events blackboard
          with 
          |  [list,_] -> 
              List.iter 
                (fun eid -> PB.A.set colors eid (Some Color.Red))
                list
          | _ -> ()
        in 
        let _ = 
          PB.A.iteri 
            (fun pid p_info -> 
              print_case_fun desc 0 (column_of_pid pid) None None (fun error -> PB.print_predicate_info desc p_info) error)
            blackboard.pre_column_map_inv
        in 
        let rec aux eid error stack = 
          if eid>=blackboard.n_eid 
          then error
          else 
            begin 
              let error,list = case_list_of_eid parameter handler error blackboard eid  in 
              let row_precondition = row_of_precondition eid in 
              let row_postcondition = row_of_postcondition eid in 
              let color,maybekept = 
                match 
                  PB.A.get blackboard.selected_events eid 
                with 
                | None -> PB.A.get colors eid,true
                | Some true -> Some Color.Red,true
                | Some false -> Some Color.Grey,false 
              in 
              let rec aux2 f g l error = 
                 match l 
                 with 
                   | [] -> error
                   | t::q -> 
                     let _ = overline_case desc row_postcondition (column_of_pid t.column_predicate_id) None in 
                     let _ = print_case desc row_precondition (column_of_pid t.column_predicate_id) None color (f t) in 
                     let _ = print_case desc row_postcondition (column_of_pid t.column_predicate_id) None color (g t) in 
                     let error = 
                       if maybekept 
                       then 
                         let error,value_before = 
                           try 
                             let error,case_value = get parameter handler error (value_before t) blackboard in 
                             error,string_of_value case_value 
                           with 
                             Not_found -> error,"Undefined"
                         in 
                         let _ = print_case desc (row_precondition-1) (column_of_pid t.column_predicate_id) (Some Color.Lightblue) None value_before
                         in 
                         let error,value_after = 
                           try 
                             let error,case_value = get parameter handler error (value_after t) blackboard in 
                             error,string_of_value case_value 
                           with 
                             Not_found -> error,"Undefined"
                         in 
                         let _ = print_case desc (row_postcondition+1) (column_of_pid t.column_predicate_id) (Some Color.Lightblue) None value_after 
                         in error
                       else 
                         error 
                     in 
                     aux2 f g q error
              in 
              let print_test t = 
                 let column = PB.A.get blackboard.blackboard t.column_predicate_id in 
		 let case = PB.A.get column t.row_short_event_id in 
                 PB.string_of_predicate_value case.static.test 
              in
              let print_action t = 
	          let column = PB.A.get blackboard.blackboard t.column_predicate_id in 
		  let case = PB.A.get column t.row_short_event_id in 
		  PB.string_of_predicate_value case.static.action 
	      in 
              let string_eid error = 
                try 
                  PB.CI.Po.K.print_step parameter handler error (PB.A.get blackboard.event eid)
                with 
                | Not_found -> let _ = Printf.fprintf desc "Event:%s" (string_of_int eid) in error 
              in
              let error = print_case_fun  desc row_precondition 1 None color string_eid error in 
              let error = print_case_fun  desc row_postcondition 1 None color string_eid error in 
              let _  = print_case desc row_precondition 2 None color "PRECONDITION" in 
              let _ = print_case desc row_postcondition 2 None color "POSTCONDITION" in 
              let error = aux2 print_test print_action list error in
              let bool = 
                try 
                  begin 
                    match PB.CI.Po.K.type_of_refined_step (PB.A.get blackboard.event eid:PB.CI.Po.K.refined_step)
                    with 
                    | PB.CI.Po.K.Event _ | PB.CI.Po.K.Obs _ | PB.CI.Po.K.Init _ -> true
                    | _ -> false
                  end
                with 
                | Not_found -> false
              in 
              let error,stack = 
                if bool 
                then 
                  let error = List.fold_left (fun error row -> print_case_fun desc row 0 None color string_eid error) error (List.rev stack) in 
                  error,[]
                else 
                  error,row_precondition::row_postcondition::stack 
              in 
              aux (eid+1) error stack 
            end
        in 
        let error = aux 0 error [] in 
        let _ = Printf.fprintf  desc "End Sub\n" in 
        let _ = close_out desc in 
        error
           
   
     let record_modif parameter handler error case_address case_value blackboard = 
       error,
       {blackboard
        with current_stack = (case_address,case_value)::blackboard.current_stack}
       
     let refine parameter handler error case_address case_value blackboard = 
       let error,old = get parameter handler error case_address blackboard in 
       if case_value = old 
       then 
           let error = 
             if debug_mode 
             then 
               let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\n***\nREFINE_VALUE\nValue before: " in 
               let error = print_case_address parameter handler error blackboard case_address in 
               let _ = print_case_value parameter old in 
               let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\nNew value: " in 
              let _ = print_case_value parameter case_value in 
               let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\nIGNORED***\n" in 
               error
             else 
               error
           in 
         error,blackboard,Ignore
       else
         let error,bool = strictly_more_refined parameter handler error old case_value in 
         if bool 
         then 
            let error = 
             if debug_mode 
             then 
               let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\n***\nREFINE_VALUE\nValue before: " in 
               let error = print_case_address parameter handler error blackboard case_address in 
               let _ = print_case_value parameter old in 
               let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\nNew value: " in 
               let _ = print_case_value parameter case_value in 
               let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\nIGNORED***\n" in 
               error
             else
               error 
           in 
           error,blackboard,Ignore 
         else 
           let error,bool = strictly_more_refined parameter handler error case_value old 
           in 
           if bool 
           then 
             let error,blackboard = set parameter handler error case_address case_value blackboard in 
             let error,blackboard = record_modif parameter handler error case_address old blackboard in 
             let error = 
               if debug_mode 
               then 
                 let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\n***\nREFINE_VALUE\nValue before: " in 
                 let error = print_case_address parameter handler error blackboard case_address in 
                 let _ = print_case_value parameter old in 
                 let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\nNew value: " in 
                 let _ = print_case_value parameter case_value in 
                 let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\nSUCCESS***\n" in 
                 error 
               else 
                 error 
           in 
             error,blackboard,Success
           else 
             let error = 
               if debug_mode 
               then 
                 let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\n***\nREFINE_VALUE\nValue before: " in 
                 let error = print_case_address parameter handler error blackboard case_address in 
                 let _ = print_case_value parameter old in 
                 let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\nNew value: " in 
                 let _ = print_case_value parameter case_value in 
                 let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "\nFAIL***\n" in 
                 error
               else 
                 error 
             in 
             error,blackboard,Fail 

     let overwrite parameter handler error case_address case_value blackboard = 
       let error,old = get parameter handler error case_address blackboard in 
       if case_value = old 
       then 
         error,blackboard
       else
         let error,blackboard = set parameter handler error case_address case_value blackboard in 
         let error,blackboard = record_modif parameter handler error case_address old blackboard in 
         error,blackboard 
           
     let dec parameter handler error case_address blackboard = 
       let error,old = get parameter handler error case_address blackboard in 
       match old 
       with 
         | Counter k -> 
           if k=0 
           then 
             error,blackboard 
           else 
             let error,blackboard = set parameter handler error case_address (Counter (k-1)) blackboard in 
             let error,blackboard = record_modif parameter handler error case_address old blackboard in 
             error,blackboard 
         | _ ->    
           let error_list,error = 
                PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "dec") (Some "916") (Some "Wrong type of case value") (failwith "Wrong type of case value")  
              in 
              PB.CI.Po.K.H.raise_error parameter handler error_list error blackboard 
         
     let branch parameter handler error log_info blackboard = 
       let error = 
         if debug_mode 
         then 
           let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "*******\n * BRANCH *\n*******" in 
           let error = print_blackboard parameter handler error blackboard in 
           error
         else 
           error
       in 
       let log_info = PB.CI.Po.K.P.inc_branch log_info in 
       error,
       log_info,
       {
         blackboard 
        with 
          stack = blackboard.current_stack::blackboard.stack ; 
          current_stack = []
       }
       
     let reset_last_branching parameter handler error log_info blackboard = 
       let error = 
         if debug_mode 
         then 
           let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "*******\n* Cut *\n*******" in 
           let error = print_blackboard parameter handler error blackboard in 
           error
         else 
           error
       in 
       let stack = blackboard.current_stack in 
       let error,blackboard = 
         List.fold_left 
           (fun (error,blackboard) (case_address,case_value) -> 
             set parameter handler error case_address case_value blackboard)
           (error,blackboard)
           stack 
       in 
       let error = 
         if debug_mode 
         then 
           let _ = Printf.fprintf parameter.PB.CI.Po.K.H.out_channel_err "*******\n* After_Cut *\n*******" in 
           let error = print_blackboard parameter handler error blackboard in 
           error
         else
           error 
       in 
       let log_info = PB.CI.Po.K.P.inc_cut log_info in 
       match blackboard.stack 
       with 
         | [] -> error,log_info,{blackboard with current_stack = []}
         | t::q -> 
             error,log_info,{blackboard with current_stack = t ; stack = q }

     let reset_init parameter handler error log_info blackboard = 
       let rec aux (error,log_info,blackboard) = 
         match blackboard.current_stack 
         with 
           | []   -> error,log_info,blackboard 
           | _  -> aux (reset_last_branching parameter handler error log_info blackboard)
       in 
       let error,log_info,blackboard = aux (error,log_info,blackboard) in 
       let log_info = PB.CI.Po.K.P.reset_log log_info in 
       error,log_info,blackboard 
    
  (** output result*)
     type result = (PB.CI.Po.K.refined_step * PB.CI.Po.K.side_effect) list  
         
  (** iteration*)
     let is_maximal_solution parameter handler error blackboard = 
       error,blackboard.n_unresolved_events = 0 

 
  (** exporting result*)
       
   let translate_blackboard parameter handler error blackboard = 
     let array = blackboard.selected_events in 
     let step_array = blackboard.event in 
     let side_array = blackboard.side_effect_of_event in 
     let size = PB.A.length array in
     let rec aux k list = 
       if k=size 
       then (List.rev list) 
       else 
         let bool = PB.A.get array k in 
         match bool 
         with 
           | None -> aux (k+1) list 
           | Some false -> aux (k+1) list
           | Some true -> 
             begin
               let step = 
                 PB.A.get step_array k 
               in 
               let side = 
                 PB.A.get side_array k 
               in 
               aux (k+1) ((step,side)::list) 
             end
     in 
     let list = aux 0 [] in 
     error,list 
       
   let print_stack parameter handler error blackboard = 
     let stack = blackboard.current_stack in 
     let log = parameter.PB.CI.Po.K.H.out_channel_err in 
     let _ = Printf.fprintf log "Current_stack_level %i " (List.length stack) in 
     let error = List.fold_left (fun error i -> let error = print_assignment parameter handler error blackboard i in let _ = Printf.fprintf log "\n" in error ) error (List.rev stack)  in 
     List.fold_left
         (fun error x -> 
           let _ = Printf.fprintf log "Other level %i "  (List.length x) in 
           List.fold_left (fun error -> print_assignment parameter handler error blackboard) error (List.rev x))
         error (List.rev blackboard.stack)
  
   let is_fictitious_obs blackboard eid = 
     Some eid = blackboard.fictitious_observable

   
   let useless_predicate_id parameter handler error blackboard list = 
     let n_events = blackboard.n_eid in  
     if Parameter.do_local_cut 
     then 
       begin 
         let event_array = PB.A.make n_events false in
         let kept_events = [] in 
         let kept_events = 
           List.fold_left 
             (fun kept_events i -> 
               let _ = 
                 PB.A.set event_array i true 
               in i::kept_events) 
             kept_events 
             list 
         in 
         let rec aux error event_list kept_events =  
           match event_list 
           with 
             | [] -> error,kept_events
             | eid::q -> 
               begin 
                 if is_fictitious_obs blackboard eid 
                 then 
                   aux error q kept_events  
                 else 
                   let list = PB.A.get blackboard.event_case_list eid in 
                   let q,kept_events = 
                     List.fold_left
                       (fun (q,kept_events) event_case_address ->
                         let error,case = get_case parameter handler error event_case_address blackboard in 
                         if PB.is_undefined case.static.test 
                         then q,kept_events
                         else 
                           let pointer = case.dynamic.pointer_previous in 
                           let eid = 
                             let rec scan_down pointer =
                               let prev_event_case_address = 
                                 {event_case_address with row_short_event_id = pointer}
                               in 
                               let error,prev_case = get_case parameter handler error prev_event_case_address blackboard in 
                               let prev_eid = prev_case.static.event_id in 
                               if is_null_pointer prev_eid                      
                               then None 
                             else 
                                 if PB.is_unknown prev_case.static.action 
                                 then 
                                   let pointer = prev_case.dynamic.pointer_previous in 
                                   scan_down pointer 
                                 else Some prev_eid 
                             in 
                           scan_down pointer 
                           in 
                           match 
                             eid 
                           with 
                             | None -> q,kept_events
                             | Some prev_eid -> 
                               let bool = 
                                 try 
                                   PB.A.get event_array prev_eid 
                                 with 
                                   | _ -> false 
                               in 
                               let q,kept_events = 
                                 if 
                                   bool 
                                 then 
                                   q,kept_events
                                 else 
                                   let _ = PB.A.set event_array prev_eid true in 
                                   prev_eid::q,prev_eid::kept_events
                               in q,kept_events)
                       (q,kept_events)
                       list 
                   in 
                   aux error q kept_events 
               end 
         in 
         let error,rep = aux error list kept_events in 
         error,List.sort compare rep,n_events-List.length rep
       end 
     else
       let events_to_keep = 
         let rec aux k list = 
           if k<0 then list 
           else aux (k-1) (k::list)
         in 
         aux (n_events-1) [] 
       in 
       error,
       events_to_keep,
       0 
         
   let cut parameter handler error log_info blackboard list = 
     let error,cut_causal_flow,n_events_removed = useless_predicate_id parameter handler error blackboard list in 
     let log_info = PB.CI.Po.K.P.set_concurrent_event_detection_time log_info in 
     let log_info = PB.CI.Po.K.P.set_step_time log_info in 
     let log_info = PB.CI.Po.K.P.inc_k_cut_events n_events_removed log_info in 
     error,log_info,blackboard,cut_causal_flow

   let n = ref 0 

   let import parameter handler error log_info list = 
     let error,preblackboard = PB.init parameter handler error in
     let error,(log_info,preblackboard,step_id,string,to_xls) = 
       match 
         parameter.PB.CI.Po.K.H.current_compression_mode 
       with 
       | None ->  
         let error_list,error = PB.CI.Po.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "import") (Some "1551") (Some "Compression mode has not been set up.") (failwith "Compression mode has not been set up.") in 
          PB.CI.Po.K.H.raise_error parameter handler error_list error (log_info,preblackboard,0,"None",false)
       | Some Parameter.Strong -> 
         let error,log_info,preblackboard,int = 
           List.fold_left 
             (fun (error,log_info,preblackboard,int) refined_event  -> 
               PB.add_step_up_to_iso parameter handler error log_info refined_event preblackboard int)
             (error,log_info,preblackboard,0)
             list 
         in 
         error,(log_info,preblackboard,int,Parameter.xlsstrongFileName,Parameter.dump_grid_before_strong_compression)
       | Some Parameter.Weak | Some Parameter.Causal -> 
         let error,log_info,preblackboard,int = 
         List.fold_left 
           (fun (error,log_info,preblackboard,int) refined_event  -> 
             PB.add_step parameter handler error log_info refined_event preblackboard int)
           (error,log_info,preblackboard,0)
           list 
         in 
         error,(log_info,preblackboard,int,Parameter.xlsweakFileName,Parameter.dump_grid_before_weak_compression)
     in 
     let error,log_info,preblackboard = 
       PB.finalize parameter handler error log_info preblackboard 
     in 
     let error,log_info,blackboard = import parameter handler error log_info preblackboard in
     let _ = Priority.n_story:=(!Priority.n_story)+1 in 
     let _ = Priority.n_branch:=1 in 
     let error = 
       if to_xls 
       then 
	 export_blackboard_to_xls parameter handler error string (!Priority.n_story) 0 blackboard 
       else
         error
     in 
    error,log_info,blackboard 

   
   end:Blackboard)


      

