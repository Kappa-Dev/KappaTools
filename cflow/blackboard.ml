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
  * Last modification: 19/03/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
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

  val build_pointer: PB.step_short_id -> pointer 
  val is_before_blackboard: pointer -> bool 
  val get_npredicate_id: blackboard -> int 
  val get_n_unresolved_events_of_pid: blackboard -> PB.predicate_id -> int 
  val get_first_linked_event: blackboard -> PB.predicate_id -> int option 
  val get_last_linked_event: blackboard -> PB.predicate_id -> int option 
  val case_address_of_case_event_address : event_case_address -> case_address 
  val predicate_value_of_case_value: (case_value -> PB.K.H.error_channel * PB.predicate_value) PB.K.H.with_handler 
  val follow_pointer_up: (blackboard -> event_case_address -> PB.K.H.error_channel * event_case_address) PB.K.H.with_handler 
  val follow_pointer_down: (blackboard -> event_case_address -> PB.K.H.error_channel * event_case_address) PB.K.H.with_handler 

  val build_event_case_address: PB.predicate_id -> pointer -> event_case_address  
  val exist_case: (blackboard -> event_case_address -> PB.K.H.error_channel * bool option) PB.K.H.with_handler   
  val get_static: (blackboard -> event_case_address -> PB.K.H.error_channel * (PB.step_short_id * PB.step_id * PB.predicate_value * PB.predicate_value)) PB.K.H.with_handler 
            
  val set: (case_address -> case_value -> blackboard  -> PB.K.H.error_channel * blackboard) PB.K.H.with_handler 
  val get: (case_address -> blackboard -> PB.K.H.error_channel * case_value) PB.K.H.with_handler 
  val dec: (case_address -> blackboard -> PB.K.H.error_channel * blackboard) PB.K.H.with_handler
  val overwrite: (case_address -> case_value -> blackboard -> PB.K.H.error_channel * blackboard) PB.K.H.with_handler  
  val refine: (case_address -> case_value -> blackboard -> PB.K.H.error_channel * blackboard * assign_result) PB.K.H.with_handler  
  val branch: (blackboard -> PB.K.H.error_channel *blackboard) PB.K.H.with_handler 
  val reset_last_branching: (blackboard -> PB.K.H.error_channel * blackboard ) PB.K.H.with_handler 
  val reset_init: (blackboard -> PB.K.H.error_channel * blackboard) PB.K.H.with_handler 
  (** initialisation*)
  val import: (PB.pre_blackboard -> PB.K.H.error_channel * blackboard) PB.K.H.with_handler 

  (** output result*)
  type result = (PB.K.refined_step * PB.K.side_effect) list 
     
  (** iteration*)
  val is_maximal_solution: (blackboard -> PB.K.H.error_channel * bool) PB.K.H.with_handler 

  (** exporting result*)
  val translate_blackboard: (blackboard -> PB.K.H.error_channel * result) PB.K.H.with_handler 

  (**pretty printing*)
  val print_blackboard:(blackboard -> PB.K.H.error_channel) PB.K.H.with_handler 
  val print_event_case_address:out_channel -> event_case_address -> unit 
  val print_stack: out_channel -> blackboard -> unit

  val exist: event_case_address -> case_address 
  val boolean: bool option -> case_value 
  val pointer_to_previous: event_case_address -> case_address 
  val pointer_to_next: event_case_address -> case_address 
  val pointer: event_case_address -> case_value 
  val value_after: event_case_address -> case_address 
  val case_list_of_eid: (blackboard -> PB.step_id -> PB.K.H.error_channel * event_case_address list) PB.K.H.with_handler 
  val state: PB.predicate_value -> case_value 
  val is_exist_event: PB.step_id -> case_address 
  val n_unresolved_events: case_address 
  val n_unresolved_events_in_column: event_case_address -> case_address 
  val forced_events: blackboard -> PB.step_id list list 
  val side_effect_of_event: blackboard -> PB.step_id -> PB.K.side_effect
  val cut_predicate_id: (blackboard -> PB.predicate_id -> PB.K.H.error_channel *   blackboard) PB.K.H.with_handler 
  val cut: (blackboard -> PB.step_id list -> PB.K.H.error_channel * blackboard * result ) PB.K.H.with_handler 
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

    let build_event_case_address pid seid = 
      {
        column_predicate_id = pid ;
        row_short_event_id = seid 
      }
	  
    type case_address = 
      | N_unresolved_events_in_column of int 
      | Pointer_to_next of event_case_address 
      | Value_after of event_case_address 
      | Value_before of event_case_address 
      | Pointer_to_previous of event_case_address
      | N_unresolved_events 
      | Exist of event_case_address 
      | Keep_event of PB.step_id 

    let is_exist_event i = Keep_event i 
    let n_unresolved_events_in_column i = N_unresolved_events_in_column (i.column_predicate_id) 
    let pointer_to_next e = Pointer_to_next e 
    let value_after e = Value_after e 
    let value_before e = Value_before e 
    let pointer_to_previous e = Pointer_to_previous e 
    let n_unresolved_events = N_unresolved_events 
    let exist e = Exist e


    type case_value = 
      | State of PB.predicate_value 
      | Counter of int 
      | Pointer of pointer 
      | Boolean of bool option 

    let print_case_value x = 
      match x 
      with 
        | State x -> let _ = Printf.fprintf stderr "State! " in 
                     let _ = PB.print_predicate_value stderr x in 
                     let _ = Printf.fprintf stderr "\n" in 
                     () 
        | Counter i -> Printf.fprintf stderr "Counter %i\n" i
        | Pointer i -> Printf.fprintf stderr "Pointer %i\n" i 
        | Boolean b -> Printf.fprintf stderr "Boolean %s\n" (match b with None -> "?" | Some true -> "true" | _ -> "false")
 
    let print_case_address x = 
      match x
      with 
       | N_unresolved_events_in_column i -> Printf.fprintf stderr "n_unresolved_events_in_pred %i" i 
       | Pointer_to_next e -> 
         let _ = Printf.fprintf stderr "Pointer" in 
         ()
      | Value_after e -> 
        let _ = Printf.fprintf stderr "Value_after" in 
        () 
      | Value_before e -> 
        let _ = Printf.fprintf stderr "Value_before" in 
        () 
      | Pointer_to_previous e -> 
        let _ = Printf.fprintf stderr "Pointer_before" in 
        () 
      | N_unresolved_events -> Printf.fprintf stderr "Unresolved_events" 
      | Exist e -> Printf.fprintf stderr "Exist" 
      | Keep_event i -> Printf.fprintf stderr "Keep %i" i

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
          let _ = print_case_value case_value in 
          let error_list,error = PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "predicate_value_of_case_value") (Some "226") (Some "wrong kinf of case_value in predicate_value_of_case_value") (failwith "predicate_value_of_case_value") in 
          PB.K.H.raise_error parameter handler error_list error PB.unknown 
        

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
          let error_list,error = PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some string) (Some "246") (Some "Counters and/or Pointers should not be compared") (failwith "strictly_more_refined") in 
          PB.K.H.raise_error parameter handler error_list error false
            
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
           event: PB.K.refined_step PB.A.t;
           pre_column_map_inv: PB.predicate_info PB.A.t; (** maps each wire id to its wire label *)
           forced_events: int list list;
           n_predicate_id: int ;
           n_eid:int;
           n_seid: int PB.A.t;
           current_stack: stack ; 
           stack: stack list ;
           blackboard: case_info PB.A.t PB.A.t ;
           selected_events: bool option PB.A.t ;
           weigth_of_predicate_id: int PB.A.t ; 
           used_predicate_id: bool PB.A.t ;  
           n_unresolved_events: int ;
           last_linked_event_of_predicate_id: int PB.A.t;
           event_case_list: event_case_address list PB.A.t;
           side_effect_of_event: PB.K.side_effect PB.A.t;
           fictitious_observable: PB.step_id option;
         }

     let forced_events blackboard = blackboard.forced_events 
     let side_effect_of_event blackboard i = PB.A.get blackboard.side_effect_of_event i 

     let case_list_of_eid parameter handler error blackboard eid = 
       try 
         error,PB.A.get blackboard.event_case_list eid 
       with 
         | _ -> 
           let error_list,error = PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "case_list_of_eid") (Some "366") (Some "Out of bound") (failwith "Dereferencing null pointer")
           in 
           PB.K.H.raise_error parameter handler error_list error [] 

     let get_case parameter handler error case_address blackboard = 
       try 
         error,PB.A.get 
             (PB.A.get blackboard.blackboard case_address.column_predicate_id)
             (case_address.row_short_event_id) 
       with 
         | _ -> 
           let error_list,error = PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "get_case") (Some "377") (Some "Dereferencing null pointer") (failwith "Dereferencing null pointer")
           in 
           PB.K.H.raise_error parameter handler error_list error dummy_case_info 

     let get_static parameter handler error blackboard address = 
       let error,case = get_case parameter handler error address blackboard in 
       let static = case.static in 
       error,(static.row_short_id,static.event_id,static.test,static.action)

     let get_npredicate_id blackboard = blackboard.n_predicate_id 
     let get_n_unresolved_events_of_pid blackboard pid = PB.A.get blackboard.weigth_of_predicate_id pid 
     let get_pointer_next case = case.dynamic.pointer_next 
     
     let follow_pointer_down parameter handler error blackboard address = 
       let error,case = get_case parameter handler error address blackboard in 
       error,{address with row_short_event_id = case.dynamic.pointer_next}

     let follow_pointer_up parameter handler error blackboard address = 
       let error,case = get_case parameter handler error address blackboard in 
       error,{address with row_short_event_id = case.dynamic.pointer_previous}

     let get_first_linked_event blackboard pid = 
       if pid <0 or pid >= blackboard.n_predicate_id 
       then 
         None 
       else 
         Some 0 

     let get_last_linked_event blackboard pid = 
       if pid<0 or pid >= blackboard.n_predicate_id 
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

     let print_pointer log seid = 
       Printf.fprintf log "event sid %i" seid

     let print_event_case_address log event_case_address = 
       let _ = print_pointer log event_case_address.row_short_event_id in 
       Printf.fprintf log "predicate %i" event_case_address.column_predicate_id 

     let print_address log address = 
       match address 
       with 
         | Keep_event i -> 
           let _ = Printf.fprintf log "Is the event %i selected ? " i in 
           () 
         | Exist i -> 
           let _ = Printf.fprintf log "Is the case " in 
           let _ = print_event_case_address  log i in 
           let _ = Printf.fprintf log "selected ? " in 
           () 
         | N_unresolved_events_in_column i -> 
           let _ = Printf.fprintf log "Number of unresolved events for the predicate %i" i in 
           ()
         | Pointer_to_next i -> 
           let _ = Printf.fprintf log "Prochain événement agissant sur "  in 
           let _ = print_event_case_address log i in 
           ()
         | Value_after i -> 
           let _ = Printf.fprintf log "Valeur après "  in 
           let _ = print_event_case_address log i in 
           ()
         | Value_before i -> 
           let _ = Printf.fprintf log "Valeur avant "  in 
           let _ = print_event_case_address log i in 
           ()
         | Pointer_to_previous i -> 
           let _ = Printf.fprintf log "Evenement précésent agissant sur " in 
           let _ = print_event_case_address log i in 
           ()
         | N_unresolved_events -> 
           let _ = Printf.fprintf log "Nombre d'événements non résolu" in 
           () 
  
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

     let print_assignment log (address,value)  = 
       let _ = print_address log address in 
       let _ = print_value log value in 
       () 

     let print_blackboard parameter handler error blackboard = 
       let log = parameter.PB.K.H.out_channel in  
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
       let _ = List.iter (print_assignment log) blackboard.current_stack in 
       let _ = Printf.fprintf log "\n" in 
       let _ = 
         List.iter 
           (fun stack -> 
             let _ = List.iter (print_assignment log) stack in 
             let _ = Printf.fprintf log "\n" in 
             ())
           blackboard.stack 
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
           (fun i j -> 
             Printf.fprintf log " %i:%i\n" i j 
           )
           blackboard. weigth_of_predicate_id 
       in 
       let _ = Printf.fprintf log "**\n" in 
       error
             


     (** propagation request *)

     let add_event eid (pid,seid) array = 
       let event_case_address = build_event_case_address pid seid in 
       let old = PB.A.get array eid in 
       PB.A.set array eid (event_case_address::old)

     let empty_stack = []

     let import parameter handler error pre_blackboard = 
       let error,n_predicates = PB.n_predicates parameter handler error pre_blackboard in  
       let error,n_events = PB.n_events parameter handler error pre_blackboard in 
       let stack = [] in 
       let current_stack = empty_stack in
       let  event_case_list = PB.A.make n_events [] in 
       let n_seid = PB.A.make n_predicates 0 in 
       let blackboard = PB.A.make n_predicates (PB.A.make 1 dummy_case_info) in
       let weigth_of_predicate_id = PB.A.make n_predicates 0 in 
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
                   let _ = add_event eid (p_id,seid) event_case_list in 
                   let _ = PB.A.set array seid info in 
                   aux2 (seid-1) q 
             in 
             let _ = aux2 (size-2) list in 
             aux1 (p_id-1) error 
         in aux1 (n_predicates-1) error 
       in 
       let error,forced_events = PB.mandatory_events parameter handler error pre_blackboard in 
       let error,event = PB.get_pre_event parameter handler error pre_blackboard in 
       let error,side_effects = PB.get_side_effect parameter handler error pre_blackboard in 
       let error,fictitious_obs = PB.get_fictitious_observable parameter handler error pre_blackboard in 
       error,
       {
         event = event ;
         side_effect_of_event = side_effects ; 
         pre_column_map_inv = PB.get_pre_column_map_inv pre_blackboard; 
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
         weigth_of_predicate_id= weigth_of_predicate_id; 
         used_predicate_id = PB.A.make n_predicates true; 
         n_unresolved_events = n_events ; 
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
           let error_list,error = PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set_case") (Some "680") (Some "Dereferencing null pointer") (failwith "Dereferencing null pointer")
           in 
           PB.K.H.raise_error parameter handler error_list error blackboard


     let set parameter handler error case_address case_value blackboard = 
       match 
         case_address 
       with 
         | N_unresolved_events_in_column int -> 
           begin 
             match case_value
             with 
               | Counter int2 -> 
                 let _ = PB.A.set blackboard.weigth_of_predicate_id int int2 in 
                 error,blackboard 
               | _ -> 
                 let error_list,error = 
                   PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "698") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.K.H.raise_error parameter handler error_list error blackboard 
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
                   PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "713") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.K.H.raise_error parameter handler error_list error blackboard 
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
                   PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "728") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.K.H.raise_error parameter handler error_list error blackboard 
           end 
         | Value_before case_address -> 
           let error_list,error = 
             PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "734") (Some "Blackboard.set should not be called with value_before") (failwith "Incompatible address in function Blackboard.set")
           in 
           PB.K.H.raise_error parameter handler error_list error blackboard 
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
                   PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "748") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.K.H.raise_error parameter handler error_list error blackboard 
              end 
         | N_unresolved_events -> 
           begin 
             match case_value
             with 
               | Counter int -> 
                 error,{blackboard with n_unresolved_events = int}
               | _ -> 
                 let error_list,error = 
                   PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "760") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.K.H.raise_error parameter handler error_list error blackboard 
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
                   PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "773") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.K.H.raise_error parameter handler error_list error blackboard 
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
                   PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "788") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.K.H.raise_error parameter handler error_list error blackboard 
           end 
             
     let rec get parameter handler error case_address blackboard = 
       match 
         case_address 
       with 
         | Keep_event step_id -> error,Boolean (PB.A.get blackboard.selected_events step_id) 
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
                PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "get") (Some "814") (Some "Value before an unexisting element requested ") (failwith "Value before an unexisting element requested")  
              in 
              PB.K.H.raise_error parameter handler error_list error (State PB.undefined)
           else 
             get parameter handler error (Value_after {case_address with row_short_event_id = pointer}) blackboard 
         | Pointer_to_previous case_address -> 
           let error,case = get_case parameter handler error case_address blackboard in 
           error,Pointer case.dynamic.pointer_previous 
         | N_unresolved_events -> error,Counter blackboard.n_unresolved_events


   
     let record_modif parameter handler error case_address case_value blackboard = 
       error,
       {blackboard
        with current_stack = (case_address,case_value)::blackboard.current_stack}
       
     let refine parameter handler error case_address case_value blackboard = 
       let error,old = get parameter handler error case_address blackboard in 
       if case_value = old 
       then 
           let _ = 
             if debug_mode 
             then 
               let _ = Printf.fprintf stderr "\n***\nREFINE_VALUE\nValue before: " in 
               let _ = print_case_value old in 
               let _ = Printf.fprintf stderr "\nNew value: " in 
              let _ = print_case_value case_value in 
               let _ = Printf.fprintf stderr "\nIGNORED***\n" in 
            () 
           in 
         error,blackboard,Ignore
       else
         let error,bool = strictly_more_refined parameter handler error old case_value in 
         if bool 
         then 
            let _ = 
             if debug_mode 
             then 
               let _ = Printf.fprintf stderr "\n***\nREFINE_VALUE\nValue before: " in 
               let _ = print_case_value old in 
               let _ = Printf.fprintf stderr "\nNew value: " in 
              let _ = print_case_value case_value in 
               let _ = Printf.fprintf stderr "\nIGNORED***\n" in 
            () 
           in 
           error,blackboard,Ignore 
         else 
           let error,bool = strictly_more_refined parameter handler error case_value old 
           in 
           if bool 
           then 
             let error,blackboard = set parameter handler error case_address case_value blackboard in 
             let error,blackboard = record_modif parameter handler error case_address old blackboard in 
              let _ = 
             if debug_mode 
             then 
               let _ = Printf.fprintf stderr "\n***\nREFINE_VALUE\nValue before: " in 
               let _ = print_case_value old in 
               let _ = Printf.fprintf stderr "\nNew value: " in 
               let _ = print_case_value case_value in 
               let _ = Printf.fprintf stderr "\nSUCCESS***\n" in 
            () 
           in 
             error,blackboard,Success
           else 
             let _ = 
               if debug_mode 
               then 
                 let _ = Printf.fprintf stderr "\n***\nREFINE_VALUE\nValue before: " in 
                 let _ = print_case_value old in 
                 let _ = Printf.fprintf stderr "\nNew value: " in 
                 let _ = print_case_value case_value in 
                 let _ = Printf.fprintf stderr "\nFAIL***\n" in 
                 () 
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
                PB.K.H.create_error parameter handler error (Some "blackboard.ml") None (Some "dec") (Some "916") (Some "Wrong type of case value") (failwith "Wrong type of case value")  
              in 
              PB.K.H.raise_error parameter handler error_list error blackboard 
         
     let branch parameter handler error blackboard = 
       error,
       {
         blackboard 
        with 
          stack = blackboard.current_stack::blackboard.stack ; 
          current_stack = []
       }
       
     let reset_last_branching parameter handler error blackboard = 
       let _ = 
         if debug_mode 
         then 
           Printf.fprintf stderr "*******\n* Cut *\n*******" 
       in 
       let stack = blackboard.current_stack in 
       let error,blackboard = 
         List.fold_left 
           (fun (error,blackboard) (case_address,case_value) -> 
             set parameter handler error case_address case_value blackboard)
           (error,blackboard)
           stack 
       in 
       match blackboard.stack 
       with 
         | [] -> error,{blackboard with current_stack = []}
         | t::q -> error,{blackboard with current_stack = t ; stack = q }

     let reset_init parameter handler error blackboard = 
       let rec aux (error,blackboard) = 
         match blackboard.current_stack 
         with 
           | []   -> error,blackboard 
           | _  -> aux (reset_last_branching parameter handler error blackboard)
       in aux (error,blackboard) 
    
  (** output result*)
     type result = (PB.K.refined_step * PB.K.side_effect) list  
         
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
       
   let print_stack log blackboard = 
     let stack = blackboard.current_stack in 
     let _ = Printf.fprintf log "Current_stack_level %i " (List.length stack) in 
     let _ = List.iter (fun i -> print_assignment log i ;Printf.fprintf log "\n") stack  in 
     List.iter 
         (fun x -> 
           let _ = Printf.fprintf log "Other level %i "  (List.length x) in 
           List.iter (print_assignment log) x)
         blackboard.stack 
  
   let is_fictitious_obs blackboard eid = 
     Some eid = blackboard.fictitious_observable

   
   let useless_predicate_id parameter handler error blackboard list = 
     let n_events = blackboard.n_eid in  
     let n_pid = blackboard.n_predicate_id in 
     let p_id_array = PB.A.make n_pid false in  
     let event_array = PB.A.make n_events false in
     let rec aux error event_list =  
       match event_list 
       with 
         | [] -> error
         | eid::q -> 
           begin 
             let bool = 
               try 
                 PB.A.get event_array eid 
               with 
                 | _ -> false 
             in 
             if 
               bool 
             then 
               aux error q 
             else 
               begin 
                 let _ = PB.A.set event_array eid true in 
                 if is_fictitious_obs blackboard eid then 
                   aux error q 
                 else 
                   let list = PB.A.get blackboard.event_case_list eid in 
                   let q = 
                     List.fold_left
                       (fun q event_case_address ->
                         let predicate_id = event_case_address.column_predicate_id in 
                         let _ = PB.A.set p_id_array predicate_id true in 
                         let error,case = get_case parameter handler error event_case_address blackboard in 
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
                           | None -> q 
                           | Some prev_eid -> prev_eid::q)
                       q 
                       list 
                 in 
                   aux error q 
               end 
           end
     in 
     let error = aux error list in 
     let rep = ref [] in 
     let rep2 = ref [] in 
     let _ = 
       PB.A.iteri 
         (fun i bool -> 
           if bool then (rep2:=i::(!rep2)) 
           else rep:=i::(!rep))
         p_id_array 
     in 
     let rep3 = ref [] in 
     let _ = 
       PB.A.iteri 
         (fun i value -> 
           if value 
           then rep3:=(i:PB.step_id)::(!rep3)
         )
         event_array 
     in 
     let rep3 = List.rev_map (fun k -> PB.A.get blackboard.event k,PB.K.empty_side_effect) (!rep3) in 
     error,!rep,!rep2,rep3
       
   let cut_predicate_id parameter handler error blackboard p_id = 
     overwrite parameter handler error (N_unresolved_events_in_column p_id) (Counter 0) blackboard 
       
   let cut_event_id parameter handler error blackboard n_event = 
     overwrite parameter handler error N_unresolved_events (Counter n_event) blackboard 
     
   let count_event_of_p_id_list parameter handler error blackboard obs_list list_p_id = 
     let n_events = blackboard.n_eid in  
     let event_array = PB.A.make n_events false in
     let _ = List.iter (fun i -> PB.A.set event_array i true) obs_list in 
     let res = List.length obs_list in 
     List.fold_left  
       (fun (error,counter,list) p_id -> 
         let array = PB.A.get blackboard.blackboard p_id in 
         let error,counter,list = 
           if PB.A.get blackboard.used_predicate_id p_id 
           then 
             let rec aux j res list error = 
               let case = PB.A.get array j in
               let eid = case.static.event_id in 
               let bool = 
                 if is_null_pointer eid  or (PB.is_unknown case.static.action && not (List.mem eid obs_list))
                 then 
                   true
                 else 
                   try 
                     PB.A.get event_array eid 
                   with 
                     | _ -> false 
               in 
               let res',list' = 
                 if bool 
                 then res,list
                 else 
                   let _ = PB.A.set event_array eid true 
                   in res+1,eid::list  
               in 
               let j' = get_pointer_next case in 
               if j=j' then error,res',list'
               else aux j' res' list' error 
             in 
             let error,counter,list = aux pointer_before_blackboard counter list error in 
             error,counter,list
           else 
             error,counter,list
         in error,counter,list)
       (error,res,obs_list) list_p_id  


   let cut parameter handler error blackboard list = 
     let error,p_id_list_to_cut,p_id_list_to_keep,cut_causal_flow  = useless_predicate_id parameter handler error blackboard list in 
     if parameter.PB.K.H.compression_mode.Parameter.weak_compression = false && parameter.PB.K.H.compression_mode.Parameter.strong_compression = false 
     then 
       error,blackboard,cut_causal_flow 
     else 
       let error,int,event_list = count_event_of_p_id_list parameter handler error blackboard list p_id_list_to_keep in
       let event_list = List.sort compare  event_list in 
       let error,blackboard = cut_event_id parameter handler error blackboard int in 
       let error,blackboard = 
         List.fold_left 
           (fun (error,blackboard) p_id -> 
             cut_predicate_id parameter handler error blackboard p_id)
           (error,blackboard)
           p_id_list_to_cut 
       in 
       error,blackboard,cut_causal_flow 

  

  

   end:Blackboard)


      

