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
  * Last modification: 13/03/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module type Blackboard = 
sig 
  module PB:Blackboard_generation.PreBlackboard 

  (** blackboard matrix *)
  type event_case_address 
  type case_info
  type case_value 
  type case_address  

  (** blackboard*)

  type blackboard      (*blackboard, once finalized*)
  type assign_result = Fail | Success | Ignore 
        

  val get_npredicate_id: blackboard -> int 
  val get_n_unresolved_events_of_pid: blackboard -> PB.predicate_id -> int 
  val get_first_linked_event: blackboard -> PB.predicate_id -> int option 
  val get_last_linked_event: blackboard -> PB.predicate_id -> int option 
(*  val get_first_unresolved_event: blackboard -> PB.predicate_id -> int option
  val get_last_unresolved_event: blackboard -> PB.predicate_id -> int option *)
  val exist: (blackboard -> PB.predicate_id -> PB.step_short_id -> PB.H.error_channel * bool option) PB.H.with_handler   

  val set: (case_address -> case_value -> blackboard  -> PB.H.error_channel * blackboard) PB.H.with_handler 
  val get: (case_address -> blackboard -> PB.H.error_channel * case_value) PB.H.with_handler 
  val overwrite: (case_address -> case_value -> blackboard -> PB.H.error_channel * blackboard) PB.H.with_handler  
  val refine: (case_address -> case_value -> blackboard -> PB.H.error_channel * blackboard * assign_result) PB.H.with_handler  
  val branch: (blackboard -> PB.H.error_channel *blackboard) PB.H.with_handler 
  val reset_last_branching: (blackboard -> PB.H.error_channel * blackboard ) PB.H.with_handler 
  val reset_init: (blackboard -> PB.H.error_channel * blackboard) PB.H.with_handler 
  (** initialisation*)
  val import: (PB.pre_blackboard -> PB.H.error_channel * blackboard) PB.H.with_handler 

  (** output result*)
  type result 

  (** iteration*)
  val is_maximal_solution: (blackboard -> PB.H.error_channel * bool) PB.H.with_handler 

  (** exporting result*)
  val translate_blackboard: (blackboard -> PB.H.error_channel * result) PB.H.with_handler 

  (**pretty printing*)
  val print_blackboard:(out_channel -> blackboard -> PB.H.error_channel) PB.H.with_handler 
end

module Blackboard = 
  (struct 
    module PB = Blackboard_generation.Preblackboard 
     (** blackboard matrix*) 

    type assign_result = Fail | Success | Ignore 
        
    type step_id = PB.step_id 
    type step_short_id = PB.step_short_id 
        
    type event_case_address = 
	{
	  column_id:PB.predicate_id; 
	  row_id: PB.step_short_id;
	}

    let build_event_case_address pid seid = 
      {
        column_id = pid ;
        row_id = seid 
      }
	  
    type case_address = 
      | N_unresolved_events_in_column of int 
      | Pointer_to_next of event_case_address 
      | Value_after of event_case_address 
      | Value_before of event_case_address 
      | Pointer_to_previous of event_case_address
      | N_unresolved_events 
      | Exist of event_case_address 
(*      | Last_seid of int 
      | First_seid of int *)

    type case_value = 
      | State of PB.predicate_value 
      | Counter of int 
      | Pointer of step_short_id 
      | Boolean of bool option 
          
    type assignment = (case_address * case_value) 
        
    let g p string parameter handler error x y = 
      match 
        x,y 
      with 
        | State x,State y -> error,p x y 
        | _,_  ->  
          let error_list,error = PB.H.create_error parameter handler error (Some "blackboard.ml") None (Some string) (Some "112") (Some "Counters and/or Pointers should not be compared") (failwith "strictly_more_refined") in 
          PB.H.raise_error parameter handler error_list stderr error false
            
    let strictly_more_refined = g PB.strictly_more_refined "strictly_more_refined"
      
    type case_info_static  = 
	{
	  row_short_id: step_short_id;
          event_id: step_id; 
	  test: PB.predicate_value;
          action: PB.predicate_value
	}
          
    type case_info_dynamic = 
        {
          pointer_previous: step_short_id;
          pointer_next: step_short_id;
          state_after: PB.predicate_value;
           selected: bool option;
        }
          
    type case_info = 
        {
          static: case_info_static;
          dynamic: case_info_dynamic
        }
          
    let null_pointer = -1 
    let is_null_pointer x = x=(-1)
      
    let dummy_case_info_static = 
      {
	row_short_id = null_pointer ;
        event_id = null_pointer ;
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
      if seid<0 or seid>=size then null_pointer else seid 
        
    let init_info_dynamic seid size= 
      { 
        pointer_previous = correct_pointer (seid-1) size ;
        pointer_next = correct_pointer (seid+1) size ;
        state_after = PB.unknown ; 
        selected = None ;
      }
        
    let init_info_static p_id seid (_,eid,test,action) = 
      {
	row_short_id = seid ;
        event_id = eid ;
	test = test ;
	action = action ;
      }
        
    let dummy_case_info = 
      {
        static = dummy_case_info_static ;
        dynamic = dummy_case_info_dynamic
      }
        
     let init_info p_id seid size triple = 
       {
         static = init_info_static p_id seid triple ;
         dynamic = init_info_dynamic seid size
       }
         
     (** blackboard *)
     type stack = assignment list 
     type blackboard = 
         {
           n_predicate_id: int ;
           current_stack: stack ; 
           stack: stack list ;
           blackboard: case_info PB.A.t PB.A.t ;
           selected_events: bool option PB.A.t ;
           weigth_of_predicate_id: int PB.A.t ; 
           used_predicate_id: bool PB.A.t ;  
           n_unresolved_events: int ;
     (*      first_linked_event_of_predicate_id: int PB.A.t;*)
           last_linked_event_of_predicate_id: int PB.A.t;
         }

     let get_npredicate_id blackboard = blackboard.n_predicate_id 
     let get_n_unresolved_events_of_pid blackboard pid = PB.A.get blackboard.weigth_of_predicate_id pid 
     let get_pointer_next case = case.dynamic.pointer_next 
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

     let print_event_case_address log event_case_address = 
       Printf.fprintf log "event %i, predicate %i" event_case_address.row_id event_case_address.column_id 

     let print_address log address = 
       match address 
       with 
         | N_unresolved_events_in_column i -> 
           let _ = Printf.fprintf log "Nombre d'événements non résolus sur le predicat %i" i in 
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
         | Counter i | Pointer i -> Printf.fprintf log "%i" i 
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

     let print_blackboard parameter handler error log blackboard = 
       let _ = Printf.fprintf log "**\nBLACKBOARD\n**\n" in 
       let _ = Printf.fprintf log "*wires:*\n" in 
       let _ = 
         PB.A.iteri 
           (fun i array -> 
             let _ = Printf.fprintf log "%i" i in 
             if PB.A.get blackboard.used_predicate_id i 
             then 
               let _ = Printf.fprintf log "*wires %i: " i in
               let rec aux j = 
                 let case = PB.A.get array j in 
                 let _ = print_case log case in 
                 let j = get_pointer_next case in 
                 if j=null_pointer 
                 then 
                   let _ = Printf.fprintf log "\n" in ()
                 else 
                   aux j
               in 
               aux 0 
             else 
               ())
           blackboard.blackboard 
       in 
       let _ = Printf.fprintf log "*stacks*\n" in 
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


         
     let empty_stack = []

     let import parameter handler error pre_blackboard = 
       let error,n_predicates = PB.n_predicates parameter handler error pre_blackboard in  
       let error,n_events = PB.n_events parameter handler error pre_blackboard in 
       let stack = [] in 
       let current_stack = empty_stack in
       let blackboard = PB.A.make n_events (PB.A.make 1 dummy_case_info) in
       let weigth_of_predicate_id = PB.A.make n_predicates 0 in 
       let first_linked_event_of_predicate_id = PB.A.make n_predicates 0 in 
       let last_linked_event_of_predicate_id = PB.A.make n_predicates 0 in 
       let error = 
         let rec aux1 p_id error = 
           if p_id < 0 
           then error
           else 
             let error,size = PB.n_events_per_predicate parameter handler error pre_blackboard p_id in 
             let _ = PB.A.set last_linked_event_of_predicate_id p_id size in 
             let _ = PB.A.set weigth_of_predicate_id p_id size in 
             let error,list = PB.event_list_of_predicate parameter handler error pre_blackboard p_id in 
             let array = PB.A.make size dummy_case_info in 
             let _ = PB.A.set blackboard p_id array in 
             let rec aux2 seid l = 
               match l 
               with 
                 | [] -> () 
                 | triple::q -> 
                   let info = init_info p_id seid size triple in 
                   let _ = PB.A.set array seid info in 
                   aux2 (seid-1) q 
             in 
             let _ = aux2 (size-1) list in 
             aux1 (p_id-1) error 
         in aux1 (n_events-1) error 
       in 
       error,
       {
 (*        first_linked_event_of_predicate_id = first_linked_event_of_predicate_id ;*)
         last_linked_event_of_predicate_id = last_linked_event_of_predicate_id ;
         n_predicate_id = n_predicates; 
         current_stack=current_stack; 
         stack=stack;
         blackboard=blackboard;
         selected_events= PB.A.make n_events None; 
         weigth_of_predicate_id= weigth_of_predicate_id; 
         used_predicate_id = PB.A.make n_predicates true; 
         n_unresolved_events = n_events ; 
       }
         
     let get_case parameter handler error case_address blackboard = 
       error,PB.A.get 
         (PB.A.get blackboard.blackboard case_address.column_id)
         case_address.row_id 

     let exist parameter handler error blackboard pid seid = 
       let case_address = {row_id = seid ; column_id = pid} in 
       let error,info = get_case parameter handler error case_address blackboard in 
       error,info.dynamic.selected

     let set_case parameter handler error case_address case_value blackboard = 
       let _ = PB.A.set (PB.A.get blackboard.blackboard case_address.column_id) case_address.row_id case_value in 
       error,blackboard 

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
                   PB.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "398") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.H.raise_error parameter handler error_list stderr error blackboard 
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
                   PB.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "423") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.H.raise_error parameter handler error_list stderr error blackboard 
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
                   PB.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "438") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.H.raise_error parameter handler error_list stderr error blackboard 
           end 
         | Value_before case_address -> 
           let error_list,error = 
             PB.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "430") (Some "Blackboard.set should not be called with value_before") (failwith "Incompatible address in function Blackboard.set")
           in 
           PB.H.raise_error parameter handler error_list stderr error blackboard 
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
                   PB.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "458") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.H.raise_error parameter handler error_list stderr error blackboard 
              end 
         | N_unresolved_events -> 
           begin 
             match case_value
             with 
               | Counter int -> 
                 error,{blackboard with n_unresolved_events = int}
               | _ -> 
                 let error_list,error = 
                   PB.H.create_error parameter handler error (Some "blackboard.ml") None (Some "set") (Some "470") (Some "Incompatible address and value in function set") (failwith "Incompatible address and value in function Blackboard.set")
                 in 
                 PB.H.raise_error parameter handler error_list stderr error blackboard 
           end 
             
     let rec get parameter handler error case_address blackboard = 
       match 
         case_address 
       with 
         | N_unresolved_events_in_column int -> error,Counter (PB.A.get blackboard.weigth_of_predicate_id int)
         | Pointer_to_next case_address -> 
           let error,case = get_case parameter handler error case_address blackboard in 
           error,Pointer case.dynamic.pointer_next 
         | Value_after case_address -> 
           let error,case = get_case parameter handler error case_address blackboard in 
           error,Pointer case.dynamic.pointer_next 
         | Value_before case_address -> 
           let error,case = get_case parameter handler error case_address blackboard in 
           let pointer = case.dynamic.pointer_previous in 
           if is_null_pointer pointer 
           then 
              let error_list,error = 
                PB.H.create_error parameter handler error (Some "blackboard.ml") None (Some "get") (Some "492") (Some "Value before an unexisting element requested ") (failwith "Value before an unexisting element requested")  
              in 
              PB.H.raise_error parameter handler error_list stderr error (State PB.undefined)
           else 
             get parameter handler error (Value_after {case_address with row_id = pointer}) blackboard 
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
         error,blackboard,Ignore
       else
         let error,bool = strictly_more_refined parameter handler error case_value old 
         in 
         if bool 
         then 
           let error,blackboard = set parameter handler error case_address case_value blackboard in 
           let error,blackboard = record_modif parameter handler error case_address old blackboard in 
           error,blackboard,Success
         else 
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


         
     let branch parameter handler error blackboard = 
       error,
       {
         blackboard 
        with 
          stack = blackboard.current_stack::blackboard.stack ; 
          current_stack = []
       }
       
     let reset_last_branching parameter handler error blackboard = 
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
     type result = ()
         
  (** iteration*)
     let is_maximal_solution parameter handler error blackboard = error,false 
 
  (** exporting result*)
       
   let translate_blackboard parameter handler error blackboard = error,()
       
  
   end:Blackboard)


      

