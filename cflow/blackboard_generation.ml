(**
  * cflow_handler.ml
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS 
  *  
  * Creation: 29/08/2011
  * Last modification: 03/08/2013
  * * 
  * Some parameter references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011,2012,2013 Institut National de Recherche en Informatique 
  * et en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let debug_mode = false

module type PreBlackboard = 
sig 
  module A:LargeArray.GenArray
  module CI:Pseudo_inverse.Cut_pseudo_inverse 

  type step_id = int 
  type step_short_id = int 

  (** blackboard predicates*)

  type predicate_id = int
  type predicate_info
  type predicate_value 
 
  module C:(Cache.Cache with type O.t = predicate_value)  


  type pre_blackboard  (*blackboard during its construction*)

  val weakening: predicate_value -> predicate_value list 
  val conj: (predicate_value -> predicate_value -> CI.Po.K.H.error_channel * predicate_value) CI.Po.K.H.with_handler
  val disjunction: (predicate_value -> predicate_value -> CI.Po.K.H.error_channel * predicate_value) CI.Po.K.H.with_handler  
 
  val defined: predicate_value
  val undefined: predicate_value 
  val unknown: predicate_value
  val is_unknown: predicate_value -> bool 
  val is_undefined: predicate_value -> bool 
  val more_refined: predicate_value -> predicate_value -> bool 
  val compatible: predicate_value -> predicate_value -> bool 
  val strictly_more_refined: predicate_value -> predicate_value -> bool 
  val get_pre_column_map_inv: pre_blackboard -> predicate_info A.t
  (** generation*)
  val init:  ((*CI.Po.K.P.log_info ->*) CI.Po.K.H.error_channel * pre_blackboard) CI.Po.K.H.with_handler 
  val add_step: (CI.Po.K.P.log_info -> CI.Po.K.refined_step -> pre_blackboard -> step_id -> CI.Po.K.H.error_channel * CI.Po.K.P.log_info * pre_blackboard * step_id) CI.Po.K.H.with_handler
  val add_step_up_to_iso: (CI.Po.K.P.log_info -> CI.Po.K.refined_step -> pre_blackboard -> step_id -> CI.Po.K.H.error_channel * CI.Po.K.P.log_info * pre_blackboard * step_id) CI.Po.K.H.with_handler
  val finalize: (CI.Po.K.P.log_info -> pre_blackboard -> CI.Po.K.H.error_channel * CI.Po.K.P.log_info * pre_blackboard) CI.Po.K.H.with_handler 

  (**pretty printing*)
  val string_of_predicate_value: predicate_value -> string 
  val print_predicate_value: out_channel ->  predicate_value -> unit 
  val print_preblackboard: (pre_blackboard -> CI.Po.K.H.error_channel) CI.Po.K.H.with_handler  

  (**interface*)
  val n_events: (pre_blackboard -> CI.Po.K.H.error_channel * int) CI.Po.K.H.with_handler 
  val n_predicates: (pre_blackboard -> CI.Po.K.H.error_channel * int) CI.Po.K.H.with_handler 
  val n_events_per_predicate: (pre_blackboard -> predicate_id -> CI.Po.K.H.error_channel * int) CI.Po.K.H.with_handler 
  val event_list_of_predicate: (pre_blackboard -> predicate_id -> CI.Po.K.H.error_channel * (int * int * predicate_value * predicate_value ) list) CI.Po.K.H.with_handler 
  val mandatory_events: (pre_blackboard -> CI.Po.K.H.error_channel * ((int list * unit Mods.simulation_info option) list)) CI.Po.K.H.with_handler 
  val get_pre_event: (pre_blackboard -> CI.Po.K.H.error_channel * CI.Po.K.refined_step A.t) CI.Po.K.H.with_handler 
  val get_side_effect: (pre_blackboard -> CI.Po.K.H.error_channel * CI.Po.K.side_effect A.t) CI.Po.K.H.with_handler 
  val get_fictitious_observable: (pre_blackboard -> CI.Po.K.H.error_channel * int option) CI.Po.K.H.with_handler 
  val get_level_of_event: (pre_blackboard -> step_id -> CI.Po.K.H.error_channel * Priority.level) CI.Po.K.H.with_handler 
  val levels: pre_blackboard -> Priority.level A.t
  val print_predicate_info: out_channel -> predicate_info -> unit 
end

module Preblackboard = 
  (struct 

     (** Useful modules *)
     module H = Cflow_handler.Cflow_handler
     module A = Mods.DynArray
     module CI = Pseudo_inverse.Pseudo_inv

     (** blackboard matrix*) 

     type step_id = int       (** global id of an event *)
     type step_short_id = int (** position of an event on a wire *)

     (** blackboard predicates*)

     type rule_type = (** kind of events*)
     | Subs 
     | Dummy 
     | Init 
     | Observable
     | Rule 
     | Side_effect_of of (step_id * (CI.Po.K.agent_id * CI.Po.K.site_name) list)
     
     type predicate_id = int (** wire identifiers *)
     type mutex = 
     | Lock_side_effect of step_id * CI.Po.K.agent_id * CI.Po.K.agent_id * CI.Po.K.site_name 
     | Lock_agent of step_id * CI.Po.K.agent_id
     | Lock_rectangular of step_id * CI.Po.K.agent_id 
     | Lock_links of step_id * (CI.Po.K.agent_id * CI.Po.K.agent_id)

     type predicate_info = (** wire labels *)
     | Fictitious 
     | Here of CI.Po.K.agent_id  
     | Bound_site of CI.Po.K.agent_id * CI.Po.K.site_name
     | Internal_state of CI.Po.K.agent_id * CI.Po.K.site_name 
     | Pointer of step_id * CI.Po.K.agent_id 
     | Link of step_id * CI.Po.K.agent_id * CI.Po.K.agent_id 
     | Mutex of mutex
     (*
       | Fictitious of int (**to handle with ambiguous site effects *)*)

     type predicate_value = 
       | Counter of int 
       | Pointer_to_agent of CI.Po.K.agent_name 
       | Internal_state_is of CI.Po.K.internal_state
       | Defined   (** the wire does exist, but we do not know what the value is *)
       | Undefined (** the wire does not exist yet *)
       | Present   (** for agent presence *)
       | Free      (** for binding sites *)
       | Bound     (** for binding sites (partial information) *)
       | Bound_to of predicate_id * CI.Po.K.agent_id * CI.Po.K.agent_name * CI.Po.K.site_name
           (** for bindinf sites (complete information) *)
       | Bound_to_type of CI.Po.K.agent_name * CI.Po.K.site_name (** for binding sites (partial information *)
       | Unknown (**  for agent presence, internal states, binding states (partial information *) 

     module C = (Cache.Cache(struct type t = predicate_value let compare = compare end):Cache.Cache with type O.t = predicate_value) 

     let weakening p = 
       match p 
       with 
         | Pointer_to_agent _ | Counter _ | Internal_state_is _ | Present | Free | Bound  -> [p;Defined]
         | Bound_to (_,_,ag,site) -> [p;Bound_to_type (ag,site);Bound;Defined]
         | Bound_to_type _ -> [p;Bound;Defined]
         | Defined | Undefined -> [p]
         | Unknown -> []

     let defined = Defined 
     let undefined = Undefined 
     let unknown = Unknown 
     let is_unknown x = x=Unknown 
     let is_undefined x = x=Undefined 

     (** maps and sets *)
     module PredicateMap = Map.Make (struct type t = predicate_info let compare = compare end)
     module PredicateSet = Set.Make (struct type t = predicate_info let compare = compare end)
     module CaseValueSet = Set.Make (struct type t = predicate_value let compare = compare end)
     module PredicateidSet = Set.Make (struct type t = predicate_id let compare = compare end)
     module PredicateidMap = Map.Make (struct type t = predicate_id let compare = compare end)
     module SidMap = Map.Make (struct type t = step_id let compare = compare end
)
     module AgentIdMap = Map.Make (struct type t = CI.Po.K.agent_id let compare = compare end)
     module AgentId2Map = Map.Make (struct type t = CI.Po.K.agent_id*CI.Po.K.agent_id let compare=compare end)
     module AgentIdSet = Set_patched.Make (struct type t = CI.Po.K.agent_id let compare = compare end)
     module AgentId2Set = Set.Make (struct type t = CI.Po.K.agent_id*CI.Po.K.agent_id let compare=compare end)
     module SiteIdSet = Set.Make (struct type t = (CI.Po.K.agent_id*CI.Po.K.site_name) let compare=compare end)
     module SiteIdMap = Map.Make (struct type t = (CI.Po.K.agent_id*CI.Po.K.site_name) let compare=compare end)

     type pre_blackboard = 
	 {
           pre_fictitious_list: predicate_id list ; (** list of wire for mutual exclusions, the state must be undefined at the end of the trace *) 
           pre_steps_by_column: (step_short_id * (step_id * step_short_id * predicate_value * predicate_value) list) A.t; (** maps each wire to the last known value and the list of step (step id,test,action)*)
           pre_kind_of_event: rule_type A.t; (** maps each event id to the kind of event *)
           pre_event: CI.Po.K.refined_step A.t; (** maps each event to the step *)
	   pre_nsteps: step_id; (**id of the last event *)
	   pre_ncolumn: predicate_id; (**id of the last wire *)
	   pre_column_map: predicate_id PredicateMap.t; (** maps each wire label to its wire id *)
	   pre_column_map_inv: predicate_info A.t; (** maps each wire id to its wire label *)
	   predicate_id_list_related_to_predicate_id: PredicateidSet.t A.t; (** maps each wire id for the presence of an agent to the set of wires for its attibute (useful, when an agent get removed, all its attributes get undefined *)
           history_of_predicate_values_to_predicate_id: C.t A.t; (* maps each wire to the set of its previous states, this summarize the potential state of a site that is freed, so as to overapproximate the set of potential side effects*)
           history_of_agent_ids_of_type: (CI.Po.K.agent_id list) A.t; 
           pre_observable_list: (step_id list * unit Mods.simulation_info option) list ;
           pre_side_effect_of_event: CI.Po.K.side_effect A.t;
           pre_fictitious_observable: step_id option; (*id of the step that closes all the side-effect mutex *) 
           pre_level_of_event: Priority.level A.t;
           } 

         let levels b = b.pre_level_of_event 
         let get_pre_column_map_inv x = x.pre_column_map_inv 
         let get_pre_event parameter handler error x = error,x.pre_event 
           
         (** pretty printing *)
         let print_predicate_info log x = 
           match x 
           with 
         | Here i -> Printf.fprintf log "Agent_Here %i" i
         | Bound_site (i,s) -> Printf.fprintf log "Binding_state (%i,%i)" i s 
         | Internal_state (i,s) -> Printf.fprintf log "Internal_state (%i,%i)" i s 
         | Pointer (eid,id) -> Printf.fprintf log "Pointer(eid:%i,ag_id:%i)" eid id 
         | Link (eid,id1,id2) -> Printf.fprintf log "Link(eid:%i,%i-%i)" eid id1 id2 
         | Mutex (Lock_agent (int,int2)) -> Printf.fprintf log "Mutex (Step-id:%i,Agent_id:%i)" int int2 
         | Mutex (Lock_rectangular (int,int2)) -> Printf.fprintf log "Mutex_inv (Step-id:%i,Agent_id:%i)" int int2 
         | Mutex (Lock_links(int,(int2,int3))) -> Printf.fprintf log "Mutex_links (Step-id:%i,%i-%i)" int int2 int3
         | Mutex (Lock_side_effect (int,int2,int3,int4)) -> Printf.fprintf log "Mutex_side_effect (Step-id:%i,%i/%i.%i)" int int2 int3 int4
         | Fictitious -> Printf.fprintf log "Fictitious" 
           
         let print_known log t x = 
           match t
           with 
             | Unknown -> ()
             | _ -> Printf.fprintf log "%s" x
               
         let string_of_predicate_value x = 
	   match x 
           with 
             | Counter int -> "Counter "^(string_of_int int)
             | Defined -> "Defined" 
             | Internal_state_is internal_state -> 
               (string_of_int internal_state)
             | Undefined -> 
               "Undefined"
             | Present ->
               "Present"
             | Free -> 
               "Free" 
             | Bound -> 
               "Bound" 
             | Bound_to (id,agent_id,agent_name,site) ->
               "Bound("^(string_of_int id)^","^(string_of_int agent_id)^"("^(string_of_int agent_name)^")@"^(string_of_int site)^")"
             | Bound_to_type (agent,site)-> 
               "Bound("^(string_of_int agent)^"@"^(string_of_int site)^")"
             | Pointer_to_agent (agent_id) -> 
	        "Pointer("^(string_of_int agent_id)^")"
             | Unknown -> ""

         let print_predicate_value log x = 
              Printf.fprintf log "%s" (string_of_predicate_value x) 
         let print_predicate_id log blackboard i = 
           let predicate_info = A.get blackboard.pre_column_map_inv i in  
           let _ = Printf.fprintf log "Predicate: %i " i in 
           let _ = print_predicate_info log predicate_info in 
           let _ = Printf.fprintf log "\n" in 
           ()
             
         let print_preblackboard parameter handler error blackboard = 
           let log = parameter.CI.Po.K.H.out_channel in 
           let _ = Printf.fprintf log "**\nPREBLACKBOARD\n**\n" in 
           let _ = Printf.fprintf log "*\n agent types \n*\n" in 
           let _ = 
             A.iteri 
               (fun name -> 
                 let _ = Printf.fprintf log "\nAgent name: %i \n" name in 
                 List.iter 
                   (Printf.fprintf log " id: %i \n"))
               blackboard.history_of_agent_ids_of_type
           in 
           let _ = Printf.fprintf log "*\n steps by column\n*\n" in 
           let _ = 
             A.iteri 
               (fun id (nevents,list) ->
                 let _ = print_predicate_id log blackboard id  in
                 let _ = Printf.fprintf log "nevents: %i \n" nevents in 
                 let _ = 
                   List.iter 
                     (fun (eid,seid,test,action) -> 
                       let _ = Printf.fprintf log "Event id: %i \n" eid in 
                       let _ = Printf.fprintf log "Short id: %i \n" seid in 
                       let _ = print_known log test "TEST:   " in
                       let _ = print_predicate_value log test in 
                       let _ = Printf.fprintf log "\n" in 
                       let _ = print_known log action "ACTION: " in 
                       let _ = print_predicate_value log action in 
                       let _ = Printf.fprintf log "\n" in 
                       ())
                     (List.rev list)
                 in 
                 let _ = Printf.fprintf log "---\n" in 
                 ())
               blackboard.pre_steps_by_column 
           in 
           let _ = Printf.fprintf log "*\nSide effects \n*\n" in 
           let _ = A.iteri 
             (fun i list -> 
               let _ = Printf.fprintf log "event %i:\n " i in
               let _ = CI.Po.K.print_side_effect log list in 
               let _ = Printf.fprintf log "\n" in 
               ()
             )
             blackboard.pre_side_effect_of_event 
           in 
           let _ = Printf.fprintf log "*\nPredicate_id related to the predicate \n*\n" in 
           
           let _ = 
             A.iteri 
               (fun i s -> 
                 let _ = print_predicate_id log blackboard i in 
                 let _ = 
                   PredicateidSet.iter
                     (fun s -> Printf.fprintf log "%i\n" s)
                     s
                 in 
                 let _ = Printf.fprintf log "---\n" in 
                 ()
               )
               blackboard.predicate_id_list_related_to_predicate_id 
           in 
           let _ = Printf.fprintf log "*\nPast values of a predicate \n*\n" in 
           let _ = 
             A.iteri 
               (fun i s -> 
                 let _ = print_predicate_id log blackboard i in 
                 let _ = 
                   C.iter 
                     (fun s -> print_predicate_value log s)
                     s
                 in 
                 let _ = Printf.fprintf log "---\n" in 
                 ()
               )
               blackboard.history_of_predicate_values_to_predicate_id 
           in 
           let _ = Printf.fprintf log "*\nObservables \n*\n" in
           let _ = 
             List.iter 
               (fun (l,_) -> 
                 let _ = List.iter (Printf.fprintf log "%i,") l in 
                 let _ = Printf.fprintf log "\n" in 
                 () 
               )
               blackboard.pre_observable_list 
           in 
           
           let _ = Printf.fprintf log "**\n" in 
           error 
             
     (** information lattice *)
         let strictly_more_refined x y = 
           match y  
           with 
             | Undefined 
             | Pointer_to_agent _ 
             | Counter _ 
             | Internal_state_is _ 
             | Present 
             | Free 
             | Bound_to (_) -> false
             | Bound_to_type (ag,s) -> 
               begin 
                 match x 
                 with 
                   | Bound_to(_,_,ag',s') when ag=ag' && s=s' -> true
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
                   | Unknown | Defined | Undefined -> false
                   | _ -> true
               end 
             | Unknown -> 
               begin
                 match x
                 with 
                   | Unknown -> false
                   | _ -> true 
               end
                 
         let more_refined x y = x=y || strictly_more_refined x y
           
         let conj parameter handler error x y = 
           if more_refined x y then error,x 
           else 
             if strictly_more_refined y x then error,y 
             else 
               let error_list,error = CI.Po.K.H.create_error parameter handler error (Some "blackboard_generation.ml") None (Some "conj") (Some "323") (Some "Arguments have no greatest lower bound") (failwith "Arguments have no greatest lower bound")  in 
               CI.Po.K.H.raise_error parameter handler error_list error Undefined  
                 
         let compatible x y = 
           x=y || more_refined x y || more_refined y x
           
         let disjunction parameter handler error x y = 
           error,
           if x=y then x 
           else 
             match x,y
             with
               | Unknown,_ | _,Unknown | Undefined,_ | _,Undefined -> Unknown 
               | Defined,_ | _,Defined -> Defined
               | Counter _,_ | _,Counter _ | Free,_ | _,Free | Present,_ | _,Present | Internal_state_is _,_ |_,Internal_state_is _ -> Defined 
               | Bound,_ | _,Bound -> Bound 
               | Bound_to_type (a,b), Bound_to (_,_,c,d)
               | Bound_to (_,_,a,b),Bound_to (_,_,c,d)
               | Bound_to (_,_,a,b),Bound_to_type (c,d) when a=c && b=d -> Bound_to_type(a,b)  
               | _ -> Bound 
                 
  (** predicate id allocation *)
                 
     (** if a wire concerns an agent, which one it is *)
         let agent_id_of_predicate x =  
           match x 
           with 
             | Here x -> Some x 
             | Bound_site (x,_) -> Some x 
             | Internal_state (x,_) -> Some x 
             | Pointer _ | Link _ | Mutex _  | Fictitious -> None 
               
               
         let rec bind parameter handler error blackboard predicate predicate_id ag_id =
           let error,blackboard,sid = allocate parameter handler error blackboard (Here ag_id)
           in 
           let old_set = 
             try 
               A.get blackboard.predicate_id_list_related_to_predicate_id sid
             with 
                 Not_found -> 
                   PredicateidSet.empty
           in 
           let new_set = 
             PredicateidSet.add predicate_id old_set 
           in 
           try 
             let _ = A.set blackboard.predicate_id_list_related_to_predicate_id sid new_set in 
             error,blackboard 
           with 
               Not_found ->
                 let error_list,error = 
                   CI.Po.K.H.create_error parameter handler error (Some "blackboard_generation.ml") None (Some "bind") (Some "375") (Some "Out of bound access") (failwith "bind") in 
                 CI.Po.K.H.raise_error parameter handler error_list error blackboard 
         and 
             allocate parameter handler error blackboard predicate  = 
           let ag_id = agent_id_of_predicate predicate in 
           let map = blackboard.pre_column_map in 
           let map_inv = blackboard.pre_column_map_inv in 
           try 
             let sid = PredicateMap.find predicate map in
             error,blackboard,sid
           with 
             | Not_found -> 
               let sid'= blackboard.pre_ncolumn + 1 in 
               let map' = PredicateMap.add predicate sid' map in 
               let _  = A.set map_inv sid' predicate in 
               let map_inv' = map_inv in 
               let _ = A.set blackboard.history_of_predicate_values_to_predicate_id sid' (C.create parameter.CI.Po.K.H.cache_size) in 
               let blackboard = 
                 {blackboard 
                  with 
                    pre_ncolumn = sid' ; 
                    pre_column_map = map' ;
                    pre_column_map_inv = map_inv' 
                 }
               in 
               let error,blackboard = 
                 match ag_id 
                 with 
                   | None -> 
                     error, blackboard 
                   | Some ag_id -> 
                     bind parameter handler error blackboard predicate sid' ag_id 
               in 
               error,blackboard,sid' 

         let create_agent parameter handler error blackboard agent_name agent_id  = 
           let old_list = 
             try 
               A.get blackboard.history_of_agent_ids_of_type agent_name 
             with 
               Not_found -> 
                 []
           in 
           let new_list = agent_id::old_list in 
           let _ = A.set blackboard.history_of_agent_ids_of_type agent_name new_list in 
           error,blackboard 


                 
         let free_agent parameter handler error blackboard agent_id = 
           let error,blackboard,predicate_id = 
             allocate parameter handler error blackboard (Here agent_id)  in 
           let error,set = 
             try 
               error,A.get blackboard.predicate_id_list_related_to_predicate_id predicate_id
             with 
               | _ -> 
                 let error_list,error = 
                   CI.Po.K.H.create_error parameter handler error (Some "blackboard_generation.ml") None (Some "free_agent") (Some "418") (Some "Try to free an unexisting agent") (failwith "free_agent") in 
                 CI.Po.K.H.raise_error parameter handler error_list error PredicateidSet.empty 
           in 
           let map = 
             PredicateidSet.fold 
               (fun predicate_id map -> 
                 let predicate = A.get blackboard.pre_column_map_inv predicate_id in 
                 PredicateMap.remove predicate map
               )
               set 
               blackboard.pre_column_map
           in 
           error,{blackboard with pre_column_map = map}
         
         let free_agent_if_it_exists parameter handler error blackboard agent_id = 
           try 
             let _ = PredicateMap.find (Here agent_id) blackboard.pre_column_map in
             free_agent parameter handler error blackboard agent_id 
           with 
             | _ -> error,blackboard 
             
         let predicates_of_action_no_subs parameter handler error blackboard init action = 
           match action with 
             | CI.Po.K.Create (ag,interface) -> 
               let ag_id = CI.Po.K.agent_id_of_agent ag in
               let agent_name = CI.Po.K.agent_name_of_agent ag in 
               let error,blackboard = create_agent parameter handler error blackboard agent_name ag_id in 
               let error,blackboard = 
                 if init 
                 then 
                   error,blackboard
                 else 
                   free_agent_if_it_exists parameter handler error blackboard ag_id in 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Here ag_id) in   
               List.fold_left 
                 (fun (error,blackboard,list1,list2) (s_id,opt) -> 
                   let error,blackboard,predicate_id = allocate parameter handler error blackboard (Bound_site(ag_id,s_id)) in 
                   let list1 = (predicate_id,Free)::list1 in
                   let list2 = (predicate_id,Undefined)::list2 in 
                   match opt 
                   with 
                     | None -> error,blackboard,list1,list2
                     | Some x -> 
                       let error,blackboard,predicate_id = allocate parameter handler error blackboard (Internal_state (ag_id,s_id)) in 
                       error,
                       blackboard,
                       (predicate_id,Internal_state_is x)::list1,
                       (predicate_id,Undefined)::list2
                 )
                 (error,blackboard,[predicate_id,Present],[predicate_id,Undefined])
                 interface
             | CI.Po.K.Mod_internal (site,int)  -> 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Internal_state (CI.Po.K.agent_id_of_site site,CI.Po.K.site_name_of_site site)) in 
               error,blackboard,[predicate_id,Internal_state_is int],[]
             | CI.Po.K.Bind_to (s1,s2) -> 
               let ag_id1 = CI.Po.K.agent_id_of_site s1 in 
               let ag_id2 = CI.Po.K.agent_id_of_site s2 in 
               let agent_name2 = CI.Po.K.agent_name_of_site s2 in 
               let site_id1 = CI.Po.K.site_name_of_site s1 in 
               let site_id2 = CI.Po.K.site_name_of_site s2 in 
               let error,blackboard,predicate_id1 = allocate parameter handler error blackboard (Bound_site (ag_id1,site_id1)) in 
               let error,blackboard,predicate_id2 = allocate parameter handler error blackboard (Bound_site (ag_id2,site_id2)) in 
               error,blackboard,
               [predicate_id1,Bound_to (predicate_id2,ag_id2,agent_name2,site_id2)],[]
             | CI.Po.K.Bind (s1,s2) -> 
               let ag_id1 = CI.Po.K.agent_id_of_site s1 in 
               let ag_id2 = CI.Po.K.agent_id_of_site s2 in 
               let agent_name1 = CI.Po.K.agent_name_of_site s1 in 
               let agent_name2 = CI.Po.K.agent_name_of_site s2 in 
               let site_id1 = CI.Po.K.site_name_of_site s1 in 
               let site_id2 = CI.Po.K.site_name_of_site s2 in 
               let error,blackboard,predicate_id1 = allocate parameter handler error blackboard (Bound_site (ag_id1,site_id1)) in 
               let error,blackboard,predicate_id2 = allocate parameter handler error blackboard (Bound_site (ag_id2,site_id2)) in 
               error,blackboard,
               [predicate_id1,Bound_to (predicate_id2,ag_id2,agent_name2,site_id2);
                predicate_id2,Bound_to (predicate_id1,ag_id1,agent_name1,site_id1)],[]
             | CI.Po.K.Unbind (s1,s2) ->
               let ag_id1 = CI.Po.K.agent_id_of_site s1 in 
               let ag_id2 = CI.Po.K.agent_id_of_site s2 in 
               let site_id1 = CI.Po.K.site_name_of_site s1 in 
               let site_id2 = CI.Po.K.site_name_of_site s2 in 
               let error,blackboard,predicate_id1 = allocate parameter handler error blackboard (Bound_site (ag_id1,site_id1)) in 
               let error,blackboard,predicate_id2 = allocate parameter handler error blackboard (Bound_site (ag_id2,site_id2)) in 
               error,blackboard,[predicate_id1,Free;predicate_id2,Free],[]
             | CI.Po.K.Free s -> 
               let ag_id = CI.Po.K.agent_id_of_site s in 
               let site_id = CI.Po.K.site_name_of_site s in 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Bound_site (ag_id,site_id)) in     
               error,blackboard,[predicate_id,Free],[]
             | CI.Po.K.Remove ag -> 
               let ag_id = CI.Po.K.agent_id_of_agent ag in 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Here ag_id) in 
               let error,blackboard = free_agent parameter handler error blackboard ag_id in 
               let set = 
                 A.get 
                   blackboard.predicate_id_list_related_to_predicate_id
                   predicate_id 
               in 
               let error,blackboard,list = 
                 PredicateidSet.fold 
                   (fun predicateid (error,blackboard,list) -> 
                     error,blackboard,(predicateid,Undefined)::list)
                   set 
                   (error,blackboard,[predicate_id,Undefined]) 
               in   
               error,blackboard,list,[]

          let predicates_of_action_subs parameter handler error blackboard init action = 
           match action with 
             | CI.Po.K.Create (ag,interface) -> 
               let ag_id = CI.Po.K.agent_id_of_agent ag in
               let agent_name = CI.Po.K.agent_name_of_agent ag in 
               let error,blackboard = create_agent parameter handler error blackboard agent_name ag_id in 
               let error,blackboard = 
                 if init 
                 then 
                   error,blackboard
                 else 
                   free_agent_if_it_exists parameter handler error blackboard ag_id in 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Here ag_id) in   
               List.fold_left 
                 (fun (error,blackboard,list1,list2) (s_id,opt) -> 
                   let error,blackboard,predicate_id = allocate parameter handler error blackboard (Bound_site(ag_id,s_id)) in 
                   let list1 = (predicate_id,Free)::list1 in
                   let list2 = (predicate_id,Undefined)::list2 in 
                   match opt 
                   with 
                     | None -> error,blackboard,list1,list2
                     | Some x -> 
                       let error,blackboard,predicate_id = allocate parameter handler error blackboard (Internal_state (ag_id,s_id)) in 
                       error,
                       blackboard,
                       (predicate_id,Internal_state_is x)::list1,
                       (predicate_id,Undefined)::list2
                 )
                 (error,blackboard,[predicate_id,Present],[predicate_id,Undefined])
                 interface
             | CI.Po.K.Mod_internal (site,int)  -> 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Internal_state (CI.Po.K.agent_id_of_site site,CI.Po.K.site_name_of_site site)) in 
               error,blackboard,[predicate_id,Internal_state_is int],[]
             | CI.Po.K.Bind_to (s1,s2) -> 
               let ag_id1 = CI.Po.K.agent_id_of_site s1 in 
               let ag_id2 = CI.Po.K.agent_id_of_site s2 in 
               let agent_name2 = CI.Po.K.agent_name_of_site s2 in 
               let site_id1 = CI.Po.K.site_name_of_site s1 in 
               let site_id2 = CI.Po.K.site_name_of_site s2 in 
               let error,blackboard,predicate_id1 = allocate parameter handler error blackboard (Bound_site (ag_id1,site_id1)) in 
               let error,blackboard,predicate_id2 = allocate parameter handler error blackboard (Bound_site (ag_id2,site_id2)) in 
               error,blackboard,
               [predicate_id1,Bound_to (predicate_id2,ag_id2,agent_name2,site_id2)],[]
             | CI.Po.K.Bind (s1,s2) -> 
               let ag_id1 = CI.Po.K.agent_id_of_site s1 in 
               let ag_id2 = CI.Po.K.agent_id_of_site s2 in 
               let agent_name1 = CI.Po.K.agent_name_of_site s1 in 
               let agent_name2 = CI.Po.K.agent_name_of_site s2 in 
               let site_id1 = CI.Po.K.site_name_of_site s1 in 
               let site_id2 = CI.Po.K.site_name_of_site s2 in 
               let error,blackboard,predicate_id1 = allocate parameter handler error blackboard (Bound_site (ag_id1,site_id1)) in 
               let error,blackboard,predicate_id2 = allocate parameter handler error blackboard (Bound_site (ag_id2,site_id2)) in 
               error,blackboard,
               [predicate_id1,Bound_to (predicate_id2,ag_id2,agent_name2,site_id2);
                predicate_id2,Bound_to (predicate_id1,ag_id1,agent_name1,site_id1)],[]
             | CI.Po.K.Unbind (s1,s2) ->
               let ag_id1 = CI.Po.K.agent_id_of_site s1 in 
               let ag_id2 = CI.Po.K.agent_id_of_site s2 in 
               let site_id1 = CI.Po.K.site_name_of_site s1 in 
               let site_id2 = CI.Po.K.site_name_of_site s2 in 
               let error,blackboard,predicate_id1 = allocate parameter handler error blackboard (Bound_site (ag_id1,site_id1)) in 
               let error,blackboard,predicate_id2 = allocate parameter handler error blackboard (Bound_site (ag_id2,site_id2)) in 
               error,blackboard,[predicate_id1,Free;predicate_id2,Free],[]
             | CI.Po.K.Free s -> 
               let ag_id = CI.Po.K.agent_id_of_site s in 
               let site_id = CI.Po.K.site_name_of_site s in 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Bound_site (ag_id,site_id)) in     
               error,blackboard,[predicate_id,Free],[]
             | CI.Po.K.Remove ag -> 
               let ag_id = CI.Po.K.agent_id_of_agent ag in 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Here ag_id) in 
(*               let error,blackboard = free_agent parameter handler error blackboard ag_id in *)
               let set = 
                 A.get 
                   blackboard.predicate_id_list_related_to_predicate_id
                   predicate_id 
               in 
               let error,blackboard,list = 
                 PredicateidSet.fold 
                   (fun predicateid (error,blackboard,list) -> 
                     error,blackboard,(predicateid,Undefined)::list)
                   set 
                   (error,blackboard,[predicate_id,Undefined]) 
               in   
               error,blackboard,list,[]
                 
          let predicates_of_action bool = 
            if bool then predicates_of_action_subs
            else predicates_of_action_no_subs 

         let predicates_of_test  parameter handler error blackboard test = 
           match test
           with 
             | CI.Po.K.Is_Here (agent) ->
               let ag_id = CI.Po.K.agent_id_of_agent agent in 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Here ag_id) in 
               error,blackboard,[predicate_id,Present]
             | CI.Po.K.Has_Internal(site,int) -> 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Internal_state (CI.Po.K.agent_id_of_site site,CI.Po.K.site_name_of_site site)) in 
               error,blackboard,[predicate_id,Internal_state_is int]
             | CI.Po.K.Is_Free s -> 
               let ag_id = CI.Po.K.agent_id_of_site s in 
               let site_id = CI.Po.K.site_name_of_site s in 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Bound_site (ag_id,site_id)) in     
               error,blackboard,[predicate_id,Free]
             | CI.Po.K.Is_Bound_to  (s1,s2) -> 
               let ag_id1 = CI.Po.K.agent_id_of_site s1 in 
               let ag_id2 = CI.Po.K.agent_id_of_site s2 in 
               let agent_name1 = CI.Po.K.agent_name_of_site s1 in 
               let agent_name2 = CI.Po.K.agent_name_of_site s2 in 
               let site_id1 = CI.Po.K.site_name_of_site s1 in 
               let site_id2 = CI.Po.K.site_name_of_site s2 in 
               let error,blackboard,predicate_id1 = allocate parameter handler error blackboard (Bound_site (ag_id1,site_id1)) in 
               let error,blackboard,predicate_id2 = allocate parameter handler error blackboard (Bound_site (ag_id2,site_id2)) in 
               error,blackboard,
               [predicate_id1,Bound_to (predicate_id2,ag_id2,agent_name2,site_id2);
                predicate_id2,Bound_to (predicate_id1,ag_id1,agent_name1,site_id1)]
             | CI.Po.K.Is_Bound s -> 
               let ag_id = CI.Po.K.agent_id_of_site s in 
               let site_id = CI.Po.K.site_name_of_site s in 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Bound_site (ag_id,site_id)) in 
               error,blackboard,
               [predicate_id,Bound]   
             | CI.Po.K.Has_Binding_type (s,btype) ->
               let ag_id = CI.Po.K.agent_id_of_site s in 
               let site_id = CI.Po.K.site_name_of_site s in
               let agent_name = CI.Po.K.agent_of_binding_type btype in 
               let site_name = CI.Po.K.site_of_binding_type btype in 
               let error,blackboard,predicate_id = allocate parameter handler error blackboard (Bound_site (ag_id,site_id)) in 
               error,blackboard,
               [predicate_id,Bound_to_type (agent_name,site_name)]
                 
         let type_of_step x = 
           match x 
           with 
           | CI.Po.K.Subs _ -> Subs  
           | CI.Po.K.Dummy _ -> Dummy 
           | CI.Po.K.Init _ -> Init 
           | CI.Po.K.Event _ -> Rule 
           | CI.Po.K.Obs _ -> Observable
               
  (** initialisation*)
         let init parameter handler error (*log_info*) = 
           error, 
           {
             pre_side_effect_of_event = A.make 1 CI.Po.K.empty_side_effect;
             pre_event = A.make 1 (CI.Po.K.dummy_refined_step "");
             pre_fictitious_list = [] ; 
             pre_steps_by_column = A.make 1 (1,[]) ; 
             pre_nsteps = -1 ;
             pre_ncolumn = -1 ;
             pre_column_map = PredicateMap.empty ; 
             pre_column_map_inv = A.make 1 (Fictitious) ; 
             pre_kind_of_event = A.make 1 (Side_effect_of (-1,[])) ;
             history_of_predicate_values_to_predicate_id = A.make 1 (C.create None);
             history_of_agent_ids_of_type = A.make 1 [];
             predicate_id_list_related_to_predicate_id = A.make 1 PredicateidSet.empty;
             pre_observable_list = [];
             pre_fictitious_observable = None ;
             pre_level_of_event = A.make 1 0 ; 
           }
    
         let get_level_of_event parameter handler error blackboard eid = 
           try 
             error,A.get blackboard.pre_level_of_event eid 
           with 
             Not_found -> 
               let error_list,error = CI.Po.K.H.create_error parameter handler error (Some "blackboard_generation.ml") None (Some "conj") (Some "323") (Some "Arguments have no greatest lower bound") (failwith "Arguments have no greatest lower bound")  in 
               CI.Po.K.H.raise_error parameter handler error_list error Priority.default 

         let pre_column_map_inv b = b.pre_column_map_inv 
           
         let init_fictitious_action log_info error predicate_id blackboard = 
           let nsid = blackboard.pre_nsteps+1 in 
           let log_info = CI.Po.K.P.inc_n_side_events log_info in 
           let test = Undefined in 
           let action = Counter 0 in
           let _ = A.set blackboard.pre_steps_by_column predicate_id (2,[nsid,1,test,action])  in 
           error,log_info,{blackboard with pre_nsteps = nsid} 
          
         let init_fictitious_action_at_nsid log_info error predicate_id blackboard nsid = 
           let test = Undefined in 
           let action = Counter 0 in
           let _ = A.set blackboard.pre_steps_by_column predicate_id (2,[nsid,1,test,action])  in 
           error,log_info,blackboard 

         let init_fictitious_action log_info error predicate_id blackboard init_step = 
           match 
             init_step
           with 
           | None -> 
             let error,log_info,blackboard = init_fictitious_action log_info error predicate_id blackboard in 
             error,log_info,blackboard,Some blackboard.pre_nsteps
           | Some nsid -> 
             let error,log_info,blackboard = init_fictitious_action_at_nsid log_info error predicate_id blackboard nsid in 
             error,log_info,blackboard,init_step 

   
         let add_fictitious_action error test action predicate_id blackboard = 
           let nsid = blackboard.pre_nsteps in 
           let map = blackboard.pre_steps_by_column in 
           let value,list = A.get map predicate_id in 
           let value' = value+1 in
           let _ = A.set map predicate_id (value',(nsid,value,test,action)::list) in 
           error,blackboard
             
         let side_effect parameter handler error predicate_target_id s site = 
           match s 
           with 
             | Defined | Counter _ | Internal_state_is _ | Undefined | Pointer_to_agent _ 
             | Present | Bound | Bound_to_type _ | Unknown -> 
               let error,error_list = 
                 CI.Po.K.H.create_error parameter handler error (Some "blackboard_generation.ml") None (Some "side_effects") (Some "602") (Some "Illegal state for a side-effects") (failwith "Blackboard_generation.side_effect") in 
               CI.Po.K.H.raise_error parameter handler error error_list []
             | Free -> 
               error,[predicate_target_id,None,(Free,Unknown)]
             | Bound_to (pid,ag,_,sname) -> 
               error,[predicate_target_id,None,(s,Unknown);
                      pid,Some (ag,sname),(Bound_to (predicate_target_id,CI.Po.K.agent_id_of_site site,CI.Po.K.agent_name_of_site site,CI.Po.K.site_name_of_site site),Free)]
                 
                 
         let predicate_value_of_binding_state parameter handler error x = 
           match x 
           with 
             | CI.Po.K.ANY -> error,Unknown 
             | CI.Po.K.FREE -> error,Free
             | CI.Po.K.BOUND -> error,Bound
             | CI.Po.K.BOUND_TYPE bt -> error,Bound_to_type (CI.Po.K.agent_name_of_binding_type bt,CI.Po.K.site_name_of_binding_type bt)
             | CI.Po.K.BOUND_to s -> 
               let error_list,error = 
                 CI.Po.K.H.create_error parameter handler error (Some "blackboard_generation.ml") None (Some "predicate_value_of_binding_state") (Some "620") (Some "Illegal binding state in predicate_value_of_binding_state") (failwith "predicate_value_of_binding_state") in 
               CI.Po.K.H.raise_error parameter handler error_list error Unknown 
                 
         let potential_target error parameter handler blackboard site binding_state =
           let agent_id = CI.Po.K.agent_id_of_site site in 
           let site_name = CI.Po.K.site_name_of_site site in 
           let error,blackboard,predicate_target_id = 
             allocate parameter handler error blackboard (Bound_site (agent_id,site_name))  in 
           let former_states = 
             A.get blackboard.history_of_predicate_values_to_predicate_id predicate_target_id
           in 
           match 
             parameter.CI.Po.K.H.current_compression_mode
           with 
           | None | Some Parameter.Causal -> 
             begin 
               let s = C.last former_states in 
               match s with 
                 | None -> error,blackboard,[]
                 | Some s -> 
                   let error,bt = predicate_value_of_binding_state parameter handler error binding_state in
                   if more_refined s bt 
                   then 
                     let error,l=side_effect parameter handler error predicate_target_id s site in 
                     error,blackboard,[l]
                   else 
                     error,blackboard,[]
             end
           | _ -> 
             begin 
               let error,bt = predicate_value_of_binding_state parameter handler error binding_state in 
               let error,list = 
                 C.fold 
                   (fun s (error,list) -> 
                     if more_refined s bt
                     then 
                       let error,l=side_effect parameter handler error predicate_target_id s site in 
                       error,l::list 
                     else 
                       error,list
                   )
                   former_states
                   (error,[])
               in 
               error,blackboard,list 
             end 
          
         type data_structure_strong = 
           {
             new_agents: AgentIdSet.t ;
             old_agents: CI.Po.K.agent_name AgentIdMap.t;
             old_agents_potential_substitution: CI.Po.K.agent_id list AgentIdMap.t;
             sure_agents: AgentIdSet.t; 
             sure_links: AgentId2Set.t;
             other_links: AgentId2Set.t;
             sites_in_other_links: SiteIdSet.t;
             sites_in_other_action_links: SiteIdSet.t; 
             other_links_test_sites: (CI.Po.K.agent_id) SiteIdMap.t;
             other_links_action_sites: (CI.Po.K.agent_id) SiteIdMap.t; 
             sure_tests: CI.Po.K.test list ;
             sure_actions: CI.Po.K.action list ; 
             create_actions: CI.Po.K.action list ; 
             sure_side_effects: (CI.Po.K.site*CI.Po.K.binding_state) list;
             other_agents_tests: CI.Po.K.test list AgentIdMap.t ; 
             other_agents_actions: CI.Po.K.action list AgentIdMap.t ;
             other_links_tests: CI.Po.K.test list AgentId2Map.t; 
             other_links_actions: CI.Po.K.action list AgentId2Map.t ;
             other_links_priority: AgentId2Set.t ; 
             other_agents_side_effects: (CI.Po.K.site*CI.Po.K.binding_state) list AgentIdMap.t ;
             subs_agents_involved_in_links: AgentIdSet.t;
             rule_agent_id_mutex: predicate_id AgentIdMap.t;
             rule_agent_id_subs: predicate_id AgentIdMap.t;
             mixture_agent_id_mutex: predicate_id AgentIdMap.t;
             links_mutex: predicate_id AgentId2Map.t;
             removed_agents: AgentIdSet.t ; 
             removed_sites_in_other_links: SiteIdSet.t ; 
           }

         let init_data_structure_strong = 
           {
             new_agents = AgentIdSet.empty;
             old_agents = AgentIdMap.empty;
             old_agents_potential_substitution = AgentIdMap.empty;
             sure_agents = AgentIdSet.empty; 
             sure_links = AgentId2Set.empty;
             other_links = AgentId2Set.empty;
             other_links_test_sites = SiteIdMap.empty;
             other_links_action_sites = SiteIdMap.empty; 
             sites_in_other_links = SiteIdSet.empty;
             sites_in_other_action_links = SiteIdSet.empty;
             sure_tests = [];
             sure_actions = [];
             create_actions = [];
             sure_side_effects = [];
             other_agents_tests = AgentIdMap.empty;
             other_agents_actions = AgentIdMap.empty;
             other_links_priority = AgentId2Set.empty ;
             other_links_tests = AgentId2Map.empty;
             other_links_actions = AgentId2Map.empty;
             other_agents_side_effects = AgentIdMap.empty;
             subs_agents_involved_in_links = AgentIdSet.empty;
             rule_agent_id_mutex = AgentIdMap.empty;
             rule_agent_id_subs = AgentIdMap.empty;
             links_mutex = AgentId2Map.empty;
             mixture_agent_id_mutex = AgentIdMap.empty;
             removed_agents = AgentIdSet.empty ; 
             removed_sites_in_other_links = SiteIdSet.empty ;
           }
     
         let print_data_structure parameter handler error data =
           let stderr = parameter.CI.Po.K.H.out_channel_err in 
           let _ = Printf.fprintf stderr "New agents: \n" in 
           let _ = 
             AgentIdSet.iter (Printf.fprintf stderr " %i \n") data.new_agents
           in 
           let _ = Printf.fprintf stderr "Old agents: \n" in 
           let _ = 
             AgentIdMap.iter (Printf.fprintf stderr " id:%i: type:%i \n") data.old_agents 
           in 
           let _ = Printf.fprintf stderr "Old agents implied in links: \n" in 
           let _ = 
             AgentIdSet.iter (Printf.fprintf stderr " %i \n") data.subs_agents_involved_in_links 
           in 
           let _ = Printf.fprintf stderr "Tested_links_map: \n" in 
           let _ = 
             SiteIdMap.iter (fun (a,b) -> Printf.fprintf stderr " %i.%i -> %i \n" a b ) data.other_links_test_sites in 
           let _ = Printf.fprintf stderr "Modified_links_map: \n" in 
           let _ = 
             SiteIdMap.iter (fun (a,b) -> Printf.fprintf stderr " %i.%i -> %i \n" a b ) data.other_links_action_sites 
           in 
           let _ = Printf.fprintf stderr "Potential substitution: \n" in 
           let _ = 
             AgentIdMap.iter 
               (fun id l -> 
                 let _ = 
                   Printf.fprintf stderr " id:%i\n" id 
                 in 
                 List.iter (Printf.fprintf stderr "   %i\n") l)
               data.old_agents_potential_substitution
           in 
           let _ = Printf.fprintf stderr "Sure agents: \n" in 
           let _ = 
             AgentIdSet.iter (Printf.fprintf stderr " %i \n") data.sure_agents
           in 
           let _ = Printf.fprintf stderr "Sure tests: \n" in 
           let error = 
             List.fold_left  
               (fun error -> CI.Po.K.print_test parameter handler error " ")
               error 
               (List.rev data.sure_tests)
           in 
           let _ = Printf.fprintf stderr "Tests to be substituted: \n" in 
           let error = 
             AgentIdMap.fold 
               (fun id l error -> 
                 let _ = Printf.fprintf stderr " %i\n" id in 
                 let error = 
                   List.fold_left 
                     (fun error -> CI.Po.K.print_test parameter handler error "  ")
                     error 
                     (List.rev l)
                 in error)
               data.other_agents_tests
               error 
           in 
           let error = 
             AgentId2Map.fold 
               (fun (id1,id2) l error -> 
                 let _ = Printf.fprintf stderr " (%i,%i)\n" id1 id2 in 
                 let error = 
                   List.fold_left 
                     (fun error -> CI.Po.K.print_test parameter handler error "  ")
                     error 
                     (List.rev l)
                 in error)
               data.other_links_tests
               error 
           in 
           let _ = Printf.fprintf stderr "Sure actions: \n" in 
           let error = 
             List.fold_left 
               (fun error -> CI.Po.K.print_action parameter handler error " ")
               error
               (List.rev data.sure_actions)
           in 
           let _ = Printf.fprintf stderr "Actions to be substituted: \n" in 
           let error = 
             AgentIdMap.fold 
               (fun id l error -> 
                 let _ = Printf.fprintf stderr " %i\n" id in 
                 let error = 
                   List.fold_left 
                     (fun error -> CI.Po.K.print_action parameter handler error "  ")
                     error
                     (List.rev l)
                 in error)
               data.other_agents_actions
               error 
           in 
           let error = 
             AgentId2Map.fold
               (fun (id1,id2) l error -> 
                 let _ = Printf.fprintf stderr " (%i,%i)\n" id1 id2 in 
                 let error = 
                   List.fold_left  
                     (fun error -> CI.Po.K.print_action parameter handler error  "  ")
                     error 
                     (List.rev l)
                 in error)
               data.other_links_actions
               error
           in 
           let _ = Printf.fprintf stderr "Sure side_effects \n" in 
           let _ = 
             List.iter 
               (CI.Po.K.print_side stderr handler " ")
               data.sure_side_effects
           in 
           let _ = Printf.fprintf stderr "Side effect to be substituted: \n" in 
           let _ = 
             AgentIdMap.iter 
               (fun id l -> 
                 let _ = Printf.fprintf stderr " %i\n" id in 
                 let _ = 
                   List.iter 
                     (CI.Po.K.print_side stderr handler "  ")
                     l
                 in ())
               data.other_agents_side_effects
           in 
           error


         let add_site_in_other_test_links site data_structure = 
           { 
             data_structure
             with sites_in_other_links = SiteIdSet.add site data_structure.sites_in_other_links 
           }
         let add_site_in_other_action_links site data_structure = 
           let data_structure = add_site_in_other_test_links site data_structure in 
            { 
             data_structure
             with sites_in_other_action_links = SiteIdSet.add site data_structure.sites_in_other_action_links 
           }

         let mem_site_in_other_action_links site data_structure = 
           SiteIdSet.mem site data_structure.sites_in_other_action_links 

         let add_sure_test test data_structure = 
           { 
             data_structure 
             with sure_tests = test::data_structure.sure_tests
           }
         let add_subs_test test ag_id data_structure = 
           let old = 
             try 
               AgentIdMap.find ag_id data_structure.other_agents_tests
             with 
             | Not_found -> []
           in 
           {
             data_structure 
            with 
              other_agents_tests = 
               AgentIdMap.add ag_id (test::old) data_structure.other_agents_tests
           }
         let add_subs_test_link test link data_structure = 
           let a,b=fst link,snd link in 
           let link = if a < b then link else b,a in 
           let data_structure,old = 
             try 
               data_structure,AgentId2Map.find link data_structure.other_links_tests 
             with 
             | Not_found -> 
               {data_structure 
                with 
                  other_links = AgentId2Set.add link data_structure.other_links; 
               },
               []
           in 
           { 
             data_structure 
             with 
               other_links_tests = 
               AgentId2Map.add link (test::old) data_structure.other_links_tests
           }
         let add_sure_action action data_structure = 
           { 
             data_structure 
             with sure_actions = action::data_structure.sure_actions
           }
         let add_create_action action data_structure = 
           { 
             data_structure 
             with create_actions = action::data_structure.create_actions
           }
         let add_subs_action action ag_id data_structure = 
           let old = 
             try 
               AgentIdMap.find ag_id data_structure.other_agents_actions
             with 
             | Not_found -> []
           in 
           {
             data_structure 
            with 
              other_agents_actions = 
               AgentIdMap.add ag_id (action::old) data_structure.other_agents_actions
           }
         let add_subs_action_link action link data_structure = 
           let a,b=fst link,snd link in 
           let link = if a < b then link else b,a in 
           let data_structure,old = 
             try 
               data_structure,
               AgentId2Map.find link data_structure.other_links_actions 
             with 
             | Not_found -> 
               {
                 data_structure 
                with 
                  other_links = AgentId2Set.add link data_structure.other_links;                               },
               []
           in 
           { 
             data_structure 
             with 
               other_links_actions = 
               AgentId2Map.add link (action::old) data_structure.other_links_actions
           } 
           
         let add_sure_side_effect side_effect data_structure = 
           { 
             data_structure 
             with 
               sure_side_effects = side_effect::data_structure.sure_side_effects
           }

         let add_subs_side_effect side_effect ag_id data_structure = 
           let site = fst side_effect in 
           let agent = CI.Po.K.agent_of_site site in 
           let agent_id = CI.Po.K.agent_id_of_agent agent in 
           let old = 
             try 
               AgentIdMap.find agent_id data_structure.other_agents_side_effects
             with 
             | Not_found -> []
           in 
           { 
             data_structure 
             with 
               other_agents_side_effects =  
               AgentIdMap.add ag_id  (side_effect::old) data_structure.other_agents_side_effects}


           
             
         let add_step_strong parameter handler error log_info step blackboard step_id = 
           let init = CI.Po.K.is_init_of_refined_step step in 
           let pre_event = blackboard.pre_event in 
           let error,test_list = CI.Po.K.tests_of_refined_step parameter handler error step in 
           let error,l = CI.Po.K.agent_id_in_obs parameter handler error step in
           let error,(action_list,side_effect) = CI.Po.K.actions_of_refined_step parameter handler error step in
           let data_structure = init_data_structure_strong in 
           let data_structure = 
             List.fold_left 
               (fun data_structure action -> 
                 match 
                   action
                 with 
                 | CI.Po.K.Create(ag,interface) -> 
                     {data_structure 
                      with new_agents = AgentIdSet.add (CI.Po.K.agent_id_of_agent ag) data_structure.new_agents}
                 
                 | CI.Po.K.Bind(site1,site2)
                     -> 
                   let ag1_id = CI.Po.K.agent_id_of_site site1 in 
                   let ag2_id = CI.Po.K.agent_id_of_site site2 in 
                   let site1_id = ag1_id,CI.Po.K.site_name_of_site site1 in 
                   let site2_id = ag2_id,CI.Po.K.site_name_of_site site2 in 
                   {data_structure
                    with 
                      other_links_action_sites = 
                       SiteIdMap.add site1_id ag2_id
                         (SiteIdMap.add site2_id ag1_id data_structure.other_links_action_sites)}
                 | CI.Po.K.Remove agent -> 
                   {data_structure 
                    with removed_agents = AgentIdSet.add (CI.Po.K.agent_id_of_agent agent) data_structure.removed_agents}
                 | _ -> data_structure)
               data_structure
               action_list
           in 
           let data_structure = 
             List.fold_left 
               (fun data_structure test -> 
                 match 
                   test
                 with 
                 | CI.Po.K.Is_Here (ag) -> 
                   {data_structure 
                    with old_agents = AgentIdMap.add (CI.Po.K.agent_id_of_agent ag) (CI.Po.K.agent_name_of_agent ag) data_structure.old_agents}
                  | CI.Po.K.Is_Bound_to(site1,site2)
                     -> 
                    let ag1_id = CI.Po.K.agent_id_of_site site1 in 
                    let ag2_id = CI.Po.K.agent_id_of_site site2 in 
                    let site1_id = ag1_id,CI.Po.K.site_name_of_site site1 in 
                    let site2_id = ag2_id,CI.Po.K.site_name_of_site site2 in 
                    let data_structure = 
                      if AgentIdSet.mem ag1_id data_structure.removed_agents
                      then 
                        {data_structure 
                         with removed_sites_in_other_links = 
                            SiteIdSet.add site1_id data_structure.removed_sites_in_other_links}
                      else
                        data_structure 
                    in 
                    let data_structure = 
                      if AgentIdSet.mem ag2_id data_structure.removed_agents
                      then 
                        {data_structure 
                         with removed_sites_in_other_links = 
                            SiteIdSet.add site2_id data_structure.removed_sites_in_other_links}
                      else
                        data_structure 
                    in 
                    {data_structure
                    with 
                      other_links_test_sites = 
                       SiteIdMap.add site1_id ag2_id
                         (SiteIdMap.add site2_id ag1_id data_structure.other_links_test_sites)}

                  | _ -> data_structure)
               data_structure 
               test_list 
           in
           let tested_sites = 
             SiteIdMap.fold 
               (fun a _ -> SiteIdSet.add a)
               data_structure.other_links_test_sites 
               SiteIdSet.empty 
           in 
           let mod_sites = 
             SiteIdMap.fold
               (fun a _ -> SiteIdSet.add a)
               data_structure.other_links_action_sites
               SiteIdSet.empty
           in 
           let priority_sites = 
             SiteIdSet.inter tested_sites mod_sites 
           in 
           let data_structure = 
             { data_structure 
               with old_agents_potential_substitution = 
                 AgentIdMap.map 
                   (A.get blackboard.history_of_agent_ids_of_type)
                   data_structure.old_agents}
           in 
           let data_structure = 
             { data_structure with sure_agents = data_structure.new_agents }
           in 
           let data_structure = 
             { data_structure with 
               sure_agents = 
                 AgentIdMap.fold 
                   (fun id l sure_agents -> 
                     match 
                       l
                     with 
                     | [_] -> AgentIdSet.add id sure_agents
                     | _ -> sure_agents)
                   data_structure.old_agents_potential_substitution
                   data_structure.sure_agents}
           in 
           let sure_agent x = 
             AgentIdSet.mem x data_structure.sure_agents 
           in 
           let data_structure = 
             {
               data_structure 
              with 
                old_agents_potential_substitution = 
                 AgentIdSet.fold AgentIdMap.remove 
                   data_structure.sure_agents 
                   data_structure.old_agents_potential_substitution 
             }
           in 


           let data_structure = 
             List.fold_left 
               (fun data_structure test -> 
                 match test
                 with 
                 | CI.Po.K.Is_Here _ 
                 | CI.Po.K.Has_Internal _ 
                 | CI.Po.K.Is_Free _  | CI.Po.K.Is_Bound _ | CI.Po.K.Has_Binding_type _  -> data_structure 
                 | CI.Po.K.Is_Bound_to  (site1,site2) -> 
                   let agent1 = CI.Po.K.agent_of_site site1 in 
                   let ag_id1 = CI.Po.K.agent_id_of_agent agent1 in 
                   let agent2 = CI.Po.K.agent_of_site site2 in 
                   let ag_id2 = CI.Po.K.agent_id_of_agent agent2 in 
                   let site_id1 = CI.Po.K.site_name_of_site site1 in 
                   let site_id2 = CI.Po.K.site_name_of_site site2 in 
                   if sure_agent ag_id1 && not (SiteIdSet.mem (ag_id1,site_id1) priority_sites) && sure_agent ag_id2 && not (SiteIdSet.mem (ag_id2,site_id2) priority_sites)
                   then 
                     data_structure 
                   else 
                     let mix_site1 = ag_id1,CI.Po.K.site_name_of_site site1 in
                     let mix_site2 = ag_id2,CI.Po.K.site_name_of_site site2 in 
                     let data_structure = 
                       add_site_in_other_test_links mix_site1 
                          (add_site_in_other_test_links mix_site2 data_structure )
                     in
                     let data_structure = 
                       if SiteIdSet.mem mix_site1 priority_sites 
                         || SiteIdSet.mem mix_site2 priority_sites
                       then 
                         let ag_id1,ag_id2 = 
                           if ag_id1<ag_id2 
                           then ag_id1,ag_id2
                           else ag_id2,ag_id1
                         in 
                         {data_structure 
                          with 
                            other_links_priority = 
                             AgentId2Set.add (ag_id1,ag_id2) data_structure.other_links_priority}
                       else
                         data_structure
                     in 
                     data_structure)
               data_structure 
               (List.rev test_list)
           in 
           let data_structure = 
             List.fold_left 
               (fun data_structure action -> 
                 match action
                 with 
                 | CI.Po.K.Create _ 
                 | CI.Po.K.Remove _ 
                 | CI.Po.K.Mod_internal _ 
                 | CI.Po.K.Free _ -> data_structure
                 | CI.Po.K.Bind(site1,site2) | CI.Po.K.Bind_to (site1,site2) | CI.Po.K.Unbind (site1,site2) -> 
                   let agent1 = CI.Po.K.agent_of_site site1 in 
                   let ag_id1 = CI.Po.K.agent_id_of_agent agent1 in 
                   let agent2 = CI.Po.K.agent_of_site site2 in 
                   let ag_id2 = CI.Po.K.agent_id_of_agent agent2 in 
                   if sure_agent ag_id1 && sure_agent ag_id2 
                   then 
                     data_structure
                   else 
                     add_site_in_other_action_links (ag_id1,CI.Po.K.site_name_of_site site1) 
                          (add_site_in_other_action_links (ag_id2,CI.Po.K.site_name_of_site site2) 
                             data_structure )
                     

               )
               data_structure
               (List.rev action_list)
           in 
           let data_structure = 
             List.fold_left 
               (fun data_structure test -> 
                 match test
                 with 
                 | CI.Po.K.Is_Here (agent) ->
                   let ag_id = CI.Po.K.agent_id_of_agent agent in 
                   if sure_agent ag_id 
                   then 
                     add_sure_test test data_structure 
                   else 
                     add_subs_test test ag_id data_structure 
                 | CI.Po.K.Has_Internal(site,_) -> 
                   let agent = CI.Po.K.agent_of_site site in 
                   let ag_id = CI.Po.K.agent_id_of_agent agent in 
                   if sure_agent ag_id 
                   then 
                     add_sure_test test data_structure 
                   else 
                     add_subs_test test ag_id data_structure 
                 | CI.Po.K.Is_Free site | CI.Po.K.Is_Bound site | CI.Po.K.Has_Binding_type (site,_) -> 
                   let agent = CI.Po.K.agent_of_site site in 
                   let ag_id = CI.Po.K.agent_id_of_agent agent in 
                   if sure_agent ag_id && not (SiteIdSet.mem (ag_id,CI.Po.K.site_name_of_site site) data_structure.sites_in_other_links)
                   then 
                     add_sure_test test data_structure
                   else 
                    begin 
                      try 
                        let site_id1 = ag_id,CI.Po.K.site_name_of_site site in 
                        let ag_id2 = SiteIdMap.find site_id1 data_structure.other_links_action_sites
                        in 
                        add_subs_test_link test (ag_id,ag_id2) data_structure 
                      with 
                        Not_found -> 
                         add_subs_test test ag_id data_structure 
                    end 
                  
                 | CI.Po.K.Is_Bound_to  (site1,site2) -> 
                   let agent1 = CI.Po.K.agent_of_site site1 in 
                   let ag_id1 = CI.Po.K.agent_id_of_agent agent1 in 
                   let ag_name1 = CI.Po.K.agent_name_of_agent agent1 in 
                   let site_name1 = CI.Po.K.site_name_of_site site1 in 
                   let agent2 = CI.Po.K.agent_of_site site2 in 
                   let ag_id2 = CI.Po.K.agent_id_of_agent agent2 in 
                   let ag_name2 = CI.Po.K.agent_name_of_agent agent2 in 
                   let site_name2 = CI.Po.K.site_name_of_site site2 in 
                   let weak1 = CI.Po.K.Has_Binding_type (site1,CI.Po.K.btype_of_names ag_name2 site_name2) in 
                   let weak2 = CI.Po.K.Has_Binding_type (site2,CI.Po.K.btype_of_names ag_name1 site_name1) in 
                   match sure_agent ag_id1 && not (SiteIdSet.mem  (ag_id1,site_name1) priority_sites),
                     sure_agent ag_id2 && not (SiteIdSet.mem (ag_id2,site_name2) priority_sites)
                   with 
                   | true,true -> add_sure_test test data_structure 
                   | true,false -> 
                     add_sure_test weak1 
                       (add_subs_test weak2 ag_id2
                          (add_subs_test_link test (ag_id1,ag_id2) data_structure))
                   | false,true -> 
                     add_subs_test weak1 ag_id1
                       (add_sure_test weak2
                          (add_subs_test_link test (ag_id1,ag_id2) data_structure))
                   | false,false -> 
                     add_subs_test weak1 ag_id1
                       (add_subs_test weak2 ag_id2 
                          (add_subs_test_link test (ag_id1,ag_id2) data_structure))
               )
               data_structure 
               (List.rev test_list)
           in 
           let data_structure = 
             List.fold_left 
               (fun data_structure action -> 
                 match action
                 with 
                 | CI.Po.K.Create _ -> add_create_action action data_structure 
                 | CI.Po.K.Remove agent -> 
                   let ag_id = CI.Po.K.agent_id_of_agent agent in 
                   if sure_agent ag_id 
                   then 
                     add_sure_action action data_structure
                   else 
                     add_subs_action action ag_id data_structure 
                 | CI.Po.K.Mod_internal (site,_) 
                   -> 
                   let agent = CI.Po.K.agent_of_site site in 
                   let ag_id = CI.Po.K.agent_id_of_agent agent in 
                   if sure_agent ag_id 
                   then 
                     add_sure_action action data_structure 
                   else 
                     add_subs_action action ag_id data_structure 
                 | CI.Po.K.Free(site) -> 
                   let agent = CI.Po.K.agent_of_site site in 
                   let ag_id = CI.Po.K.agent_id_of_agent agent in 
                   let site_id1 = ag_id,CI.Po.K.site_name_of_site site in 
                    if mem_site_in_other_action_links site_id1 data_structure 
                    then 
                      data_structure
                    else 
                      if sure_agent ag_id 
                      then 
                        add_sure_action action data_structure
                      else 
                        begin 
                            try 
                              
                              let ag_id2 = SiteIdMap.find site_id1 data_structure.other_links_test_sites
                              in 
                              add_subs_action_link action (ag_id,ag_id2) data_structure 
                            with 
                              Not_found -> 
                                add_subs_action action ag_id data_structure 
                          end 
                          
                 | CI.Po.K.Bind(site1,site2) | CI.Po.K.Bind_to (site1,site2) | CI.Po.K.Unbind (site1,site2) -> 
                   let agent1 = CI.Po.K.agent_of_site site1 in 
                   let ag_id1 = CI.Po.K.agent_id_of_agent agent1 in 
                   let agent2 = CI.Po.K.agent_of_site site2 in 
                   let ag_id2 = CI.Po.K.agent_id_of_agent agent2 in 
                   if sure_agent ag_id1 && sure_agent ag_id2 
                   then 
                     add_sure_action action data_structure 
                   else 
                     add_subs_action_link action (ag_id1,ag_id2) data_structure 
               )
               data_structure 
               (List.rev action_list)
           in 

           let data_structure = 
             List.fold_left 
               (fun data_structure side_effect -> 
                 let (site,_) = side_effect in 
                 let agent = CI.Po.K.agent_of_site site in 
                 let ag_id = CI.Po.K.agent_id_of_agent agent in 
                 if sure_agent ag_id 
                 then 
                   add_sure_side_effect side_effect data_structure
                 else 
                   add_subs_side_effect side_effect ag_id data_structure)
               data_structure 
               side_effect
           in 
           let data_structure = 
             {
               data_structure 
              with 
                subs_agents_involved_in_links = 
                 let f x set = 
                 AgentId2Map.fold 
                   (fun (a1,a2) _ set -> AgentIdSet.add a1 (AgentIdSet.add a2 set))
                   x set 
                 in 
                 f 
                   data_structure.other_links_tests 
                   (f 
                      data_structure.other_links_actions 
                      AgentIdSet.empty)}
           in 
           let init_step = None in 
           let error,log_info,blackboard,rule_agent_id_mutex,rule_agent_id_subs,mixture_agent_id_mutex,fictitious_list,fictitious_local_list,_,init_step = 
             AgentIdMap.fold
               (fun x l (error,log_info,blackboard,rule_agent_id_mutex,rule_agent_id_subs,mixture_agent_id_mutex,fictitious_list,fictitious_local_list,set,init_step) -> 
                 let predicate_info = Mutex (Lock_agent (step_id,x)) in 
                 let error,blackboard,predicate_id = allocate parameter handler error blackboard predicate_info in 
                 let error,log_info,blackboard,init_step = init_fictitious_action log_info error predicate_id blackboard init_step 
                 in 
                 let rule_agent_id_mutex = AgentIdMap.add x predicate_id rule_agent_id_mutex in 
                 let fictitious_local_list = predicate_id::fictitious_local_list in
                 let fictitious_list = predicate_id::fictitious_list in 
                 let error,log_info,blackboard,rule_agent_id_subs,init_step = 
                   if AgentIdSet.mem x data_structure.subs_agents_involved_in_links 
                   then 
                     begin 
                       let predicate_info = Pointer (step_id,x) in 
                       let error,blackboard,predicate_id = allocate parameter handler error blackboard predicate_info in 
                       let rule_agent_id_subs =  AgentIdMap.add x predicate_id rule_agent_id_subs in 
                       let error,log_info,blackboard,init_step = init_fictitious_action log_info error predicate_id blackboard init_step in 
                       error,log_info,blackboard,rule_agent_id_subs,init_step 
                     end
                   else
                     error,log_info,blackboard,rule_agent_id_subs,init_step
                 in 
                 let error,log_info,blackboard,mixture_agent_id_mutex,set,init_step = 
                   List.fold_left 
                     (fun (error,log_info,blackboard,mixture_agent_id_mutex,set,init_step) id -> 
                       let set' = AgentIdSet.add_if_not_mem id set in 
                       if set == set'
                       then 
                         try 
                           let _ = AgentIdMap.find id mixture_agent_id_mutex in
                           (error,log_info,blackboard,mixture_agent_id_mutex,set,init_step)
                         with
                           Not_found -> 
                             begin 
                               let predicate_info = Mutex (Lock_rectangular (step_id,id)) in 
                               let error,blackboard,predicate_id = allocate parameter handler error blackboard predicate_info in 
                               let mixture_agent_id_mutex = AgentIdMap.add x predicate_id mixture_agent_id_mutex in 
                               let error,log_info,blackboard,init_step = init_fictitious_action log_info error predicate_id blackboard init_step in 
                               error,log_info,blackboard,mixture_agent_id_mutex,set',init_step 
                             end 
                       else 
                         (error,log_info,blackboard,mixture_agent_id_mutex,set',init_step))
                     (error,log_info,blackboard,mixture_agent_id_mutex,set,init_step) 
                     l 
                 in 
                 (error,log_info,blackboard,rule_agent_id_mutex,rule_agent_id_subs,mixture_agent_id_mutex,fictitious_list,fictitious_local_list,set,init_step))
               data_structure.old_agents_potential_substitution
               (error,log_info,blackboard,AgentIdMap.empty,AgentIdMap.empty,AgentIdMap.empty,blackboard.pre_fictitious_list,[],AgentIdSet.empty,init_step)
           in 
           let links_mutex = AgentId2Map.empty in 
           let error,log_info,blackboard,links_mutex,fictitious_list,fictitious_local_list,init_step = 
             AgentId2Set.fold
               (fun x  (error,log_info,blackboard,links_mutex,fictitious_list,fictitious_local_list,init_step) -> 
                 let predicate_info = Mutex (Lock_links (step_id,x)) in 
                 let error,blackboard,predicate_id = allocate parameter handler error blackboard predicate_info in 
                 let error,log_info,blackboard,init_step = init_fictitious_action log_info error predicate_id blackboard init_step in 
                 let links_mutex = AgentId2Map.add x predicate_id links_mutex in 
                 let fictitious_local_list = predicate_id::fictitious_local_list in
                 let fictitious_list = predicate_id::fictitious_list in 
                 (error,log_info,blackboard,links_mutex,fictitious_list,fictitious_local_list,init_step))
               data_structure.other_links 
               (error,log_info,blackboard,links_mutex,fictitious_list,fictitious_local_list,init_step)
           in 
           let data_structure = 
             { 
               data_structure 
               with 
                 links_mutex = links_mutex ;
                 rule_agent_id_mutex = rule_agent_id_mutex ;
                 rule_agent_id_subs = rule_agent_id_subs ; 
                 mixture_agent_id_mutex = mixture_agent_id_mutex ;
             }
           in 
           let blackboard = {blackboard with pre_fictitious_list = fictitious_list} in 
           let _ = 
             if debug_mode 
             then 
               let _ = print_data_structure parameter handler error data_structure in
               ()
           in 
           let fictitious_list = blackboard.pre_fictitious_list in 
           let build_map list map = 
             List.fold_left 
               (fun map (id,value) -> PredicateidMap.add id value map)
               map 
               list 
           in 
           let add_state pid (test,action) map = 
             let test',action' = 
               try 
                 PredicateidMap.find pid map  
               with 
                 | Not_found -> Unknown,Unknown 
             in 
             let test = 
               if strictly_more_refined test test' 
               then 
                 test
               else 
                 test'
             in 
             let action = 
               if strictly_more_refined action action' 
               then 
                 action
               else 
                 action'
             in 
             let map = PredicateidMap.add pid (test,action) map in 
             map 
           in 
           let fadd pid p map = 
             match p 
             with 
               | Counter _ | Internal_state_is _ | Undefined 
               | Defined | Present | Bound | Bound_to_type _ | Unknown -> 
                 ()
               | _ -> 
                 let old = A.get map pid in 
                 A.set map pid (C.add p old) 
           in 


             (* deal with created agents *) 
           let error,log_info,blackboard,step_id = 
             match init_step 
             with 
             | None -> error,log_info,blackboard,step_id
             | Some nsid -> 
               begin
                 let nsid = nsid in 
                 let nsid_void = nsid+1 in 
                 let nsid_next = nsid+1 in 
                 let side_effect = [] in 
                 let action_list = data_structure.create_actions in 
                 let test_list = [] in 
                 let fictitious_list = [] in 
                   
                 let error,blackboard,fictitious_list,fictitious_local_list,unambiguous_side_effects,init_step  = 
                   List.fold_left 
                     (fun (error,blackboard,fictitious_list,fictitious_local_list,unambiguous_side_effects,init_step) (site,(binding_state)) -> 
                       begin
                         let error,blackboard,potential_target = potential_target error parameter handler blackboard site binding_state in 
                         match 
                           potential_target 
                         with 
                         | [l]  ->  
                             begin
                               let list = 
                                 List.fold_left 
                                   (fun list t -> t::l)
                                   unambiguous_side_effects
                                   l 
                               in 
                               error,
                               blackboard,
                               fictitious_list,
                               fictitious_local_list,
                               list,
                               init_step 
                             end
                         | _ -> 
                           begin 
                             let rule_ag_id = CI.Po.K.agent_id_of_agent (CI.Po.K.agent_of_site site) in 
                             let predicate_info = Mutex (Lock_side_effect (step_id,rule_ag_id,rule_ag_id,CI.Po.K.site_name_of_site site)) in 
                             let error,blackboard,predicate_id = allocate parameter handler error blackboard predicate_info  in 
                             let  error,log_info,blackboard,step_id = 
                               init_fictitious_action log_info error predicate_id  blackboard init_step 
                             in 
                             let error,log_info,blackboard = 
                               List.fold_left 
                                 (fun (error,log_info,blackboard) list -> 
                                   let blackboard = {blackboard with pre_nsteps = blackboard.pre_nsteps+1} in 
                                   let log_info = CI.Po.K.P.inc_n_side_events log_info in 
                                     let side_effect = 
                                       List.fold_left 
                                         (fun list (_,a,_) -> 
                                           match a 
                                           with 
                                           | None -> list
                                           | Some a -> a::list)
                                         []
                                         list 
                                     in 
                                     let side_effect = CI.Po.K.side_effect_of_list 
                                       side_effect in 
                                     let _ = A.set blackboard.pre_side_effect_of_event 
                                       blackboard.pre_nsteps
                                       side_effect in
                                     let error,blackboard = 
                                       List.fold_left
                                         (fun (error,blackboard) (predicate_id,_,(test,action)) -> 
                                           add_fictitious_action error test action predicate_id blackboard)
                                         (error,blackboard) 
                                         ((predicate_id,None,(Counter 0,Counter 1))::list)
                                     in 
                                     error,log_info,blackboard)
                                 (error,log_info,blackboard)
                                 potential_target
                             in 
                             error,
                             blackboard,
                             (predicate_id::fictitious_list),
                             (predicate_id::fictitious_local_list),
                             unambiguous_side_effects,
                             init_step
                           end
                       end)
                     (error,blackboard,fictitious_list,fictitious_local_list,[],init_step)
                     side_effect 
                 in 
                 let error,blackboard,test_map = 
                   List.fold_left 
                     (fun (error,blackboard,map) test -> 
                       let error,blackboard,test_list = predicates_of_test parameter handler error blackboard test in
                       error,blackboard,build_map test_list map)
                     (error,blackboard,PredicateidMap.empty)
                     test_list in 
                 let error,blackboard,action_map,test_map = 
                   List.fold_left 
                     (fun (error,blackboard,action_map,test_map) action -> 
                       let error,blackboard,action_list,test_list = predicates_of_action true parameter handler error blackboard init action in 
                       error,blackboard,build_map action_list action_map,build_map test_list test_map)
                     (error,blackboard,PredicateidMap.empty,test_map)
                     action_list in 
                 let g x = 
                   match x 
                   with 
                   | None -> Unknown
                   | Some x -> x
                 in 
                 let merged_map = 
                   PredicateidMap.merge 
                     (fun _ test action -> Some(g test,g action))
                     test_map
                     action_map 
                 in 
(*                 let merged_map = 
                   List.fold_left 
                     (fun map pid -> PredicateidMap.add pid (Counter 1,Undefined) map)
                     merged_map
                     fictitious_local_list 
                 in *)
                 let merged_map = 
                   List.fold_left 
                     (fun map (pid,_,(test,action)) -> add_state pid (test,action) map)
                     merged_map
                     unambiguous_side_effects
                 in 
                 let side_effect = 
                   List.fold_left 
                     (fun list (_,a,_) -> 
                       match a 
                       with 
                       | None -> list
                       | Some a -> a::list)
                     []
                     unambiguous_side_effects 
                 in 
                 if side_effect = []
                 && PredicateidMap.is_empty merged_map 
                 then 
                   error,log_info,blackboard,
                   nsid_void
                 else 
                   begin 
                       let _ = A.set blackboard.pre_side_effect_of_event nsid (CI.Po.K.side_effect_of_list side_effect) in
       (*                let _ = A.set pre_event nsid step in *)
                       let pre_steps_by_column = 
                         PredicateidMap.fold 
                           (fun id (test,action) map -> 
                             begin 
                               let value,list = A.get map id in 
                               let value' = value + 1 in 
                               let _ = fadd id action blackboard.history_of_predicate_values_to_predicate_id in 
                               let _ = A.set map id (value',(nsid,value,test,action)::list)
                               in map
                             end)
                           merged_map
                           blackboard.pre_steps_by_column 
                       in 
(*                       let _ = A.set blackboard.pre_kind_of_event nsid (type_of_step (CI.Po.K.type_of_refined_step step)) in *)
                       let observable_list = 
                         if CI.Po.K.is_obs_of_refined_step step 
                         then 
                           ([nsid],CI.Po.K.simulation_info_of_refined_step step)::blackboard.pre_observable_list 
                         else
                           blackboard.pre_observable_list 
                       in 
                       let blackboard = 
                         { 
                           blackboard with 
                             pre_event = pre_event ;
                             pre_fictitious_list = fictitious_list ; 
                             pre_steps_by_column = pre_steps_by_column; 
                             pre_nsteps = nsid;
                             pre_observable_list = observable_list; 
                         }
                       in 
                       error,log_info,blackboard,nsid_next 
                     end 
               end
           in 
          
           (*** deal with substitutable agents ***)
           let error,log_info,blackboard,init_step = 
             AgentIdMap.fold 
               (fun rule_ag_id l (error,log_info,blackboard,init_step) -> 
                 let test_list,action_list,side_effect = 
                   begin 
                     try 
                       AgentIdMap.find rule_ag_id data_structure.other_agents_tests 
                     with 
                       Not_found -> []
                   end ,
                     begin 
                       try 
                         AgentIdMap.find rule_ag_id data_structure.other_agents_actions 
                       with 
                         Not_found -> []
                     end ,
                       try 
                         AgentIdMap.find rule_ag_id data_structure.other_agents_side_effects
                       with 
                         Not_found -> []
                 in 
                 List.fold_left 
                   (fun (error,log_info,blackboard,init_step) mixture_ag_id -> 
                     let (step:CI.Po.K.refined_step) = CI.Po.K.build_subs_refined_step rule_ag_id mixture_ag_id in 
                     let test_list = 
                       List.rev_map 
                         (CI.Po.K.subs_agent_in_test rule_ag_id mixture_ag_id)
                         (List.rev test_list)
                     in 
                     let action_list = 
                       List.rev_map
                         (CI.Po.K.subs_agent_in_action rule_ag_id mixture_ag_id)
                         (List.rev action_list)
                     in 
                     let side_effect = 
                       List.rev_map
                         (CI.Po.K.subs_agent_in_side_effect rule_ag_id mixture_ag_id)
                         (List.rev side_effect)
                     in 
                     let fictitious_local_list = [] in 
                     let error,blackboard,fictitious_list,fictitious_local_list,unambiguous_side_effects,init_step = 
                       List.fold_left 
                         (fun (error,blackboard,fictitious_list,fictitious_local_list,unambiguous_side_effects,init_step) (site,(binding_state)) -> 
                           begin
                             let error,blackboard,potential_target = potential_target error parameter handler blackboard site binding_state in 
                             match 
                               potential_target 
                             with 
                             | [l]  ->  
                               begin
                                 let list = 
                                   List.fold_left 
                                     (fun list t -> t::l)
                                     unambiguous_side_effects
                                     l 
                                 in 
                                 error,
                                 blackboard,
                                 fictitious_list,
                                 fictitious_local_list,
                                 list,
                                 init_step
                               end
                             | _ -> 
                               begin 
                                 let predicate_info = Mutex (Lock_side_effect (step_id,rule_ag_id,mixture_ag_id,CI.Po.K.site_name_of_site site)) in 
                                 let error,blackboard,predicate_id = allocate parameter handler error blackboard predicate_info  in 
                                 let fictitious_list = predicate_id::fictitious_list in 
                                 let error,log_info,blackboard,init_step = init_fictitious_action log_info error predicate_id  blackboard init_step in 
                                 let error,log_info,blackboard = 
                                   List.fold_left 
                                     (fun (error,log_info,blackboard) list -> 
                                       let blackboard = {blackboard with pre_nsteps = blackboard.pre_nsteps+1} in 
                                       let log_info = CI.Po.K.P.inc_n_side_events log_info in 
                                       let side_effect = 
                                         List.fold_left 
                                           (fun list (_,a,_) -> 
                                             match a 
                                             with 
                                             | None -> list
                                             | Some a -> a::list)
                                           []
                                           list 
                                       in 
                                       let side_effect = CI.Po.K.side_effect_of_list side_effect in 
                                       let _ = A.set blackboard.pre_side_effect_of_event blackboard.pre_nsteps side_effect in
                                       let error,blackboard = 
                                         List.fold_left
                                           (fun (error,blackboard) (predicate_id,_,(test,action)) -> 
                                             add_fictitious_action error test action predicate_id blackboard)
                                           (error,blackboard) 
                                           ((predicate_id,None,(Counter 0,Counter 1))::list)
                                       in 
                                       error,log_info,blackboard)
                                     (error,log_info,blackboard)
                                     potential_target
                                 in 
                                 error,
                                 blackboard,
                                 (predicate_id::fictitious_list),
                                 (predicate_id::fictitious_local_list),
                                 unambiguous_side_effects,
                                 init_step
                               end
                           end)
                         (error,blackboard,fictitious_list,fictitious_local_list,[],init_step)
                         side_effect 
                     in 
                     let pid_rule_agent_mutex = 
                       try 
                         AgentIdMap.find 
                           rule_ag_id 
                           data_structure.rule_agent_id_mutex 
                       with 
                         Not_found -> 
                           let _ = Printf.fprintf stdout "ERROR line 1332: %i \n" rule_ag_id  in raise Exit
                     in 
                     let error,blackboard,test_map = 
                       List.fold_left 
                         (fun (error,blackboard,map) test -> 
                           let error,blackboard,test_list = predicates_of_test parameter handler error blackboard test in
                           error,blackboard,build_map test_list map)
                         (error,blackboard,PredicateidMap.empty)
                         test_list in 
                     let test_map = 
                       PredicateidMap.add 
                         pid_rule_agent_mutex 
                         (Counter 0)
                         test_map 
                     in 
                     let error,blackboard,action_map,test_map = 
                       List.fold_left 
                         (fun (error,blackboard,action_map,test_map) action -> 
                           let error,blackboard,action_list,test_list = predicates_of_action true parameter handler error blackboard init action in 
                           error,blackboard,build_map action_list action_map,build_map test_list test_map)
                         (error,blackboard,PredicateidMap.empty,test_map)
                         action_list in 
                     
                     let action_map = 
                       PredicateidMap.add 
                         pid_rule_agent_mutex 
                         (Counter 1)
                         action_map
                     in 
                     let test_map,action_map = 
                       try 
                         let m_id = 
                           AgentIdMap.find 
                             rule_ag_id 
                             data_structure.rule_agent_id_subs 
                         in 
                         PredicateidMap.add 
                           m_id 
                           (Counter 0)
                           test_map,
                         PredicateidMap.add 
                           m_id 
                           (Pointer_to_agent mixture_ag_id)
                           action_map 
                       with Not_found -> test_map,action_map
                     in
                     let test_map,action_map = 
                       try 
                         let m_id = 
                           AgentIdMap.find 
                             mixture_ag_id 
                             data_structure.mixture_agent_id_mutex 
                         in 
                         PredicateidMap.add 
                           m_id 
                           (Counter 0)
                           test_map,
                         PredicateidMap.add
                           m_id
                           (Counter 1)
                           action_map 
                       with 
                       | Not_found -> test_map,action_map
                     in 
                     let g x = 
                       match x 
                       with 
                       | None -> Unknown
                       | Some x -> x
                     in 
                     let merged_map = 
                       PredicateidMap.merge 
                         (fun _ test action -> Some(g test,g action))
                         test_map
                         action_map 
                     in 
                     let merged_map = 
                       List.fold_left 
                         (fun map pid -> PredicateidMap.add pid (Counter 1,Undefined) map)
                         merged_map
                         fictitious_local_list 
                     in 
                     let merged_map = 
                       List.fold_left 
                         (fun map (pid,_,(test,action)) -> add_state pid (test,action) map)
                         merged_map
                         unambiguous_side_effects
                     in 
                     let side_effect = 
                       List.fold_left 
                         (fun list (_,a,_) -> 
                           match a 
                           with 
                           | None -> list
                           | Some a -> a::list)
                         []
                         unambiguous_side_effects 
                     in 
                     let merged_map = 
                       PredicateidMap.mapi 
                         (fun pid (test,action) ->
                             if action = Undefined 
                               && 
                                 begin 
                                   match A.get blackboard.pre_column_map_inv pid
                                   with 
                                   | Bound_site (ag_id,site_id) ->  
                                     ag_id = mixture_ag_id && (SiteIdSet.mem (rule_ag_id,site_id) data_structure.removed_sites_in_other_links)
                                   | _ -> false
                                 end
                             then 
                               (test,Unknown)
                             else 
                               (test,action))
                         merged_map 
                     in
                     if side_effect = []
                     && PredicateidMap.is_empty  merged_map 
                     then 
                       error,log_info,blackboard,init_step
                     else 
                       begin 
                         let nsid = blackboard.pre_nsteps + 1 in 
                         let _ = A.set blackboard.pre_side_effect_of_event nsid  (CI.Po.K.side_effect_of_list side_effect) in
                         let _ = A.set pre_event nsid step in 
                         let pre_steps_by_column = 
                           PredicateidMap.fold 
                             (fun id (test,action) map -> 
                               begin 
                                 let value,list = A.get map id in 
                                 let value' = value + 1 in 
                                 let _ = fadd id action blackboard.history_of_predicate_values_to_predicate_id in 
                                 let _ = A.set map id (value',(nsid,value,test,action)::list)
                                 in map
                               end)
                             merged_map
                             blackboard.pre_steps_by_column 
                         in 
                         let _ = A.set blackboard.pre_kind_of_event nsid (type_of_step (CI.Po.K.type_of_refined_step step)) in 
                         let blackboard = 
                           { 
                             blackboard with 
                               pre_event = pre_event ;
                               pre_fictitious_list = fictitious_list ; 
                               pre_steps_by_column = pre_steps_by_column; 
                               pre_nsteps = nsid;
                           }
                         in 
                         error,log_info,blackboard,init_step 
                       end )
                   (error,log_info,blackboard,init_step) l 
               ) 
               data_structure.old_agents_potential_substitution
               (error,log_info,blackboard,init_step)

           in 
          
            (* deal with substitutable agent in links*)
           let f error log_info blackboard set = 
              AgentId2Set.fold 
               (fun link (error,log_info,blackboard) -> 
                 let link_mutex = AgentId2Map.find link data_structure.links_mutex in 
                 let rule_ag_id1,rule_ag_id2=link in 
                 let subs_1,l_ag_1 = 
                   try 
                     true,AgentIdMap.find rule_ag_id1 data_structure.old_agents_potential_substitution
                   with 
                     Not_found -> false,[rule_ag_id1]
                 in 
                 let subs_2,l_ag_2 = 
                    try 
                     true,AgentIdMap.find rule_ag_id2 data_structure.old_agents_potential_substitution
                   with 
                     Not_found -> false,[rule_ag_id2]
                 in 
                 let test_list = 
                   try 
                     AgentId2Map.find link data_structure.other_links_tests 
                   with 
                   | Not_found -> []
                 in 
                 let action_list = 
                   try 
                     AgentId2Map.find link data_structure.other_links_actions 
                   with 
                   | Not_found -> []
                 in 
                 List.fold_left 
                   (fun (error,log_info,blackboard) mixture_ag_1 -> 
                     let subs = AgentIdMap.empty in 
                     let subs = 
                       if rule_ag_id1 = mixture_ag_1 
                       then 
                         subs 
                       else 
                         AgentIdMap.add rule_ag_id1 mixture_ag_1 subs 
                     in 
                     List.fold_left 
                       (fun (error,log_info,blackboard) mixture_ag_2 -> 
                         if (rule_ag_id1 = rule_ag_id2) = (mixture_ag_1 = mixture_ag_2)
                         then 
                           begin 
                             let step = CI.Po.K.dummy_refined_step ("LINK "^(string_of_int mixture_ag_1)^"/"^(string_of_int rule_ag_id1)^","^(string_of_int  mixture_ag_2)^"/"^(string_of_int rule_ag_id2)^")")
                             in 
                             let subs = 
                               if rule_ag_id2 = mixture_ag_2
                               then 
                                 subs 
                               else 
                                 AgentIdMap.add rule_ag_id2 mixture_ag_2 subs 
                             in 
                             let test_list,action_list = 
                               if subs = AgentIdMap.empty
                               then 
                                 test_list,action_list
                               else 
                                 let f x = 
                                   try 
                                     AgentIdMap.find x subs 
                                   with 
                                   | Not_found -> x 
                                 in 
                                 List.rev_map (CI.Po.K.subs_map_agent_in_test f) (List.rev test_list),
                                 List.rev_map (CI.Po.K.subs_map_agent_in_action f) (List.rev action_list)
                             in
                             let error,blackboard,test_map = 
                               List.fold_left 
                                 (fun (error,blackboard,map) test -> 
                                   let error,blackboard,test_list = predicates_of_test parameter handler error blackboard test in
                                   error,blackboard,build_map test_list map)
                                 (error,blackboard,PredicateidMap.empty)
                                 test_list in 
                             let error,blackboard,action_map,test_map = 
                               List.fold_left 
                                 (fun (error,blackboard,action_map,test_map) action -> 
                                   let error,blackboard,action_list,test_list = predicates_of_action true parameter handler error blackboard init action in 
                                   let action_list = 
                                     List.rev_map
                                       (fun (pid,x) 
                                         -> 
                                           match x with 
                                           | Free -> 
                                             begin
                                               match A.get blackboard.pre_column_map_inv pid
                                               with 
                                               | Bound_site (ag_id,site_id) ->  
                                                 if 
                                                   (ag_id = mixture_ag_1 && (SiteIdSet.mem (rule_ag_id1,site_id) data_structure.removed_sites_in_other_links))
                                                 || (ag_id = mixture_ag_2 && (SiteIdSet.mem (rule_ag_id2,site_id) data_structure.removed_sites_in_other_links))
                                                 then (pid,Undefined)
                                                 else (pid,x)
                                               | _ -> (pid,x)
                                             end
                                           | _ -> (pid,x))
                                       (List.rev action_list)
                                   in 
                                   error,blackboard,build_map action_list action_map,build_map test_list test_map)
                                 (error,blackboard,PredicateidMap.empty,test_map)
                                 action_list in 
                             let g x = 
                               match x 
                               with 
                               | None -> Unknown
                               | Some x -> x
                             in 
                             let merged_map = 
                               PredicateidMap.merge 
                                 (fun _ test action -> Some(g test,g action))
                                 test_map
                                 action_map 
                             in 
                             let merged_map = 
                               PredicateidMap.add link_mutex (Counter 0,Counter 1) merged_map 
                             in 
                         (*** Pointer ->  ***)
                             let merged_map = 
                               try 
                                 let m_id = 
                                   AgentIdMap.find rule_ag_id1 data_structure.rule_agent_id_subs
                                 in 
                                 PredicateidMap.add 
                                   m_id 
                                   (Pointer_to_agent mixture_ag_1,Unknown)
                                   merged_map
                               with Not_found -> merged_map 
                             in 
                             let merged_map = 
                               try 
                                 let m_id = 
                                   AgentIdMap.find rule_ag_id2 data_structure.rule_agent_id_subs 
                                 in 
                                 PredicateidMap.add 
                                   m_id 
                                   (Pointer_to_agent mixture_ag_2,Unknown)
                                   merged_map
                               with Not_found -> merged_map 
                             in 
                             if PredicateidMap.is_empty  merged_map 
                             then 
                               error,log_info,blackboard                              else 
                               begin 
                                 let nsid = blackboard.pre_nsteps + 1 in 
                                 let _ = A.set pre_event nsid step in 
                                 let pre_steps_by_column = 
                                   PredicateidMap.fold 
                                     (fun id (test,action) map -> 
                                       begin 
                                         let value,list = A.get map id in 
                                         let value' = value + 1 in 
                                         let _ = fadd id action blackboard.history_of_predicate_values_to_predicate_id in 
                                         let _ = A.set map id (value',(nsid,value,test,action)::list)
                                         in map
                                       end)
                                     merged_map
                                     blackboard.pre_steps_by_column 
                                 in 
                                 let _ = A.set blackboard.pre_kind_of_event nsid (type_of_step (CI.Po.K.type_of_refined_step step)) in 
                                 let blackboard = 
                                   { 
                                     blackboard with 
                                       pre_event = pre_event ;
                                       pre_steps_by_column = pre_steps_by_column; 
                                       pre_nsteps = nsid;
                                   }
                                 in 
                                 error,log_info,blackboard 
                               end
                           end 
                         else 
                       error,log_info,blackboard)
                       (error,log_info,blackboard)
                       l_ag_2)
                   (error,log_info,blackboard)
                   (l_ag_1))
               set 
               (error,log_info,blackboard)
           in 
           let data_structure = 
             {
               data_structure 
              with 
                other_links = AgentId2Set.diff data_structure.other_links  data_structure.other_links_priority}
           in 
           
           let error,log_info,blackboard = 
             f error log_info blackboard data_structure.other_links_priority
           in 
           let error,log_info,blackboard = 
             f error log_info blackboard data_structure.other_links
           in 

           (*** deal with rigid elements ***)
           let side_effect = data_structure.sure_side_effects in 
           let action_list = 
             match init_step with 
               None -> data_structure.create_actions@data_structure.sure_actions 
             | Some _ -> data_structure.sure_actions 
           in 
           let test_list = data_structure.sure_tests in 
           let fictitious_list = blackboard.pre_fictitious_list in 
          
           let error,blackboard,fictitious_list,fictitious_local_list,unambiguous_side_effects,init_step  = 
             List.fold_left 
               (fun (error,blackboard,fictitious_list,fictitious_local_list,unambiguous_side_effects,init_step) (site,(binding_state)) -> 
                 begin
                   let error,blackboard,potential_target = potential_target error parameter handler blackboard site binding_state in 
                   match 
                     potential_target 
                   with 
                     | [l]  ->  
                       begin
                         let list = 
                           List.fold_left 
                             (fun list t -> t::l)
                             unambiguous_side_effects
                             l 
                         in 
                         error,
                         blackboard,
                         fictitious_list,
                         fictitious_local_list,
                         list,
                         init_step 
                       end
                     | _ -> 
                       begin 
                         let rule_ag_id = CI.Po.K.agent_id_of_agent (CI.Po.K.agent_of_site site) in 
                         let predicate_info = Mutex (Lock_side_effect (step_id,rule_ag_id,rule_ag_id,CI.Po.K.site_name_of_site site)) in 
                         let error,blackboard,predicate_id = allocate parameter handler error blackboard predicate_info  in 
                         let  error,log_info,blackboard,step_id = 
                           init_fictitious_action log_info error predicate_id  blackboard init_step 
                         in 
                         let error,log_info,blackboard = 
                           List.fold_left 
                             (fun (error,log_info,blackboard) list -> 
                               let blackboard = {blackboard with pre_nsteps = blackboard.pre_nsteps+1} in 
                               let log_info = CI.Po.K.P.inc_n_side_events log_info in 
                               let side_effect = 
                                 List.fold_left 
                                   (fun list (_,a,_) -> 
                                     match a 
                                     with 
                                       | None -> list
                                       | Some a -> a::list)
                                   []
                                   list 
                               in 
                               let side_effect = CI.Po.K.side_effect_of_list 
                                 side_effect in 
                               let _ = A.set blackboard.pre_side_effect_of_event 
                                 blackboard.pre_nsteps
                                 side_effect in
                               let error,blackboard = 
                                 List.fold_left
                                   (fun (error,blackboard) (predicate_id,_,(test,action)) -> 
                                     add_fictitious_action error test action predicate_id blackboard)
                                   (error,blackboard) 
                                   ((predicate_id,None,(Counter 0,Counter 1))::list)
                               in 
                               error,log_info,blackboard)
                             (error,log_info,blackboard)
                             potential_target
                         in 
                         error,
                         blackboard,
                         (predicate_id::fictitious_list),
                         (predicate_id::fictitious_local_list),
                         unambiguous_side_effects,
                         init_step
                       end
                 end)
               (error,blackboard,fictitious_list,fictitious_local_list,[],init_step)
               side_effect 
           in 
           let error,blackboard,test_map = 
             List.fold_left 
               (fun (error,blackboard,map) test -> 
                 let error,blackboard,test_list = predicates_of_test parameter handler error blackboard test in
                 error,blackboard,build_map test_list map)
               (error,blackboard,PredicateidMap.empty)
               test_list in 
           let error,blackboard,action_map,test_map = 
             List.fold_left 
               (fun (error,blackboard,action_map,test_map) action -> 
                 let error,blackboard,action_list,test_list = predicates_of_action true parameter handler error blackboard init action in 
                 error,blackboard,build_map action_list action_map,build_map test_list test_map)
               (error,blackboard,PredicateidMap.empty,test_map)
               action_list in 
           let g x = 
             match x 
             with 
               | None -> Unknown
               | Some x -> x
           in 
           let merged_map = 
             PredicateidMap.merge 
               (fun _ test action -> Some(g test,g action))
               test_map
               action_map 
           in 
           let merged_map = 
             List.fold_left 
               (fun map pid -> PredicateidMap.add pid (Counter 1,Undefined) map)
               merged_map
               fictitious_local_list 
           in 
           let merged_map = 
             List.fold_left 
               (fun map (pid,_,(test,action)) -> add_state pid (test,action) map)
               merged_map
               unambiguous_side_effects
           in 
           let side_effect = 
             List.fold_left 
               (fun list (_,a,_) -> 
                 match a 
                 with 
                   | None -> list
                   | Some a -> a::list)
               []
               unambiguous_side_effects 
           in 
           if side_effect = []
             && PredicateidMap.is_empty  merged_map 
           then 
             error,log_info,blackboard,step_id+1
           else 
             begin 
               let nsid = blackboard.pre_nsteps + 1 in 
               let _ = A.set blackboard.pre_side_effect_of_event nsid (CI.Po.K.side_effect_of_list side_effect) in
               let _ = A.set pre_event nsid step in 
               let pre_steps_by_column = 
                 PredicateidMap.fold 
                   (fun id (test,action) map -> 
                     begin 
                       let value,list = A.get map id in 
                       let value' = value + 1 in 
                       let _ = fadd id action blackboard.history_of_predicate_values_to_predicate_id in 
                       let _ = A.set map id (value',(nsid,value,test,action)::list)
                   in map
                     end)
                   merged_map
                   blackboard.pre_steps_by_column 
               in 
               let _ = A.set blackboard.pre_kind_of_event nsid (type_of_step (CI.Po.K.type_of_refined_step step)) in 
               let observable_list = 
                 if CI.Po.K.is_obs_of_refined_step step 
                 then 
                   ([nsid],CI.Po.K.simulation_info_of_refined_step step)::blackboard.pre_observable_list 
                 else
                   blackboard.pre_observable_list 
               in 
               let blackboard = 
                 { 
                   blackboard with 
                     pre_event = pre_event ;
                     pre_fictitious_list = fictitious_list ; 
                     pre_steps_by_column = pre_steps_by_column; 
                     pre_nsteps = nsid;
                     pre_observable_list = observable_list; 
                 }
               in 
               error,log_info,blackboard,step_id+1
             end 
               (*** ***)
       
         let add_step parameter handler error log_info step blackboard step_id = 
           let init = CI.Po.K.is_init_of_refined_step step in 
           let init_step = None in 
           let pre_event = blackboard.pre_event in 
           let error,test_list = CI.Po.K.tests_of_refined_step parameter handler error step in 
           let error,(action_list,side_effect) = CI.Po.K.actions_of_refined_step parameter handler error step in
           let fictitious_local_list = [] in 
           let fictitious_list = blackboard.pre_fictitious_list in 
           let build_map list map = 
             List.fold_left 
               (fun map (id,value) -> PredicateidMap.add id value map)
               map 
               list 
           in 
           let add_state pid (test,action) map = 
             let test',action' = 
               try 
                 PredicateidMap.find pid map  
               with 
                 | Not_found -> Unknown,Unknown 
             in 
             let test = 
               if strictly_more_refined test test' 
               then 
                 test
               else 
                 test'
             in 
             let action = 
               if strictly_more_refined action action' 
               then 
                 action
               else 
                 action'
             in 
             let map = PredicateidMap.add pid (test,action) map in 
             map 
           in 
           let fadd pid p map = 
             match p 
             with 
               | Counter _ | Internal_state_is _ | Undefined 
               | Defined | Present | Bound | Bound_to_type _ | Unknown -> 
                 ()
               | _ -> 
                 let old = A.get map pid in 
                 A.set map pid (C.add p old) 
           in 
           let error,blackboard,fictitious_list,fictitious_local_list,unambiguous_side_effects,init_step = 
             List.fold_left 
               (fun (error,blackboard,fictitious_list,fictitious_local_list,unambiguous_side_effects,init_step) (site,(binding_state)) -> 
                 begin
                   let error,blackboard,potential_target = potential_target error parameter handler blackboard site binding_state in 
                   match 
                     potential_target 
                   with 
                     | [l]  ->  
                       begin
                         let list = 
                           List.fold_left 
                             (fun list t -> t::l)
                             unambiguous_side_effects
                             l 
                         in 
                         error,
                         blackboard,
                         fictitious_list,
                         fictitious_local_list,
                         list,
                         init_step
                       end
                     | _ -> 
                       begin 
                         let rule_ag_id = CI.Po.K.agent_id_of_agent (CI.Po.K.agent_of_site site) in 
                         let predicate_info = Mutex (Lock_side_effect (step_id,rule_ag_id,rule_ag_id,CI.Po.K.site_name_of_site site)) in 
                         let error,blackboard,predicate_id = allocate parameter handler error blackboard predicate_info  in 
                         let error,log_info,blackboard,init_step = init_fictitious_action log_info error predicate_id  blackboard init_step in
                         let error,log_info,blackboard = 
                           List.fold_left 
                             (fun (error,log_info,blackboard) list -> 
                               let blackboard = {blackboard with pre_nsteps = blackboard.pre_nsteps+1} in 
                               let log_info = CI.Po.K.P.inc_n_side_events log_info in 
                               let side_effect = 
                                 List.fold_left 
                                   (fun list (_,a,_) -> 
                                     match a 
                                     with 
                                       | None -> list
                                       | Some a -> a::list)
                                   []
                                   list 
                               in 
                               let side_effect = CI.Po.K.side_effect_of_list side_effect in 
                               let _ = A.set blackboard.pre_side_effect_of_event blackboard.pre_nsteps side_effect in
                               let error,blackboard = 
                                 List.fold_left
                                   (fun (error,blackboard) (predicate_id,_,(test,action)) -> 
                                     add_fictitious_action error test action predicate_id blackboard)
                                   (error,blackboard) 
                                   ((predicate_id,None,(Counter 0,Counter 1))::list)
                               in 
                               error,log_info,blackboard)
                             (error,log_info,blackboard)
                             potential_target
                         in 
                         error,
                         blackboard,
                         (predicate_id::fictitious_list),
                         (predicate_id::fictitious_local_list),
                         unambiguous_side_effects,
                         init_step
                       end
                 end)
               (error,blackboard,fictitious_list,fictitious_local_list,[],init_step)
               side_effect 
           in 
           let error,blackboard,test_map = 
             List.fold_left 
               (fun (error,blackboard,map) test -> 
                 let error,blackboard,test_list = predicates_of_test parameter handler error blackboard test in
                 error,blackboard,build_map test_list map)
               (error,blackboard,PredicateidMap.empty)
               test_list in 
           let error,blackboard,action_map,test_map = 
             List.fold_left 
               (fun (error,blackboard,action_map,test_map) action -> 
                 let error,blackboard,action_list,test_list = predicates_of_action false parameter handler error blackboard init action in 
                 error,blackboard,build_map action_list action_map,build_map test_list test_map)
               (error,blackboard,PredicateidMap.empty,test_map)
               action_list in 
           let g x = 
             match x 
             with 
               | None -> Unknown
               | Some x -> x
           in 
           let merged_map = 
             PredicateidMap.merge 
               (fun _ test action -> Some(g test,g action))
               test_map
               action_map 
           in 
           let merged_map = 
             List.fold_left 
               (fun map pid -> PredicateidMap.add pid (Counter 1,Undefined) map)
               merged_map
               fictitious_local_list 
           in 
           let merged_map = 
             List.fold_left 
               (fun map (pid,_,(test,action)) -> add_state pid (test,action) map)
               merged_map
               unambiguous_side_effects
           in 
           let side_effect = 
             List.fold_left 
               (fun list (_,a,_) -> 
                 match a 
                 with 
                   | None -> list
                   | Some a -> a::list)
               []
               unambiguous_side_effects 
           in 
           if side_effect = []
             && PredicateidMap.is_empty  merged_map 
           then 
             error,log_info,blackboard,step_id+1
           else 
             begin 
               let nsid = blackboard.pre_nsteps + 1 in 
               let _ = A.set blackboard.pre_side_effect_of_event nsid (CI.Po.K.side_effect_of_list side_effect) in
               let _ = A.set pre_event nsid step in 
               let pre_steps_by_column = 
                 PredicateidMap.fold 
                   (fun id (test,action) map -> 
                     begin 
                       let value,list = A.get map id in 
                       let value' = value + 1 in 
                       let _ = fadd id action blackboard.history_of_predicate_values_to_predicate_id in 
                       let _ = A.set map id (value',(nsid,value,test,action)::list)
                   in map
                     end)
                   merged_map
                   blackboard.pre_steps_by_column 
               in 
               let _ = A.set blackboard.pre_kind_of_event nsid (type_of_step (CI.Po.K.type_of_refined_step step)) in 
               let observable_list = 
                 if CI.Po.K.is_obs_of_refined_step step 
                 then 
                   ([nsid],CI.Po.K.simulation_info_of_refined_step step)::blackboard.pre_observable_list 
                 else
                   blackboard.pre_observable_list 
               in 
               let blackboard = 
                 { 
                   blackboard with 
                     pre_event = pre_event ;
                     pre_fictitious_list = fictitious_list ; 
                     pre_steps_by_column = pre_steps_by_column; 
                     pre_nsteps = nsid;
                     pre_observable_list = observable_list; 
                 }
               in 
               error,log_info,blackboard,step_id+1
             end 
               
         let finalize parameter handler error log_info blackboard = 
           let l = blackboard.pre_fictitious_list in 
           match l 
           with 
             | [] -> error,log_info,blackboard 
             | _ -> 
               let nsid = blackboard.pre_nsteps + 1 in 
               let log_info = CI.Po.K.P.inc_n_side_events log_info in 
               let observable_list = 
                 List.rev_map (fun (x,info) -> (nsid::x,info)) (List.rev blackboard.pre_observable_list) 
               in 
               let blackboard = 
                 {
                   blackboard 
                  with 
                    pre_nsteps = nsid ;
                    pre_observable_list = observable_list ;
                    pre_fictitious_observable = Some nsid ;
                 }
               in 
               let error,blackboard = 
                 List.fold_left
                   (fun (error,blackboard) predicate_id ->             
                     add_fictitious_action error Undefined Unknown predicate_id blackboard)
                   (error,blackboard)
                   l
               in 
               let error,set = 
                 List.fold_left 
                   (fun set (steps,_) -> 
                     List.fold_left
                       (fun (error,set) eid -> 
                         let step = A.get blackboard.pre_event eid in 
                         let error,agents_in_obs = CI.Po.K.agent_id_in_obs parameter handler error step in 
                         error,CI.Po.K.AgentIdSet.union set agents_in_obs)
                       set steps)
                 (error,CI.Po.K.AgentIdSet.empty)
                 observable_list 
               in 
               let set x = 
                   CI.Po.K.AgentIdSet.mem x set 
               in 
               let _ = 
                 A.iteri 
                   (fun i step  -> 
                     let _,level = CI.Po.K.level_of_event parameter handler error step set in 
                     A.set 
                       blackboard.pre_level_of_event 
                       i 
                       level
                   )
                   blackboard.pre_event 
               in 
               let _ = 
                 if debug_mode 
                 then 
                   let _ = print_preblackboard parameter handler error blackboard in () 
               in 
               error,log_info,blackboard 

         let add_step_up_to_iso = add_step_strong
                 
  (**interface*)
         let n_predicates parameter handler error blackboard = 
           error,blackboard.pre_ncolumn+1
             
         let event_list_of_predicate parameter handler error blackboard predicate_id = 
           try 
             error,snd (A.get blackboard.pre_steps_by_column predicate_id) 
           with 
             | _ -> 
               let error_list,error = CI.Po.K.H.create_error parameter handler error (Some "blackboard_generation.ml") None (Some "event_list_of_predicate") (Some "881") (Some "Unknown predicate id") (failwith "event_list_of_predicate") in 
               CI.Po.K.H.raise_error parameter handler error_list error []
                 
         let n_events_per_predicate parameter handler error blackboard predicate_id = 
           try 
             error,fst (A.get blackboard.pre_steps_by_column predicate_id) 
           with 
             | _ -> 
               let error_list,error = CI.Po.K.H.create_error parameter handler error (Some "blackboard_generation.ml") None (Some "n_events_per_predicate") (Some "889") (Some "Unknown predicate id") (failwith "n_events_per_predicate") in 
               CI.Po.K.H.raise_error parameter handler error_list error 0
                 
         let n_events parameter handler error blackboard = 
           error,blackboard.pre_nsteps+1 
             
         let mandatory_events parameter handler error blackboard = 
           error,blackboard.pre_observable_list 
             
         let get_fictitious_observable parameter handler error blackboard = 
           error,blackboard.pre_fictitious_observable
             
         let get_side_effect parameter handler error blackboard = 
           error,blackboard.pre_side_effect_of_event
             

          
          end:PreBlackboard)
           

      




