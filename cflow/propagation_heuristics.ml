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
  * Last modification: 13/03/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module type Blackboard_with_heuristic = 
  sig
    module B:Blackboard.Blackboard 

    type update_order 
    type propagation_check 
      
     (** heuristics *)
    val next_choice: (B.blackboard -> B.PB.H.error_channel * update_order list) B.PB.H.with_handler 
    val apply_instruction: (B.blackboard -> update_order -> update_order list -> propagation_check list -> B.PB.H.error_channel * B.blackboard * update_order list * propagation_check list * B.assign_result) B.PB.H.with_handler 

    val propagate: (B.blackboard -> propagation_check -> update_order list -> propagation_check list 
                    -> B.PB.H.error_channel * B.blackboard * update_order list * propagation_check list * B.assign_result) B.PB.H.with_handler
  end 

module Propagation_heuristic = 
  (struct 

    module B=(Blackboard.Blackboard:Blackboard.Blackboard) 

    type update_order = 
      | Keep_event of B.PB.step_id
      | Discard_event of B.PB.step_id 
      | Refine_value_after of B.event_case_address * B.PB.predicate_value 
      | Refine_value_before of B.event_case_address * B.PB.predicate_value 

    type propagation_check = 
      | Propagate_up of (B.event_case_address * B.PB.predicate_value)
      | Propagate_down of (B.event_case_address * B.PB.predicate_value)

    let get_last_unresolved_event parameter handler error blackboard p_id = 
      let k = B.get_last_linked_event blackboard p_id in 
      match k 
      with 
        | None -> error,k 
        | Some i -> 
          begin
            let rec aux i error = 
              if i<0 
              then error,None 
              else 
                let error,exist = 
                  B.exist parameter handler error blackboard (B.build_event_case_address p_id i) 
                in 
                match exist 
                with 
                  | None | Some true -> error,Some i 
                  | Some false -> aux (i-1) error 

            in 
            aux i error 
          end 
          
    let next_choice parameter handler error (blackboard:B.blackboard) = 
      let n_p_id = B.get_npredicate_id blackboard in 
      let list  = 
        if n_p_id = 0 
        then 
          []
        else 
          let rec aux step best_grade best_predicate = 
            if step=n_p_id 
            then 
              best_predicate 
            else 
              let grade = B.get_n_unresolved_events_of_pid blackboard n_p_id in 
              if grade < best_grade 
              then 
                aux (step+1) grade step 
              else 
                aux (step+1) best_grade best_predicate 
          in 
          let p_id = aux 1 (B.get_n_unresolved_events_of_pid blackboard 0) 0 in 
          let error,event_id = get_last_unresolved_event parameter handler error blackboard p_id in 
          match event_id 
          with 
            | None -> []
            | Some event_id -> [Discard_event event_id;Keep_event event_id]
      in 
      error,list

    let propagate_up parameter handler error blackboard (case_address,case_value) instruction_list propagate_list = 
      let error,bool = B.exist parameter handler error blackboard case_address in 
      match bool 
      with 
        | Some false -> 
          (* the case has been removed from the blackboard, nothing to be done *)
          error,
          blackboard,
          instruction_list,
          propagate_list,
          B.Success 
        | Some true ->
          (* we know that the pair (test/action) has been executed *)
          let error,(seid,eid,test,action) = B.get_static parameter handler error blackboard case_address in 
          if B.PB.is_unknown action 
          then 
            (* no action, we keep on propagating with the conjonction of the test of the value *)
            let error,new_value = B.PB.conj parameter handler error test case_value in 
            error,
            blackboard,
            (Refine_value_before(case_address,new_value))::instruction_list,
            propagate_list,
            B.Success 
          else 
            if B.PB.more_refined action case_value 
            then
              if B.PB.is_undefined test 
              then (*the wire has just be created, nothing to be done *)
                error,
                blackboard,
                instruction_list,
                propagate_list,
                B.Success
              else (*we know that the wire was defined before*)
                error,
                blackboard,
                (Refine_value_before(case_address,B.PB.defined))::instruction_list,
                propagate_list,
                B.Success
            else (*The event has to be discarded which is absurd *)
              error,
              blackboard,
              [],
              [],
              B.Fail 
        | None -> 
          (* we do not know whether the pair (test/action) has been executed *)
          let error,(seid,eid,test,action) = B.get_static parameter handler error blackboard case_address in 
          if B.PB.more_refined action case_value 
          then 
            let error,new_value = B.PB.disjunction parameter handler error test case_value in 
            error,
            blackboard,
            (Refine_value_before(case_address,new_value))::instruction_list,
            propagate_list,
            B.Success
          else (*The event has to be discarded *)
            error,blackboard,(Discard_event(eid)::instruction_list),propagate_list,B.Success 

    let propagate_down parameter handler error blackboard x instruction_list propagate_list = 
      error,blackboard,instruction_list,propagate_list,B.Success 

    let propagate parameter handler error blackboard check instruction_list propagate_list = 
      match check 
      with 
        | Propagate_up x -> propagate_up parameter handler error blackboard x instruction_list propagate_list
        | Propagate_down x -> propagate_down parameter handler error blackboard x instruction_list propagate_list

    let keep_case parameter handler error blackboard case instruction_list propagate_list = 
      error,blackboard,instruction_list,propagate_list,B.Success 

    let discard_case parameter handler error blackboard case instruction_list propagate_list = 
      error,blackboard,instruction_list,propagate_list,B.Success 

    let keep_event parameter handler error blackboard step_id instruction_list propagate_list = 
      error,blackboard,instruction_list,propagate_list,B.Success

    let discard_event parameter handler error blackboard step_id instruction_list propagate_list = 
      error,blackboard,instruction_list,propagate_list,B.Success

    let refine_value_after parameter handler error blackboard address value instruction_list propagate_list =
          error,blackboard,instruction_list,propagate_list,B.Success

    let refine_value_before parameter handler error blackboard address value instruction_list propagate_list =
          error,blackboard,instruction_list,propagate_list,B.Success



    let apply_instruction parameter handler error blackboard instruction instruction_list propagate_list = 
        match instruction 
        with 
          | Keep_event step_id -> keep_event parameter handler error blackboard step_id instruction_list propagate_list 
          | Discard_event step_id -> discard_event parameter handler error blackboard step_id instruction_list propagate_list 
          | Refine_value_after (address,value) -> refine_value_after parameter handler error blackboard address value instruction_list propagate_list 
          | Refine_value_before (address,value) -> refine_value_before parameter handler error blackboard address value instruction_list propagate_list 

  end:Blackboard_with_heuristic)
