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
    val forced_events: (B.blackboard -> B.PB.H.error_channel * update_order list list) B.PB.H.with_handler 
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
      | Propagate_up of B.event_case_address
      | Propagate_down of B.event_case_address

    let print_output log x = 
      if B.is_failed x
      then Printf.fprintf log "FAILED"
      else if B.is_ignored x 
      then Printf.fprintf log "IGNORED" 
      else Printf.fprintf log "SUCCESS" 

    let forced_events parameter handler error blackboard = 
      let list = B.forced_events blackboard in 
      error,List.map (List.map (fun x -> Keep_event x)) list
      
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
                let event_case_address = B.build_event_case_address p_id (B.build_pointer i) in 
                let error,exist = 
                  B.exist_case  parameter handler error blackboard event_case_address in 

                match exist 
                with 
                  | None -> 
                    let error,(seid,eid,test,action) = B.get_static parameter handler error blackboard event_case_address in 
                    error,Some eid 
                  | Some true | Some false -> aux (i-1) error 
            in 
            aux i error 
          end 
          
    let compare_int i j = 
      if i=0 then false
      else if j=0 then true 
      else i<j 

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
              let grade = B.get_n_unresolved_events_of_pid blackboard step in 
              if compare_int grade best_grade  
              then 
                aux (step+1) grade step 
              else 
                aux (step+1) best_grade best_predicate 
          in 
          let p_id = aux 1 (B.get_n_unresolved_events_of_pid blackboard 0) 0 in 
          let _ = Printf.fprintf stderr "NEXT %i\n" p_id in 
          let error,event_id = get_last_unresolved_event parameter handler error blackboard p_id 
          in 
          match event_id 
          with 
            | None -> []
            | Some event_id -> [Discard_event event_id;Keep_event event_id]
      in 
      error,list

    let propagate_down parameter handler error blackboard event_case_address instruction_list propagate_list = 
      begin 
        let error,bool = B.exist_case parameter handler error blackboard event_case_address in 
        match bool 
        with 
          | Some false -> 
              (* the case has been removed from the blackboard, nothing to be done *)
            error,
            blackboard,
            instruction_list,
            propagate_list,
            B.success 
          | Some true | None ->
               (* we know that the pair (test/action) can been executed *)
            let error,next_event_case_address = B.follow_pointer_down parameter handler error blackboard event_case_address in 
            let error,bool2 = B.exist_case parameter handler error blackboard next_event_case_address in 
            match bool2 
            with 
              | Some false -> 
                begin 
                  let error,unit = 
                    let error_list,error = B.PB.H.create_error parameter handler error (Some "propagation_heuristic.ml") None (Some "propagate_down") (Some "123") (Some "inconsistent pointers in blackboard") (failwith "inconsistent pointers in blackboard") in 
                    B.PB.H.raise_error parameter handler error_list stderr error () 
                  in 
                  error,
                  blackboard,
                  instruction_list,
                  propagate_list,
                  B.success 
                end 
              | Some true -> 
                begin 
                  let error,(seid,eid,test,action) = B.get_static parameter handler error blackboard next_event_case_address in 
                  let case_address = B.case_address_of_case_event_address event_case_address in 
                  let error,case_value = B.get parameter handler error case_address blackboard in 
                  let error,predicate_value = B.predicate_value_of_case_value parameter handler error case_value in 
                  match B.PB.is_unknown test,B.PB.is_unknown action 
                  with 
                    | true,true -> 
                      begin
                        error,
                        blackboard,
                        (Refine_value_after(next_event_case_address,predicate_value))::instruction_list,
                        propagate_list,
                        B.success 
                      end 
                    | true,false -> 
                      begin 
                        error,
                        blackboard,
                        instruction_list,
                        propagate_list,
                        B.success
                      end 
                    | false,true -> 
                      begin 
                        if B.PB.compatible predicate_value test  
                        then 
                          let error,conj = B.PB.conj parameter handler error test predicate_value in 
                          error,
                          blackboard,
                          (Refine_value_before(next_event_case_address,conj))::(Refine_value_after(next_event_case_address,conj))::instruction_list,
                          propagate_list,
                          B.success
                        else 
                          error,
                          blackboard,
                          [],
                          [],
                          B.fail 
                      end 
                    | false,false -> 
                      begin 
                        if B.PB.compatible predicate_value test  
                        then 
                          error,
                          blackboard,
                          instruction_list,
                          propagate_list,
                          B.success
                        else 
                          error,
                          blackboard,
                          [],
                          [],
                          B.fail 
                      end 
                end
              | None -> 
                begin 
                  let error,(seid,eid,test,action) = B.get_static parameter handler error blackboard next_event_case_address in 
                  let case_address = B.case_address_of_case_event_address event_case_address in 
                  let error,case_value = B.get parameter handler error case_address blackboard in 
                  let error,predicate_value = B.predicate_value_of_case_value parameter handler error case_value in 
                  match B.PB.is_unknown action 
                  with 
                    | true  -> 
                      begin 
                        match 
                          B.PB.is_unknown test
                        with 
                          | true -> 
                            begin
                              error,
                              blackboard,
                              (Refine_value_after(next_event_case_address,predicate_value))::instruction_list,
                              propagate_list,
                              B.success 
                            end 
                          | false -> 
                            begin 
                              if B.PB.compatible test predicate_value   
                              then 
                                error,
                                blackboard,
                                (Refine_value_after(next_event_case_address,predicate_value))::instruction_list,
                                propagate_list,
                                B.success
                              else 
                                error,
                                blackboard,
                                (Discard_event(eid)::instruction_list),
                                propagate_list,
                                B.success
                            end 
                      end 
                    | false -> 
                      begin 
                        let next_case_address = B.case_address_of_case_event_address next_event_case_address in 
                        let error,next_case_value = B.get parameter handler error next_case_address blackboard in 
                        let error,next_predicate_value = B.predicate_value_of_case_value parameter handler error next_case_value in 
                        if not (B.PB.compatible action next_predicate_value) 
                        then 
                          error,
                          blackboard,
                          (Keep_event(eid)::instruction_list),
                          propagate_list,
                          B.success 
                        else 
                          begin
                            let error,computed_next_predicate_value = 
                              B.PB.disjunction parameter handler error predicate_value action 
                            in 
                            match B.PB.is_unknown test 
                            with 
                              | true -> 
                                begin 
                                  error,
                                  blackboard,
                                  (Refine_value_after(next_event_case_address,computed_next_predicate_value))::instruction_list,
                                  propagate_list,
                                  B.success
                                end 
                              | false -> 
                                begin 
                                  if B.PB.compatible test predicate_value  
                                  then 
                                    error,
                                    blackboard,
                                    (Refine_value_after(next_event_case_address,computed_next_predicate_value))::instruction_list,
                                    propagate_list,
                                    B.success
                                  else 
                                    error,
                                    blackboard,
                                    (Discard_event(eid)::instruction_list),
                                    propagate_list,
                                    B.success
                                end 
                          end 
                      end
                end 
      end 
        
    let propagate_up parameter handler error blackboard event_case_address instruction_list propagate_list = 
      begin 
        let error,bool = B.exist_case parameter handler error blackboard event_case_address in 
        match bool 
        with 
          | Some false -> 
            (* the case has been removed from the blackboard, nothing to be done *)
            error,
            blackboard,
            instruction_list,
            propagate_list,
            B.success 
          | Some true ->
              (* we know that the pair (test/action) has been executed *)
            let error,(seid,eid,test,action) = B.get_static parameter handler error blackboard event_case_address in 
            let case_address = B.case_address_of_case_event_address event_case_address in 
            let error,case_value = B.get parameter handler error case_address blackboard in 
            let error,predicate_value = B.predicate_value_of_case_value parameter handler error case_value in 
            if B.PB.is_unknown action 
            then 
                (* no action, we keep on propagating with the conjonction of the test of the value *)
                begin 
                  if B.PB.compatible test predicate_value 
                  then 
                    let error,new_value = B.PB.conj parameter handler error test predicate_value in 
                    error,
                    blackboard,
                    (Refine_value_before(event_case_address,new_value))::instruction_list,
                    propagate_list,
                    B.success
                  else 
                    let _ = B.PB.print_predicate_value stderr test in 
                    let _ = B.PB.print_predicate_value stderr predicate_value in 
                    let _ = Printf.fprintf stderr "FAIL!" in 
                    error,
                    blackboard,
                    [],
                    [],
                    B.fail 
                end 
              else 
                if B.PB.more_refined action predicate_value 
                then
                  if B.PB.is_undefined test 
                  then (*the wire has just be created, nothing to be done *)
                    error,
                    blackboard,
                    instruction_list,
                    propagate_list,
                    B.success
                  else (*we know that the wire was defined before*)
                    if B.PB.compatible test B.PB.defined 
                    then 
                      begin 
                        let error,state = B.PB.conj parameter handler error test B.PB.defined in 
                        error,
                        blackboard,
                        (Refine_value_before(event_case_address,state)::instruction_list),
                        propagate_list,
                        B.success
                      end
                    else 
                      begin 
                        error,
                        blackboard,
                        [],
                        [],
                        B.fail
                      end
                else (*The event has to be discarded which is absurd *)
                  error,
                  blackboard,
                  [],
                  [],
                  B.fail 
            | None ->
              (* we do not know whether the pair (test/action) has been executed *)
              let error,(seid,eid,test,action) = B.get_static parameter handler error blackboard event_case_address in 
              let case_address = B.case_address_of_case_event_address event_case_address in 
              let error,case_value = B.get parameter handler error case_address blackboard in 
              let error,predicate_value = B.predicate_value_of_case_value parameter handler error case_value in 
              match B.PB.is_unknown action 
              with 
                | true -> 
                  begin 
                    match 
                      B.PB.is_unknown test 
                    with 
                      | true -> 
                        begin
                          error,
                          blackboard,
                          (Refine_value_before(event_case_address,predicate_value))::instruction_list,
                          propagate_list,
                          B.success 
                        end 
                      | false -> 
                        begin 
                          if B.PB.compatible test predicate_value 
                          then 
                            error,
                            blackboard,
                            (Refine_value_before(event_case_address,predicate_value))::instruction_list,
                            propagate_list,
                            B.success 
                          else 
                            error,
                            blackboard,
                            (Discard_event(eid))::instruction_list,
                            propagate_list,
                            B.success 
                        end 
                  end 
                | false -> 
                  begin
                    let error,preview_event_case_address = B.follow_pointer_up parameter handler error blackboard event_case_address in 
                    let preview_case_address = B.case_address_of_case_event_address preview_event_case_address in 
                    let error,preview_case_value = B.get parameter handler error preview_case_address blackboard in 
                    let error,preview_predicate_value = B.predicate_value_of_case_value parameter handler error preview_case_value in 
                    if B.PB.compatible preview_predicate_value predicate_value 
                    then 
                      if B.PB.more_refined action predicate_value 
                      then 
                        begin 
                          match 
                            B.PB.is_unknown test 
                          with 
                            | true -> 
                              begin
                                let error,new_predicate_value = B.PB.disjunction parameter handler error test predicate_value in 
                                error,
                                blackboard,
                                (Refine_value_before(event_case_address,new_predicate_value))::instruction_list,
                                propagate_list,
                                B.success
                              end 
                            | false -> 
                              begin 
                                if B.PB.compatible test predicate_value 
                                then 
                                  begin 
                                    if B.PB.compatible test preview_predicate_value 
                                    then 
                                      let error,new_test = B.PB.conj parameter handler error test preview_predicate_value in 
                                      let error,new_predicate_value = B.PB.disjunction parameter handler error new_test predicate_value in 
                                      error,
                                      blackboard,
                                      (Refine_value_before(event_case_address,new_predicate_value))::instruction_list,
                                         propagate_list,
                                      B.success
                                    else
                                      error,
                                      blackboard,
                                      (Discard_event(eid))::instruction_list,
                                      propagate_list,
                                      B.success 
                                  end 
                                else 
                                  let error,prev' = B.PB.disjunction parameter handler error predicate_value test in 
                                  error,
                                  blackboard,
                                  (Refine_value_before(event_case_address,prev'))::instruction_list,
                                  propagate_list,
                                  B.success 
                                 end 
                        end 
                      else 
                        error,
                        blackboard,
                        Discard_event(eid)::instruction_list,
                        propagate_list,
                        B.success 
                    else 
                      if B.PB.more_refined action predicate_value 
                      then 
                        error,
                        blackboard,
                        (Keep_event(eid))::instruction_list,
                        propagate_list,
                        B.success
                      else 
                        error,
                        blackboard,
                        [],
                        [],
                        B.fail
                  end 
      end 
        

    let propagate parameter handler error blackboard check instruction_list propagate_list = 
      match check 
      with 
        | Propagate_up x -> 

          propagate_up parameter handler error blackboard x instruction_list propagate_list
        | Propagate_down x -> propagate_down parameter handler error blackboard x instruction_list propagate_list

    let discard_case parameter handler case (error,blackboard,instruction_list,propagate_list) = 
      let error,pointer_next = B.follow_pointer_down parameter handler error blackboard case in 
      let error,pointer_previous = B.follow_pointer_up parameter handler error blackboard case in 
      (** we remove the case *)
      let error,blackboard,result = 
        B.refine 
          parameter 
          handler 
          error 
          (B.exist case) 
          (B.boolean (Some false)) 
          blackboard 
      in 
      if B.is_failed result 
      then (error,blackboard,[],[]),result
      else if B.is_ignored result 
      then (error,blackboard,instruction_list,propagate_list),result 
      else 
        begin 
          let error,blackboard = B.dec parameter handler error (B.n_unresolved_events_in_column case) blackboard in 
            (** we plug pointer next of the previous event *)
          let error,blackboard = 
            B.overwrite
              parameter 
              handler
              error 
              (B.pointer_to_next pointer_previous)
              (B.pointer pointer_next)
              blackboard 
          in 
            (** we plug pointer previous of the next event *)
          let error,blackboard = 
            B.overwrite 
              parameter 
              handler 
              error 
              (B.pointer_to_previous pointer_next)
              (B.pointer pointer_previous)
              blackboard 
          in 
          let propagate_list = 
            (Propagate_up pointer_next)::(Propagate_down pointer_previous)::propagate_list 
          in 
          (error,blackboard,instruction_list,propagate_list),result 
        end 
          
    let keep_case parameter handler case (error,blackboard,instruction_list,propagate_list) = 
      (** we keep the case *)
      let error,blackboard,result = 
        B.refine 
          parameter 
          handler 
          error 
          (B.exist case) 
          (B.boolean (Some true)) 
          blackboard 
      in 
      if B.is_failed result 
      then 
        (error,blackboard,[],[]),result
      else if B.is_ignored result 
      then 
        (error,blackboard,instruction_list,propagate_list),result
      else 
          begin 
            let error,blackboard = B.dec parameter handler error (B.n_unresolved_events_in_column case) blackboard in 
            let propagate_list = 
              (Propagate_up case)::(Propagate_down case)::propagate_list 
            in 
            (error,blackboard,instruction_list,propagate_list),result 
          end 


    let keep_event parameter handler error blackboard step_id instruction_list propagate_list = 
      let error,blackboard,success = 
        B.refine parameter handler error 
          (B.is_exist_event step_id)
          (B.boolean (Some true)) 
          blackboard
      in 
      let error,blackboard = B.dec parameter handler error (B.n_unresolved_events) blackboard  in 
      let error,list = B.case_list_of_eid parameter handler error blackboard step_id in 
      let rec aux l x success = 
        match l 
        with 
          | [] -> x,success 
          | t::q ->
            begin 
              let y,success2 = keep_case parameter handler t x in 
              if B.is_ignored success2 
              then 
                aux q y success
              else if B.is_succeeded success2 
              then aux q y success2 
              else 
                y,success2 
            end 
      in 
      let (error,blackboard,instruction_list,propagate_list),success = aux list (error,blackboard,instruction_list,propagate_list) B.ignore in 
      error,blackboard,instruction_list,propagate_list,success 

    let discard_event parameter handler error blackboard step_id instruction_list propagate_list = 
      let error,blackboard,success = 
        B.refine parameter handler error 
          (B.is_exist_event step_id)
          (B.boolean (Some false)) 
          blackboard
      in 
          if B.is_failed success 
          then 
            error,blackboard,[],[],success
          else if B.is_ignored success 
          then 
            error,blackboard,instruction_list,propagate_list,success
          else 
            begin
              let error,blackboard = B.dec parameter handler error (B.n_unresolved_events) blackboard  in 
              let error,list = B.case_list_of_eid parameter handler error blackboard step_id in 
              let rec aux l x success = 
                match l 
                  with 
                    | [] -> x,success 
                    | t::q ->
                      begin 
                        let y,success2 = discard_case parameter handler t x in 
                        if B.is_ignored success2
                        then aux q y success 
                        else if B.is_succeeded success2 
                        then aux q y success2
                        else y,success2 
                      end 
              in 
              let (error,blackboard,instruction_list,propagate_list),success = aux list (error,blackboard,instruction_list,propagate_list) B.ignore in 
                error,blackboard,instruction_list,propagate_list,success 
              end 


    let refine_value_after parameter handler error blackboard address value instruction_list propagate_list =
      let case_address = B.value_after address in 
      let state = B.state value in 
      let error,blackboard,result = B.refine parameter handler error case_address state blackboard in 
      if B.is_ignored result 
      then 
        error,blackboard,instruction_list,propagate_list,result 
      else if B.is_failed result 
      then 
        error,blackboard,[],[],result 
      else 
        let propagate_list = (Propagate_up address)::(Propagate_down address)::propagate_list in 
        error,blackboard,instruction_list,propagate_list,result 
          
    let refine_value_before parameter handler error blackboard address value instruction_list propagate_list =
      let error,pointer_previous = B.follow_pointer_up parameter handler error blackboard address in 
      refine_value_after parameter handler error blackboard pointer_previous value instruction_list propagate_list 

    let apply_instruction parameter handler error blackboard instruction instruction_list propagate_list = 
        match instruction 
        with 
          | Keep_event step_id -> 
            let error,blackboard,ins,prop,output = keep_event parameter handler error blackboard step_id instruction_list propagate_list in 
            error,blackboard,ins,prop,output 
          | Discard_event step_id -> discard_event parameter handler error blackboard step_id instruction_list propagate_list 
          | Refine_value_after (address,value) -> refine_value_after parameter handler error blackboard address value instruction_list propagate_list 
          | Refine_value_before (address,value) -> refine_value_before parameter handler error blackboard address value instruction_list propagate_list 

    let keep x = Keep_event x
  end:Blackboard_with_heuristic)
