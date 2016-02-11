(**
  * propagation_heuristic.ml
  *
  * Creation:                      <2016-09-05 feret>
  * Last modification: Time-stamp: <2016-02-03 20:03:11 feret>
  *
  * Causal flow compression: a module for KaSim
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS
  *
  * *
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation
  *
  * Copyright 2011,2012 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)


let debug_mode = false
let look_up_for_better_cut = Parameter.look_up_for_better_cut
let look_down_for_better_cut = Parameter.look_down_for_better_cut

module type Blackboard_with_heuristic =
  sig
    module B:Blackboard.Blackboard

    type update_order
    type propagation_check

    val dummy_update_order: update_order
    val forced_events: (B.blackboard,(update_order list * B.PB.step_id list * unit Mods.simulation_info option) list) B.PB.CI.Po.K.H.unary
    val forbidden_events: (B.PB.step_id list,update_order list) B.PB.CI.Po.K.H.unary
    val next_choice: (B.blackboard,update_order list) B.PB.CI.Po.K.H.unary
    val apply_instruction: (B.blackboard,update_order,update_order list,propagation_check list,B.blackboard * update_order list * propagation_check list * B.assign_result) B.PB.CI.Po.K.H.quaternary

    val propagate: (B.blackboard,propagation_check,update_order list,propagation_check list,B.blackboard * update_order list * propagation_check list * B.assign_result) B.PB.CI.Po.K.H.quaternary

  end

module Propagation_heuristic =
  (struct

    module B=(Blackboard.Blackboard:Blackboard.Blackboard)

    let warn parameter error option exn default =
       Exception.warn (B.PB.CI.Po.K.H.get_kasa_parameters parameter) error (Some "propagation_heuristic.ml") option exn (fun () -> default)

    type update_order =
      | Keep_event of B.PB.step_id
      | Discard_event of B.PB.step_id
      | Cut_event of B.PB.step_id
      | Refine_value_after of B.event_case_address * B.PB.predicate_value
      | Refine_value_before of B.event_case_address * B.PB.predicate_value
      | Skip

    let dummy_update_order = Skip

    type propagation_check =
      | Propagate_up of B.event_case_address
      | Propagate_down of B.event_case_address

    let print_output log x =
      if B.is_failed x
      then Format.fprintf log "FAILED"
      else if B.is_ignored x
      then Format.fprintf log "IGNORED"
      else Format.fprintf log "SUCCESS"

    let forced_events parameter handler log_info error blackboard =
      let list = B.forced_events blackboard in
      error,log_info,
      List.rev_map
        (fun (l,info)->
          List.rev_map
            (fun x -> Keep_event x)
            (List.rev l),l,info)
        (List.rev list)

    let forbidden_events paramter handler log_info error list =
      error,log_info,List.rev_map (fun x -> Cut_event x) (List.rev list)

    let get_gen_unresolved_event_on_pid first last succ stop parameter handler log_info error blackboard p_id level =
      let k_init = first blackboard p_id in
      let k_end = last blackboard p_id in
      match k_init,k_end
      with
        | None,_|_,None-> error,log_info,k_init
        | Some i,Some j ->
          begin
            let rec aux i log_info error =
              if stop i j
              then error,log_info,None
              else
                let event_case_address = B.build_event_case_address p_id (B.build_pointer i) in
                let error,log_info,exist =
                  B.exist_case  parameter handler log_info error blackboard event_case_address in

                match exist
                with
                  | None ->
                    let error,log_info,(seid,eid,test,action) = B.get_static parameter handler log_info error blackboard event_case_address in
                    let error,log_info,level_of_event = B.level_of_event parameter handler log_info error blackboard eid in
                    if level_of_event = level
                    then
                      error,log_info,Some eid
                    else
                      aux (succ i) log_info error
                  | Some true | Some false -> aux (succ i) log_info error
            in
            aux i log_info error
          end

	    
    let get_last_unresolved_event_on_pid parameter handler log_info error blackboard p_id level =
      get_gen_unresolved_event_on_pid B.get_last_linked_event B.get_first_linked_event pred (fun i j -> i<j) parameter handler log_info error blackboard p_id level

    let get_first_unresolved_event_on_pid parameter handler log_info error blackboard p_id level =
      get_gen_unresolved_event_on_pid B.get_first_linked_event B.get_last_linked_event succ (fun i j -> i>j) parameter handler log_info error blackboard p_id level

    let get_gen_unresolved_event first last succ stop parameter handler log_info error blackboard level =
      begin
            let rec aux i log_info error =
              if stop i last
              then error,log_info,None
              else
                let error, log_info, exist = B.is_selected_event parameter handler log_info error i blackboard in
                match exist
                with
                  | None ->
                    let error,log_info,level_of_event = B.level_of_event parameter handler log_info error blackboard i in
                    if level_of_event = level
                    then
                      error,log_info,Some i
                    else
                      aux (succ i) log_info error
                  | Some true | Some false -> aux (succ i) log_info error
            in
            aux first log_info error
      end

	    
    let get_last_unresolved_event parameter handler log_info error blackboard level =
      get_gen_unresolved_event (B.get_n_eid blackboard) 0 pred (fun i j -> i<j) parameter handler log_info error blackboard level

    let get_first_unresolved_event parameter handler log_info error blackboard level =
      get_gen_unresolved_event 0 (B.get_n_eid blackboard) succ (fun i j -> i>j) parameter handler log_info error blackboard level

			       
    let compare_int i j =
      if i=0 then false
      else if j=0 then true
      else i<j


    let next_choice parameter handler log_info error blackboard =
      let bool,string=
          begin
            match
              parameter.B.PB.CI.Po.K.H.current_compression_mode
            with
            | None | Some Parameter.Causal-> false,""
            | Some Parameter.Weak -> Parameter.dump_grid_after_branching_during_weak_compression,Parameter.xlsweakFileName
            | Some Parameter.Strong -> Parameter.dump_grid_after_branching_during_strong_compression,Parameter.xlsstrongFileName
	  end
      in
      let error,log_info =
        if bool
        then
          let error,log_info,() = B.export_blackboard_to_xls parameter handler log_info error string (!Priority.n_story) (!Priority.n_branch) blackboard in
          let _ = Priority.n_branch:= (!Priority.n_branch)+1 in
          error,log_info
	else
	  error,log_info
      in
      let error, priority =
	match
	  B.PB.CI.Po.K.H.get_priorities parameter
	with
	| Some x -> error,x
	| None -> warn parameter error (Some "next_choice, line 147, Compression mode has to been selected") (Failure "Compression mode has not been selected") Priority.causal
      in
      let n_p_id = B.get_npredicate_id blackboard in
      let error,() =
	match
	  priority.Priority.candidate_set_of_events
	with
	| Priority.All_remaining_events -> warn parameter error (Some "All_remaining_events strategy is not implemented yet") (Failure "All remaining events strategy is not implemented yet") ()
	| Priority.Wire_with_the_least_number_of_events -> error,()
	| Priority.Wire_with_the_most_number_of_events -> error,()
      in
      let best_pair x a b =
	match
	  x.Priority.candidate_set_of_events
	with
	| Priority.All_remaining_events
	| Priority.Wire_with_the_most_number_of_events -> Tools.min_pos_int_not_zero a b
	| Priority.Wire_with_the_least_number_of_events -> Tools.max_pos_int_not_zero a b
      in
      let get_unresolved_event x parameter handler log_info error blackboard p_id level =
	match
	  x.Priority.try_to_remove_first
	with
	| Priority.Late_events ->
	   begin
	     match
	       x.Priority.candidate_set_of_events
	     with
	     | Priority.All_remaining_events -> get_last_unresolved_event parameter handler log_info error blackboard level
	     | Priority.Wire_with_the_most_number_of_events 
	     | Priority.Wire_with_the_least_number_of_events ->
	       get_last_unresolved_event_on_pid parameter handler log_info error blackboard p_id level
	   end
	| Priority.Early_events ->
	   begin
	     match
	       x.Priority.candidate_set_of_events
	     with
	     | Priority.All_remaining_events -> get_first_unresolved_event parameter handler log_info error blackboard level
	     | Priority.Wire_with_the_most_number_of_events 
	     | Priority.Wire_with_the_least_number_of_events ->
		get_first_unresolved_event_on_pid parameter handler log_info error blackboard p_id level
	   end
      in
      let error,list  =
        if n_p_id = 0
        then
          error,[]
        else
          let rec try_level level_opt error =
            match level_opt
	    with
	    | None ->
              error,[]
	    | Some level ->
              let rec aux level n_p_id step best =
                if step=n_p_id
                then
                  best
                else
                  let grade = B.get_n_unresolved_events_of_pid_by_level blackboard step level in
                  aux level n_p_id (step+1) (best_pair priority best (grade,step))
              in
	      let n,p_id =
		aux
		  level
		  n_p_id
		  1
		  (B.get_n_unresolved_events_of_pid_by_level blackboard 0 level,0)
	      in
	      if n = 0
	      then
		try_level (Priority.lower level) error
	      else
		let error,log_info,event_id =
                  get_unresolved_event
		    priority
                    parameter
                    handler
		    log_info
                    error
                    blackboard
                    p_id
                    level
		in
		match event_id
		with
		| None ->
		   let error,() = warn parameter error (Some "next_choice, line 210, An empty wire has been selected") (Failure "An empty wire has been selected") () in
     		   try_level (Priority.lower level) error
		| Some event_id ->
                   error,[Discard_event event_id;Keep_event event_id]
	  in try_level (Some Priority.highest) error
      in
      error,log_info,list

    let print_event_case_address parameter handler log_info error blackboard  case =
      let error,log_info,(_,eid,_,_) = B.get_static parameter handler log_info error blackboard case in
      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err  "Event: %i, Predicate: %i\n" eid (B.predicate_id_of_case_address case) in
      error,log_info,()
		
    let propagate_down parameter handler log_info error blackboard event_case_address instruction_list propagate_list =
      begin
        let error,log_info,bool = B.exist_case parameter handler log_info error blackboard event_case_address in
        match bool
        with
          | Some false ->
              (* the case has been removed from the blackboard, nothing to be done *)
            error,
            log_info,
            (blackboard,
            instruction_list,
            propagate_list,
            B.success)
          | Some true | None ->
               (* we know that the pair (test/action) can been executed *)
            let case_address = B.case_address_of_case_event_address event_case_address in
            let error,log_info,case_value = B.get parameter handler log_info error case_address blackboard in
            let error,log_info,predicate_value = B.predicate_value_of_case_value parameter handler log_info error case_value in
            begin
              let error,log_info,next_event_case_address = B.follow_pointer_down parameter handler log_info error blackboard event_case_address in
              let error,log_info,bool2 = B.exist_case parameter handler log_info error blackboard next_event_case_address in
              match bool2
              with
                | Some false ->
                  begin
                      (* The blackboard is inconsistent: *)
                      (* Pointers should not point to removed events.*)
                    let error,() =
                      warn parameter error (Some "propagate_down, line 154, inconsistent pointers in blackboard") (Failure "inconsistent pointers in blackboard") ()
                    in
                    error,
                    log_info,
                    (blackboard,
                    instruction_list,
                    propagate_list,
                    B.success)
                  end
                | Some true ->
                  begin (* next event is selected *)
                    let error,log_info,(next_seid,next_eid,next_test,next_action) = B.get_static parameter handler log_info error blackboard next_event_case_address in
                    let case_address = B.case_address_of_case_event_address event_case_address in
                    let error,log_info,case_value = B.get parameter handler log_info error case_address blackboard in
                    let error,log_info,predicate_value = B.predicate_value_of_case_value parameter handler log_info error case_value in
                    match B.PB.is_unknown next_test,B.PB.is_unknown next_action
                    with
                      | true,true ->
                        begin (* no test, no action in next event *)
                          let error,log_info =
                            if debug_mode
                            then
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down (case 1):\n" in
                              let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event is kept but has no test and no action\n" in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "Value is propagated after the next event\n" in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                              error,log_info
			    else
			      error,log_info
                          in
                            (* next event is selected *)
                            (* no test, no action in next event *)
                            (* we propagate the value after the next event*)
                          let log_info = StoryProfiling.StoryStats.add_propagation_case_down 1 log_info in
                          error,
                          log_info,
                          (blackboard,
                          (Refine_value_after(next_event_case_address,predicate_value))::instruction_list,
                          propagate_list,
                          B.success)
                        end
                      | true,false -> (* no test, but an action in next event *)
                        begin
                          let error,log_info =
                            if debug_mode
                            then
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down  (case 2):\n" in
                              let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event is kept, no test, but an action \n" in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "Nothing to be done\n" in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                              error,log_info
			    else
			      error,log_info
                          in
                            (* next event is selected *)
                            (* no test, but an action in next event *)
                            (* nothing to propagate downward*)
                          let log_info = StoryProfiling.StoryStats.add_propagation_case_down 2 log_info in
                          error,
                          log_info,
                          (blackboard,
                          instruction_list,
                          propagate_list,
                          B.success)
                        end
                      | false,true -> (* no action, but a test in next event *)
                        begin
                          if B.PB.compatible predicate_value next_test
                          then
                              (* the test is compatible with the value *)
                            let error,log_info,conj = B.PB.conj parameter handler log_info error next_test predicate_value in
                            let error,log_info =
                              if debug_mode
                              then
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down  (case 3):\n" in
                                let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event is kept, a test but no action \n" in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "Next event Test: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_test in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate new predicate_value " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err conj in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err " before and after next event \n" in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                error,log_info
			      else
				error,log_info
                            in
                              (* next event is selected *)
                              (* no action, but a test in next event *)
                              (* the test is compatible with the value *)
                              (* we propagate the meet of the test and the value before and after the next event *)
                            let log_info = StoryProfiling.StoryStats.add_propagation_case_down 3 log_info in
                            error,
                            log_info,
                            (blackboard,
                            (Refine_value_before(next_event_case_address,conj))::(Refine_value_after(next_event_case_address,conj))::instruction_list,
                            propagate_list,
                            B.success)
                          else (* the test and the value are incompatible *)
                            let error,log_info =
                              if debug_mode
                              then
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down  (case 4):\n" in
                                let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event is kept, a test but no action \n" in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "Next event Test: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_test in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nCut\n" in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                error,log_info
			      else
				error,log_info
                            in
                            let log_info = StoryProfiling.StoryStats.add_propagation_case_down 4 log_info in
                            (* next event is selected *)
                            (* no action, but a test in next event *)
                            (* the test is not compatible with the value *)
                            (* we cut the exploration *)
                            error,
                            log_info,
                            (blackboard,
                            [],
                            [],
                            B.fail)
                        end
                      | false,false ->
                        begin (*there is a test and an action in the next event *)
                          if B.PB.compatible predicate_value next_test
                          then (* the test and the value are compatible *)
                            let error,log_info,conj = B.PB.conj parameter handler log_info error next_test predicate_value in
                            let error,log_info =
                              if debug_mode
                              then
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down  (case 5):\n" in
                                let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event is kept, a test but no action \n" in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event Test: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_test in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event Action:" in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_action in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate new predicate_value " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err conj in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err " before the next event \n" in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                error,log_info
			      else
				error,log_info
                            in
                            let log_info = StoryProfiling.StoryStats.add_propagation_case_down 5 log_info in
                            (* next event is selected *)
                            (* an action and a test in next event *)
                            (* the test is compatible with the value *)
                            (* we propagate the meet of the test and the value before the next event *)
                            error,
                            log_info,
                            (blackboard,
                            (Refine_value_before(next_event_case_address,conj))::instruction_list,
                            propagate_list,
                            B.success)
                          else (* test and value are incompatible *)
                            let error,log_info =
                              if debug_mode
                              then
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down  (case 6):\n" in
                                let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event is kept, a test, an action \n" in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "Next event Test: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_test in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nNext event Action: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_action in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nCut\n" in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                error,log_info
			      else
				error,log_info
                            in
                            let log_info = StoryProfiling.StoryStats.add_propagation_case_down 6 log_info in
                            (* next event is selected *)
                            (* an action and a test in next event *)
                            (* the test is not compatible with the value *)
                            (* we cut the exploration *)
                            error,
                            log_info,
                            (blackboard,
                            [],
                            [],
                            B.fail)
                        end
                  end
                | None -> (* we do not know whether the event is played or not *)
                  begin
                    let error,log_info,(next_seid,next_eid,next_test,next_action) = B.get_static parameter handler log_info error blackboard next_event_case_address in
                    match B.PB.is_unknown next_action
                    with
                      | true  ->
                        begin (* there is no action in the next event *)
                          match
                            B.PB.is_unknown next_test
                          with
                            | true -> (*there is no test in the next event *)
                              begin
                                let error,log_info =
                                  if debug_mode
                                  then
                                    let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down  (case 7):\n" in
                                    let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                    let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the next event is kept\n there is no test, no action \n " in
                                    let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                    let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                    let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nThe value is propagated after and before the next event\n" in
                                    let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                    error,log_info
				  else
				    error,log_info
				in
                                let log_info = StoryProfiling.StoryStats.add_propagation_case_down 7 log_info in
                                (* we do not know whether the event is played or not *)
                                (*there is no test in the next event *)
                                (* there is no action in the next event *)
                                (* we propagate the value after the next event*)
                                error,
                                log_info,
                                (blackboard,
                                (Refine_value_after(next_event_case_address,predicate_value))::instruction_list,
                                propagate_list,
                                B.success)
                              end
                            | false ->
                              begin (* there is a test in the next event *)
                                if B.PB.compatible next_test predicate_value
                                then (* test and predicate_value are compatible *)
                                  let error,log_info =
                                    if debug_mode
                                    then
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down  (case 8):\n" in
                                      let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the next event is kept\n there is a test, but no action \n " in
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event Test: " in
                                      let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_test in
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                      let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nThe value " in
                                      let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err " is propagated after and before the next event\n" in
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                      error,log_info
				    else
				      error,log_info
                                  in
                                  let log_info = StoryProfiling.StoryStats.add_propagation_case_down 8 log_info in
                                  (* we do not know whether the event is played or not *)
                                  (* there is a test in the next event *)
                                  (* there is no action in the next event *)
                                  (* the test is compatible with the value *)
                                  (* we propagate the value after the next event*)
                                  error,
                                  log_info,
                                 ( blackboard,
                                  (Refine_value_after(next_event_case_address,predicate_value))::instruction_list,
                                  propagate_list,
                                  B.success)
                                else
                                  let error,log_info =
                                    if debug_mode
                                    then
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down  (case 9):\n" in
                                      let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the next event is kept\n there is a test, but no action \n " in
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event Test: " in
                                      let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_test in
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                      let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWe discard the next event (%i) \n" next_eid in
                                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                      error,log_info
				    else
				      error,log_info
                                  in
                                  let log_info  = StoryProfiling.StoryStats.add_propagation_case_down 9 log_info in
                                  (* we do not know whether the event is played or not *)
                                  (* there is a test in the next event *)
                                  (* there is no action in the next event *)
                                  (* the test is not compatible with the value *)
                                  (* we discard the next event *)
                                  error,
                                  log_info,
                                  (blackboard,
                                  (Discard_event(next_eid)::instruction_list),
                                  propagate_list,
                                  B.success)
                              end
                        end
                      | false ->
                        begin (* there is an action in the next event *)
                          if not (B.PB.compatible next_action predicate_value)
                          then (* the action is not compatible with the value *)
                            let error,log_info,computed_next_predicate_value =
                              B.PB.disjunction parameter handler log_info error predicate_value next_action
                             in
                            let error,log_info =
                              if debug_mode
                              then
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down  (case 10):\n" in
                                let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the next event is kept\n there is an action \n " in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event Action: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_action in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nThe value " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err computed_next_predicate_value in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err " is propagated after the next event\n" in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                error,log_info
			      else
				error,log_info
                            in
                            let log_info = StoryProfiling.StoryStats.add_propagation_case_down 10 log_info in
                            (* we do not know whether the event is played or not *)
                            (* there is an action in the next event *)
                            (* the action is compatible with the value *)
                            (* we propagate the join of the value and the action after the next event*)
                            error,
                            log_info,
                            (blackboard,
                            (Refine_value_after(next_event_case_address,computed_next_predicate_value))::instruction_list,
                            propagate_list,
                            B.success)
                          else
                            begin (*the action is compatible with the value *)
                              let error,log_info,computed_next_predicate_value =
                                B.PB.disjunction parameter handler log_info error predicate_value next_action
                              in
                              match B.PB.is_unknown next_test
                              with
                                | true ->
                                  begin (* there is no test in the next event *)
                                    let error,log_info =
                                      if debug_mode
                                      then
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down  (case 11):\n" in
                                        let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the next event is kept\n there is no test, but there is an action \n " in
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event Action: " in
                                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_action in
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nThe value " in
                                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err computed_next_predicate_value in
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err " is propagated after the next event\n" in
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                        error,log_info
				      else
					error,log_info
                                    in
                                    let log_info =
                                      StoryProfiling.StoryStats.add_propagation_case_down 11 log_info in
                                    (* we do not know whether the event is played or not *)
                                    (* there is no test in the next event *)
                                    (* there is an action in the next event *)
                                    (* the action is compatible with the value *)
                                    (* we propagate the join of the value and the action after the next event*)
                                    error,
                                    log_info,
                                    (blackboard,
                                    (Refine_value_after(next_event_case_address,computed_next_predicate_value))::instruction_list,
                                    propagate_list,
                                    B.success)
                                  end
                                | false ->
                                  begin
                                    if B.PB.compatible next_test predicate_value
                                    then
                                      let error,log_info =
                                        if debug_mode
                                        then
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down  (case 12):\n" in
                                          let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the next event is kept\n there is a test, but there is an action \n " in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event Test: " in
                                          let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_test in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nnext event Action: " in
                                          let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_action in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                          let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nThe value " in
                                          let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err computed_next_predicate_value in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err " is propagated after the next event\n" in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                          error,log_info
					else
					  error,log_info
                                      in
                                      let log_info =
                                        StoryProfiling.StoryStats.add_propagation_case_down 12 log_info in
                                      (* we do not know whether the event is played or not *)
                                      (* there is a test in the next event *)
                                      (* there is an action in the next event *)
                                      (* the test is compatible with the value *)
                                      (* the action is compatible with the value *)
                                      (* we propagate the join of the value and the action after the next event*)
                                      error,
                                      log_info,
                                      (blackboard,
                                      (Refine_value_after(next_event_case_address,computed_next_predicate_value))::instruction_list,
                                      propagate_list,
                                      B.success)
                                    else
                                      let error,log_info =
                                        if debug_mode
                                        then
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_down  (case 13):\n" in
                                          let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the next event is kept\n there is a test, but there is an action \n " in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "next event Test: " in
                                          let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_test in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nnext event Action: " in
                                          let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err next_action in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                          let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nNext event (%i) is discarded \n " next_eid in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                          error,log_info
					else
					  error,log_info
                                      in
                                      let log_info = StoryProfiling.StoryStats.add_propagation_case_down 13 log_info in
                                      (* we do not know whether the event is played or not *)
                                      (* there is a test in the next event *)
                                      (* there is an action in the next event *)
                                      (* the test is not compatible with the value *)
                                      (* we discard the next event *)
                                      error,
                                      log_info,
                                      (blackboard,
                                      (Discard_event(next_eid)::instruction_list),
                                      propagate_list,
                                      B.success)
                                  end
                            end
                        end
                  end
            end
      end

    let rec last_chance_up parameter handler log_info error blackboard predicate_value event_case_address =
      let error,log_info,bool = B.exist_case parameter handler log_info error blackboard event_case_address in
      match
        bool
      with
        | Some false ->
          error,false,log_info,blackboard
        | Some true ->
          begin
            let error,log_info,(seid,eid,test,action) = B.get_static parameter handler log_info error blackboard event_case_address in
            if B.PB.is_unknown action
            then
              begin
                 let error,log_info,preview_event_case_address = B.follow_pointer_up parameter handler log_info error blackboard event_case_address in
                 let preview_case_address = B.case_address_of_case_event_address preview_event_case_address in
                 let error,log_info,preview_case_value = B.get parameter handler log_info error preview_case_address blackboard in
                 let error,log_info,preview_predicate_value = B.predicate_value_of_case_value parameter handler log_info error preview_case_value in
                 if B.PB.compatible preview_predicate_value predicate_value
                 then
                   let error,log_info,bool = B.is_boundary parameter handler log_info error blackboard event_case_address in
                   if bool
                   then
                      let log_info = StoryProfiling.StoryStats.add_look_up_case 1 log_info in
                      let _ = Format.pp_print_flush
				parameter.B.PB.CI.Po.K.H.out_channel_err () in
                      error,not (B.PB.is_undefined predicate_value),log_info,blackboard
                   else
                     last_chance_up  parameter handler log_info error blackboard predicate_value preview_event_case_address
                 else
                   let log_info = StoryProfiling.StoryStats.add_look_up_case 2 log_info in
                   error,true,log_info,blackboard
              end
            else
              begin
                if B.PB.more_refined action predicate_value
                then
                  error,false,log_info,blackboard
                else
                   let log_info = StoryProfiling.StoryStats.add_look_up_case 3 log_info in
                   error,true,log_info,blackboard
              end
          end
        | None ->
            begin
              let error,log_info,(seid,eid,test,action) = B.get_static parameter handler log_info error blackboard event_case_address in
              if B.PB.more_refined action predicate_value
              then
                error,false,log_info,blackboard
              else
                begin
                  let error,log_info,preview_event_case_address = B.follow_pointer_up parameter handler log_info error blackboard event_case_address in
                  let preview_case_address = B.case_address_of_case_event_address preview_event_case_address in
                  let error,log_info,preview_case_value = B.get parameter handler log_info error preview_case_address blackboard in
                  let error,log_info,preview_predicate_value = B.predicate_value_of_case_value parameter handler log_info error preview_case_value in
                  if B.PB.compatible preview_predicate_value predicate_value
                  then
                      last_chance_up  parameter handler log_info error blackboard predicate_value preview_event_case_address
                  else
                    let log_info = StoryProfiling.StoryStats.add_look_up_case 4 log_info in
                    error,true,log_info,blackboard
                end
            end

    let last_chance_up parameter handler log_info error blackboard predicate_value address =
      if B.PB.is_unknown predicate_value
      then error,false,log_info,blackboard
      else last_chance_up parameter handler log_info error blackboard predicate_value address

    let last_chance_up =
      if look_up_for_better_cut
      then last_chance_up
      else (fun _ _ log_info error blackboard _ _ ->
	    error,
	    false,
	    log_info,
	    blackboard)


    let propagate_up parameter handler log_info error blackboard event_case_address instruction_list propagate_list =
      begin
        let error,log_info,bool = B.exist_case parameter handler log_info error blackboard event_case_address in
        match bool
        with
          | Some false ->
            (* the case has been removed from the blackboard, nothing to be done *)
            error,
            log_info,
            (blackboard,
            instruction_list,
            propagate_list,
            B.success)
          | Some true ->
              (* we know that the pair (test/action) has been executed *)
            let error,log_info,(seid,eid,test,action) = B.get_static parameter handler log_info error blackboard event_case_address in
            let case_address = B.case_address_of_case_event_address event_case_address in
            let error,log_info,case_value = B.get parameter handler log_info error case_address blackboard in
            let error,log_info,predicate_value = B.predicate_value_of_case_value parameter handler log_info error case_value in
            begin
              if B.PB.is_unknown action
              then
                  (* no action, we keep on propagating with the conjonction of the test of the value *)
                begin
                  if B.PB.compatible test predicate_value
                  then
                    let error,log_info,new_value = B.PB.conj parameter handler log_info error test predicate_value in
                    let error,log_info =
                      if debug_mode
                      then
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 1):\n" in
                        let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "The event before is kept, there is no action \n " in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Test: " in
                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err test in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nRefine before the event (before) with the state " in
                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err new_value in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n" in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                        error,log_info
		      else
			error,log_info
                    in
                    let log_info = StoryProfiling.StoryStats.add_propagation_case_up 1 log_info in
                    error,
                    log_info,
                    (blackboard,
                    (Refine_value_before(event_case_address,new_value))::instruction_list,
                    propagate_list,
                    B.success)
                  else
                    let error,log_info =
                      if debug_mode
                      then
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 2):\n" in
                        let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "The event before is kept, there is no action \n " in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Action: " in
                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nCut\n" in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                        error,log_info
		      else
			error,log_info
                    in
                    let log_info = StoryProfiling.StoryStats.add_propagation_case_up 2 log_info in
                    error,
                    log_info,
                    (blackboard,
                    [],
                    [],
                    B.fail)
                end
              else
                if B.PB.more_refined action predicate_value
                then
                  if B.PB.is_undefined test
                  then (*the wire has just be created, nothing to be done *)
                    let error,log_info =
                      if debug_mode
                      then
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 3):\n" in
                        let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "The event before is kept, there is an action and a test \n " in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Test: " in
                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err test in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nbefore event Action: " in
                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nNothing to be done\n" in
                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                        error,log_info
		      else
			error,log_info
                    in
                    let log_info = StoryProfiling.StoryStats.add_propagation_case_up 3 log_info in
                    error,
                    log_info,
                    (blackboard,
                    instruction_list,
                    propagate_list,
                    B.success)
                  else (*we know that the wire was defined before*)
                    if B.PB.compatible test B.PB.defined
                    then
                      begin
                        let error,log_info,state = B.PB.conj parameter handler log_info error test B.PB.defined in
                        let error,log_info =
                          if debug_mode
                          then
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 4):\n" in
                            let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "The event before is kept, there is an action and a test \n " in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Test: " in
                            let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err test in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nbefore event Action: " in
                            let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                            let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nRefine before the event (before) with the state " in
                            let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err state in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n" in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                            error,log_info
			  else
			    error,log_info
                        in
                        let log_info = StoryProfiling.StoryStats.add_propagation_case_up 4 log_info in
                        error,
                        log_info,
                        (blackboard,
                        (Refine_value_before(event_case_address,state)::instruction_list),
                        propagate_list,
                        B.success)
                      end
                    else
                      begin
                        let error,log_info =
                          if debug_mode
                          then
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 5):\n" in
                            let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "The event before is kept, there is an action and a test \n " in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Test: " in
                            let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err test in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nbefore event Action: " in
                            let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                            let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nCut\n" in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                            error,log_info
			  else
			    error,log_info
                        in
                        let log_info = StoryProfiling.StoryStats.add_propagation_case_up 5 log_info in
                        error,
                        log_info,
                        (blackboard,
                        [],
                        [],
                        B.fail)
                      end
                else (*The event has to be discarded which is absurd *)
                  let error,log_info =
                    if debug_mode
                    then
                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 6):\n" in
                      let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "The event before is kept, there is an action \n " in
                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nbefore event Action: " in
                      let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                      let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nCut\n" in
                      let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                      error,log_info
		    else
		      error,log_info
                  in
                  let log_info = StoryProfiling.StoryStats.add_propagation_case_up 6 log_info in
                  error,
                  log_info,
                  (blackboard,
                  [],
                  [],
                  B.fail)
            end
          | None ->
            (* we do not know whether the pair (test/action) has been executed *)
            let error,log_info,(seid,eid,test,action) = B.get_static parameter handler log_info error blackboard event_case_address in
            let case_address = B.case_address_of_case_event_address event_case_address in
            let error,log_info,case_value = B.get parameter handler log_info error case_address blackboard in
            let error,log_info,predicate_value = B.predicate_value_of_case_value parameter handler log_info error case_value in
            begin
              match B.PB.is_unknown action
              with
                | true ->
                  begin
                    match
                      B.PB.is_unknown test
                    with
                      | true ->
                        begin
                          let error,log_info =
                            if debug_mode
                            then
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 7):\n" in
                              let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the event before is kept,  there is neither a  test, nor  action \n " in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "Wire_state: " in
                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nRefine before the event (before) with the state " in
                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n" in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                              error,log_info
			    else
			      error,log_info
                          in
                          let log_info = StoryProfiling.StoryStats.add_propagation_case_up 7 log_info in
                          error,
                          log_info,
                          (blackboard,
                          (Refine_value_before(event_case_address,predicate_value))::instruction_list,
                          propagate_list,
                          B.success)
                        end
                      | false ->
                        begin
                          if B.PB.compatible test predicate_value
                          then
                            let error,log_info =
                              if debug_mode
                              then
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 8):\n" in
                                let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the event is kept, there is a  test, but no action \n " in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Test: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err test in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nRefine before the event (before) with the state " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n" in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                error,log_info
			      else
				error,log_info
                            in
                            let log_info = StoryProfiling.StoryStats.add_propagation_case_up 8 log_info in
                            error,
                            log_info,
                            (blackboard,
                            (Refine_value_before(event_case_address,predicate_value))::instruction_list,
                            propagate_list,
                            B.success)
                          else
                            let error,log_info =
                              if debug_mode
                              then
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 9):\n" in
                                let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the event before is kept, there is a  test, but no action \n " in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Test: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err test in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nEvent before (%i) is discarded \n " eid in
                                let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                error,log_info
			      else
				error,log_info
                            in
                            let log_info = StoryProfiling.StoryStats.add_propagation_case_up 9 log_info in
                            error,
                            log_info,
                            (blackboard,
                            (Discard_event(eid))::instruction_list,
                            propagate_list,
                            B.success)
                        end
                  end
                | false ->
                  begin
                    let error,log_info,preview_event_case_address = B.follow_pointer_up parameter handler log_info error blackboard event_case_address in
                    let preview_case_address = B.case_address_of_case_event_address preview_event_case_address in
                    let error,log_info,preview_case_value = B.get parameter handler log_info error preview_case_address blackboard in
                    let error,log_info,preview_predicate_value = B.predicate_value_of_case_value parameter handler log_info error preview_case_value in
                    if B.PB.compatible preview_predicate_value predicate_value
                    then
                      if B.PB.more_refined action predicate_value
                      then
                        begin
                          let error,bool,log_info,blackboard = last_chance_up parameter handler log_info error blackboard predicate_value preview_event_case_address
                          in
                          if bool
                          then
                            begin
                                let error,log_info =
                                  if debug_mode
                                  then
                                    let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 10):\n" in
                                    let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                    let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the event before is kept, there is an action \n " in
                                    let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Action: " in
                                    let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                                    let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                    let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                    let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nThis is the only opportunity to set up the wire, we keep the event" in
                                    let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n" in
                                    let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                    error,log_info
				  else
				    error,log_info
                                in
                                let log_info = StoryProfiling.StoryStats.add_propagation_case_up 10 log_info in
                                error,
                                log_info,
                                (blackboard,
                                (Keep_event(eid))::instruction_list,
                                propagate_list,
                                B.success)
                            end
                          else
                            begin
                              match
                                B.PB.is_unknown test
                              with
                                | true ->
                                  begin
                                    let error,log_info,new_predicate_value = B.PB.disjunction parameter handler log_info error test predicate_value in
                                    let error,log_info =
                                      if debug_mode
                                      then
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 11):\n" in
                                        let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the event before is kept, there is an action, but no test \n " in
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Action: " in
                                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nRefine before the event (before) with the state " in
                                        let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err preview_predicate_value in
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n" in
                                        let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                        error,log_info
				      else
					error,log_info
                                    in
                                    let log_info = StoryProfiling.StoryStats.add_propagation_case_up 11 log_info in
                                    error,
                                    log_info,
                                    (blackboard,
                                    (Refine_value_before(event_case_address,new_predicate_value))::instruction_list,
                                    propagate_list,
                                    B.success)
                                  end
                                | false ->
                                  begin
                                    if B.PB.compatible test predicate_value
                                    then
                                      begin
                                        if B.PB.compatible test preview_predicate_value
                                        then
                                          let error,log_info,new_test = B.PB.conj parameter handler log_info error test preview_predicate_value in
                                          let error,log_info,new_predicate_value = B.PB.disjunction parameter handler log_info error new_test predicate_value in
                                          let error,log_info =
                                            if debug_mode
                                            then
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 12):\n" in
                                              let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the event before is kept, there is an action and a test \n " in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Test:" in
                                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err test in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nbefore event Action: " in
                                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nRefine before the event (before) with the state " in
                                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err new_predicate_value in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n" in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                              error,log_info
					    else
                                              error,log_info
					  in
                                          let log_info = StoryProfiling.StoryStats.add_propagation_case_up 12 log_info in
                                          error,
                                          log_info,
                                          (blackboard,
                                          (Refine_value_before(event_case_address,new_predicate_value))::instruction_list,
                                          propagate_list,
                                          B.success)
                                        else
                                          let error,log_info =
                                            if debug_mode
                                            then
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 13):\n" in
                                              let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the event before is kept, there is an action and a test \n " in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Test:" in
                                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err test in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nbefore event Action: " in
                                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nDiscard the event before (%i)" eid in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n" in
                                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                              error,log_info
					    else
					      error,log_info
                                          in
                                          let log_info = StoryProfiling.StoryStats.add_propagation_case_up 13 log_info in
                                          error,
                                          log_info,
                                          (blackboard,
                                          (Discard_event(eid))::instruction_list,
                                          propagate_list,
                                          B.success)
                                      end
                                    else
                                      let error,log_info,prev' = B.PB.disjunction parameter handler log_info error predicate_value test in
                                      let error,log_info =
                                        if debug_mode
                                        then
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 14):\n" in
                                          let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the event before is kept, there is an action and a test \n " in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Test:" in
                                          let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err test in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nbefore event Action: " in
                                          let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                                          let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nRefine before the event (before) with the state " in
                                          let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err prev' in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n" in
                                          let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                                          error,log_info
					else
                                          error,log_info
                                      in
                                       let log_info = StoryProfiling.StoryStats.add_propagation_case_up 14 log_info in
                                       error,
                                       log_info,
                                       (blackboard,
                                       (Refine_value_before(event_case_address,prev'))::instruction_list,
                                       propagate_list,
                                       B.success)
                                  end
                            end
                        end
                      else
                          let error,log_info =
                            if debug_mode
                            then
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 15):\n" in
                              let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the event before is kept, there is an action and maybe a test \n " in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nbefore event Action: " in
                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nDiscard the event before (%i)" eid in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n" in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                              error,log_info
			    else
                              error,log_info
                          in
                          let log_info = StoryProfiling.StoryStats.add_propagation_case_up 15 log_info in
                          error,
                          log_info,
                          (blackboard,
                          Discard_event(eid)::instruction_list,
                          propagate_list,
                          B.success)
                    else
                      if B.PB.more_refined action predicate_value
                      then
                        let error,log_info =
                            if debug_mode
                            then
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 16):\n" in
                              let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the event before is kept, there is an action and a test \n " in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Test:" in
                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err test in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nbefore event Action: " in
                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPrevious wire state: " in
                              let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err preview_predicate_value in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nSelect the event before (%i)" eid in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n" in
                              let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                              error,log_info
			    else
                              error,log_info
                        in
                        let log_info = StoryProfiling.StoryStats.add_propagation_case_up 16 log_info in
                        error,
                        log_info,
                        (blackboard,
                        (Keep_event(eid))::instruction_list,
                        propagate_list,
                        B.success)
                      else
                        let error,log_info =
                          if debug_mode
                          then
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nPropagate_up  (case 17):\n" in
                            let error,log_info,() = print_event_case_address parameter handler log_info error blackboard  event_case_address in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "we do not know if the event before is kept, there is an action and a test \n " in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "before event Test:" in
                            let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err test in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nbefore event Action: " in
                            let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err action in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nWire_state: " in
                            let _ = B.PB.print_predicate_value parameter.B.PB.CI.Po.K.H.out_channel_err predicate_value in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\nCut\n" in
                            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "***\n" in
                            error,log_info
			  else
                            error,log_info
                        in
                        let log_info = StoryProfiling.StoryStats.add_propagation_case_up 17 log_info in
                        error,
                        log_info,
                        (blackboard,
                        [],
                        [],
                        B.fail)
                  end
            end
      end
	
    let propagate parameter handler log_info error blackboard check instruction_list propagate_list =
      match check
      with
      | Propagate_up x -> propagate_up   parameter handler log_info error blackboard x instruction_list propagate_list
      | Propagate_down x -> propagate_down parameter handler log_info error blackboard x instruction_list propagate_list

    let cut_case parameter handler case (error,log_info,blackboard,instruction_list,propagate_list) =
      let error,log_info,pointer_next = B.follow_pointer_down parameter handler log_info error blackboard case in
      let error,log_info,pointer_previous = B.follow_pointer_up parameter handler log_info error blackboard case in
      (** we remove the case *)
      let error,log_info,(blackboard,result) =
        B.refine
          parameter
          handler
	  log_info
          error
          (B.exist case)
          (B.boolean (Some false))
          blackboard
      in
      if B.is_failed result
      then (error,log_info,blackboard,[],[]),result
      else if B.is_ignored result
      then (error,log_info,blackboard,instruction_list,propagate_list),result
      else
        begin
          let error,log_info,blackboard = B.dec parameter handler log_info error (B.n_unresolved_events_in_column case) blackboard in
            (** we plug pointer next of the previous event *)
          let error,log_info,blackboard =
            B.overwrite
              parameter
              handler
	      log_info
              error
              (B.pointer_to_next pointer_previous)
              (B.pointer pointer_next)
              blackboard
          in
            (** we plug pointer previous of the next event *)
          let error,log_info,blackboard =
            B.overwrite
              parameter
              handler
	      log_info
              error
              (B.pointer_to_previous pointer_next)
              (B.pointer pointer_previous)
              blackboard
          in
          (error,log_info,blackboard,instruction_list,propagate_list),result
        end

    let look_down parameter handler log_info error blackboard propagate_list case =
      begin
        let error,log_info,(_,_,_,action) = B.get_static parameter handler log_info error blackboard case in
        if B.PB.is_unknown action
        then
          error,log_info,blackboard,propagate_list
        else
          let list_values = B.PB.weakening action in
          begin
            let propagate_list,error,log_info,blackboard =
              List.fold_left
                (fun (propagate_list,error,log_info,blackboard) value ->
                  let rec aux case bool error log_info =
                    let ca = B.case_address_of_case_event_address case in
                    let error,log_info,case_value = B.get parameter handler log_info error ca blackboard in
                    let error,log_info,predicate_value = B.predicate_value_of_case_value parameter handler log_info error case_value in
                    if B.PB.more_refined predicate_value value
                    then
                      let error,log_info,pointer_next = B.follow_pointer_down parameter handler log_info error blackboard case in
                      let log_info = StoryProfiling.StoryStats.add_look_down_case 1 log_info in
                      (Propagate_up pointer_next)::propagate_list,error,log_info,blackboard
                    else
                      let error,log_info,next_case = B.follow_pointer_down parameter handler log_info error blackboard case in
                      let error,log_info,exist = B.exist_case parameter handler log_info error blackboard next_case in
                      match exist
                      with
                        | Some true ->
                          let log_info = StoryProfiling.StoryStats.add_look_down_case 2 log_info in
                          propagate_list,error,log_info,blackboard
                        | Some false -> aux next_case bool error log_info
                        | None ->
                          let error,log_info,(_,_,_,next_action) = B.get_static parameter handler log_info error blackboard next_case in
                          if B.PB.more_refined next_action value
                          then
                            if bool
                            then
                              let log_info = StoryProfiling.StoryStats.add_look_down_case 3 log_info in
                              propagate_list,error,log_info,blackboard
                            else
                              aux next_case true error log_info
                          else
                            aux next_case bool error log_info
                  in
                  aux case false error log_info)
                (propagate_list,error,log_info,blackboard)
                list_values
            in error,log_info,blackboard,propagate_list
          end
      end

    let look_down =
      if look_down_for_better_cut
      then look_down
      else (fun _ _ log_info error blackboard list _ -> error,log_info,blackboard,list)


    let refine_value_after parameter handler log_info error blackboard address value instruction_list propagate_list =
      let case_address = B.value_after address in
      let state = B.state value in
      let error,log_info,(blackboard,result) = B.refine parameter handler log_info error case_address state blackboard in
      if B.is_ignored result
      then
        error,log_info,(blackboard,instruction_list,propagate_list,result)
      else if B.is_failed result
      then
        error,log_info,(blackboard,[],[],result)
      else
        let propagate_list = (Propagate_up address)::(Propagate_down address)::propagate_list in
        error,log_info,(blackboard,instruction_list,propagate_list,result)

    let refine_value_before parameter handler log_info error blackboard address value instruction_list propagate_list =
      let error,log_info,pointer_previous = B.follow_pointer_up parameter handler log_info error blackboard address in
      refine_value_after parameter handler log_info error blackboard pointer_previous value instruction_list propagate_list


    let discard_case parameter handler case (error,log_info,blackboard,instruction_list,propagate_list) =
      let error,log_info,pointer_next = B.follow_pointer_down parameter handler log_info error blackboard case in
      let error,log_info,pointer_previous = B.follow_pointer_up parameter handler log_info error blackboard case in
      (** we remove the case *)
      let error,log_info,(blackboard,result) =
        B.refine
          parameter
          handler
	  log_info
          error
          (B.exist case)
          (B.boolean (Some false))
          blackboard
      in
      if B.is_failed result
      then (error,log_info,blackboard,[],[]),result
      else if B.is_ignored result
      then (error,log_info,blackboard,instruction_list,propagate_list),result
      else
        let ca = B.case_address_of_case_event_address case in
        let error,log_info,case_value = B.get parameter handler log_info error ca blackboard in
        let error,log_info,predicate_value = B.predicate_value_of_case_value parameter handler log_info error case_value in
        begin
          let error,log_info,(blackboard,instruction_list,_,result') = refine_value_after parameter handler log_info error blackboard pointer_previous predicate_value instruction_list propagate_list in
          if B.is_failed result'
          then (error,log_info,blackboard,[],[]),result'
          else
            let error,log_info,blackboard = B.dec parameter handler log_info error (B.n_unresolved_events_in_column case) blackboard in
            let error,log_info,(_,event,_,_) = B.get_static parameter handler log_info error blackboard case in
            let error,log_info,level = B.level_of_event parameter handler log_info error blackboard event in
            let error,log_info,blackboard = B.dec parameter handler log_info error (B.n_unresolved_events_in_column_at_level  case level) blackboard in
            (** we plug pointer next of the previous event *)
            let error,log_info,blackboard =
              B.overwrite
                parameter
                handler
		log_info
                error
                (B.pointer_to_next pointer_previous)
                (B.pointer pointer_next)
                blackboard
            in
            (** we plug pointer previous of the next event *)
            let error,log_info,blackboard =
              B.overwrite
                parameter
                handler
		log_info
                error
                (B.pointer_to_previous pointer_next)
                     (B.pointer pointer_previous)
                blackboard
            in
            let propagate_list =
              (Propagate_up pointer_next)::(Propagate_down pointer_previous)::(Propagate_up pointer_previous)::propagate_list
            in
            let error,log_info,blackboard,propagate_list =
              look_down parameter handler log_info error blackboard propagate_list case
            in
            (error,log_info,blackboard,instruction_list,propagate_list),result
        end

    let keep_case parameter handler case (error,log_info,blackboard,instruction_list,propagate_list) =
      (** we keep the case *)
      let error,log_info,(blackboard,result) =
        B.refine
          parameter
          handler
	  log_info
	  error
          (B.exist case)
          (B.boolean (Some true))
          blackboard
      in
      if B.is_failed result
      then
        (error,log_info,blackboard,[],[]),result
      else if B.is_ignored result
      then
        (error,log_info,blackboard,instruction_list,propagate_list),result
      else
        begin
          let error,log_info,pointer_previous = B.follow_pointer_up parameter handler log_info error blackboard case in
          let error,log_info,pointer_next = B.follow_pointer_down parameter handler log_info error blackboard case in
          let error,log_info,(seid,eid,test,action) = B.get_static parameter handler log_info error blackboard case in
          begin
            let error,log_info,(blackboard,instruction_list,_,result') = refine_value_before parameter handler log_info error blackboard case test   instruction_list propagate_list in
            let error,log_info,(blackboard,instruction_list,_,result'') = refine_value_after  parameter handler log_info error blackboard case action instruction_list propagate_list in
            if B.is_failed result' || B.is_failed result''
            then
              (error,
               log_info,
               blackboard,
               [],
               []),
              B.fail
            else
              let error,log_info,blackboard = B.dec parameter handler log_info error (B.n_unresolved_events_in_column case) blackboard in
              let error,log_info,level = B.level_of_event parameter handler log_info error blackboard eid in
              let error,log_info,blackboard = B.dec parameter handler log_info error (B.n_unresolved_events_in_column_at_level  case level) blackboard in
              let propagate_list =
                (Propagate_up case)::(Propagate_down case)::(Propagate_down pointer_previous)::(Propagate_up pointer_previous)::propagate_list
              in
              (error,
               log_info,
               blackboard,
               instruction_list,
               propagate_list),
              result
          end
        end

    let keep_event parameter handler log_info error blackboard step_id instruction_list propagate_list =
      let error,log_info,(blackboard,success) =
        B.refine parameter handler log_info error
          (B.is_exist_event step_id)
          (B.boolean (Some true))
          blackboard
      in
      if B.is_failed success
      then
        error,log_info,(blackboard,[],[],success)
      else if B.is_ignored success
      then
        error,log_info,(blackboard,instruction_list,propagate_list,success)
      else
        let log_info = StoryProfiling.StoryStats.inc_selected_events log_info in
        let _ =
          if debug_mode
          then
            let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n***\nWe keep event %i\n***\n" step_id in
            ()
         in
        let error,log_info,blackboard = B.dec parameter handler log_info error (B.n_unresolved_events) blackboard  in
        let error,log_info,list = B.case_list_of_eid parameter handler log_info error blackboard step_id in
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
        let (error,log_info,blackboard,instruction_list,propagate_list),success = aux list (error,log_info,blackboard,instruction_list,propagate_list) B.ignore in
        error,log_info,(blackboard,instruction_list,propagate_list,success)

    let gen_event f_case g parameter handler log_info error blackboard step_id instruction_list propagate_list =
      let error,log_info,(blackboard,success) =
        B.refine parameter handler log_info error
          (B.is_exist_event step_id)
          (B.boolean (Some false))
          blackboard
      in
          if B.is_failed success
          then
            error,log_info,(blackboard,[],[],success)
          else if B.is_ignored success
          then
            error,log_info,(blackboard,instruction_list,propagate_list,success)
          else
            begin
              let _ =
                if debug_mode
                then
                  let _ = Format.fprintf parameter.B.PB.CI.Po.K.H.out_channel_err "\n***\nWe remove event %i\n***\n" step_id in
                  ()
              in
              let log_info = g log_info in
              let error,log_info,blackboard = B.dec parameter handler log_info error (B.n_unresolved_events) blackboard  in
              let error,log_info,level = B.level_of_event parameter handler log_info error blackboard step_id in
              let error,log_info,blackboard = B.dec parameter handler log_info error (B.n_unresolved_events_at_level  level) blackboard in
              let error,log_info,list = B.case_list_of_eid parameter handler log_info error blackboard step_id in
              let rec aux l x success =
                match l
                  with
                    | [] -> x,success
                    | t::q ->
                      begin
                        let y,success2 = f_case parameter handler t x in
                        if B.is_ignored success2
                        then aux q y success
                        else if B.is_succeeded success2
                        then aux q y success2
                        else y,success2
                      end
              in
              let (error,log_info,blackboard,instruction_list,propagate_list),success = aux list (error,log_info,blackboard,instruction_list,propagate_list) B.ignore in
                error,log_info,(blackboard,instruction_list,propagate_list,success)
              end

    let cut_event = gen_event cut_case StoryProfiling.StoryStats.inc_cut_events
    let discard_event = gen_event discard_case StoryProfiling.StoryStats.inc_removed_events

    let apply_instruction parameter handler log_info error blackboard instruction instruction_list propagate_list =
        match instruction
        with
          | Keep_event step_id ->
            keep_event parameter handler log_info error blackboard step_id instruction_list propagate_list
          | Cut_event step_id -> cut_event parameter handler log_info error blackboard step_id instruction_list propagate_list
          | Discard_event step_id -> discard_event parameter handler log_info error blackboard step_id instruction_list propagate_list
          | Refine_value_after (address,value) -> refine_value_after parameter handler log_info error blackboard address value instruction_list propagate_list
          | Refine_value_before (address,value) -> refine_value_before parameter handler log_info error blackboard address value instruction_list propagate_list
          | Skip -> error,log_info,(blackboard,instruction_list,propagate_list,B.ignore)

    let keep x = Keep_event x
  end:Blackboard_with_heuristic)
