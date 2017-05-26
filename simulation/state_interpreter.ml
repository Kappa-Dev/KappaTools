(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  init_stopping_times : (Nbr.t * int) list;
  mutable stopping_times : (Nbr.t * int) list;
  perturbations_alive : bool array;
  time_dependent_perts : int list;
  mutable active_perturbations : int list;
  perturbations_not_done_yet : bool array;
  (* internal array for perturbate function (global to avoid useless alloc) *)
  activities : Random_tree.tree;
  (* pair numbers are regular rule, odd unary instances *)
  mutable flux: (Data.flux_data) list;
  with_delta_activities : bool;
}

let initial_activity env counter graph activities =
  Model.fold_rules
    (fun i () rule ->
       if Array.length rule.Primitives.connected_components = 0 then
         match Nbr.to_float @@ Rule_interpreter.value_alg
             counter graph (fst rule.Primitives.rate) with
         | None ->
           ExceptionDefn.warning ~pos:(snd rule.Primitives.rate)
             (fun f -> Format.fprintf f "Problematic rule rate replaced by 0")
         | Some rate -> Random_tree.add (2*i) rate activities)
    () env

let empty ~with_delta_activities env stopping_times =
  let activity_tree =
    Random_tree.create (2*Model.nb_rules env) in
  let stops =
    List.sort (fun (a,_) (b,_) -> Nbr.compare a b) stopping_times in
  let time_dependent_perts =
    let rec aux dep acc =
      Operator.DepSet.fold
        (fun dep perts ->
           match dep with
           | Operator.ALG j ->
             aux (Model.get_alg_reverse_dependencies env j) perts
           | Operator.PERT p ->
             List_util.merge_uniq Mods.int_compare [p] perts
           | Operator.RULE _ -> perts)
    dep acc in
    aux (let x,_,_,_ = Model.all_dependencies env in x) [] in
  {
    init_stopping_times = stops;
    stopping_times = stops;
    perturbations_alive =
      Array.make (Model.nb_perturbations env) true;
    active_perturbations = [];
    time_dependent_perts;
    perturbations_not_done_yet =
      Array.make (Model.nb_perturbations env) true;
    activities = activity_tree;
    flux = [];
    with_delta_activities;
  }

let observables_values env graph counter =
  Model.map_observables
    (Rule_interpreter.value_alg counter graph)
    env

let do_modification ~outputs env counter graph state extra modification =
  let print_expr_val =
    Kappa_printer.print_expr_val
      (Rule_interpreter.value_alg counter graph) in
  match modification with
  | Primitives.ITER_RULE ((v,_),r) ->
    let text =
      Format.asprintf
        "@[<h>%a@]" (Kappa_printer.modification ~env) modification in
    let graph' =
      Nbr.maybe_iteri
        (fun _ g ->
           Rule_interpreter.force_rule
             ~outputs env counter g (Trace.PERT text) r)
        graph (Rule_interpreter.value_alg counter graph v) in
    let graph'',extra' =
      Rule_interpreter.update_outdated_activities
        (fun x _ y -> Random_tree.add x y state.activities)
        env counter graph' in
    ((false,graph'',state),List_util.merge_uniq Mods.int_compare  extra' extra)
  | Primitives.UPDATE (i,(expr,_)) ->
    let graph' = Rule_interpreter.overwrite_var i counter graph expr in
    let graph'',extra' =
        Rule_interpreter.update_outdated_activities
          (fun x _ y -> Random_tree.add x y state.activities)
          env counter graph' in
    ((false, graph'', state),List_util.merge_uniq Mods.int_compare  extra' extra)
  | Primitives.STOP pexpr ->
    let () = if pexpr <> [] then
        let file = Format.asprintf "@[<h>%a@]" print_expr_val pexpr in
        outputs (Data.Snapshot
                   (Rule_interpreter.snapshot env counter file graph)) in
    ((true,graph,state),extra)
  | Primitives.PRINT (pe_file,pe_expr) ->
    let file_opt =
      match pe_file with
        [] -> None
      | _ -> Some (Format.asprintf "@[<h>%a@]" print_expr_val pe_file)
    in
    let line = Format.asprintf "@[<h>%a@]" print_expr_val pe_expr in
    let () = outputs
        (Data.Print
           {Data.file_line_name = file_opt ; Data.file_line_text = line;}) in
    ((false, graph, state),extra)
  | Primitives.PLOTENTRY ->
    let () = outputs (Data.Plot (observables_values env graph counter)) in
    ((false, graph, state),extra)
  | Primitives.SNAPSHOT pexpr  ->
    let file = Format.asprintf "@[<h>%a@]" print_expr_val pexpr in
    let () = outputs (Data.Snapshot
                        (Rule_interpreter.snapshot env counter file graph)) in
    ((false, graph, state),extra)
  | Primitives.CFLOW (name,cc,tests) ->
    let name = match name with
      | Some s -> s
      | None ->
        let domain = Model.domain env in
        Format.asprintf
          "@[<h>%a@]"
          (Pp.array Pp.comma
             (fun _ -> Pattern.print ~new_syntax:true ~domain ~with_id:false))
          cc in
    ((false,
      Rule_interpreter.add_tracked cc name tests graph,
      state),
     extra)
  | Primitives.CFLOWOFF cc ->
    ((false, Rule_interpreter.remove_tracked cc graph, state),extra)
  | Primitives.SPECIES_OFF cc ->
    ((false, Rule_interpreter.remove_tracked_species cc graph, state),extra)
  | Primitives.FLUX (rel,s) ->
    let file = Format.asprintf "@[<h>%a@]" print_expr_val s in
    let () =
      if List.exists
          (fun x -> Fluxmap.flux_has_name file x && x.Data.flux_kind = rel)
          state.flux
      then ExceptionDefn.warning
          (fun f ->
             Format.fprintf
               f "At t=%f, e=%i: tracking FLUX into \"%s\" was already on"
               (Counter.current_time counter)
               (Counter.current_event counter) file)
    in
    let () = state.flux <-
        Fluxmap.create_flux env counter rel file::state.flux in
    ((false, graph, state),extra)
  | Primitives.FLUXOFF s ->
    let file = Format.asprintf "@[<h>%a@]" print_expr_val s in
    let (these,others) =
      List.partition (Fluxmap.flux_has_name file) state.flux in
    let () = List.iter
        (fun x -> outputs (Data.Flux (Fluxmap.stop_flux env counter x)))
        these in
    let () = state.flux <- others in
    ((false, graph, state),extra)
  | Primitives.SPECIES (s,cc,tests) ->
    let file = Format.asprintf "@[<h>%a@]" print_expr_val s in
    ((false,
      Rule_interpreter.add_tracked_species cc file tests graph,
      state),
     extra)

let rec perturbate ~outputs env counter graph state = function
  | [] -> (false,graph,state)
  | i :: tail ->
    let pert = Model.get_perturbation env i in
    if state.perturbations_alive.(i) &&
       state.perturbations_not_done_yet.(i) &&
       Rule_interpreter.value_bool
         counter graph (fst pert.Primitives.precondition)
    then
      let (stop,graph,state as acc,extra) =
        List.fold_left (fun ((stop,graph,state),extra as acc) effect ->
            if stop then acc else
              do_modification ~outputs env counter graph state extra effect)
          ((false,graph,state),[]) pert.Primitives.effect in
      let () = state.perturbations_not_done_yet.(i) <- false in
      let alive =
          match pert.Primitives.abort with
          | None -> false
          | Some (ex,_) -> not (Rule_interpreter.value_bool counter graph ex) in
      let () = if alive then
          state.active_perturbations <- List_util.merge_uniq
              Mods.int_compare [i] state.active_perturbations in
      let () = state.perturbations_alive.(i) <- alive in
      if stop then acc else
        perturbate ~outputs env counter graph state
          (List_util.merge_uniq Mods.int_compare extra tail)
    else
      perturbate ~outputs env counter graph state tail

let do_modifications ~outputs env counter graph state list =
  let (stop,graph,state as acc,extra) =
    List.fold_left (fun ((stop,graph,state),extra as acc) effect ->
        if stop then acc else
          do_modification ~outputs env counter graph state extra effect)
      ((false,graph,state),[]) list in
  if stop then acc else perturbate ~outputs env counter graph state extra

let initialize ~bind ~return ~outputs env counter graph0 state0 init_l =
  let mgraph =
    List.fold_left
      (fun mstate (alg,compiled_rule,pos) ->
         bind
           mstate
           (fun (stop,state,state0) ->
              let value =
                Rule_interpreter.value_alg counter state alg in
              let actions =
                compiled_rule.Primitives.instantiations.Instantiation.actions in
              let creations_sort =
                List.fold_left
                  (fun l -> function
                     | Instantiation.Create (x,_) ->
                       Matching.Agent.get_type x :: l
                     | Instantiation.Mod_internal _ | Instantiation.Bind _
                     | Instantiation.Bind_to _ | Instantiation.Free _
                     | Instantiation.Remove _ -> l) [] actions in
              return (stop,
                Nbr.iteri
                  (fun _ s ->
                     match Rule_interpreter.apply_rule
                             ~outputs env counter s (Trace.INIT creations_sort)
                             compiled_rule with
                     | Rule_interpreter.Success s -> s
                     | (Rule_interpreter.Clash | Rule_interpreter.Corrected) ->
                       raise (ExceptionDefn.Internal_Error
                                ("Bugged initial rule",pos)))
                  state value,state0))) (return (false,graph0,state0)) init_l in
  bind
    mgraph
    (fun (_,graph,state0) ->
       let () =
         initial_activity env counter graph state0.activities in
       let mid_graph,_ =
         Rule_interpreter.update_outdated_activities
           (fun x _ y -> Random_tree.add x y state0.activities)
           env counter graph in
       let everybody =
         let t  = Array.length state0.perturbations_alive in
         Tools.recti (fun l i -> (t-i-1)::l) [] t in
       let out = perturbate ~outputs env counter mid_graph state0 everybody in
       let () =
         Array.iteri (fun i _ -> state0.perturbations_not_done_yet.(i) <- true)
           state0.perturbations_not_done_yet in
       return out)

let one_rule ~outputs ~maxConsecutiveClash env counter graph state =
  let choice,_ = Random_tree.random
      (Rule_interpreter.get_random_state graph) state.activities in
  let rule_id = choice/2 in
  let rule = Model.get_rule env rule_id in
  let prev_activity = Random_tree.total state.activities in
  let act_stack = ref [] in
  let register_new_activity rd_id syntax_rd_id new_act =
    let () =
      match state.flux, state.with_delta_activities with
      | [], false -> ()
      | l,_ ->
        let old_act = Random_tree.find rd_id state.activities in
        let () = act_stack := (syntax_rd_id,(old_act,new_act))::!act_stack in
        List.iter
          (fun fl ->
             Fluxmap.incr_flux_flux
               rule.Primitives.syntactic_rule syntax_rd_id
               (
                 let cand =
                   match fl.Data.flux_kind with
                   | Primitives.ABSOLUTE -> new_act -. old_act
                   | Primitives.PROBABILITY ->
                    -. (old_act /. prev_activity)
                   | Primitives.RELATIVE ->
                     if (match classify_float old_act with
                         | (FP_zero | FP_nan | FP_infinite) -> false
                         | (FP_normal | FP_subnormal) -> true)
                     then (new_act -. old_act) /. old_act
                     else (new_act -. old_act) in
                 match classify_float cand with
                 | (FP_nan | FP_infinite) ->
                   let () =
                     let ct = Counter.current_time counter in
                     ExceptionDefn.warning
                       (fun f -> Format.fprintf
                           f "An infinite (or NaN) activity variation has been ignored at t=%f"
                           ct) in 0.
                 | (FP_zero | FP_normal | FP_subnormal) -> cand) fl)
          l
    in Random_tree.add rd_id new_act state.activities in
  let finalize_registration () =
    match state.flux, state.with_delta_activities with
    | [], false -> ()
    | l, _ ->
      let () =
        if state.with_delta_activities then
          outputs (Data.DeltaActivities
                     (rule.Primitives.syntactic_rule,!act_stack)) in
      let n_activity = Random_tree.total state.activities in
      let () =
        List.iter
          (fun fl ->
             let () = Fluxmap.incr_flux_hit rule.Primitives.syntactic_rule fl in
             match fl.Data.flux_kind with
             | Primitives.ABSOLUTE | Primitives.RELATIVE -> ()
             | Primitives.PROBABILITY ->
               List.iter
                 (fun (syntax_rd_id,(_,new_act)) ->
                    Fluxmap.incr_flux_flux
                      rule.Primitives.syntactic_rule syntax_rd_id
                      (let cand = new_act /. n_activity in
                       match classify_float cand with
                       | (FP_nan | FP_infinite) ->
                         let () =
                           let ct = Counter.current_time counter in
                           ExceptionDefn.warning
                             (fun f -> Format.fprintf
                                 f "An infinite (or NaN) activity variation has been ignored at t=%f"
                                 ct) in 0.
                       | (FP_zero | FP_normal | FP_subnormal) -> cand) fl)
                 !act_stack) l in
      act_stack := [] in
  let () =
    if !Parameter.debugModeOn then
      Format.printf
        "@[<v>@[Applied@ %t%i:@]@ @[%a@]@]@."
        (fun f -> if choice mod 2 = 1 then Format.fprintf f "unary@ ")
        rule_id (Kappa_printer.elementary_rule ~env) rule
        (*Rule_interpreter.print_dist env graph rule_id*) in
  (* let () = *)
  (*   Format.eprintf "%a@." (Rule_interpreter.print_injections env) graph in *)
  let cause = Trace.RULE rule_id in
  let apply_rule =
    if choice mod 2 = 1
    then Rule_interpreter.apply_unary_rule ~outputs ~rule_id
    else Rule_interpreter.apply_rule ~outputs ~rule_id in
  match apply_rule env counter graph cause rule with
  | Rule_interpreter.Success (graph') ->
    let final_step = not (Counter.one_constructive_event counter) in
    let graph'',extra_pert =
      Rule_interpreter.update_outdated_activities
        register_new_activity env counter graph' in
    let () = finalize_registration () in
    let actives = state.active_perturbations in
    let () = state.active_perturbations <- [] in
    let (stop,graph''',state') =
      perturbate ~outputs env counter graph'' state
        (List.rev_append actives extra_pert) in
    let () =
      Array.iteri (fun i _ -> state.perturbations_not_done_yet.(i) <- true)
        state.perturbations_not_done_yet in
    let () =
      if !Parameter.debugModeOn then
        Format.printf "@[<v>Obtained@ %a@]@."
          (Rule_interpreter.print env) graph'' in
    (final_step||stop,graph''',state')
  | (Rule_interpreter.Clash | Rule_interpreter.Corrected) as out ->
    let continue =
      if out = Rule_interpreter.Clash then
        Counter.one_clashing_instance_event counter
      else if choice mod 2 = 1
      then Counter.one_no_more_unary_event counter
      else Counter.one_no_more_binary_event counter in
    if Counter.consecutive_null_event counter < maxConsecutiveClash
    then (not continue,graph,state)
    else
      let register_new_activity rd_id _ new_act =
        Random_tree.add rd_id new_act state.activities in
      (not continue,
       (if choice mod 2 = 1
        then Rule_interpreter.adjust_unary_rule_instances
            ~rule_id register_new_activity env counter graph rule
        else Rule_interpreter.adjust_rule_instances
            ~rule_id register_new_activity env counter graph rule),
       state)

let activity state = Random_tree.total state.activities

let a_loop
    ~outputs ~dumpIfDeadlocked ~maxConsecutiveClash env counter graph state =
  let activity = activity state in
  let rd = Random.State.float (Rule_interpreter.get_random_state graph) 1.0 in
  let dt = abs_float (log rd /. activity) in

  let out =
    (*Activity is null or dt is infinite*)
    if not (activity > 0.) || dt = infinity then
      match state.stopping_times with
      | [] ->
        let () =
          if dumpIfDeadlocked then
            outputs
              (Data.Snapshot
                 (Rule_interpreter.snapshot env counter "deadlock.ka" graph)) in
        let () =
          ExceptionDefn.warning
            (fun f ->
               Format.fprintf
                 f "A deadlock was reached after %d events and %Es (Activity = %.5f)"
                 (Counter.current_event counter)
                 (Counter.current_time counter) activity) in
        (true,graph,state)
      | (ti,pe) :: tail ->
        let () = state.stopping_times <- tail in
        let continue = Counter.one_time_correction_event counter ti in
        let () =
          Counter.fill ~outputs counter ~dt:0. (observables_values env graph) in
        let stop,graph',state' =
          perturbate ~outputs env counter graph state [pe] in
        (not continue||stop,graph',state')
    else
      (*activity is positive*)
      match state.stopping_times with
      | (ti,pe) :: tail
        when Nbr.is_smaller ti (Nbr.F (Counter.current_time counter +. dt)) ->
        let () = state.stopping_times <- tail in
        let continue = Counter.one_time_correction_event counter ti in
        let () =
          Counter.fill ~outputs counter ~dt:0. (observables_values env graph) in
        let stop,graph',state' =
          perturbate ~outputs env counter graph state [pe] in
        (not continue||stop,graph',state')
      | _ ->
        let () =
          Counter.fill ~outputs counter ~dt (observables_values env graph) in
        let () = Counter.one_time_advance counter dt in
        let (stop,graph',state' as pack) = perturbate
            ~outputs env counter graph state state.time_dependent_perts in
        if stop then pack else
          one_rule ~outputs ~maxConsecutiveClash env counter graph' state' in
  out

let end_of_simulation ~outputs form env counter graph state =
  let () =
    Counter.fill ~outputs counter ~dt:0. (observables_values env graph) in
  let () =
    List.iter
      (fun e ->
         let () =
           ExceptionDefn.warning
             (fun f ->
                Format.fprintf
                  f "Tracking FLUX into \"%s\" was not stopped before end of simulation"
                  (Fluxmap.get_flux_name e)) in
         outputs (Data.Flux (Fluxmap.stop_flux env counter e)))
      state.flux in
  ExceptionDefn.flush_warning form
