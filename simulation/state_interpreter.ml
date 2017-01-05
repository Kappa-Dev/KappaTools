type t = {
  init_stopping_times : (Nbr.t * int) list;
  mutable stopping_times : (Nbr.t * int) list;
  perturbations_alive : bool array;
  perturbations_not_done_yet : bool array;
  (* internal array for perturbate function (global to avoid useless alloc) *)
  activities : Random_tree.tree;
  (* pair numbers are regular rule, odd unary instances *)
  variables_overwrite: Alg_expr.t option array;
  mutable flux: (Data.flux_data) list;
}

let get_alg env state i =
  match state.variables_overwrite.(i) with
  | None -> Environment.get_alg env i
  | Some expr -> expr

let initial_activity get_alg env counter graph activities =
  Environment.fold_rules
    (fun i () rule ->
       if Array.length rule.Primitives.connected_components = 0 then
         match Nbr.to_float @@ Rule_interpreter.value_alg
             counter graph ~get_alg (fst rule.Primitives.rate) with
         | None ->
           ExceptionDefn.warning ~pos:(snd rule.Primitives.rate)
             (fun f -> Format.fprintf f "Problematic rule rate replaced by 0")
         | Some rate -> Random_tree.add (2*i) rate activities)
    () env

let empty env stopping_times alg_overwrite =
  let activity_tree =
    Random_tree.create (2*Environment.nb_rules env) in
  let stops =
    List.sort (fun (a,_) (b,_) -> Nbr.compare a b) stopping_times in
  let overwrite = Array.make (Environment.nb_algs env) None in
  let () = List.iter (fun (i,v) -> overwrite.(i) <- Some v) alg_overwrite in
  {
    init_stopping_times = stops;
    stopping_times = stops;
    perturbations_alive =
      Array.make (Environment.nb_perturbations env) true;
    perturbations_not_done_yet =
      Array.make (Environment.nb_perturbations env) true;
    activities = activity_tree;
    variables_overwrite = overwrite;
    flux = [];
  }

let initialize ~bind ~return ~outputs env counter graph0 state0 init_l =
  let get_alg i = get_alg env state0 i in
  let mgraph =
    List.fold_left
      (fun mstate (alg,compiled_rule,pos) ->
         bind
           mstate
           (fun (state,state0) ->
              let value =
                Rule_interpreter.value_alg counter state ~get_alg alg in
              let actions,_,_ = snd compiled_rule.Primitives.instantiations in
              let creations_sort =
                List.fold_left
                  (fun l -> function
                     | Instantiation.Create (x,_) ->
                       Matching.Agent.get_type x :: l
                     | Instantiation.Mod_internal _ | Instantiation.Bind _
                     | Instantiation.Bind_to _ | Instantiation.Free _
                     | Instantiation.Remove _ -> l) [] actions in
              return (
                Nbr.iteri
                  (fun _ s ->
                     match Rule_interpreter.apply_rule
                             ~outputs ~get_alg env
                             (Environment.connected_components_of_unary_rules env)
                             counter s (Trace.INIT creations_sort)
                             compiled_rule with
                     | Rule_interpreter.Success s -> s
                     | (Rule_interpreter.Clash | Rule_interpreter.Corrected) ->
                       raise (ExceptionDefn.Internal_Error
                                ("Bugged initial rule",pos)))
                  state value,state0))) (return (graph0,state0)) init_l in
  bind
    mgraph
    (fun (graph,state0) ->
       let () =
         initial_activity get_alg env counter graph state0.activities in
       return (Rule_interpreter.update_outdated_activities
                 ~get_alg:(fun i -> Environment.get_alg env i)
                 (fun x _ y -> Random_tree.add x y state0.activities)
                 env counter graph,state0))

let observables_values env graph state counter =
  let get_alg i = get_alg env state i in
  Environment.map_observables
    (Rule_interpreter.value_alg counter graph ~get_alg)
    env

let do_modification ~outputs env counter graph state modification =
  let get_alg i = get_alg env state i in
  let print_expr_val =
    Kappa_printer.print_expr_val
      (Rule_interpreter.value_alg counter graph ~get_alg) in
  match modification with
  | Primitives.ITER_RULE ((v,_),r) ->
    let n = Rule_interpreter.value_alg counter graph ~get_alg v in
    (false,
     Nbr.iteri
       (fun _ g ->
          Rule_interpreter.force_rule
            ~outputs ~get_alg env
            (Environment.connected_components_of_unary_rules env)
            counter g (Trace.PERT "pert") r)
       graph n,state)
  | Primitives.UPDATE (i,(expr,_)) ->
    let () =
      state.variables_overwrite.(i) <-
        Some (Alg_expr.CONST (Rule_interpreter.value_alg
                                counter graph ~get_alg expr)) in
    (false, Rule_interpreter.extra_outdated_var i graph, state)
  | Primitives.STOP pexpr ->
    let () = if pexpr <> [] then
        let file = Format.asprintf "@[<h>%a@]" print_expr_val pexpr in
        outputs (Data.Snapshot
                   (Rule_interpreter.snapshot env counter file graph)) in
    (true,graph,state)
  | Primitives.PRINT (pe_file,pe_expr) ->
    let file_opt =
      match pe_file with
        [] -> None
      | _ -> Some (Format.asprintf "@[<h>%a@]" print_expr_val pe_file)
    in
    let line = Format.asprintf "%a" print_expr_val pe_expr in
    let () = outputs
        (Data.Print {Data.file_line_name = file_opt ; Data.file_line_text = line;}) in
    (false, graph, state)
  | Primitives.PLOTENTRY ->
    let () = outputs (Data.Plot (observables_values env graph state counter)) in
    (false, graph, state)
  | Primitives.SNAPSHOT pexpr  ->
    let file = Format.asprintf "@[<h>%a@]" print_expr_val pexpr in
    let () = outputs (Data.Snapshot
                        (Rule_interpreter.snapshot env counter file graph)) in
    (false, graph, state)
  | Primitives.CFLOW (name,cc,tests) ->
    let name = match name with
      | Some s -> s
      | None ->
        let domain = Environment.domain env in
        Format.asprintf
          "@[<h>%a@]"
          (Pp.array Pp.comma
             (fun _ -> Pattern.print ~domain ~with_id:false)) cc in
    (false,
     Rule_interpreter.add_tracked cc (Trace.OBS name) tests graph,
     state)
  | Primitives.CFLOWOFF cc ->
    (false, Rule_interpreter.remove_tracked cc graph, state)
  | Primitives.FLUX (rel,s) ->
    let file = Format.asprintf "@[<h>%a@]" print_expr_val s in
    let () =
      if List.exists
          (fun x -> Fluxmap.flux_has_name file x && x.Data.flux_normalized = rel)
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
    (false, graph, state)
  | Primitives.FLUXOFF s ->
    let file = Format.asprintf "@[<h>%a@]" print_expr_val s in
    let (these,others) =
      List.partition (Fluxmap.flux_has_name file) state.flux in
    let () = List.iter
        (fun x -> outputs (Data.Flux (Fluxmap.stop_flux env counter x)))
        these in
    let () = state.flux <- others in
    (false, graph, state)

let perturbate ~outputs env counter graph state =
  let get_alg i = get_alg env state i in
  let rec do_until_noop i graph state stop =
    if stop || i >= Environment.nb_perturbations env then
      let graph' =
        Rule_interpreter.update_outdated_activities
          ~get_alg (fun x _ y -> Random_tree.add x y state.activities)
          env counter graph in
      (stop,graph',state)
    else
      let pert = Environment.get_perturbation env i in
      if state.perturbations_alive.(i) &&
         state.perturbations_not_done_yet.(i) &&
         Rule_interpreter.value_bool
           counter graph ~get_alg (fst pert.Primitives.precondition)
      then
        let stop,graph,state =
          List.fold_left (fun (stop,graph,state as acc) effect ->
              if stop then acc else
                do_modification ~outputs env counter graph state effect)
            (stop,graph,state) pert.Primitives.effect in
        let () = state.perturbations_not_done_yet.(i) <- false in
        let () =
          state.perturbations_alive.(i) <-
            match pert.Primitives.abort with
            | None -> false
            | Some (ex,_) ->
              not (Rule_interpreter.value_bool counter graph ~get_alg ex) in
        do_until_noop 0 graph state stop
      else
        do_until_noop (succ i) graph state stop in
  do_until_noop 0 graph state false

let one_rule ~outputs dt stop env counter graph state =
  let choice,_ = Random_tree.random
      (Rule_interpreter.get_random_state graph) state.activities in
  let rule_id = choice/2 in
  let rule = Environment.get_rule env rule_id in
  let register_new_activity rd_id syntax_rd_id new_act =
    let () =
      match state.flux with
      | [] -> ()
      | l ->
        let old_act = Random_tree.find rd_id state.activities in
        List.iter
          (fun fl ->
             Fluxmap.incr_flux_flux
               rule.Primitives.syntactic_rule syntax_rd_id
               (
                 let cand =
                   if fl.Data.flux_normalized &&
                      (match classify_float old_act with
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
  let () =
    if !Parameter.debugModeOn then
      Format.printf
        "@[<v>@[Applied@ %t%i:@]@ @[%a@]@]@."
        (fun f -> if choice mod 2 = 1 then Format.fprintf f "unary@ ")
        rule_id (Kappa_printer.elementary_rule ~env) rule
        (*Rule_interpreter.print_dist env graph rule_id*) in
  let get_alg i = get_alg env state i in
  (* let () = *)
  (*   Format.eprintf "%a@." (Rule_interpreter.print_injections env) graph in *)
  let cause = Trace.RULE rule.Primitives.syntactic_rule in
  let apply_rule =
    if choice mod 2 = 1
    then Rule_interpreter.apply_unary_rule ~outputs ~rule_id
    else Rule_interpreter.apply_rule ~outputs ~rule_id in
  match apply_rule
          ~get_alg env
          (Environment.connected_components_of_unary_rules env)
          counter graph cause rule with
  | Rule_interpreter.Success (graph') ->
    let graph'' =
      Rule_interpreter.update_outdated_activities
        ~get_alg register_new_activity env counter graph' in
    let () =
      List.iter
        (fun fl -> Fluxmap.incr_flux_hit rule.Primitives.syntactic_rule fl)
        state.flux in
    let () =
      Array.iteri (fun i _ -> state.perturbations_not_done_yet.(i) <- true)
        state.perturbations_not_done_yet in
    let () =
      if !Parameter.debugModeOn then
        Format.printf "@[<v>Obtained@ %a@]@."
          (Rule_interpreter.print env) graph'' in
    (not (Counter.one_constructive_event counter dt)||stop,graph'',state)
  | (Rule_interpreter.Clash | Rule_interpreter.Corrected) as out ->
    let continue =
      if out = Rule_interpreter.Clash then
        Counter.one_clashing_instance_event counter dt
      else if choice mod 2 = 1
      then Counter.one_no_more_unary_event counter dt
      else Counter.one_no_more_binary_event counter dt in
    if Counter.consecutive_null_event counter <
       !Parameter.maxConsecutiveClash
    then (not continue||stop,graph,state)
    else
      (not continue||stop,
       (if choice mod 2 = 1
        then Rule_interpreter.adjust_unary_rule_instances
            ~rule_id ~get_alg register_new_activity env counter graph rule
        else Rule_interpreter.adjust_rule_instances
            ~rule_id ~get_alg register_new_activity env counter graph rule),
       state)

let activity state = Random_tree.total state.activities

let a_loop ~outputs env counter graph state =
  let (stop,graph',state') =
    perturbate ~outputs env counter graph state in
  let activity = activity state' in
  let rd = Random.State.float (Rule_interpreter.get_random_state graph') 1.0 in
  let dt = abs_float (log rd /. activity) in

  let (stop,graph'',state'' as out) =
    (*Activity is null or dt is infinite*)
    if not (activity > 0.) || dt = infinity then
      match state'.stopping_times with
      | [] ->
        let () =
          if !Parameter.dumpIfDeadlocked then
            outputs
              (Data.Snapshot
                 (Rule_interpreter.snapshot env counter "deadlock.ka" graph')) in
        let () =
          ExceptionDefn.warning
            (fun f ->
               Format.fprintf
                 f "A deadlock was reached after %d events and %Es (Activity = %.5f)"
                 (Counter.current_event counter)
                 (Counter.current_time counter) activity) in
        (true,graph',state')
      | (ti,_) :: tail ->
        let () = state'.stopping_times <- tail in
        let continue = Counter.one_time_correction_event counter ti in
        let stop,graph'',state'' =
          perturbate ~outputs env counter graph' state' in
        (not continue||stop,graph'',state'')
    else
      (*activity is positive*)
      match state.stopping_times with
      | (ti,_) :: tail
        when Nbr.is_smaller ti (Nbr.F (Counter.current_time counter +. dt)) ->
        let () = state'.stopping_times <- tail in
        let continue = Counter.one_time_correction_event counter ti in
        let stop,graph'',state'' =
          perturbate ~outputs env counter graph' state' in
        (not continue||stop,graph'',state'')
      | _ ->
        one_rule ~outputs dt stop env counter graph' state' in
  let () =
    Counter.fill ~outputs
      counter (observables_values env graph'' state'') in
  if stop then
    let () =
      Array.iteri (fun i _ -> state.perturbations_not_done_yet.(i) <- true)
        state.perturbations_not_done_yet in
    let (_,graph''',state''') =
      perturbate ~outputs env counter graph'' state'' in
    (true,graph''',state''')
  else out

let end_of_simulation ~outputs form env counter state =
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

let batch_loop ~outputs form env counter graph state =
  let rec iter graph state =
    let stop,graph',state' = a_loop ~outputs env counter graph state in
    if stop then (graph',state')
    else let () = Counter.tick form counter in iter graph' state'
  in iter graph state

let interactive_loop ~outputs form pause_criteria env counter graph state =
  let get_alg i = get_alg env state i in
  let user_interrupted = ref false in
  let old_sigint_behavior =
    Sys.signal
      Sys.sigint (Sys.Signal_handle
                    (fun _ -> if !user_interrupted then raise Sys.Break
                      else user_interrupted := true)) in
  let rec iter graph state =
    if !user_interrupted ||
       Rule_interpreter.value_bool counter graph ~get_alg pause_criteria then
      let () = Sys.set_signal Sys.sigint old_sigint_behavior in
      (false,graph,state)
    else
      let stop,graph',state' as out =
        a_loop ~outputs env counter graph state in
      if stop then
        let () = Sys.set_signal Sys.sigint old_sigint_behavior in
        out
      else let () = Counter.tick form counter in iter graph' state'
  in iter graph state
