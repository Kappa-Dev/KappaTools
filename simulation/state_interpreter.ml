type t = {
  stopping_times : (Nbr.t * int) list ref;
  perturbations_alive : bool array;
  activities : Random_tree.tree;(* pair numbers are binary rule, odd unary *)
  variables_overwrite: Alg_expr.t option array;
  flux: Data.flux_data list;
}

let initial_activity get_alg env counter graph activities =
  Environment.fold_rules
    (fun i () rule ->
     if Array.length rule.Primitives.connected_components = 0 then
       let rate = Rule_interpreter.value_alg
		    counter graph ~get_alg rule.Primitives.rate in
       Random_tree.add (2*i) (Nbr.to_float rate) activities)
    () env

let initial env counter graph stopping_times =
  let activity_tree =
    Random_tree.create (2*Environment.nb_rules env) in
  let () =
    initial_activity (Environment.get_alg env)
		     env counter graph activity_tree in
  let graph' =
    Rule_interpreter.update_outdated_activities
      ~get_alg:(fun i -> Environment.get_alg env i)
      (fun x _ y -> Random_tree.add x y activity_tree)
      env counter graph in
  let stops =
    ref (List.sort (fun (a,_) (b,_) -> Nbr.compare a b) stopping_times) in
  graph',{
    stopping_times = stops;
    perturbations_alive =
      Array.make (Environment.nb_perturbations env) true;
    activities = activity_tree;
    variables_overwrite =
      Array.make (Environment.nb_algs env) None;
    flux = [];
}

let get_alg env state i =
  match state.variables_overwrite.(i) with
  | None -> Environment.get_alg env i
  | Some expr -> expr

let observables_values env counter graph state =
  let get_alg i = get_alg env state i in
   Environment.map_observables
     (Rule_interpreter.value_alg counter graph ~get_alg)
     env

let snapshot env counter file graph =
  if Filename.check_suffix file ".dot" then
    Kappa_files.with_snapshot
      file (Counter.current_event counter) "dot"
      (fun f -> Format.fprintf f "%a@." (Rule_interpreter.print_dot env) graph)
  else
    Kappa_files.with_snapshot
      file (Counter.current_event counter) "ka"
      (fun f -> Format.fprintf f "%a@." (Rule_interpreter.print env) graph)


let do_it ~outputs env domain counter graph state modification =
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
		~get_alg env domain
		(Environment.connected_components_of_unary_rules env)
		counter g (Causal.PERT "pert") r)
	graph n,state)
  | Primitives.UPDATE (i,(expr,_)) ->
     let () =
       state.variables_overwrite.(i) <-
	 Some (Alg_expr.CONST (Rule_interpreter.value_alg
				 counter graph ~get_alg expr)) in
     (false, Rule_interpreter.extra_outdated_var i graph, state)
  | Primitives.STOP pexpr ->
     let file = Format.asprintf "@[<h>%a@]" print_expr_val pexpr in
     let () = snapshot env counter file graph in
     (true,graph,state)
  (*     raise (ExceptionDefn.StopReached
	      (Format.sprintf
		 "STOP instruction was satisfied at (%d e,%f t.u)"
		 (Mods.Counter.event counter) (Mods.Counter.time counter))) *)
  | Primitives.PRINT (pe_file,pe_expr) ->
     let file = Format.asprintf "@[<h>%a@]" print_expr_val pe_file in
     let line = Format.asprintf "%a" print_expr_val pe_expr in
     let () = outputs (Data.Print {Data.file_name = file; Data.line = line;}) in
     (false, graph, state)
  | Primitives.PLOTENTRY ->
     let () = outputs (Data.Plot (Counter.current_time counter,
				 observables_values env counter graph state)) in
     (false, graph, state)
  | Primitives.SNAPSHOT pexpr  ->
     let file = Format.asprintf "@[<h>%a@]" print_expr_val pexpr in
     let () = snapshot env counter file graph in
     (false, graph, state)
  | Primitives.CFLOW (name,cc,tests) ->
     let name = match name with
       | Some s -> s
       | None ->
	  let sigs = Environment.signatures env in
	  Format.asprintf
	    "@[<h>%a@]"
	    (Pp.array Pp.comma
		      (fun _ -> Connected_component.print ~sigs ?with_id:None))
	    cc in
     (false,
      Rule_interpreter.add_tracked cc (Causal.OBS name) tests graph,
      state)
  | Primitives.CFLOWOFF cc ->
     (false, Rule_interpreter.remove_tracked cc graph, state)
  | Primitives.FLUX s ->
     let file = Format.asprintf "@[<h>%a@]" print_expr_val s in
     let () =
       if List.exists (Fluxmap.flux_has_name file) state.flux
       then ExceptionDefn.warning
	      (fun f ->
	       Format.fprintf
		 f "At t=%f, e=%i: tracking FLUX into \"%s\" was already on"
		 (Counter.current_time counter) (Counter.current_event counter) file)
     in
     (false, graph, {state with
		      flux = Fluxmap.create_flux env counter file::state.flux})
  | Primitives.FLUXOFF s ->
     let file = Format.asprintf "@[<h>%a@]" print_expr_val s in
     let (these,others) = List.partition (Fluxmap.flux_has_name file) state.flux in
     let () = List.iter (fun x -> outputs (Data.Flux (Fluxmap.stop_flux env counter x))) these in
     (false, graph, {state with flux = others})

let perturbate ~outputs env domain counter graph state =
  let not_done_yet =
    Array.make (Environment.nb_perturbations env) true in
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
      if state.perturbations_alive.(i) && not_done_yet.(i) &&
	   Rule_interpreter.value_bool
	     counter graph ~get_alg pert.Primitives.precondition
      then
	let stop,graph,state =
	  List.fold_left (fun (stop,graph,state as acc) effect ->
			  if stop then acc else
			    do_it ~outputs env domain counter graph state effect)
			 (stop,graph,state) pert.Primitives.effect in
	let () = not_done_yet.(i) <- false in
	let () =
	  state.perturbations_alive.(i) <-
	    match pert.Primitives.abort with
	    | None -> false
	    | Some ex ->
	       not (Rule_interpreter.value_bool counter graph ~get_alg ex) in
	do_until_noop 0 graph state stop
      else
	do_until_noop (succ i) graph state stop in
  do_until_noop 0 graph state false

let one_rule dt stop env domain counter graph state =
  let choice,_ = Random_tree.random state.activities in
  let rule_id = choice/2 in
  let rule = Environment.get_rule env rule_id in
  let register_new_activity rd_id syntax_rd_id new_act =
    let () =
      if state.flux <> [] then
	let old_act = Random_tree.find rd_id state.activities in
	List.iter
	  (Fluxmap.incr_flux_flux
	     rule.Primitives.syntactic_rule syntax_rd_id (new_act -. old_act))
	  state.flux
    in Random_tree.add rd_id new_act state.activities in
  let () =
    if !Parameter.debugModeOn then
      begin
	Format.printf "@[<v>@[Applied@ %t%i:@]@ @[%a@]@]@."
		      (fun f -> if choice mod 2 = 1 then Format.fprintf f "unary@ ")
		      rule_id (Kappa_printer.elementary_rule ~env) rule;
	if !Parameter.store_unary_distance
	(*&&(choice mod 2 = 1)*) then
	  Rule_interpreter.print_dist env graph rule_id
      end in
  let get_alg i = get_alg env state i in
  (* let () = *)
  (*   Format.eprintf "%a@." (Rule_interpreter.print_injections env) graph in *)
  let cause = Causal.RULE rule.Primitives.syntactic_rule in
  let apply_rule ~rule_id =
    if choice mod 2 = 1
    then Rule_interpreter.apply_unary_rule ~rule_id
    else Rule_interpreter.apply_rule ~rule_id in
  match apply_rule
	  ~rule_id ~get_alg env domain
	  (Environment.connected_components_of_unary_rules env)
	  counter graph cause rule with
  | Rule_interpreter.Success graph' ->
     let graph'' =
       Rule_interpreter.update_outdated_activities
	 ~get_alg register_new_activity env counter graph' in
     let () =
       List.iter
	 (Fluxmap.incr_flux_hit rule.Primitives.syntactic_rule) state.flux in
     let () =
       if !Parameter.debugModeOn then
	 Format.printf "@[<v>Obtained@ %a@]@."
		       (Rule_interpreter.print env) graph'' in
     (not (Counter.one_constructive_event counter dt)||stop,graph'',state)
  | Rule_interpreter.Clash ->
     if Counter.consecutive_null_event counter <
	  !Parameter.maxConsecutiveClash
     then
       (not (Counter.one_clashing_instance_event counter dt)||stop,graph,state)
     else
       (not (Counter.one_clashing_instance_event counter dt)||stop,
	(if choice mod 2 = 1
	 then Rule_interpreter.adjust_unary_rule_instances
		~rule_id ~get_alg register_new_activity env counter graph rule
	 else Rule_interpreter.adjust_rule_instances
		~rule_id ~get_alg register_new_activity env counter graph rule),
	state)
  | Rule_interpreter.Corrected graph' ->
     let graph'' =
       Rule_interpreter.update_outdated_activities
	 ~get_alg register_new_activity env counter graph' in
     let continue =
       if choice mod 2 = 1
       then Counter.one_no_more_unary_event counter dt
       else Counter.one_no_more_binary_event counter dt in
     (not continue||stop,graph'',state)

let activity state =
  Random_tree.total state.activities

let a_loop ~outputs form env domain counter graph state =
  let activity = activity state in
  let rd = Random.float 1.0 in
  let dt = abs_float (log rd /. activity) in

(*Activity is null or dt is infinite*)
  if not (activity > 0.) || dt = infinity then
    match !(state.stopping_times) with
    | [] ->
       let () =
	 if !Parameter.dumpIfDeadlocked then
	   snapshot env counter "deadlock.ka" graph in
       let () =
	 Format.fprintf
	   form
	   "?@.A deadlock was reached after %d events and %Es (Activity = %.5f)"
	   (Counter.current_event counter)
	   (Counter.current_time counter) activity in
       (true,graph,state)
    | (ti,_) :: tail ->
       let () = state.stopping_times := tail in
       let continue = Counter.one_time_correction_event counter ti in
       let stop,graph',state' =
	 perturbate ~outputs env domain counter graph state in
       (not continue||stop,graph',state')
  else
(*activity is positive*)
    match !(state.stopping_times) with
    | (ti,_) :: tail
	 when Nbr.is_smaller ti (Nbr.F (Counter.current_time counter +. dt)) ->
       let () = state.stopping_times := tail in
       let continue = Counter.one_time_correction_event counter ti in
       let stop,graph',state' =
	 perturbate ~outputs env domain counter graph state in
       (not continue||stop,graph',state')
    | _ ->
       let (stop,graph',state') =
	 perturbate ~outputs env domain counter graph state in
       one_rule dt stop env domain counter graph' state'

let loop_cps ~outputs form hook return env domain counter graph state =
  let rec iter graph state =
    let stop,graph',state' =
      try
	let (stop,graph',state') as out =
	  a_loop ~outputs form env domain counter graph state in
	let () =
	  Counter.fill ~outputs
	    counter (observables_values env counter graph' state') in
	let () = if stop then
		   ignore (perturbate ~outputs env domain counter graph' state') in
	out
      with ExceptionDefn.UserInterrupted f ->
	let () = Format.pp_print_newline form () in
	let msg = f (Counter.current_time counter) (Counter.current_event counter) in
	let () =
	  Format.fprintf
	    form
	    "@.***%s: would you like to record the current state? (y/N)***@."
	    msg in
	let () = if not !Parameter.batchmode then
		   match String.lowercase (Tools.read_input ()) with
		   | ("y" | "yes") -> snapshot env counter "dump.ka" graph
		   | _ -> () in
	(true,graph,state) in
    if stop then return graph' state'
    else hook (fun () -> iter graph' state')
  in iter graph state

let finalize ~outputs form env counter graph state =
  let () = Outputs.close () in
  let () = Counter.complete_progress_bar form counter in
  let () = if !Parameter.store_unary_distance then
	     Kappa_files.with_unary_dist (Counter.current_event counter) (Rule_interpreter.print_all_dist graph) in
  let () =
    List.iter
      (fun e ->
       let () =
	 ExceptionDefn.warning
	   (fun f ->
	    Format.fprintf
	      f "Tracking FLUX into \"%s\" was not stopped before end of simulation"
	      (Fluxmap.get_flux_name e)) in
       outputs (Data.Flux (Fluxmap.stop_flux env counter e))) state.flux in
  let () = ExceptionDefn.flush_warning form in
  Rule_interpreter.generate_stories form env graph

let go form counter f =
  let () = Counter.tick form counter in
  f ()

let loop ~outputs form env domain counter graph state =
  loop_cps ~outputs form (go form counter) (finalize ~outputs form env counter) env domain counter graph state
