type t = {
  stopping_times : (Nbr.t * int) list ref;
  perturbations_alive : bool array;
  activities : Random_tree.tree;
  variables_overwrite: Alg_expr.t option array;
}

let initial_activity get_alg env counter graph activities =
  Environment.iteri_rules
    (fun i rule ->
     let rate = Rule_interpreter.value_alg
		  counter graph ~get_alg rule.Primitives.rate in
     let cc_va =
       Rule_interpreter.value_alg
	 counter graph ~get_alg
	 (Alg_expr.KAPPA_INSTANCE [rule.Primitives.connected_components]) in
     let act =
       if Nbr.is_zero cc_va then Nbr.zero else Nbr.mult rate cc_va in
     Random_tree.add i (Nbr.to_float act) activities)
    env

let initial env counter graph stopping_times =
  let activity_tree =
    Random_tree.create (Environment.nb_rules env) in
  let () =
    initial_activity (Environment.get_alg env)
		    env counter graph activity_tree in
  let stops =
    ref (List.sort (fun (a,_) (b,_) -> Nbr.compare a b) stopping_times) in
  {
    stopping_times = stops;
    perturbations_alive =
      Array.make (Environment.nb_perturbations env) true;
    activities = activity_tree;
    variables_overwrite =
      Array.make (Environment.nb_algs env) None;
}

let get_alg env state i =
  match state.variables_overwrite.(i) with
  | None -> Environment.get_alg env i
  | Some expr -> expr

let observables_values env counter graph state =
  let get_alg i = get_alg env state i in
  (counter.Mods.Counter.time,
   Environment.map_observables
     (Rule_interpreter.value_alg counter graph ~get_alg)
     env)

let do_it env domain counter graph state p_id = function
  | Primitives.ITER_RULE ((v,_),r) ->
     let get_alg i = get_alg env state i in
     let n = Rule_interpreter.value_alg counter graph ~get_alg v in
     (false,
      Nbr.iteri
	(fun _ g ->
	 fst (Rule_interpreter.force_rule
		~get_alg domain counter g (Causal.PERT p_id) r))
	graph n,state)
  | Primitives.UPDATE (va,(expr,_)) ->
     let () =
       match va with
       | Term.ALG i -> state.variables_overwrite.(i) <- Some expr
       | (Term.RULE _ | Term.PERT _) ->
	  failwith "Problematic update perturbation" in
     (false, graph, state)
  | Primitives.STOP pexpr ->
     let get_alg i = get_alg env state i in
     let file =
       Format.asprintf
	 "@[<h>%a@]" (Kappa_printer.print_expr_val
			~env (fun ?env ->
			      Rule_interpreter.value_alg counter graph ~get_alg))
	 pexpr in
     let () =
       Kappa_files.with_snapshot
	 file (Mods.Counter.event counter) "ka"
	 (fun f -> Rule_interpreter.print env f graph) in
     (true,graph,state)
  (*     raise (ExceptionDefn.StopReached
	      (Format.sprintf
		 "STOP instruction was satisfied at (%d e,%f t.u)"
		 (Mods.Counter.event counter) (Mods.Counter.time counter))) *)
  | Primitives.PRINT (pe_file,pe_expr) ->
     let get_alg i = get_alg env state i in
     let file =
       Format.asprintf
	 "@[<h>%a@]" (Kappa_printer.print_expr_val
			~env (fun ?env ->
			      Rule_interpreter.value_alg counter graph ~get_alg))
	 pe_file in
     let desc =
       match file with "" -> Format.std_formatter
		     | _ -> Environment.get_desc file env in
     let () =
       Format.fprintf
	 desc "%a@." (Kappa_printer.print_expr_val
			~env (fun ?env ->
			      Rule_interpreter.value_alg counter graph ~get_alg))
	 pe_expr in
     (false, graph, state)
  | Primitives.PLOTENTRY ->
     let () = Plot.plot_now env (observables_values env counter graph state) in
     (false, graph, state)
  | Primitives.SNAPSHOT pexpr  ->
     let get_alg i = get_alg env state i in
     let file =
       Format.asprintf
	 "@[<h>%a@]" (Kappa_printer.print_expr_val
			~env (fun ?env ->
			      Rule_interpreter.value_alg counter graph ~get_alg))
	 pexpr in
     let () =
       Kappa_files.with_snapshot
	 file (Mods.Counter.event counter) "ka"
	 (fun f -> Rule_interpreter.print env f graph) in
     (false, graph, state)
  | Primitives.CFLOW (cc,tests) ->
     (false, Rule_interpreter.add_tracked cc tests graph, state)
  | Primitives.CFLOWOFF cc ->
     (false, Rule_interpreter.remove_tracked cc graph, state)
  | Primitives.FLUX _ -> (false, graph, state)
  | Primitives.FLUXOFF _ -> (false, graph, state)

let perturbate env domain counter graph state =
  let not_done_yet =
    Array.make (Environment.nb_perturbations env) true in
  let get_alg i = get_alg env state i in
  let rec do_until_noop i graph state stop =
    if stop || i >= Environment.nb_perturbations env then
      let graph' =
	Rule_interpreter.update_outdated_activities
	  ~get_alg Random_tree.add env counter graph state.activities in
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
			    do_it env domain counter graph state i effect)
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

let one_rule env domain counter graph state =
  let rule_id,_ = Random_tree.random state.activities in
  let rule = Environment.get_rule env rule_id in
  let () =
    if !Parameter.debugModeOn then
      Format.printf "@[<v>Applied@ @[%a@]@]@."
		    (Kappa_printer.elementary_rule ~env) rule in
  let get_alg i = get_alg env state i in
  (* let () = *)
  (*   Format.eprintf "%a@." (Rule_interpreter.print_injections env) graph in *)
  match Rule_interpreter.apply_rule
	  ~get_alg domain counter graph (Causal.RULE rule_id) rule with
  | None -> None
  | Some graph' ->
     let graph'' =
       Rule_interpreter.update_outdated_activities
	 ~get_alg Random_tree.add env counter graph' state.activities in
     let () =
       if !Parameter.debugModeOn then
	 Format.printf "@[<v>Obtained@ %a@]@."
		       (Rule_interpreter.print env) graph' in
     Some (graph'',state)

let activity state =
  Random_tree.total state.activities

let a_loop form env domain counter graph state =
  let activity = activity state in
  let rd = Random.float 1.0 in
  let dt = abs_float (log rd /. activity) in

(*Activity is null or dt is infinite*)
  if not (activity > 0.) || dt = infinity then
    match !(state.stopping_times) with
    | [] -> (true,graph,state)
    | (ti,_) :: tail ->
       let () = state.stopping_times := tail in
       let () = counter.Mods.Counter.time <- Nbr.to_float ti in
       perturbate env domain counter graph state
  else
(*activity is positive*)
    match !(state.stopping_times) with
    | (ti,_) :: tail
	 when Nbr.is_smaller ti (Nbr.F (Mods.Counter.time counter +. dt)) ->
       let () = state.stopping_times := tail in
       let () = counter.Mods.Counter.time <- Nbr.to_float ti in
       perturbate env domain counter graph state
    | _ ->
       let (stop,graph',state') =
	 perturbate env domain counter graph state in
       match one_rule env domain counter graph' state' with
       | None ->
	  (not (Mods.Counter.one_null_event counter dt)||stop,graph',state')
       | Some (graph'',state'') ->
	  let () =
	    Plot.fill form counter env dt
		      (observables_values env counter graph state) in
	  (not (Mods.Counter.one_constructive_event counter dt)||stop,graph'',state'')

let loop_cps form hook return env domain counter graph state =
  let () =
    Mods.Counter.tick
      form counter counter.Mods.Counter.time counter.Mods.Counter.events in
  let rec iter graph state =
    let stop,graph',state' = a_loop form env domain counter graph state in
    if stop then
      let () =
	Plot.fill form counter env 0.0
		  (observables_values env counter graph' state') in
      let (_,_,_) = perturbate env domain counter graph' state' in
      return form env counter graph
    else
      hook (fun () -> iter graph' state')
  in iter graph state

let finalize form env counter graph =
  let () = Rule_interpreter.generate_stories form env graph in
  Plot.close form counter

let go f = f ()

let loop form env domain counter graph state =
  loop_cps form go finalize env domain counter graph state
