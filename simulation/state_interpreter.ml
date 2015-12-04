type t = {
  stopping_times : (Nbr.t * int) list ref;
  perturbations_alive : bool array;
  activities : Random_tree.tree;(* pair numbers are binary rule, odd unary *)
  variables_overwrite: Alg_expr.t option array;
  flux: (string * float array array) list;
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
  if !Parameter.dotSnapshots then
    Kappa_files.with_snapshot
      file (Mods.Counter.event counter) "dot"
      (fun f -> Format.fprintf f "%a@." (Rule_interpreter.print_dot env) graph)
  else
    Kappa_files.with_snapshot
      file (Mods.Counter.event counter) "ka"
      (fun f -> Format.fprintf f "%a@." (Rule_interpreter.print env) graph)


let do_it env domain counter graph state modification =
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
	 fst (Rule_interpreter.force_rule
		~get_alg env domain
		(Environment.connected_components_of_unary_rules env)
		counter g (Causal.PERT "pert") r))
	graph n,state)
  | Primitives.UPDATE (va,(expr,_)) ->
     begin
       match va with
       | Operator.ALG i ->
	  let () =
	    state.variables_overwrite.(i) <-
	      Some (Alg_expr.CONST (Rule_interpreter.value_alg
				      counter graph ~get_alg expr)) in
	  (false, Rule_interpreter.extra_outdated_var i graph, state)
       | (Operator.RULE _ | Operator.PERT _) ->
	  failwith "Problematic update perturbation"
     end
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
     let desc =
       match file with "" -> Format.std_formatter
		     | _ -> Environment.get_desc file env in
     let () = Format.fprintf desc "%a@." print_expr_val pe_expr in
     (false, graph, state)
  | Primitives.PLOTENTRY ->
     let () = Plot.plot_now env counter.Mods.Counter.time
			    (observables_values env counter graph state) in
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
	    (Pp.array Pp.comma (fun _ -> Connected_component.print ~sigs false))
	    cc in
     (false,
      Rule_interpreter.add_tracked cc (Causal.OBS name) tests graph,
      state)
  | Primitives.CFLOWOFF cc ->
     (false, Rule_interpreter.remove_tracked cc graph, state)
  | Primitives.FLUX s ->
     let file = Format.asprintf "@[<h>%a@]" print_expr_val s in
     let size = Environment.nb_syntactic_rules env + 1 in
     let () =
       if List.exists (fun (x,_) -> x = file) state.flux
       then ExceptionDefn.warning
	      (fun f ->
	       Format.fprintf
		 f "At t=%f, e=%i: tracking FLUX into \"%s\" was already on"
		 (Mods.Counter.time counter) (Mods.Counter.event counter) file)
     in
     let el = file,Array.make_matrix size size 0. in
     (false, graph, {state with flux = el::state.flux})
  | Primitives.FLUXOFF s ->
     let file = Format.asprintf "@[<h>%a@]" print_expr_val s in
     let (these,others) = List.partition (fun (x,_) -> x = file) state.flux in
     let () = List.iter (Outputs.output_flux env) these in
     (false, graph, {state with flux = others})

let perturbate env domain counter graph state =
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
			    do_it env domain counter graph state effect)
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

let one_rule _form dt stop env domain counter graph state =
  let choice,_ = Random_tree.random state.activities in
  let rule_id = choice/2 in
  let rule = Environment.get_rule env rule_id in
  let register_new_activity rd_id syntax_rd_id new_act =
    let () =
      if state.flux <> [] then
	let old_act = Random_tree.find rd_id state.activities in
	List.iter (fun (_,flux) ->
		   flux.(rule.Primitives.syntactic_rule).(syntax_rd_id) <-
		     flux.(rule.Primitives.syntactic_rule).(syntax_rd_id) +.
		       (new_act -. old_act)) state.flux
    in Random_tree.add rd_id new_act state.activities in
  let () =
    if !Parameter.debugModeOn then
      Format.printf "@[<v>@[Applied@ %t%i:@]@ @[%a@]@]@."
		    (fun f -> if choice mod 2 = 1 then Format.fprintf f "unary@ ")
		    rule_id (Kappa_printer.elementary_rule ~env) rule in
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
       if !Parameter.debugModeOn then
	 Format.printf "@[<v>Obtained@ %a@]@."
		       (Rule_interpreter.print env) graph'' in
     (not (Mods.Counter.one_constructive_event counter dt)||stop,graph'',state)
  | Rule_interpreter.Clash ->
     if Mods.Counter.consecutive_null_event counter <
	  !Parameter.maxConsecutiveClash
     then
       (not (Mods.Counter.one_clashing_instance_event counter dt)||stop,graph,state)
     else
       (*let graph' =
	   match Rule_interpreter.force_rule
		   ~get_alg domain counter graph cause rule with
	   | graph',Some [] ->
	      let () = Random_tree.add (2*rule_id) 0.0 state.activities in graph'
	   | graph',(None | Some (_::_)) -> graph' in
	 let graph'' =
	   Rule_interpreter.update_outdated_activities
	     ~get_alg register_new_activity  env counter graph' in
	 let () =
	   if !Parameter.debugModeOn then
	     Format.printf "@[<v>Obtained after forcing rule@ %a@]@."
			   (Rule_interpreter.print env) graph' in
	 Some (graph'',state)*)
       (not (Mods.Counter.one_clashing_instance_event counter dt)||stop,graph,state)
  | Rule_interpreter.Corrected graph' ->
     let graph'' =
       Rule_interpreter.update_outdated_activities
	 ~get_alg register_new_activity env counter graph' in
     let continue =
       if choice mod 2 = 1
       then Mods.Counter.one_no_more_unary_event counter dt
       else Mods.Counter.one_no_more_binary_event counter dt in
     (not continue||stop,graph'',state)

let activity state =
  Random_tree.total state.activities

let a_loop form env domain counter graph state =
  let activity = activity state in
  let rd = Random.float 1.0 in
  let dt = abs_float (log rd /. activity) in

(*Activity is null or dt is infinite*)
  if not (activity > 0.) || dt = infinity then
    match !(state.stopping_times) with
    | [] ->
       let () =
	 if !Parameter.dumpIfDeadlocked then
	   snapshot env counter "deadlock" graph in
       let () =
	 Format.fprintf
	   form
	   "?@.A deadlock was reached after %d events and %Es (Activity = %.5f)"
	   (Mods.Counter.event counter) (Mods.Counter.time counter) activity in
       (true,graph,state)
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
       let stop,graph',state' = perturbate env domain counter graph state in
       (not (Mods.Counter.one_time_correction_event counter 0.)||stop,graph',state')
    | _ ->
       let (stop,graph',state') =
	 perturbate env domain counter graph state in
       one_rule form dt stop env domain counter graph' state'

let loop_cps form hook return env domain counter graph state =
  let () =
    Mods.Counter.tick
      form counter counter.Mods.Counter.time counter.Mods.Counter.events in
  let rec iter graph state =
    let stop,graph',state' =
      try
	let (stop,graph',state') as out =
	  a_loop form env domain counter graph state in
	let () =
	  Plot.fill
	    form counter env (observables_values env counter graph' state') in
	let () = if stop then
		   ignore (perturbate env domain counter graph' state') in
	out
      with ExceptionDefn.UserInterrupted f ->
	let () = Format.pp_print_newline form () in
	let msg = f (Mods.Counter.time counter) (Mods.Counter.event counter) in
	let () =
	  Format.fprintf
	    form
	    "@.***%s: would you like to record the current state? (y/N)***@."
	    msg in
	let () = if not !Parameter.batchmode then
		   match String.lowercase (Tools.read_input ()) with
		   | ("y" | "yes") -> snapshot env counter "dump" graph
		   | _ -> () in
	(true,graph,state) in
    if stop then return form env counter graph' state'
    else hook (fun () -> iter graph' state')
  in iter graph state

let finalize form env counter graph state =
  let () = Plot.close form counter in
  let () =
    List.iter
      (fun (file,_ as e) ->
       let () =
	 ExceptionDefn.warning
	   (fun f ->
	    Format.fprintf
	      f "Tracking FLUX into \"%s\" was not stopped before end of simulation"
	      file) in
       Outputs.output_flux env e) state.flux in
  let () = ExceptionDefn.flush_warning form in
  Rule_interpreter.generate_stories form env graph

let go f = f ()

let loop form env domain counter graph state =
  loop_cps form go finalize env domain counter graph state
