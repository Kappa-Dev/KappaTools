open Dynamics
open State
open Tools
open ExceptionDefn
open Graph
open Mods
open LargeArray

let eval_abort_pert just_applied pert state counter env =
  match pert.Primitives.abort with
  | None -> just_applied
  | Some var -> State.value_bool state counter env var

let raw_pr_pexpr state counter env f pexpr =
  let rec aux f = function
    | Ast.Str_pexpr str,_ -> Format.pp_print_string f str
    | Ast.Alg_pexpr alg,_ ->
       Nbr.print f (State.value_alg state counter env alg)
  in Pp.list (fun f -> Format.pp_print_cut f ()) aux f pexpr
let pr_pexpr state counter env f pexpr =
  Format.fprintf f "%a@." (raw_pr_pexpr state counter env) pexpr
let eval_pexpr pexpr state counter env =
  Format.asprintf "@[<h>%a@]" (raw_pr_pexpr state counter env) pexpr

let apply_n_time x r state env counter pert_ids pert_events tracked =
  Nbr.iteri
    (fun n (env,state,pert_ids,with_tracked,pert_events as pack) ->
     try
       (*FIXME: highly unefficient to compute new injection at each loop*)
       let embedding_t =
	 try State.select_injection (infinity,None) (0.,None)
				    state r.Primitives.lhs counter env
	 with Null_event _ ->
	      let mix_id = Mixture.get_id r.Primitives.lhs in
	      if !Parameter.debugModeOn then
		Debug.tag "Clashing instance detected: building matrix";
	      match State.instances_of_square mix_id (-1) state env with
	      (*JK: un peu bete de generer la matrice pour ne prendre que la premiere injection*)
	      | (embedding,_,_)::_ -> Embedding.DISJOINT
					{Embedding.map=embedding;
					 Embedding.roots = IntSet.empty ;
					 Embedding.components = None ;
					 Embedding.depth_map = None}
	      | [] -> raise Not_found
       in (*empty embedding, cannot raise null-event*)
       let (env, state, side_effects, embedding_t, psi, pert_ids_neg) =
	 State.apply state r embedding_t counter env in
       let phi = State.Embedding.map_of embedding_t in
       let env,state,pert_ids_pos,new_injs,tracked' =
	 State.positive_update ~with_tracked state r phi psi side_effects Int2Set.empty counter env
       in
       let pert_ids =
	 if Nbr.is_equal n x then (*only the first time*)
	   IntSet.union pert_ids (IntSet.union pert_ids_neg pert_ids_pos)
	 else pert_ids in
       (env,state,pert_ids,tracked',(r,phi,psi,side_effects)::pert_events)
     with Not_found ->
       let () =
	 Debug.tag_if_debug "No more non clashing instances were found!"
       in pack)
    (env,state,pert_ids,tracked,pert_events) x

let trigger_effect state env pert_ids tracked pert_events pert p_id eff snapshot counter =
  match eff with
  | Primitives.ITER_RULE ((v,_),r) ->
     let x = State.value_alg state counter env v in
    if x = Nbr.F infinity then
      invalid_arg
	(Format.asprintf
	   "Perturbation %a would be applied infinitely, aborting..."
	   (Kappa_printer.perturbation env) pert)
    else
      let () =
	Debug.tag_if_debug "Applying %a instances of %a"
			   Nbr.print x (Kappa_printer.modification env) eff
      in apply_n_time x r state env counter pert_ids pert_events tracked
  | Primitives.UPDATE (g_id,(v,_)) ->
     let () = Debug.tag_if_debug "Updating %a" Term.print_dep_type g_id in
     State.update_dep_value state counter env v g_id;
     let env,pert_ids =
       State.update_dep state ~cause:p_id g_id pert_ids counter env in
     (env,state ,pert_ids,tracked,pert_events)
  | Primitives.SNAPSHOT pexpr ->
      let str = eval_pexpr pexpr state counter env in
      snapshot str;
      (env, state ,pert_ids,tracked,pert_events)
  | Primitives.PRINT (pexpr_file,pexpr) ->
    let str = eval_pexpr pexpr_file state counter env in
    let desc =
      match str with "" -> Format.std_formatter
		   | _ -> Environment.get_desc str env
    in
    pr_pexpr state counter env desc pexpr;
    (env,state,pert_ids,tracked,pert_events)
  | Primitives.CFLOW id ->
    Debug.tag_if_debug "Tracking causality" ;
    Parameter.causalModeOn := true;
    let env =
      if Environment.is_tracked id env then env
      else Environment.inc_active_cflows env in
    let env = Environment.track id env in
    (env, state, pert_ids,tracked,pert_events)
  | Primitives.CFLOWOFF id ->
    begin
      let env = Environment.dec_active_cflows env in
      let env = Environment.untrack id env in
      if Environment.active_cflows env = 0 then Parameter.causalModeOn := false;
      (env,state,pert_ids,tracked,pert_events)
    end
  | Primitives.FLUXOFF pexpr ->
    begin
      let str = eval_pexpr pexpr state counter env in
      let desc = Tools.kasim_open_out
		   (match str with "" -> !Parameter.fluxFileName
				 | _ -> str) in
      Parameter.add_out_desc desc ;
      State.dot_of_flux (Format.formatter_of_out_channel desc) state env ;
      close_out desc ;
      Parameter.openOutDescriptors := List.tl (!Parameter.openOutDescriptors) ;
      Parameter.fluxModeOn := false ;
      (env,state,pert_ids,tracked,pert_events)
    end
  | Primitives.STOP pexpr ->
     Debug.tag_if_debug "Interrupting simulation now!" ;
     let str = eval_pexpr pexpr state counter env in
     snapshot str ;
     raise (ExceptionDefn.StopReached
	      (Printf.sprintf "STOP instruction was satisfied at (%d e,%f t.u)"
			      (Counter.event counter) (Counter.time counter)))
  | Primitives.FLUX pexpr ->
    begin
      if !Parameter.fluxModeOn
      then ExceptionDefn.warning
	     (fun f -> Format.fprintf f "Flux modes are overlapping");
      Parameter.fluxModeOn := true ;
      let nme = eval_pexpr pexpr state counter env in
      let _ =
	match nme with
	| "" -> Parameter.fluxFileName := "flux"^"_"^(string_of_int (Counter.event counter))
	| _ -> Parameter.fluxFileName := nme
      in
      Parameter.set Parameter.fluxFileName (Some "dot");
      (env, state, pert_ids,tracked,pert_events)
    end

let apply_effect p_id pert tracked pert_events state counter env =
  let snapshot str =
    Debug.tag_if_debug "Taking a snapshot of current state (%s)" str;
    let ext = if !Parameter.dotOutput then "dot" else "ka" in
    let str = if str="" then !Parameter.snapshotFileName else str in
    let desc =
      Tools.open_out_fresh_filename
	str [string_of_int (Counter.event counter)] ext in
    let hr = !Parameter.snapshotHighres in
    Parameter.openOutDescriptors := desc::(!Parameter.openOutDescriptors) ;
    State.snapshot state counter desc hr env;
    (*could use a dedicated thread here*)
    close_out desc ;
    Parameter.openOutDescriptors := List.tl (!Parameter.openOutDescriptors)
  in
  List.fold_left
    (fun (env, state, pert_ids,tracked,pert_events) effect ->
     try
       trigger_effect state env pert_ids tracked pert_events pert p_id effect
		      snapshot counter
     with ExceptionDefn.StopReached msg ->
       counter.Counter.stop <- true;
       Debug.tag msg;
       (env, state, pert_ids,tracked,pert_events)
    )
    (env,state,IntSet.empty,tracked,pert_events) pert.Primitives.effect

let has_reached_a_stopping_time state counter env =
  let depset = Environment.get_dependencies Term.TIME env in
  Term.DepSet.fold
    (fun dep st_time ->
     match dep with
     | Term.TIME | Term.EVENT | Term.KAPPA _ | Term.RULE _
     | Term.ALG _ | Term.TOK _ -> st_time
     | Term.PERT pert_id
     | Term.ABORT pert_id ->
	match State.maybe_find_perturbation pert_id state with
	| None -> st_time
	| Some pert ->
	   match st_time,pert.Primitives.stopping_time with
	   | Some a, Some b
		when Nbr.is_smaller b (Nbr.F (Mods.Counter.time counter))
	     -> Some (Nbr.min a b)
	   | None, Some b
		when Nbr.is_smaller b (Nbr.F (Mods.Counter.time counter))
	     -> Some b
	   | e, (Some _ | None) -> e
    ) depset None

let try_perturbate tracked state pert_ids pert_events counter env =
  let rec iter state pert_ids triggered_perts tracked pert_events env =
    let state,env,pert_ids',triggered_perts,tracked,pert_events =
      let () = Debug.tag_if_debug
		 "Should now try perturbations %a"
		 (Pp.set IntSet.elements Pp.colon Format.pp_print_int)
		 pert_ids in
      IntSet.fold
	(fun pert_id
	     (state,env,pert_ids,triggered_perts,tracked,pert_events as acc) ->
	 match State.maybe_find_perturbation pert_id state with
	 | None -> acc
	 | Some pert ->
	    if State.value_bool state counter env pert.Primitives.precondition then
	      begin
		Debug.tag_if_debug
		  "\n*************Applying perturbation %d***************" pert_id;
		let env,state,new_pert_ids,tracked,pert_events =
		  apply_effect pert_id pert tracked pert_events state counter env in
		Debug.tag_if_debug "************End perturbation*************" ;
		let state,triggered_perts =
		  if eval_abort_pert true pert state counter env then
		    let () =
		      Debug.tag_if_debug "***Aborting pert[%d]***" pert_id in
		    (State.remove_perturbation pert_id state,triggered_perts)
		  else
		    let () =
		      Debug.tag_if_debug "************Maintaining perturbation*************"
		    in (state,IntSet.add pert_id triggered_perts)
		in
		(state,env,IntSet.union new_pert_ids pert_ids,
		 triggered_perts,tracked,pert_events)
	      end
	    else
	      if eval_abort_pert false pert state counter env then
		(Debug.tag_if_debug "***Aborting pert[%d]***" pert_id;
		 (State.remove_perturbation pert_id state,env,
		  IntSet.remove pert_id pert_ids,triggered_perts,tracked,pert_events))
	      else acc
	)
	pert_ids (state,env,IntSet.empty,triggered_perts,tracked,pert_events)
    in
    if IntSet.is_empty pert_ids' then
      (state,triggered_perts,env,tracked,pert_events)
    else
      (*Chance of looping perturbation if user was not careful*)
      iter state pert_ids' triggered_perts tracked pert_events env
  in
  iter state pert_ids IntSet.empty tracked pert_events env
