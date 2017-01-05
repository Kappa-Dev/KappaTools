(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t =
  {
    mutable outdated : bool;

    (* With rectangular approximation *)
    roots_of_patterns: Mods.IntSet.t Pattern.ObsMap.t;
    roots_of_unary_patterns:
      Mods.IntSet.t Mods.IntMap.t Pattern.ObsMap.t;

    (* Without rectangular approximation *)
    matchings_of_rule:
      (Matching.t * int list) list Mods.IntMap.t;
    unary_candidates:
      (Matching.t * Edges.path option) list Mods.IntMap.t;

    variables_cache: Nbr.t array;
    variables_overwrite: Alg_expr.t option array;

    edges: Edges.t;
    tokens: Nbr.t array;
    outdated_elements: Operator.DepSet.t * bool;

    random_state : Random.State.t;
    story_machinery :
       (Trace.event_kind * Pattern.id array *
        Instantiation.abstract Instantiation.test list)
         list Pattern.ObsMap.t (*currently tracked ccs *) option;
    store_distances: bool;
  }

type result = Clash | Corrected | Success of t

let raw_get_alg env overwr i =
  match overwr.(i) with
  | None -> Environment.get_alg env i
  | Some expr -> expr

let raw_instance_number state patterns_l =
  let size pattern =
    Mods.IntSet.size (Pattern.ObsMap.get state.roots_of_patterns pattern) in
  let rect_approx patterns =
    Array.fold_left (fun acc pattern ->  acc * (size pattern)) 1 patterns in
  List.fold_left (fun acc patterns -> acc + (rect_approx patterns)) 0 patterns_l
let instance_number state patterns_l =
  Nbr.I (raw_instance_number state patterns_l)

let value_bool counter state expr =
  let () = assert (not state.outdated) in
  Expr_interpreter.value_bool
    counter ~get_alg:(fun i -> Alg_expr.CONST state.variables_cache.(i))
    ~get_mix:(fun patterns -> instance_number state patterns)
    ~get_tok:(fun i -> state.tokens.(i))
    expr
let value_alg counter state alg =
  let () = assert (not state.outdated) in
  Expr_interpreter.value_alg
    counter ~get_alg:(fun i -> Alg_expr.CONST state.variables_cache.(i))
    ~get_mix:(fun patterns -> instance_number state patterns)
    ~get_tok:(fun i -> state.tokens.(i))
    alg

let recompute env counter state i =
  state.variables_cache.(i) <-
    value_alg counter state (raw_get_alg env state.variables_overwrite i)

let empty ~with_trace ~store_distances random_state env counter alg_overwrite =
  let with_connected_components =
    not (Pattern.Set.is_empty
           (Environment.connected_components_of_unary_rules env)) in
  let variables_overwrite = Array.make (Environment.nb_algs env) None in
  let () =
    List.iter (fun (i,v) -> variables_overwrite.(i) <- Some v) alg_overwrite in
  let variables_cache = Array.make (Environment.nb_algs env) Nbr.zero in
  let cand =
    {
      outdated = false;
      roots_of_patterns = Pattern.Env.new_obs_map
          (Environment.domain env) (fun _ -> Mods.IntSet.empty);
      roots_of_unary_patterns = Pattern.Env.new_obs_map
          (Environment.domain env) (fun _ -> Mods.IntMap.empty);
      matchings_of_rule = Mods.IntMap.empty;
      unary_candidates = Mods.IntMap.empty;
      variables_overwrite; variables_cache;
      edges = Edges.empty ~with_connected_components;
      tokens = Array.make (Environment.nb_tokens env) Nbr.zero;
      outdated_elements = Operator.DepSet.empty,false;
      random_state;
      story_machinery =
        if with_trace then
          Some (Pattern.Env.new_obs_map
                  (Environment.domain env) (fun _ -> []))
        else None;
      store_distances;
    } in
  let () = Tools.iteri (recompute env counter cand) (Environment.nb_algs env) in
  cand

let print_injections ?domain f roots_of_patterns =
  Format.fprintf
    f "@[<v>%a@]"
    (Pattern.ObsMap.print Pp.space
       (fun pattern f roots ->
          Format.fprintf
            f "@[# @[%a@] ==>@ @[%a@]@]"
            (Pattern.print ?domain ~with_id:true) pattern
            Mods.IntSet.print roots
       )
    ) roots_of_patterns
let print_unary_injections ?domain f roots_of_patterns =
  Format.fprintf
    f "@[<v>%a@]"
    (Pattern.ObsMap.print Pp.space
       (fun pattern f root_maps ->
          Format.fprintf
            f "@[# @[%a@] ==>@ @[%a@]@]"
            (Pattern.print ?domain ~with_id:true) pattern
            (Pp.set Mods.IntMap.bindings Pp.space
               (fun f (_cc_id, roots) -> Mods.IntSet.print f roots))
            root_maps
       )
    ) roots_of_patterns

let add_intset_in_intmap id set map =
  if Mods.IntSet.is_empty set
  then Mods.IntMap.remove id map
  else Mods.IntMap.add id set map

let update_roots is_add unary_ccs edges map unary_map pattern root =
  let va = Pattern.ObsMap.get map pattern in
  let () = Pattern.ObsMap.set map pattern
      ((if is_add then Mods.IntSet.add else Mods.IntSet.remove) root va) in
  if Pattern.Set.mem pattern unary_ccs then
    let cc_map =
      Pattern.ObsMap.get unary_map pattern in
    let cc_map' =
      match Edges.get_connected_component root edges with
      | Some cc_id ->
        let set = Mods.IntMap.find_default Mods.IntSet.empty cc_id cc_map in
        let set' =
          (if is_add then Mods.IntSet.add else Mods.IntSet.remove) root set in
        add_intset_in_intmap cc_id set' cc_map
      | None ->
        Mods.IntMap.map
          (fun set ->
             (if is_add then Mods.IntSet.add else Mods.IntSet.remove) root set)
          cc_map in
    Pattern.ObsMap.set unary_map pattern cc_map'

let new_place free_id (inj_nodes,inj_fresh) = function
  | Matching.Agent.Existing _ -> failwith "Rule_interpreter.new_place"
  | Matching.Agent.Fresh (_,id) ->
    (inj_nodes,Mods.IntMap.add id free_id inj_fresh)

let all_injections ?excp ?unary_rate domain edges roots patterna =
  let _,out =
    Tools.array_fold_lefti
      (fun id (excp,inj_list) pattern ->
         let cands,excp' =
           match excp with
           | Some (cc',root)
             when Pattern.is_equal_canonicals pattern cc' ->
             Mods.IntSet.add root Mods.IntSet.empty,None
           | (Some _ | None) -> Pattern.ObsMap.get roots pattern, excp in
         (excp',
          Mods.IntSet.fold
            (fun root new_injs ->
               List.fold_left
                 (fun corrects (inj,roots) ->
                    match Matching.reconstruct
                            domain edges inj id pattern root with
                    | None -> corrects
                    | Some new_inj -> (new_inj,root::roots) :: corrects)
                 new_injs inj_list)
            cands []))
      (excp,[Matching.empty,[]]) patterna in
  match unary_rate with
  | None -> out
  | Some (_,None) ->
    List.filter
      (function
        | _, [ r1; r2 ] -> not (Edges.in_same_connected_component r1 r2 edges)
        | _, _ -> false)
      out
  | Some (_,(Some _ as max_distance)) ->
    List.filter
      (fun (inj,_) ->
         let nodes = Matching.elements_with_types
             domain patterna inj in
         None =
         Edges.are_connected ?max_distance edges nodes.(0) nodes.(1))
      out

let break_apart_cc edges roots_of_unary_patterns = function
  | None -> roots_of_unary_patterns
  | Some (origin_cc,new_cc) ->
    Pattern.ObsMap.map
      (fun cc_map ->
         let oset =
           Mods.IntMap.find_default Mods.IntSet.empty origin_cc cc_map in
         if Mods.IntSet.is_empty oset then cc_map
         else
           let nset,oset' =
             Mods.IntSet.partition
               (fun x -> Edges.get_connected_component x edges = Some new_cc)
               oset in
           add_intset_in_intmap
             new_cc nset (add_intset_in_intmap origin_cc oset' cc_map)
      )
      roots_of_unary_patterns

let merge_cc roots_of_unary_patterns = function
  | None -> roots_of_unary_patterns
  | Some (cc1,cc2) ->
    Pattern.ObsMap.map
      (fun cc_map ->
         let set1 = Mods.IntMap.find_default Mods.IntSet.empty cc1 cc_map in
         match Mods.IntMap.pop cc2 cc_map with
         | None,_ -> cc_map
         | Some set2, cc_map' ->
           add_intset_in_intmap cc1 (Mods.IntSet.union set1 set2) cc_map')
      roots_of_unary_patterns

let apply_negative_transformation
    (side_effects,roots_by_cc,edges) = function
  | Primitives.Transformation.Agent (id,_) ->
    let edges' = Edges.remove_agent id edges in
    (side_effects,roots_by_cc,edges')
  | Primitives.Transformation.Freed ((id,_),s) -> (*(n,s)-bottom*)
    let edges' = Edges.remove_free id s edges in
    (side_effects,roots_by_cc,edges')
  | Primitives.Transformation.Linked (((id,_),s),((id',_),s')) ->
    let edges',cc_modif = Edges.remove_link id s id' s' edges in
    (side_effects,break_apart_cc edges' roots_by_cc cc_modif,edges')
  | Primitives.Transformation.NegativeWhatEver ((id,_),s) ->
    begin
      match Edges.link_destination id s edges with
      | None -> (side_effects,roots_by_cc,Edges.remove_free id s edges)
      | Some ((id',_ as nc'),s') ->
        let edges',cc_modif = Edges.remove_link id s id' s' edges in
        ((nc',s')::side_effects,break_apart_cc edges' roots_by_cc cc_modif,edges')
    end
  | Primitives.Transformation.PositiveInternalized _ ->
    raise
      (ExceptionDefn.Internal_Error
         (Location.dummy_annot "PositiveInternalized in negative update"))
  | Primitives.Transformation.NegativeInternalized ((id,_),s) ->
    let edges' = Edges.remove_internal id s edges in
    (side_effects,roots_by_cc,edges')

let apply_positive_transformation
    sigs (inj2graph,side_effects,roots_by_cc,edges) = function
  | Primitives.Transformation.Agent n ->
    let nc, inj2graph',edges' =
      let ty = Matching.Agent.get_type n in
      let id,edges' = Edges.add_agent sigs ty edges in
      (id,ty),new_place id inj2graph n,edges' in
    (inj2graph',side_effects,roots_by_cc,edges'),
    Primitives.Transformation.Agent nc
  | Primitives.Transformation.Freed (n,s) -> (*(n,s)-bottom*)
    let (id,_ as nc) = Matching.Agent.concretize inj2graph n in (*(A,23)*)
    let edges' = Edges.add_free id s edges in
    let side_effects' =
      Tools.list_smart_filter (fun x -> x <> (nc,s)) side_effects in
    (inj2graph,side_effects',roots_by_cc,edges'),
    Primitives.Transformation.Freed (nc,s)
  | Primitives.Transformation.Linked ((n,s),(n',s')) ->
    let nc = Matching.Agent.concretize inj2graph n in
    let nc' = Matching.Agent.concretize inj2graph n' in
    let edges',modif_cc = Edges.add_link nc s nc' s' edges in
    let side_effects' = Tools.list_smart_filter
        (fun x -> x<>(nc,s) && x<>(nc',s')) side_effects in
    (inj2graph,side_effects',merge_cc roots_by_cc modif_cc,edges'),
    Primitives.Transformation.Linked ((nc,s),(nc',s'))
  | Primitives.Transformation.NegativeWhatEver _ ->
    raise
      (ExceptionDefn.Internal_Error
         (Location.dummy_annot "NegativeWhatEver in positive update"))
  | Primitives.Transformation.PositiveInternalized (n,s,i) ->
    let (id,_ as nc) = Matching.Agent.concretize inj2graph n in
    let edges' = Edges.add_internal id s i edges in
    (inj2graph,side_effects,roots_by_cc,edges'),
    Primitives.Transformation.PositiveInternalized (nc,s,i)
  | Primitives.Transformation.NegativeInternalized _ ->
    raise
      (ExceptionDefn.Internal_Error
         (Location.dummy_annot "NegativeInternalized in positive update"))

let obs_from_transformation domain edges acc = function
  | Primitives.Transformation.Agent nc ->
    Matching.observables_from_agent domain edges acc nc
  | Primitives.Transformation.Freed (nc,s) -> (*(n,s)-bottom*)
    Matching.observables_from_free domain edges acc nc s
  | Primitives.Transformation.Linked ((nc,s),(nc',s')) ->
    Matching.observables_from_link
      domain edges acc nc s nc' s'
  | Primitives.Transformation.PositiveInternalized (nc,s,i) ->
    Matching.observables_from_internal
      domain edges acc nc s i
  | Primitives.Transformation.NegativeInternalized ((id,_ as nc),s) ->
    let i  = Edges.get_internal id s edges in
    Matching.observables_from_internal
      domain edges acc nc s i
  | Primitives.Transformation.NegativeWhatEver ((id,_ as nc),s) ->
    match Edges.link_destination id s edges with
    | None ->
      Matching.observables_from_free domain edges acc nc s
    | Some (nc',s') ->
      Matching.observables_from_link
        domain edges acc nc s nc' s'

let add_path_to_tests path tests =
  let path_agents,path_tests =
    List.fold_left
      (fun (ag,te) (((id,_ as a),_),((id',_ as a'),_)) ->
         let ag',te' =
           if Mods.IntSet.mem id ag then ag,te
           else Mods.IntSet.add id ag,Instantiation.Is_Here a::te in
         if Mods.IntSet.mem id' ag' then ag',te'
         else Mods.IntSet.add id' ag',Instantiation.Is_Here a'::te')
      (Mods.IntSet.empty,[]) path in
  let tests' =
    List.filter (function
        | Instantiation.Is_Here (id, _) ->
          not @@ Mods.IntSet.mem id path_agents
        | Instantiation.Is_Bound_to (a,b) ->
          List.for_all (fun (x,y) -> x <> a && x <> b && y<>a && y<>b) path
        | (Instantiation.Has_Internal _ | Instantiation.Is_Free _
          | Instantiation.Is_Bound _
          | Instantiation.Has_Binding_type _) -> true)
      tests in
  List.rev_append
    path_tests
    (Tools.list_rev_map_append
       (fun (x,y) -> Instantiation.Is_Bound_to (x,y)) path tests')

let step_of_event counter = function
  | Trace.INIT _,(_,(actions,_,_)) -> (Trace.Init actions)
  | Trace.OBS _,_ -> assert false
  | (Trace.RULE _ | Trace.PERT _ as k),x ->
    (Trace.Event (k,x,Counter.current_simulation_info counter))

let store_event counter inj2graph new_tracked_obs_instances event_kind
    ?path extra_side_effects rule outputs = function
  | None -> ()
  | Some _ ->
    let (ctests,(ctransfs,cside_sites,csides)) =
      Instantiation.concretize_event
        inj2graph rule.Primitives.instantiations in
    let cactions =
      (ctransfs,cside_sites,List.rev_append extra_side_effects csides) in
    let full_concrete_event =
      match path with
      | None -> ctests,cactions
      | Some path ->
        add_path_to_tests path ctests,cactions in
    let () =
      outputs (Data.TraceStep
                 (step_of_event counter (event_kind,full_concrete_event))) in
      List.iter
        (fun (i,x) ->
           outputs (Data.TraceStep
                      (Trace.Obs(i,x,Counter.next_story counter))))
        new_tracked_obs_instances

let store_obs domain edges roots obs acc = function
  | None -> acc
  | Some tracked ->
    List.fold_left
      (fun acc (pattern,(root,_)) ->
         try
           List.fold_left
             (fun acc (ev,patterns,tests) ->
                List.fold_left
                  (fun acc (inj,_) ->
                     let tests' =
                       List.map (Instantiation.concretize_test
                                   (inj,Mods.IntMap.empty)) tests in
                     (ev,tests') :: acc)
                  acc
                  (all_injections ~excp:(pattern,root) domain edges roots patterns))
             acc (Pattern.ObsMap.get tracked pattern)
         with Not_found -> acc)
      acc obs

let update_edges outputs counter domain unary_patterns inj_nodes
    state event_kind ?path rule =
  let () = assert (not state.outdated) in
  let () = state.outdated <- true in
  (*Negative update*)
  let concrete_removed =
    List.map (Primitives.Transformation.concretize
                (inj_nodes,Mods.IntMap.empty)) rule.Primitives.removed in
  let ((del_obs,del_deps),_) =
    List.fold_left
      (obs_from_transformation domain state.edges)
      (([],Operator.DepSet.empty),Matching.empty_cache)
      concrete_removed in
  let (side_effects,roots_by_cc,edges_after_neg) =
    List.fold_left
      apply_negative_transformation
      ([],state.roots_of_unary_patterns,state.edges) concrete_removed in
  let () =
    List.iter
      (fun (pat,(root,_)) ->
         update_roots false unary_patterns edges_after_neg
           state.roots_of_patterns roots_by_cc pat root)
      del_obs in
  (*Positive update*)
  let (final_inj2graph,remaining_side_effects,roots_by_cc',edges'),
      concrete_inserted =
    List.fold_left
      (fun (x,p) h ->
         let (x', h') =
           apply_positive_transformation
             (Pattern.Env.signatures domain) x h in
         (x',h'::p))
      (((inj_nodes,Mods.IntMap.empty),side_effects,roots_by_cc,
        edges_after_neg),[])
      rule.Primitives.inserted in
  let (edges'',concrete_inserted') =
    List.fold_left
      (fun (e,i)  ((id,_ as nc),s) ->
         Edges.add_free id s e,Primitives.Transformation.Freed (nc,s)::i)
      (edges',concrete_inserted) remaining_side_effects in
  let ((new_obs,new_deps),_) =
    List.fold_left
      (obs_from_transformation domain edges'')
      (([],Operator.DepSet.empty),Matching.empty_cache)
      concrete_inserted' in
  let () =
    List.iter
      (fun (pat,(root,_)) ->
         update_roots true unary_patterns edges''
           state.roots_of_patterns roots_by_cc' pat root)
      new_obs in
  (*Store event*)
  let new_tracked_obs_instances =
    store_obs domain edges'' state.roots_of_patterns
      new_obs [] state.story_machinery in
  let () =
    store_event
      counter final_inj2graph new_tracked_obs_instances event_kind
      ?path remaining_side_effects rule outputs state.story_machinery in

  let former_deps,changed_connectivity  = state.outdated_elements in
  let rev_deps = Operator.DepSet.union
      former_deps (Operator.DepSet.union del_deps new_deps) in
  let mod_connectivity =
    changed_connectivity ||
    Pattern.ObsMap.fold_lefti
      (fun i b x ->
         b || x != Pattern.ObsMap.get state.roots_of_unary_patterns i)
      false roots_by_cc' in

  { outdated = false;
    roots_of_patterns = state.roots_of_patterns;
    roots_of_unary_patterns = roots_by_cc';
    unary_candidates = state.unary_candidates;
    matchings_of_rule = state.matchings_of_rule;
    variables_cache = state.variables_cache;
    variables_overwrite = state.variables_overwrite;
    edges = edges''; tokens = state.tokens;
    outdated_elements = rev_deps,mod_connectivity;
    random_state = state.random_state;
    story_machinery = state.story_machinery;
    store_distances = state.store_distances; }

let max_dist_to_int counter state d =
  Nbr.to_int (value_alg counter state d)

let store_activity store env counter state id syntax_id rate cc_va =
  let () =
    if !Parameter.debugModeOn then
      Format.printf "@[%sule %a has now %i instances.@]@."
        (if id mod 2 = 1 then "Unary r" else "R")
        (Environment.print_rule ~env) (id/2) cc_va in
  let act =
    match Nbr.to_float @@ value_alg counter state rate with
    | None -> if cc_va = 0 then 0. else infinity
    | Some rate -> rate *. float_of_int cc_va in
  store id syntax_id act

let update_outdated_activities store env counter state =
  let () = assert (not state.outdated) in
  let deps,changed_connectivity = state.outdated_elements in
  let unary_rule_update i state rule =
    match rule.Primitives.unary_rate with
    | None -> state
    | Some (unrate, _) ->
      let map1 =
        Pattern.ObsMap.get state.roots_of_unary_patterns
          rule.Primitives.connected_components.(0)in
      let map2 =
        Pattern.ObsMap.get state.roots_of_unary_patterns
          rule.Primitives.connected_components.(1)in
      let (),va =
        Mods.IntMap.monadic_fold2_sparse
          () ()
          (fun () () _ set1 set2 acc ->
             (),(Mods.IntSet.size set1 * Mods.IntSet.size set2) + acc)
          map1 map2 0 in
      let () =
        store_activity
          store env counter state (2*i+1)
          rule.Primitives.syntactic_rule (fst unrate) va in
      match Mods.IntMap.pop i state.unary_candidates with
           | None,_ -> state
           | Some _, match' -> { state with unary_candidates = match' } in
  let rec aux dep acc =
    Operator.DepSet.fold
      (fun dep (state,perts as acc) ->
         match dep with
         | Operator.ALG j ->
           let () = recompute env counter state j in
           aux (Environment.get_alg_reverse_dependencies env j) acc
         | Operator.PERT p -> (state,p::perts)
         | Operator.RULE i ->
           let rule = Environment.get_rule env i in
           let pattern_va = raw_instance_number
               state [rule.Primitives.connected_components] in
           let () =
             store_activity store env counter state (2*i)
               rule.Primitives.syntactic_rule
               (fst rule.Primitives.rate) pattern_va in
           let state' = if changed_connectivity then state
             else unary_rule_update i state rule in
           ((match Mods.IntMap.pop i state'.matchings_of_rule with
               | None,_ -> state'
               | Some _, match' ->
                 { state' with matchings_of_rule = match' }),perts))
      dep acc in
  let pack = aux (Environment.get_always_outdated env) (state,[]) in
  let state',perts = aux deps pack in
  let state'' =
    if not changed_connectivity then state'
    else Environment.fold_rules unary_rule_update state' env in
  ({state'' with outdated_elements = Operator.DepSet.empty,false },perts)

let overwrite_var i counter state expr =
  let rdeps,changed_connectivity = state.outdated_elements in
  let () =
    state.variables_overwrite.(i) <-
      Some (Alg_expr.CONST (value_alg counter state expr)) in
  {state with
   outdated_elements =
     (Operator.DepSet.add (Operator.ALG i) rdeps,changed_connectivity)}

let update_tokens env counter state consumed injected =
  let compute_them = List.rev_map
      (fun  ((expr,_),i) -> (value_alg counter state expr,i)) in
  let do_op op state l =
    List.fold_left
      (fun st (va,i) ->
         let () = st.tokens.(i) <- op st.tokens.(i) va in
         let deps' = Environment.get_token_reverse_dependencies env i in
         if Operator.DepSet.is_empty deps' then st
         else
           let rdeps,changed_connectivity = st.outdated_elements in
           { st with outdated_elements =
                       Operator.DepSet.union rdeps deps',changed_connectivity }
      ) state l in
  let consumed' = compute_them consumed in
  let injected' = compute_them injected in
  let state' = do_op Nbr.sub state consumed' in do_op Nbr.add state' injected'

let transform_by_a_rule outputs env unary_patterns counter
    state event_kind ?path rule inj =
  let state' =
    update_tokens
      env counter state rule.Primitives.consumed_tokens
      rule.Primitives.injected_tokens in
  update_edges outputs counter (Environment.domain env) unary_patterns inj
    state' event_kind ?path rule

let apply_unary_rule
    ~outputs ~rule_id env unary_ccs counter state event_kind rule =
  let () = assert (not state.outdated) in
  let domain = Environment.domain env in
  let inj,path =
    match Mods.IntMap.find_option rule_id state.unary_candidates with
    | Some l ->
      let inj,path = Tools.list_random state.random_state l in Some inj,path
    | None ->
      let map1 =
        Pattern.ObsMap.get state.roots_of_unary_patterns
          rule.Primitives.connected_components.(0) in
      let map2 =
        Pattern.ObsMap.get state.roots_of_unary_patterns
           rule.Primitives.connected_components.(1) in
      let rtree = Random_tree.create
          (min (Mods.IntMap.size map1) (Mods.IntMap.size map2)) in
      let (),() =
        Mods.IntMap.monadic_fold2_sparse
          () ()
          (fun () () i set1 set2 () ->
             let () =
               Random_tree.add
                 i (float_of_int
                      (Mods.IntSet.size set1 * Mods.IntSet.size set2))
                 rtree in
             (),())
          map1 map2 () in
      let cc_id,_ = Random_tree.random state.random_state rtree in
      let root1 =
        Tools.unsome (-1)
          (Mods.IntSet.random state.random_state
             (Mods.IntMap.find_default Mods.IntSet.empty cc_id map1)) in
      let root2 =
        Tools.unsome (-1)
          (Mods.IntSet.random state.random_state
             (Mods.IntMap.find_default Mods.IntSet.empty cc_id map2)) in
      let () =
        if !Parameter.debugModeOn then
          Format.printf "@[On roots:@ %i@ %i@]@." root1 root2 in
      let pattern1 = rule.Primitives.connected_components.(0) in
      let pattern2 = rule.Primitives.connected_components.(1) in
      let inj1 =
        Matching.reconstruct
          domain state.edges
          Matching.empty 0 pattern1 root1 in
      match inj1 with
      | None -> None,None
      | Some inj -> Matching.reconstruct
                      domain state.edges inj 1 pattern2 root2,None in
  let rdeps,changed_c = state.outdated_elements in
  let state' =
    {state with
     outdated_elements =
       (Operator.DepSet.add (Operator.RULE rule_id) rdeps,changed_c)} in
  match inj with
  | None -> Clash
  | Some inj ->
    let nodes = Matching.elements_with_types
        domain rule.Primitives.connected_components inj in
    match path with
    | Some p ->
      let () =
        if state'.store_distances then
          outputs (Data.UnaryDistance {
              Data.distance_rule = rule.Primitives.syntactic_rule;
              Data.distance_time = Counter.current_time counter;
              Data.distance_length = (List.length p);
            }) in
      Success
        (transform_by_a_rule outputs env unary_ccs counter state'
           event_kind ?path rule inj)
    | None ->
      let max_distance = match rule.Primitives.unary_rate with
        | None -> None
        | Some (_, dist_opt) ->
           (match dist_opt with
           | None -> None
           | Some d -> Some (max_dist_to_int counter state' d)) in
      match Edges.are_connected ?max_distance state.edges nodes.(0) nodes.(1) with
      | None -> Corrected
      | Some p as path ->
        let () =
          if state'.store_distances then
            outputs (Data.UnaryDistance {
                Data.distance_rule = rule.Primitives.syntactic_rule;
                Data.distance_time = Counter.current_time counter;
                Data.distance_length = (List.length p);
              }) in
        Success
          (transform_by_a_rule outputs env unary_ccs counter state'
             event_kind ?path rule inj)

let apply_rule
    ~outputs ?rule_id env unary_patterns counter state event_kind rule =
  let () = assert (not state.outdated) in
  let domain = Environment.domain env in
  let from_patterns () =
    Tools.array_fold_lefti
      (fun id inj_rev_roots pattern ->
         match inj_rev_roots with
         | None -> None
         | Some (inj,rev_roots) ->
           match
             Mods.IntSet.random state.random_state
               (Pattern.ObsMap.get state.roots_of_patterns pattern) with
           | None -> None
           | Some root ->
             match Matching.reconstruct
                     domain state.edges inj id pattern root with
             | None -> None
             | Some inj' -> Some (inj',root::rev_roots))
      (Some (Matching.empty,[]))
      rule.Primitives.connected_components in
  let inj_roots =
    match rule_id with
    | None -> from_patterns ()
    | Some id ->
      match Mods.IntMap.find_option id state.matchings_of_rule with
      | Some [] -> None
      | Some l -> Some (Tools.list_random state.random_state l)
      | None -> from_patterns () in
  match inj_roots with
  | None -> Clash
  | Some (inj,rev_roots) ->
    let roots = Tools.array_rev_of_list rev_roots in
    let () =
      if !Parameter.debugModeOn then
        Format.printf "@[On roots:@ @[%a@]@]@."
          (Pp.array Pp.space (fun _ -> Format.pp_print_int)) roots in
    match rule.Primitives.unary_rate with
    | None ->
      let out =
        transform_by_a_rule outputs env unary_patterns counter
          state event_kind rule inj in
      Success out
    | Some (_,max_distance) ->
      match max_distance with
      | None ->
        if Edges.in_same_connected_component roots.(0) roots.(1) state.edges then
          Corrected
        else
          Success (transform_by_a_rule outputs env unary_patterns
                                       counter state event_kind rule inj)
      | Some dist ->
         let dist' = Some (max_dist_to_int counter state dist) in
         let nodes = Matching.elements_with_types
                       domain rule.Primitives.connected_components inj in
         match
           Edges.are_connected ?max_distance:dist' state.edges nodes.(0)
                               nodes.(1) with
         | None ->
            Success (transform_by_a_rule outputs env unary_patterns
                                         counter state event_kind rule inj)
         | Some _ -> Corrected

let force_rule
    ~outputs env unary_patterns counter state event_kind rule =
  match apply_rule
          ~outputs env unary_patterns counter state event_kind rule with
  | Success out -> out
  | Corrected | Clash ->
    let () = assert (not state.outdated) in
    let max_distance = match rule.Primitives.unary_rate with
      | None -> None
      | Some (loc, dist_opt) ->
         (match dist_opt with
          | None -> Some (loc,None)
          | Some d ->
             Some (loc,Some (max_dist_to_int counter state d))) in
    match all_injections
            ?unary_rate:max_distance
            (Environment.domain env) state.edges
            state.roots_of_patterns rule.Primitives.connected_components with
    | [] -> state
    | l ->
       let (h,_) = Tools.list_random state.random_state l in
       (transform_by_a_rule
          outputs env unary_patterns counter state event_kind rule h)

let adjust_rule_instances ~rule_id store env counter state rule =
  let () = assert (not state.outdated) in
  let domain = Environment.domain env in
  let max_distance = match rule.Primitives.unary_rate with
    | None -> None
    | Some (loc, dist_opt) ->
       (match dist_opt with
        | None -> Some (loc,None)
        | Some d ->
           Some (loc,Some (max_dist_to_int counter state d))) in
  let matches =
    all_injections
      ?unary_rate:max_distance domain state.edges
      state.roots_of_patterns rule.Primitives.connected_components in
  let () =
    store_activity store env counter state (2*rule_id)
      rule.Primitives.syntactic_rule
      (fst rule.Primitives.rate) (List.length matches) in
  { state with
    matchings_of_rule =
      Mods.IntMap.add rule_id matches state.matchings_of_rule }

let adjust_unary_rule_instances ~rule_id store env counter state rule =
  let () = assert (not state.outdated) in
  let domain = Environment.domain env in
  let pattern1 = rule.Primitives.connected_components.(0) in
  let pattern2 = rule.Primitives.connected_components.(1) in
  let (),(cands,len) =
    Mods.IntMap.monadic_fold2_sparse
      () ()
      (fun () () _ set1 set2 out ->
         (),
         Mods.IntSet.fold
           (fun root1 ->
              Mods.IntSet.fold
                (fun root2 (list,len as out) ->
                   let inj1 =
                     Matching.reconstruct
                       domain state.edges Matching.empty 0
                       pattern1 root1 in
                   match inj1 with
                   | None -> out
                   | Some inj ->
                     match Matching.reconstruct
                             domain state.edges inj 1 pattern2 root2 with
                     | None -> out
                     | Some inj' ->
                       let max_distance = match rule.Primitives.unary_rate with
                         | None -> None
                         | Some (_, dist_opt) -> dist_opt in
                       match max_distance with
                       | None -> (inj',None)::list,succ len
                       | Some dist ->
                         let dist' =
                           Some (max_dist_to_int counter state dist) in
                          let nodes =
                            Matching.elements_with_types
                              domain rule.Primitives.connected_components inj' in
                          match Edges.are_connected ?max_distance:dist'
                                 state.edges nodes.(0) nodes.(1) with
                         | None -> out
                         | Some _ as p -> (inj',p)::list,succ len)
                set2)
           set1 out)
      (Pattern.ObsMap.get state.roots_of_unary_patterns pattern1)
      (Pattern.ObsMap.get state.roots_of_unary_patterns pattern2)
      ([],0) in
  let () =
    store_activity store env counter state (2*rule_id+1)
      rule.Primitives.syntactic_rule (fst rule.Primitives.rate) len in
  { state with
    unary_candidates =
      if len = 0 then Mods.IntMap.remove rule_id state.unary_candidates
      else Mods.IntMap.add rule_id cands state.unary_candidates;
  }

let incorporate_extra_pattern domain state pattern =
  let () = assert (not state.outdated) in
  let () = state.outdated <- true in
  let () =
    if not (Mods.IntSet.is_empty
              (Pattern.ObsMap.get state.roots_of_patterns pattern)) then
      Pattern.ObsMap.set
      state.roots_of_patterns
      pattern
      (Matching.roots_of domain state.edges pattern) in
  { state with outdated = false }

let snapshot env counter fn state = {
  Data.snapshot_file = fn;
  Data.snapshot_event = Counter.current_event counter;
  Data.snapshot_agents =
    Edges.build_snapshot (Environment.signatures env) state.edges;
  Data.snapshot_tokens = Array.mapi (fun i x ->
      (Format.asprintf "%a" (Environment.print_token ~env) i,x)) state.tokens;
}

let print env f state =
  let sigs = Environment.signatures env in
  Format.fprintf
    f "@[<v>%a@,%a@]"
    (Pp.list Pp.space (fun f (i,mix) ->
         Format.fprintf f "%%init: %i @[<h>%a@]" i
           (Raw_mixture.print ~compact:false sigs) mix))
    (Edges.build_snapshot sigs state.edges)
    (Pp.array Pp.space (fun i f el ->
         Format.fprintf
           f "%%init: %a <- %a"
           (Environment.print_token ~env) i Nbr.print el))
    state.tokens

let debug_print f state =
  Format.fprintf
    f "@[<v>%a@,%a@,%a@,%a@]"
    Edges.debug_print state.edges
    (Pp.array Pp.space (fun i f el ->
         Format.fprintf f "token_%i <- %a"
           i Nbr.print el))
    state.tokens
    (print_injections ?domain:None) state.roots_of_patterns
    (print_unary_injections ?domain:None)
    state.roots_of_unary_patterns

let add_tracked patterns event_kind tests state =
  let () = assert (not state.outdated) in
  let () = state.outdated <- true in
  match state.story_machinery with
  | None -> state
  | Some tpattern ->
    let () =
      Array.iter
        (fun pattern ->
           let acc = Pattern.ObsMap.get tpattern pattern in
           Pattern.ObsMap.set tpattern
             pattern ((event_kind,patterns,tests)::acc))
        patterns in
    { state with outdated = false }
let remove_tracked patterns state =
  let () = assert (not state.outdated) in
  let () = state.outdated <- true in
  match state.story_machinery with
  | None -> state
  | Some tpattern ->
    let tester (_,el,_) =
      not @@
      Tools.array_fold_lefti
        (fun i b x -> b && Pattern.is_equal_canonicals x el.(i))
        true patterns in
    let () =
      Array.iter
        (fun pattern ->
           let acc = Pattern.ObsMap.get tpattern pattern in
           Pattern.ObsMap.set tpattern pattern (List.filter tester acc))
        patterns in
    { state with outdated = false }

let get_random_state state = state.random_state
