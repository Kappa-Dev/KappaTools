type t =
  {
    (* With rectangular approximation *)
    roots_of_patterns: Mods.IntSet.t Pattern.Map.t;
    roots_of_unary_patterns:
      Mods.IntSet.t Mods.IntMap.t Pattern.Map.t;

    (* Without rectangular approximation *)
    matchings_of_rule:
      (Pattern.Matching.t * int list) list Mods.IntMap.t;
    unary_candidates:
      (Pattern.Matching.t * Edges.path option) list Mods.IntMap.t;

    edges: Edges.t;
    tokens: Nbr.t array;
    outdated_elements: Operator.DepSet.t * bool;

    random_state : Random.State.t;
    story_machinery :
      (string *
       (Trace.event_kind * Pattern.id array *
        Instantiation.abstract Instantiation.test list)
         list Pattern.Map.t (*currently tracked ccs *) *
       Trace.t) option;
    store_distances: bool;
  }

type result = Clash | Corrected | Success of (int option * t)

let empty ?trace_file ~store_distances random_state env =
  let with_connected_components =
    not (Pattern.Set.is_empty
           (Environment.connected_components_of_unary_rules env)) in
  {
    roots_of_patterns = Pattern.Map.empty;
    roots_of_unary_patterns = Pattern.Map.empty;
    matchings_of_rule = Mods.IntMap.empty;
    unary_candidates = Mods.IntMap.empty;
    edges = Edges.empty ~with_connected_components;
    tokens = Array.make (Environment.nb_tokens env) Nbr.zero;
    outdated_elements = Operator.DepSet.empty,false;
    random_state;
    story_machinery =
      Tools.option_map (fun s -> (s,Pattern.Map.empty,[])) trace_file;
    store_distances;
  }

let print_injections ?domain f roots_of_patterns =
  Format.fprintf
    f "@[<v>%a@]"
    (Pp.set Pattern.Map.bindings Pp.space
       (fun f (pattern,roots) ->
          Format.fprintf
            f "@[# @[%a@] ==>@ @[%a@]@]"
            (Pattern.print ?domain ~with_id:true) pattern
            Mods.IntSet.print roots
       )
    ) roots_of_patterns
let print_unary_injections ?domain f roots_of_patterns =
  Format.fprintf
    f "@[<v>%a@]"
    (Pp.set Pattern.Map.bindings Pp.space
       (fun f (pattern,root_maps) ->
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

let update_roots is_add unary_ccs edges (map,unary_map) pattern root =
  let va =
    Pattern.Map.find_default Mods.IntSet.empty pattern map in
  Pattern.Map.add pattern
    ((if is_add then Mods.IntSet.add else Mods.IntSet.remove) root va) map,
  (if Pattern.Set.mem pattern unary_ccs then
     let cc_map =
       Pattern.Map.find_default Mods.IntMap.empty pattern unary_map in
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
     if Mods.IntMap.is_empty cc_map'
     then Pattern.Map.remove pattern unary_map
     else Pattern.Map.add pattern cc_map' unary_map
   else unary_map)

let new_place free_id (inj_nodes,inj_fresh) = function
  | Agent_place.Existing _ -> failwith "Rule_interpreter.new_place"
  | Agent_place.Fresh (_,id) ->
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
           | (Some _ | None) ->
             Pattern.Map.find_default
               Mods.IntSet.empty pattern roots, excp in
         (excp',
          Mods.IntSet.fold
            (fun root new_injs ->
               List.fold_left
                 (fun corrects (inj,roots) ->
                    match Pattern.Matching.reconstruct
                            domain edges inj id pattern root with
                    | None -> corrects
                    | Some new_inj -> (new_inj,root::roots) :: corrects)
                 new_injs inj_list)
            cands []))
      (excp,[Pattern.Matching.empty,[]]) patterna in
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
         let nodes = Pattern.Matching.elements_with_types
             domain patterna inj in
         None =
         Edges.are_connected ?max_distance edges nodes.(0) nodes.(1))
      out

let break_apart_cc edges roots_of_unary_patterns = function
  | None -> roots_of_unary_patterns
  | Some (origin_cc,new_cc) ->
    Pattern.Map.map
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
    Pattern.Map.map
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
      let ty = Agent_place.get_type n in
      let id,edges' = Edges.add_agent sigs ty edges in
      (id,ty),new_place id inj2graph n,edges' in
    (inj2graph',side_effects,roots_by_cc,edges'),
    Primitives.Transformation.Agent nc
  | Primitives.Transformation.Freed (n,s) -> (*(n,s)-bottom*)
    let (id,_ as nc) = Agent_place.concretize inj2graph n in (*(A,23)*)
    let edges' = Edges.add_free id s edges in
    let side_effects' =
      Tools.list_smart_filter (fun x -> x <> (nc,s)) side_effects in
    (inj2graph,side_effects',roots_by_cc,edges'),
    Primitives.Transformation.Freed (nc,s)
  | Primitives.Transformation.Linked ((n,s),(n',s')) ->
    let nc = Agent_place.concretize inj2graph n in
    let nc' = Agent_place.concretize inj2graph n' in
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
    let (id,_ as nc) = Agent_place.concretize inj2graph n in
    let edges' = Edges.add_internal id s i edges in
    (inj2graph,side_effects,roots_by_cc,edges'),
    Primitives.Transformation.PositiveInternalized (nc,s,i)
  | Primitives.Transformation.NegativeInternalized _ ->
    raise
      (ExceptionDefn.Internal_Error
         (Location.dummy_annot "NegativeInternalized in positive update"))

let obs_from_transformation domain edges acc = function
  | Primitives.Transformation.Agent nc ->
    Pattern.Matching.observables_from_agent domain edges acc nc
  | Primitives.Transformation.Freed (nc,s) -> (*(n,s)-bottom*)
    Pattern.Matching.observables_from_free domain edges acc nc s
  | Primitives.Transformation.Linked ((nc,s),(nc',s')) ->
    Pattern.Matching.observables_from_link
      domain edges acc nc s nc' s'
  | Primitives.Transformation.PositiveInternalized (nc,s,i) ->
    Pattern.Matching.observables_from_internal
      domain edges acc nc s i
  | Primitives.Transformation.NegativeInternalized ((id,_ as nc),s) ->
    let i  = Edges.get_internal id s edges in
    Pattern.Matching.observables_from_internal
      domain edges acc nc s i
  | Primitives.Transformation.NegativeWhatEver ((id,_ as nc),s) ->
    match Edges.link_destination id s edges with
    | None ->
      Pattern.Matching.observables_from_free domain edges acc nc s
    | Some (nc',s') ->
      Pattern.Matching.observables_from_link
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

let store_event counter inj2graph new_tracked_obs_instances event_kind
    ?path extra_side_effects rule = function
  | None as x -> x
  | Some (compressions,x,steps) ->
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
    let steps' =
      Trace.store_event counter (event_kind,full_concrete_event) steps in
    let steps'' =
      List.fold_left
        (fun steps x ->
           Trace.store_obs counter x steps)
        steps' new_tracked_obs_instances
    in
    Some (compressions,x,steps'')

let store_obs domain edges roots obs acc = function
  | None -> acc
  | Some (_,tracked,_) ->
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
             acc (Pattern.Map.find_default [] pattern tracked)
         with Not_found -> acc)
      acc obs

let update_edges
    counter domain unary_patterns inj_nodes state event_kind ?path rule =
  (*Negative update*)
  let concrete_removed =
    List.map (Primitives.Transformation.concretize
                (inj_nodes,Mods.IntMap.empty)) rule.Primitives.removed in
  let ((del_obs,del_deps),_) =
    List.fold_left
      (obs_from_transformation domain state.edges)
      (([],Operator.DepSet.empty),Pattern.Matching.empty_cache)
      concrete_removed in
  let (side_effects,roots_by_cc,edges_after_neg) =
    List.fold_left
      apply_negative_transformation
      ([],state.roots_of_unary_patterns,state.edges) concrete_removed in
  let roots_of_p,roots_of_up =
    List.fold_left
      (fun r' (pat,(root,_)) -> update_roots false unary_patterns edges_after_neg r' pat root)
      (state.roots_of_patterns,roots_by_cc) del_obs in
  (*Positive update*)
  let (final_inj2graph,remaining_side_effects,roots_by_cc',edges'),concrete_inserted =
    List.fold_left
      (fun (x,p) h ->
         let (x', h') = apply_positive_transformation
             (Pattern.Env.signatures domain) x h in
         (x',h'::p))
      (((inj_nodes,Mods.IntMap.empty),side_effects,roots_of_up,edges_after_neg),[])
      rule.Primitives.inserted in
  let (edges'',concrete_inserted') =
    List.fold_left
      (fun (e,i)  ((id,_ as nc),s) ->
         Edges.add_free id s e,Primitives.Transformation.Freed (nc,s)::i)
      (edges',concrete_inserted) remaining_side_effects in
  let ((new_obs,new_deps),_) =
    List.fold_left
      (obs_from_transformation domain edges'')
      (([],Operator.DepSet.empty),Pattern.Matching.empty_cache)
      concrete_inserted' in
  let roots_of_p',roots_of_up' =
    List.fold_left
      (fun r' (pat,(root,_)) -> update_roots true unary_patterns edges'' r' pat root)
      (roots_of_p,roots_by_cc') new_obs in
  (*Store event*)
  let new_tracked_obs_instances =
    store_obs domain edges'' roots_of_p new_obs [] state.story_machinery in
  let story_machinery' =
    store_event
      counter final_inj2graph new_tracked_obs_instances event_kind
      ?path remaining_side_effects rule state.story_machinery in

  let former_deps,changed_connectivity  = state.outdated_elements in
  let rev_deps = Operator.DepSet.union
      former_deps (Operator.DepSet.union del_deps new_deps) in
  let mod_connectivity =
    changed_connectivity || roots_of_up' != state.roots_of_unary_patterns in

  { roots_of_patterns = roots_of_p'; roots_of_unary_patterns = roots_of_up';
    unary_candidates = state.unary_candidates;
    matchings_of_rule = state.matchings_of_rule;
    edges = edges''; tokens = state.tokens;
    outdated_elements = rev_deps,mod_connectivity;
    random_state = state.random_state;
    story_machinery = story_machinery';
    store_distances = state.store_distances; }

let raw_instance_number state patterns_l =
  let size pattern =
    Mods.IntSet.size (Pattern.Map.find_default
                        Mods.IntSet.empty pattern state.roots_of_patterns) in
  let rect_approx patterns =
    Array.fold_left (fun acc pattern ->  acc * (size pattern)) 1 patterns in
  List.fold_left (fun acc patterns -> acc + (rect_approx patterns)) 0 patterns_l
let instance_number state patterns_l =
  Nbr.I (raw_instance_number state patterns_l)

let value_bool ~get_alg counter state expr =
  Expr_interpreter.value_bool
    counter ~get_alg
    ~get_mix:(fun patterns -> instance_number state patterns)
    ~get_tok:(fun i -> state.tokens.(i))
    expr
let value_alg ~get_alg counter state alg =
  Expr_interpreter.value_alg
    counter ~get_alg
    ~get_mix:(fun patterns -> instance_number state patterns)
    ~get_tok:(fun i -> state.tokens.(i))
    alg

let extra_outdated_var i state =
  let rdeps,changed_connectivity = state.outdated_elements in
  {state with
   outdated_elements =
     (Operator.DepSet.add (Operator.ALG i) rdeps,changed_connectivity)}

let store_activity ~get_alg store env counter state id syntax_id rate cc_va =
  let rate =
    Nbr.to_float @@ value_alg counter state ~get_alg rate in
  let () =
    if !Parameter.debugModeOn then
      Format.printf "@[%sule %a has now %i instances.@]@."
        (if id mod 2 = 1 then "Unary r" else "R")
        (Environment.print_rule ~env) (id/2) cc_va in
  let act =
    if cc_va = 0 then 0. else rate *. float_of_int cc_va in
  store id syntax_id act

let update_outdated_activities ~get_alg store env counter state =
  let deps,changed_connectivity = state.outdated_elements in
  let unary_rule_update i state rule =
    match rule.Primitives.unary_rate with
    | None -> state
    | Some (unrate, _) ->
      let map1 =
        Pattern.Map.find_default
          Mods.IntMap.empty rule.Primitives.connected_components.(0)
          state.roots_of_unary_patterns in
      let map2 =
        Pattern.Map.find_default
          Mods.IntMap.empty rule.Primitives.connected_components.(1)
          state.roots_of_unary_patterns in
      let (),va =
        Mods.IntMap.monadic_fold2_sparse
          () ()
          (fun () () _ set1 set2 acc ->
             (),(Mods.IntSet.size set1 * Mods.IntSet.size set2) + acc)
          map1 map2 0 in
      let () =
        store_activity
          ~get_alg store env counter state (2*i+1)
          rule.Primitives.syntactic_rule (fst unrate) va in
      match Mods.IntMap.pop i state.unary_candidates with
           | None,_ -> state
           | Some _, match' -> { state with unary_candidates = match' } in
  let rec aux state deps =
    Operator.DepSet.fold
      (fun dep state ->
         match dep with
         | Operator.ALG j ->
           aux state (Environment.get_alg_reverse_dependencies env j)
         | Operator.PERT (-1) -> state (* TODO *)
         | Operator.PERT _ -> assert false
         | Operator.RULE i ->
           let rule = Environment.get_rule env i in
           let pattern_va = raw_instance_number
               state [rule.Primitives.connected_components] in
           let () =
             store_activity
               ~get_alg store env counter state (2*i)
               rule.Primitives.syntactic_rule
               (fst rule.Primitives.rate) pattern_va in
           let state' = if changed_connectivity then state
             else unary_rule_update i state rule in
           match Mods.IntMap.pop i state'.matchings_of_rule with
           | None,_ -> state'
           | Some _, match' -> { state' with matchings_of_rule = match' })
      deps state in
  let state' = aux state (Environment.get_always_outdated env) in
  let state'' = aux state' deps in
  let state''' =
    if not changed_connectivity then state''
    else Environment.fold_rules unary_rule_update state'' env in
  {state''' with outdated_elements = Operator.DepSet.empty,false }

let update_tokens ~get_alg env counter state consumed injected =
  let do_op op state l =
    List.fold_left
      (fun st ((expr,_),i) ->
         let () =
           st.tokens.(i) <-
             op st.tokens.(i) (value_alg ~get_alg counter st expr) in
         let deps' = Environment.get_token_reverse_dependencies env i in
         if Operator.DepSet.is_empty deps' then st
         else
           let rdeps,changed_connectivity = st.outdated_elements in
           { st with outdated_elements =
                       Operator.DepSet.union rdeps deps',changed_connectivity }
      ) state l in
  let state' = do_op Nbr.sub state consumed in do_op Nbr.add state' injected

let transform_by_a_rule
    ~get_alg env unary_patterns counter state event_kind ?path rule inj =
  let state' =
    update_tokens
      ~get_alg env counter state rule.Primitives.consumed_tokens
      rule.Primitives.injected_tokens in
  update_edges
    counter (Environment.domain env) unary_patterns inj state' event_kind ?path rule

let apply_unary_rule
    ~rule_id ~get_alg env unary_ccs counter state event_kind rule =
  let domain = Environment.domain env in
  let inj,path =
    match Mods.IntMap.find_option rule_id state.unary_candidates with
    | Some l ->
      let inj,path = Tools.list_random state.random_state l in Some inj,path
    | None ->
      let map1 =
        Pattern.Map.find_default
          Mods.IntMap.empty rule.Primitives.connected_components.(0)
          state.roots_of_unary_patterns in
      let map2 =
        Pattern.Map.find_default
          Mods.IntMap.empty rule.Primitives.connected_components.(1)
          state.roots_of_unary_patterns in
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
        Pattern.Matching.reconstruct
          domain state.edges
          Pattern.Matching.empty 0 pattern1 root1 in
      match inj1 with
      | None -> None,None
      | Some inj -> Pattern.Matching.reconstruct
                      domain state.edges inj 1 pattern2 root2,None in
  let rdeps,changed_c = state.outdated_elements in
  let state' =
    {state with
     outdated_elements =
       (Operator.DepSet.add (Operator.RULE rule_id) rdeps,changed_c)} in
  match inj with
  | None -> Clash
  | Some inj ->
    let nodes = Pattern.Matching.elements_with_types
        domain rule.Primitives.connected_components inj in
    match path with
    | Some p ->
      Success
        ((if state'.store_distances then Some (List.length p) else None),
         transform_by_a_rule ~get_alg env unary_ccs counter state'
           event_kind ?path rule inj)
    | None ->
      let max_distance = match rule.Primitives.unary_rate with
        | None -> None
        | Some (_, dist_opt) -> dist_opt in
      match Edges.are_connected ?max_distance state.edges nodes.(0) nodes.(1) with
      | None -> Corrected
      | Some p as path ->
        Success
          ((if state'.store_distances then Some (List.length p) else None),
           transform_by_a_rule ~get_alg env unary_ccs counter state'
             event_kind ?path rule inj)

let apply_rule
    ?rule_id ~get_alg env unary_patterns counter state event_kind rule =
  let domain = Environment.domain env in
  let from_patterns () =
    Tools.array_fold_left_mapi
      (fun id inj pattern ->
         let root =
           match Mods.IntSet.random state.random_state
                   (Pattern.Map.find_default
                      Mods.IntSet.empty pattern state.roots_of_patterns) with
           | None -> failwith "Tried to apply_rule with no root"
           | Some x -> x in
         (match inj with
          | Some inj -> Pattern.Matching.reconstruct
                          domain state.edges inj id pattern root
          | None -> None),root)
      (Some Pattern.Matching.empty)
      rule.Primitives.connected_components in
  let inj,roots =
    match rule_id with
    | None -> from_patterns ()
    | Some id ->
      match Mods.IntMap.find_option id state.matchings_of_rule with
      | Some [] -> assert false
      | Some l ->
        let (inj,rev_roots) = Tools.list_random state.random_state l in
        Some inj, Tools.array_rev_of_list rev_roots
      | None -> from_patterns () in
  let () =
    if !Parameter.debugModeOn then
      Format.printf "@[On roots:@ @[%a@]@]@."
        (Pp.array Pp.space (fun _ -> Format.pp_print_int)) roots in
  match inj with
  | None -> Clash
  | Some inj ->
    match rule.Primitives.unary_rate with
    | None ->
      let out =
        transform_by_a_rule
          ~get_alg env unary_patterns counter state event_kind rule inj in
      Success (None,out)
    | Some (_,max_distance) ->
      match max_distance with
      | None ->
        if Edges.in_same_connected_component roots.(0) roots.(1) state.edges then
          Corrected
        else
          Success (None,transform_by_a_rule
                     ~get_alg env unary_patterns counter state
                     event_kind rule inj)
      | Some _ ->
        let nodes = Pattern.Matching.elements_with_types
            domain rule.Primitives.connected_components inj in
        match
          Edges.are_connected ?max_distance state.edges nodes.(0) nodes.(1) with
        | None ->
          Success (None,transform_by_a_rule
                     ~get_alg env unary_patterns counter state
                     event_kind rule inj)
        | Some _ -> Corrected

let force_rule
    ~get_alg env unary_patterns counter state event_kind rule =
  match apply_rule
          ~get_alg env unary_patterns counter state event_kind rule with
  | Success (_,out) -> out
  | Corrected -> state (*TODO*)
  | Clash ->
    match all_injections
            ?unary_rate:rule.Primitives.unary_rate
            (Environment.domain env) state.edges
            state.roots_of_patterns rule.Primitives.connected_components with
    | [] -> state
    | l ->
      let (h,_) = Tools.list_random state.random_state l in
      (transform_by_a_rule
         ~get_alg env unary_patterns counter state event_kind rule h)

let adjust_rule_instances ~rule_id ~get_alg store env counter state rule =
  let domain = Environment.domain env in
  let matches =
    all_injections
      ?unary_rate:rule.Primitives.unary_rate domain state.edges
      state.roots_of_patterns rule.Primitives.connected_components in
  let () =
    store_activity
      ~get_alg store env counter state (2*rule_id)
      rule.Primitives.syntactic_rule
      (fst rule.Primitives.rate) (List.length matches) in
  { state with
    matchings_of_rule =
      Mods.IntMap.add rule_id matches state.matchings_of_rule }

let adjust_unary_rule_instances ~rule_id ~get_alg store env counter state rule =
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
                     Pattern.Matching.reconstruct
                       domain state.edges Pattern.Matching.empty 0
                       pattern1 root1 in
                   match inj1 with
                   | None -> out
                   | Some inj ->
                     match Pattern.Matching.reconstruct
                             domain state.edges inj 1 pattern2 root2 with
                     | None -> out
                     | Some inj' ->
                       let max_distance = match rule.Primitives.unary_rate with
                         | None -> None
                         | Some (_, dist_opt) -> dist_opt in
                       match max_distance with
                       | None -> (inj',None)::list,succ len
                       | Some _ ->
                         let nodes =
                           Pattern.Matching.elements_with_types
                             domain rule.Primitives.connected_components inj' in
                         match Edges.are_connected ?max_distance
                                 state.edges nodes.(0) nodes.(1) with
                         | None -> out
                         | Some _ as p -> (inj',p)::list,succ len)
                set2)
           set1 out)
      (Pattern.Map.find_default
         Mods.IntMap.empty pattern1 state.roots_of_unary_patterns)
      (Pattern.Map.find_default
         Mods.IntMap.empty pattern2 state.roots_of_unary_patterns)
      ([],0) in
  let () =
    store_activity
      ~get_alg store env counter state (2*rule_id+1)
      rule.Primitives.syntactic_rule (fst rule.Primitives.rate) len in
  { state with
    unary_candidates =
      if len = 0 then Mods.IntMap.remove rule_id state.unary_candidates
      else Mods.IntMap.add rule_id cands state.unary_candidates;
  }

let incorporate_extra_pattern domain state pattern =
  if Pattern.Map.mem pattern state.roots_of_patterns
  then state
  else {state with
        roots_of_patterns =
          Pattern.Map.add
            pattern
            (Pattern.Matching.roots_of domain state.edges pattern)
            state.roots_of_patterns}

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
  match state.story_machinery with
  | None -> state
  | Some (comp,tpattern,x) ->
    let tpattern' =
      Array.fold_left
        (fun tpattern pattern ->
           let acc = Pattern.Map.find_default [] pattern tpattern in
           Pattern.Map.add
             pattern ((event_kind,patterns,tests)::acc) tpattern)
        tpattern patterns in
    { state with story_machinery = Some (comp,tpattern',x) }

let remove_tracked patterns state =
  match state.story_machinery with
  | None -> state
  | Some (comp,tpattern,x) ->
    let tester (_,el,_) =
      not @@
      Tools.array_fold_lefti
        (fun i b x -> b && Pattern.is_equal_canonicals x el.(i))
        true patterns in
    let tpattern' =
      Array.fold_left
        (fun tpattern pattern ->
           let acc = Pattern.Map.find_default [] pattern tpattern in
           match List.filter tester acc with
           | [] -> Pattern.Map.remove pattern tpattern
           | l -> Pattern.Map.add pattern l tpattern)
        tpattern patterns in
    { state with story_machinery = Some (comp,tpattern',x) }

let generate_stories state =
  Tools.option_map
    (fun (comp,_,steps) -> (comp,List.rev steps)) state.story_machinery

let get_random_state state = state.random_state
