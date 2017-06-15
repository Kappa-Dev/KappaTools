(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type precomputed =
  {
    unary_patterns: Pattern.Set.t;
    always_outdated: Operator.DepSet.t;
  }

type t =
  {
    mutable outdated : bool;

    precomputed: precomputed;

    (* With rectangular approximation *)
    roots_of_patterns: IntCollection.t Pattern.ObsMap.t;
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
    nb_rectangular_instances_by_cc: ValMap.t Mods.IntMap.t;
    outdated_elements: Operator.DepSet.t * (int,unit) Hashtbl.t;

    random_state : Random.State.t;
    story_machinery :
      (string (*obs name*) * Pattern.id array *
        Instantiation.abstract Instantiation.test list list)
         list Pattern.ObsMap.t (*currently tracked ccs *) option;

    species :
      (string (*filename*) * Pattern.id array (*with only one pattern*) *
        Instantiation.abstract Instantiation.test list list) list
        Pattern.ObsMap.t;
  }

type result = Clash | Corrected | Success of t

let raw_get_alg env overwr i =
  match overwr.(i) with
  | None -> Model.get_alg env i
  | Some expr -> expr

let size_rin state pattern =
  IntCollection.size (Pattern.ObsMap.get state.roots_of_patterns pattern)
let raw_instance_number state patterns_l =
  let rect_approx patterns =
    Array.fold_left
      (fun acc pattern ->  acc * (size_rin state  pattern)) 1 patterns in
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

let empty ~with_trace random_state env counter =
  let unary_patterns =
    Model.fold_rules
      (fun _ acc r ->
         match r.Primitives.unary_rate with
         | None -> acc
         | Some _ ->
           Pattern.Set.add
             r.Primitives.connected_components.(0)
             (Pattern.Set.add r.Primitives.connected_components.(1) acc)
      ) Pattern.Set.empty env in
  let always_outdated =
    let (deps_in_t,deps_in_e,_,_) = Model.all_dependencies env in
        Operator.DepSet.union deps_in_t deps_in_e in
  let with_connected_components = not (Pattern.Set.is_empty unary_patterns) in
  let variables_overwrite = Array.make (Model.nb_algs env) None in
  let variables_cache = Array.make (Model.nb_algs env) Nbr.zero in
  let cand =
    {
      outdated = false;
      precomputed = { unary_patterns; always_outdated};
      roots_of_patterns = Pattern.Env.new_obs_map
          (Model.domain env) (fun _ -> IntCollection.create 64);
      roots_of_unary_patterns = Pattern.Env.new_obs_map
          (Model.domain env) (fun _ -> Mods.IntMap.empty);
      matchings_of_rule = Mods.IntMap.empty;
      unary_candidates = Mods.IntMap.empty;
      variables_overwrite; variables_cache;
      edges = Edges.empty ~with_connected_components;
      tokens = Array.make (Model.nb_tokens env) Nbr.zero;
      outdated_elements = always_outdated,Hashtbl.create 32;
      nb_rectangular_instances_by_cc = Mods.IntMap.empty;
      random_state;
      story_machinery =
        if with_trace then
          Some (Pattern.Env.new_obs_map
                  (Model.domain env) (fun _ -> []))
        else None;
      species = Pattern.Env.new_obs_map
                  (Model.domain env) (fun _ -> []);
    } in
  let () = Tools.iteri (recompute env counter cand) (Model.nb_algs env) in
  cand

let print_injections ?domain f roots_of_patterns =
  Format.fprintf
    f "@[<v>%a@]"
    (Pattern.ObsMap.print Pp.space
       (fun pattern f roots ->
          Format.fprintf
            f "@[# @[%a@] ==>@ @[%a@]@]"
            (Pattern.print ~new_syntax:true ?domain ~with_id:true) pattern
            IntCollection.print roots
       )
    ) roots_of_patterns
let print_unary_injections ?domain f roots_of_patterns =
  Format.fprintf
    f "@[<v>%a@]"
    (Pattern.ObsMap.print Pp.space
       (fun pattern f root_maps ->
          Format.fprintf
            f "@[# @[%a@] ==>@ @[%a@]@]"
            (Pattern.print ~new_syntax:true ?domain ~with_id:true) pattern
            (Pp.set Mods.IntMap.bindings Pp.space
               (fun f (_cc_id, roots) -> Mods.IntSet.print f roots))
            root_maps
       )
    ) roots_of_patterns

type stats = { mixture_stats : Edges.stats }

let stats state = {
  mixture_stats = Edges.stats state.edges;
}

let print_stats f state =
  Format.fprintf f "%i agents" (stats state).mixture_stats.Edges.nb_agents

let add_intset_in_intmap id set map =
  if Mods.IntSet.is_empty set
  then Mods.IntMap.remove id map
  else Mods.IntMap.add id set map

let update_roots
    is_add unary_ccs edges map unary_map mod_connectivity pattern root =
  let va = Pattern.ObsMap.get map pattern in
  let () =
    (if is_add then IntCollection.add else IntCollection.remove) root va in
  if Pattern.Set.mem pattern unary_ccs then
    let cc_map =
      Pattern.ObsMap.get unary_map pattern in
    let cc_map' =
      match Edges.get_connected_component root edges with
      | Some cc_id ->
        let () = Hashtbl.replace mod_connectivity cc_id () in
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
             let foo = IntCollection.create 1 in
             let () = IntCollection.add root foo in
             foo,None
           | (Some _ | None) -> Pattern.ObsMap.get roots pattern, excp in
         (excp',
          IntCollection.fold
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

let break_apart_cc edges mod_conn roots_of_unary_patterns = function
  | None -> roots_of_unary_patterns
  | Some (origin_cc,new_cc) ->
    let () = Hashtbl.replace mod_conn origin_cc () in
    let () = Hashtbl.replace mod_conn new_cc () in
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

let merge_cc mod_connectivity roots_of_unary_patterns = function
  | None -> roots_of_unary_patterns
  | Some (cc1,cc2) ->
    let () = Hashtbl.replace mod_connectivity cc1 () in
    let () = Hashtbl.replace mod_connectivity  cc2 () in
    Pattern.ObsMap.map
      (fun cc_map ->
         let set1 = Mods.IntMap.find_default Mods.IntSet.empty cc1 cc_map in
         match Mods.IntMap.pop cc2 cc_map with
         | None,_ -> cc_map
         | Some set2, cc_map' ->
           add_intset_in_intmap cc1 (Mods.IntSet.union set1 set2) cc_map')
      roots_of_unary_patterns

let apply_negative_transformation
    mod_connectivity (side_effects,stuff4unaries,edges) = function
  | Primitives.Transformation.Agent (id,_) ->
    let edges' = Edges.remove_agent id edges in
    (side_effects,stuff4unaries,edges')
  | Primitives.Transformation.Freed ((id,_),s) -> (*(n,s)-bottom*)
    let edges' = Edges.remove_free id s edges in
    (side_effects,stuff4unaries,edges')
  | Primitives.Transformation.Linked (((id,_),s),((id',_),s')) ->
    let edges',cc_modif = Edges.remove_link id s id' s' edges in
    (side_effects,break_apart_cc edges' mod_connectivity stuff4unaries cc_modif,edges')
  | Primitives.Transformation.NegativeWhatEver ((id,_),s as n) ->
     begin
       match (List.partition (fun x -> x =n) side_effects) with
       | (_::_,side_effects') -> (side_effects',stuff4unaries,edges)
       | ([],_) ->
          match Edges.link_destination id s edges with
          | None -> (side_effects,stuff4unaries,Edges.remove_free id s edges)
          | Some ((id',_ as nc'),s') ->
             let edges',cc_modif = Edges.remove_link id s id' s' edges in
             ((nc',s')::side_effects,
              break_apart_cc edges' mod_connectivity stuff4unaries cc_modif,
              edges')
     end
  | Primitives.Transformation.PositiveInternalized _ ->
    raise
      (ExceptionDefn.Internal_Error
         (Locality.dummy_annot "PositiveInternalized in negative update"))
  | Primitives.Transformation.NegativeInternalized ((id,_),s) ->
    let _,edges' = Edges.remove_internal id s edges in
    (side_effects,stuff4unaries,edges')

let apply_positive_transformation
    sigs mod_connectivity (inj2graph,side_effects,stuff4unaries,edges) = function
  | Primitives.Transformation.Agent n ->
    let nc, inj2graph',edges' =
      let ty = Matching.Agent.get_type n in
      let id,edges' = Edges.add_agent sigs ty edges in
      (id,ty),new_place id inj2graph n,edges' in
    (inj2graph',side_effects,stuff4unaries,edges'),
    Primitives.Transformation.Agent nc
  | Primitives.Transformation.Freed (n,s) -> (*(n,s)-bottom*)
    let (id,_ as nc) = Matching.Agent.concretize inj2graph n in (*(A,23)*)
    let edges' = Edges.add_free id s edges in
    let side_effects' =
      List_util.smart_filter (fun x -> x <> (nc,s)) side_effects in
    (inj2graph,side_effects',stuff4unaries,edges'),
    Primitives.Transformation.Freed (nc,s)
  | Primitives.Transformation.Linked ((n,s),(n',s')) ->
    let nc = Matching.Agent.concretize inj2graph n in
    let nc' = Matching.Agent.concretize inj2graph n' in
    let edges',modif_cc = Edges.add_link nc s nc' s' edges in
    let side_effects' = List_util.smart_filter
        (fun x -> x<>(nc,s) && x<>(nc',s')) side_effects in
    (inj2graph,side_effects',merge_cc mod_connectivity stuff4unaries modif_cc,edges'),
    Primitives.Transformation.Linked ((nc,s),(nc',s'))
  | Primitives.Transformation.NegativeWhatEver _ ->
    raise
      (ExceptionDefn.Internal_Error
         (Locality.dummy_annot "NegativeWhatEver in positive update"))
  | Primitives.Transformation.PositiveInternalized (n,s,i) ->
    let (id,_ as nc) = Matching.Agent.concretize inj2graph n in
    let edges' = Edges.add_internal id s i edges in
    (inj2graph,side_effects,stuff4unaries,edges'),
    Primitives.Transformation.PositiveInternalized (nc,s,i)
  | Primitives.Transformation.NegativeInternalized _ ->
    raise
      (ExceptionDefn.Internal_Error
         (Locality.dummy_annot "NegativeInternalized in positive update"))

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

let path_tests path tests =
  let known_agents =
    List.fold_left
      (List.fold_left
         (fun acc -> function
            | Instantiation.Is_Here (id,_) -> Mods.IntSet.add id acc
            | Instantiation.Is_Bound_to _ | Instantiation.Is_Bound _
            | Instantiation.Has_Internal _ | Instantiation.Has_Binding_type _
            | Instantiation.Is_Free _ -> acc)) Mods.IntSet.empty tests in
  let pretests =
    List_util.map_option
      (fun (x,y) ->
         if List.for_all
             (List.for_all
                (function
                  | Instantiation.Is_Bound_to (a,b) ->
                    x <> a && x <> b && y<>a && y<>b
                  | Instantiation.Has_Internal _ | Instantiation.Is_Free _
                  | Instantiation.Is_Bound _ | Instantiation.Has_Binding_type _
                  | Instantiation.Is_Here _ -> true)) tests
         then Some (Instantiation.Is_Bound_to (x,y))
         else None) path in
  let _,path_tests =
    List.fold_left
      (fun (ag,te) (((id,_ as a),_),((id',_ as a'),_)) ->
         let ag',te' =
           if Mods.IntSet.mem id ag then ag,te
           else Mods.IntSet.add id ag,Instantiation.Is_Here a::te in
         if Mods.IntSet.mem id' ag' then ag',te'
         else Mods.IntSet.add id' ag',Instantiation.Is_Here a'::te')
      (known_agents,pretests) path in
  path_tests

let step_of_event counter = function
  | Trace.INIT _,e -> (Trace.Init e.Instantiation.actions)
  | Trace.RULE r,x ->
    (Trace.Rule (r,x,Counter.current_simulation_info counter))
  | Trace.PERT p,x ->
    (Trace.Pert (p,x,Counter.current_simulation_info counter))

let store_event counter inj2graph new_tracked_obs_instances event_kind
    ?path extra_side_effects rule outputs = function
  | None -> ()
  | Some _ ->
    let cevent =
      Instantiation.concretize_event inj2graph rule.Primitives.instantiations in
    let full_concrete_event = {
      Instantiation.tests = cevent.Instantiation.tests;
      Instantiation.actions = cevent.Instantiation.actions;
      Instantiation.side_effects_src = cevent.Instantiation.side_effects_src;
      Instantiation.side_effects_dst = List.rev_append
          extra_side_effects cevent.Instantiation.side_effects_dst;
      Instantiation.connectivity_tests =
        (match path with
         | None -> []
         | Some path -> path_tests path cevent.Instantiation.tests);
    } in
    let () =
      outputs (Data.TraceStep
                 (step_of_event counter (event_kind,full_concrete_event))) in
      List.iter
        (fun (i,x) ->
           outputs (Data.TraceStep
                      (Trace.Obs(i,x,Counter.next_story counter))))
        new_tracked_obs_instances

let get_species_obs sigs edges obs acc tracked =
  List.fold_left
    (fun acc (pattern,(root,_)) ->
      try
        List.fold_left
          (fun acc (fn,patterns,_) ->
            if Array.fold_left
                 (fun ok pid ->
                   ((Pattern.compare_canonicals pid pattern) = 0)||ok)
                 false patterns
            then
              let spec = Edges.species sigs root edges in
              (fn,patterns,spec)::acc else acc)
          acc (Pattern.ObsMap.get tracked pattern)
      with Not_found -> acc)
    acc obs

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
                       List.map
                         (List.map (Instantiation.concretize_test
                                      (inj,Mods.IntMap.empty))) tests in
                     (ev,tests') :: acc)
                  acc
                  (all_injections ~excp:(pattern,root) domain edges roots patterns))
             acc (Pattern.ObsMap.get tracked pattern)
         with Not_found -> acc)
      acc obs

let update_edges
      outputs counter domain inj_nodes state event_kind ?path rule sigs =
  let () = assert (not state.outdated) in
  let () = state.outdated <- true in
  let former_deps,mod_connectivity  = state.outdated_elements in
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
      (apply_negative_transformation mod_connectivity)
      ([],(state.roots_of_unary_patterns),state.edges)
      concrete_removed in
  let () =
    List.iter
      (fun (pat,(root,_)) ->
         update_roots false state.precomputed.unary_patterns edges_after_neg
           state.roots_of_patterns roots_by_cc mod_connectivity pat root)
      del_obs in
  (*Positive update*)
  let (final_inj2graph,remaining_side_effects,roots_by_cc',edges'),
      concrete_inserted =
    List.fold_left
      (fun (x,p) h ->
         let (x', h') =
           apply_positive_transformation
             (Pattern.Env.signatures domain) mod_connectivity x h in
         (x',h'::p))
      (((inj_nodes,Mods.IntMap.empty),side_effects,
        roots_by_cc,edges_after_neg),[])
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
         update_roots true state.precomputed.unary_patterns edges''
           state.roots_of_patterns roots_by_cc' mod_connectivity pat root)
      new_obs in
  (*Store event*)
  let new_tracked_obs_instances =
    store_obs domain edges'' state.roots_of_patterns
      new_obs [] state.story_machinery in
  let () =
    store_event
      counter final_inj2graph new_tracked_obs_instances event_kind
      ?path remaining_side_effects rule outputs state.story_machinery in
  (*Print species*)
  let species =
    get_species_obs sigs edges'' new_obs [] state.species in
  let () =
    List.iter
      (fun (file,_,mixture) ->
        outputs (Data.Species
                   (file,(Counter.current_time counter),mixture))) species in
  let rev_deps = Operator.DepSet.union
      former_deps (Operator.DepSet.union del_deps new_deps) in
  { outdated = false;
    precomputed = state.precomputed;
    roots_of_patterns = state.roots_of_patterns;
    roots_of_unary_patterns = roots_by_cc';
    unary_candidates = state.unary_candidates;
    matchings_of_rule = state.matchings_of_rule;
    variables_cache = state.variables_cache;
    variables_overwrite = state.variables_overwrite;
    edges = edges''; tokens = state.tokens;
    outdated_elements = rev_deps,mod_connectivity;
    nb_rectangular_instances_by_cc = state.nb_rectangular_instances_by_cc;
    random_state = state.random_state;
    story_machinery = state.story_machinery;
    species = state.species;
  }

let max_dist_to_int counter state d =
  Nbr.to_int (value_alg counter state d)

let store_activity store env counter state id syntax_id rate cc_va =
  let () =
    if !Parameter.debugModeOn then
      Format.printf "@[%sule %a has now %i instances.@]@."
        (if id mod 2 = 1 then "Unary r" else "R")
        (Model.print_rule ~env) (id/2) cc_va in
  let act =
    match Nbr.to_float @@ value_alg counter state rate with
    | None -> if cc_va = 0 then 0. else infinity
    | Some rate -> rate *. float_of_int cc_va in
  let () =
    if act < 0. then
      let unary = id mod 2 = 1 in
      raise
        (ExceptionDefn.Malformed_Decl
           ((Format.asprintf
               "At t=%.2f %sctivity of rule %a has become negative (%f)"
               (Counter.current_time counter)
               (if unary then "Unary " else "")
               (Model.print_rule ~env) id act),
            Model.get_ast_rule_rate_pos ~unary env syntax_id)) in
  store id syntax_id act

let update_outdated_activities store env counter state =
  let () = assert (not state.outdated) in
  let deps,changed_connectivity = state.outdated_elements in
  let unary_rule_update modified_cc i state rule =
    match rule.Primitives.unary_rate with
    | None -> state
    | Some (unrate, _) ->
      let map1 =
        Pattern.ObsMap.get state.roots_of_unary_patterns
          rule.Primitives.connected_components.(0) in
      let map2 =
        Pattern.ObsMap.get state.roots_of_unary_patterns
          rule.Primitives.connected_components.(1) in
      let old_pack =
        Mods.IntMap.find_default
          ValMap.empty i state.nb_rectangular_instances_by_cc in
      let new_pack =
        Hashtbl.fold
          (fun cc () i_inst ->
             let set1 = Mods.IntMap.find_default Mods.IntSet.empty cc map1 in
             let set2 = Mods.IntMap.find_default Mods.IntSet.empty cc map2 in
             let new_v = Mods.IntSet.size set1 * Mods.IntSet.size set2 in
             if new_v = 0 then ValMap.remove cc i_inst
             else ValMap.add cc new_v i_inst)
          modified_cc old_pack in
      let va = ValMap.total new_pack in
      let nb_rectangular_instances_by_cc =
        if va = 0 then
          Mods.IntMap.remove i state.nb_rectangular_instances_by_cc
        else Mods.IntMap.add i new_pack state.nb_rectangular_instances_by_cc in
      let () =
        store_activity
          store env counter state (2*i+1)
          rule.Primitives.syntactic_rule (fst unrate) va in
      match Mods.IntMap.pop i state.unary_candidates with
           | None,_ -> { state with nb_rectangular_instances_by_cc }
           | Some _, unary_candidates ->
             { state with nb_rectangular_instances_by_cc; unary_candidates; } in
  let rec aux dep acc =
    Operator.DepSet.fold
      (fun dep (state,perts as acc) ->
         match dep with
         | Operator.ALG j ->
           let () = recompute env counter state j in
           aux (Model.get_alg_reverse_dependencies env j) acc
         | Operator.PERT p ->
           (state,List_util.merge_uniq Mods.int_compare [p] perts)
         | Operator.RULE i ->
           let rule = Model.get_rule env i in
           let pattern_va = raw_instance_number
               state [rule.Primitives.connected_components] in
           let () =
             store_activity store env counter state (2*i)
               rule.Primitives.syntactic_rule
               (fst rule.Primitives.rate) pattern_va in
           ((match Mods.IntMap.pop i state.matchings_of_rule with
               | None,_ -> state
               | Some _, match' ->
                 { state with matchings_of_rule = match' }),perts))
      dep acc in
  let state',perts = aux deps (state,[]) in
  let state'' =
    if Hashtbl.length changed_connectivity = 0 then state'
    else Model.fold_rules (unary_rule_update changed_connectivity) state' env in
  ({state'' with
    outdated_elements =
      state.precomputed.always_outdated,Hashtbl.create 32},perts)

let overwrite_var i counter state expr =
  let rdeps,changed_connectivity = state.outdated_elements in
  let () =
    state.variables_overwrite.(i) <-
      Some (Alg_expr.CONST (value_alg counter state expr)) in
  {state with
   outdated_elements =
     (Operator.DepSet.add (Operator.ALG i) rdeps,changed_connectivity)}

let update_tokens env counter state injected =
  let injected' = List.rev_map
      (fun  ((expr,_),i) -> (value_alg counter state expr,i)) injected in
  List.fold_left
    (fun st (va,i) ->
       let () = st.tokens.(i) <- Nbr.add st.tokens.(i) va in
       let deps' = Model.get_token_reverse_dependencies env i in
       if Operator.DepSet.is_empty deps' then st
       else
         let rdeps,changed_connectivity = st.outdated_elements in
         { st with outdated_elements =
                     Operator.DepSet.union rdeps deps',changed_connectivity }
    ) state injected'

let transform_by_a_rule outputs env counter state event_kind ?path rule inj =
  let state' =
    update_tokens
      env counter state rule.Primitives.delta_tokens in
  update_edges outputs counter (Model.domain env) inj
    state' event_kind ?path rule (Model.signatures env)

let apply_unary_rule ~outputs ~rule_id env counter state event_kind rule =
  let () = assert (not state.outdated) in
  let domain = Model.domain env in
  let inj,path =
    match Mods.IntMap.find_option rule_id state.unary_candidates with
    | Some l ->
      let inj,path = List_util.random state.random_state l in Some inj,path
    | None ->
      let map1 =
        Pattern.ObsMap.get state.roots_of_unary_patterns
          rule.Primitives.connected_components.(0) in
      let map2 =
        Pattern.ObsMap.get state.roots_of_unary_patterns
           rule.Primitives.connected_components.(1) in
      let cc_id = ValMap.random
          state.random_state
          (Mods.IntMap.find_default
             ValMap.empty rule_id state.nb_rectangular_instances_by_cc) in
      let root1 =
        Option_util.unsome (-1)
          (Mods.IntSet.random state.random_state
             (Mods.IntMap.find_default Mods.IntSet.empty cc_id map1)) in
      let root2 =
        Option_util.unsome (-1)
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
    | Some _ ->
      Success (transform_by_a_rule
                 outputs env counter state' event_kind ?path rule inj)
    | None ->
      let max_distance = match rule.Primitives.unary_rate with
        | None -> None
        | Some (_, dist_opt) ->
           (match dist_opt with
           | None -> None
           | Some d -> Some (max_dist_to_int counter state' d)) in
      match Edges.are_connected ?max_distance state.edges nodes.(0) nodes.(1) with
      | None -> Corrected
      | Some _ as path ->
        Success (transform_by_a_rule
                   outputs env counter state' event_kind ?path rule inj)

let apply_rule ~outputs ?rule_id env counter state event_kind rule =
  let () = assert (not state.outdated) in
  let domain = Model.domain env in
  let from_patterns () =
    Tools.array_fold_lefti
      (fun id inj_rev_roots pattern ->
         match inj_rev_roots with
         | None -> None
         | Some (inj,rev_roots) ->
           match
             IntCollection.random state.random_state
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
      | Some l -> Some (List_util.random state.random_state l)
      | None -> from_patterns () in
  match inj_roots with
  | None -> Clash
  | Some (inj,rev_roots) ->
    let () =
      if !Parameter.debugModeOn then
        let roots = Tools.array_rev_of_list rev_roots in
        Format.printf "@[On roots:@ @[%a@]@]@."
          (Pp.array Pp.space (fun _ -> Format.pp_print_int)) roots in
    match rule.Primitives.unary_rate with
    | None ->
      let out =
        transform_by_a_rule outputs env counter state event_kind rule inj in
      Success out
    | Some (_,max_distance) ->
      match max_distance with
      | None ->
        (match rev_roots with
         | root1 :: root0 :: [] ->
           if Edges.in_same_connected_component root0 root1 state.edges then
             Corrected
           else
             Success
               (transform_by_a_rule outputs env counter state event_kind rule inj)
         | _ -> failwith "apply_given_rule unary rule without 2 patterns")
      | Some dist ->
         let dist' = Some (max_dist_to_int counter state dist) in
         let nodes = Matching.elements_with_types
                       domain rule.Primitives.connected_components inj in
         match
           Edges.are_connected ?max_distance:dist' state.edges nodes.(0)
                               nodes.(1) with
         | None ->
           Success (transform_by_a_rule
                      outputs env counter state event_kind rule inj)
         | Some _ -> Corrected

let force_rule ~outputs env counter state event_kind rule =
  match apply_rule ~outputs env counter state event_kind rule with
  | Success out -> Some out
  | Corrected | Clash ->
    let () = assert (not state.outdated) in
    let unary_rate = match rule.Primitives.unary_rate with
      | None -> None
      | Some (loc, dist_opt) ->
         (match dist_opt with
          | None -> Some (loc,None)
          | Some d ->
             Some (loc,Some (max_dist_to_int counter state d))) in
    match all_injections
            ?unary_rate (Model.domain env) state.edges
            state.roots_of_patterns rule.Primitives.connected_components with
    | [] ->
      let () =
        ExceptionDefn.warning
          (fun f -> Format.fprintf f "At t=%f, %a does not apply (anymore)"
              (Counter.current_time counter)
              (Trace.print_event_kind ~env) event_kind) in
        None
    | l ->
       let (h,_) = List_util.random state.random_state l in
       Some (transform_by_a_rule
               outputs env counter state event_kind rule h)

let adjust_rule_instances ~rule_id store env counter state rule =
  let () = assert (not state.outdated) in
  let domain = Model.domain env in
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
  let domain = Model.domain env in
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
    if IntCollection.is_empty
        (Pattern.ObsMap.get state.roots_of_patterns pattern) then
      Pattern.ObsMap.set
        state.roots_of_patterns
        pattern
        (Matching.roots_of domain state.edges pattern) in
  { state with outdated = false }

let snapshot env counter fn state = {
  Data.snapshot_file = fn;
  Data.snapshot_event = Counter.current_event counter;
  Data.snapshot_time = Counter.current_time counter;
  Data.snapshot_agents =
    Edges.build_snapshot (Model.signatures env) state.edges;
  Data.snapshot_tokens = Array.mapi (fun i x ->
      (Format.asprintf "%a" (Model.print_token ~env) i,x)) state.tokens;
}

let print env f state =
  let sigs = Model.signatures env in
  Format.fprintf
    f "@[<v>%a@,%a@]"
    (Pp.list Pp.space (fun f (i,mix) ->
         Format.fprintf f "%%init: %i @[<h>%a@]" i
           (Raw_mixture.print ~new_syntax:false ~compact:false ~created:false ~sigs)
           mix))
    (Edges.build_snapshot sigs state.edges)
    (Pp.array Pp.space (fun i f el ->
         Format.fprintf
           f "%%init: %a <- %a"
           (Model.print_token ~env) i Nbr.print el))
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

let aux_add_tracked patterns name tests state tpattern =
  let () = state.outdated <- true in
  let () =
    Array.iter
      (fun pattern ->
        let acc = Pattern.ObsMap.get tpattern pattern in
        Pattern.ObsMap.set tpattern
                           pattern ((name,patterns,tests)::acc))
      patterns in
  { state with outdated = false }

let add_tracked patterns name tests state =
  let () = assert (not state.outdated) in
  match state.story_machinery with
  | None ->
    let () =
      ExceptionDefn.warning
        (fun f -> Format.fprintf f
            "Observable %s should be tracked but the trace is not stored"
            name) in
    state
  | Some tpattern -> aux_add_tracked patterns name tests state tpattern
let remove_tracked patterns name state =
  let () = assert (not state.outdated) in
  match state.story_machinery with
  | None -> state
  | Some tpattern ->
     match name with
     | None ->
        let () = state.outdated <- true in
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
     | Some name ->
        let () = state.outdated <- true in
        let tester (n,_,_) = not((String.compare name n) = 0) in
        let tpattern' =
          Pattern.ObsMap.map
            (fun plist -> List.filter tester plist) tpattern in
        { state with outdated = false; story_machinery = Some tpattern' }

let add_tracked_species patterns name tests state =
  aux_add_tracked patterns name tests state state.species

let remove_tracked_species name state =
  let () = state.outdated <- true in
  let tester (n,_,_) = not((String.compare name n) = 0) in
  let species' =
    Pattern.ObsMap.map
      (fun plist -> List.filter tester plist) state.species in
  { state with outdated = false; species = species' }

let get_random_state state = state.random_state
