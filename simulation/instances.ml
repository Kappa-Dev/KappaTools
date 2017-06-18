(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  (* With rectangular approximation *)
  roots_of_patterns: IntCollection.t Pattern.ObsMap.t;
  roots_of_unary_patterns:
    Mods.IntSet.t Mods.IntMap.t Pattern.ObsMap.t;

  (* Without rectangular approximation *)
  matchings_of_rule:
    (Matching.t * int list) list Mods.IntMap.t;
  unary_candidates:
    (Matching.t * Edges.path option) list Mods.IntMap.t;

  nb_rectangular_instances_by_cc: ValMap.t Mods.IntMap.t;
}

let size_rin state pattern =
  IntCollection.size (Pattern.ObsMap.get state.roots_of_patterns pattern)
let number state patterns =
    Array.fold_left
      (fun acc pattern ->  acc * (size_rin state  pattern)) 1 patterns


let empty env = {
    roots_of_patterns = Pattern.Env.new_obs_map
        (Model.domain env) (fun _ -> IntCollection.create 64);
    roots_of_unary_patterns = Pattern.Env.new_obs_map
        (Model.domain env) (fun _ -> Mods.IntMap.empty);
    matchings_of_rule = Mods.IntMap.empty;
    unary_candidates = Mods.IntMap.empty;
    nb_rectangular_instances_by_cc = Mods.IntMap.empty;
  }

let incorporate_extra_pattern state pattern matchings =
  if IntCollection.is_empty
      (Pattern.ObsMap.get state.roots_of_patterns pattern) then
    Pattern.ObsMap.set
      state.roots_of_patterns
      pattern matchings

let add_intset_in_intmap id set map =
  if Mods.IntSet.is_empty set
  then Mods.IntMap.remove id map
  else Mods.IntMap.add id set map

let break_apart_cc state edges mod_conn = function
  | None -> state
  | Some (origin_cc,new_cc) ->
    let () = Hashtbl.replace mod_conn origin_cc () in
    let () = Hashtbl.replace mod_conn new_cc () in
    { state with
      roots_of_unary_patterns =
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
          state.roots_of_unary_patterns
    }

let merge_cc state mod_connectivity = function
  | None -> state
  | Some (cc1,cc2) ->
    let () = Hashtbl.replace mod_connectivity cc1 () in
    let () = Hashtbl.replace mod_connectivity  cc2 () in
    { state with
      roots_of_unary_patterns =
        Pattern.ObsMap.map
          (fun cc_map ->
             let set1 = Mods.IntMap.find_default Mods.IntSet.empty cc1 cc_map in
             match Mods.IntMap.pop cc2 cc_map with
             | None,_ -> cc_map
             | Some set2, cc_map' ->
               add_intset_in_intmap cc1 (Mods.IntSet.union set1 set2) cc_map')
          state.roots_of_unary_patterns
    }

let update_roots state is_add unary_ccs edges mod_connectivity pattern root =
  let va = Pattern.ObsMap.get state.roots_of_patterns pattern in
  let () =
    (if is_add then IntCollection.add else IntCollection.remove) root va in
  if Pattern.Set.mem pattern unary_ccs then
    let cc_map =
      Pattern.ObsMap.get state.roots_of_unary_patterns pattern in
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
    Pattern.ObsMap.set state.roots_of_unary_patterns pattern cc_map'

let compute_unary_number state modified_cc rule cc =
  let map1 =
    Pattern.ObsMap.get state.roots_of_unary_patterns
      rule.Primitives.connected_components.(0) in
  let map2 =
    Pattern.ObsMap.get state.roots_of_unary_patterns
      rule.Primitives.connected_components.(1) in
  let old_pack =
    Mods.IntMap.find_default
      ValMap.empty cc state.nb_rectangular_instances_by_cc in
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
      Mods.IntMap.remove cc state.nb_rectangular_instances_by_cc
    else Mods.IntMap.add cc new_pack state.nb_rectangular_instances_by_cc in
  va,
  match Mods.IntMap.pop cc state.unary_candidates with
  | None,_ -> { state with nb_rectangular_instances_by_cc }
  | Some _, unary_candidates ->
    { state with nb_rectangular_instances_by_cc; unary_candidates; }

let pop_exact_matchings state rule =
  match Mods.IntMap.pop rule state.matchings_of_rule with
  | None,_ -> state
  | Some _, match' -> { state with matchings_of_rule = match' }

let pick_an_unary_instance state random_state domain edges ~rule_id rule =
  match Mods.IntMap.find_option rule_id state.unary_candidates with
  | Some l ->
    let inj,path = List_util.random random_state l in Some inj,path
  | None ->
    let map1 =
      Pattern.ObsMap.get state.roots_of_unary_patterns
        rule.Primitives.connected_components.(0) in
    let map2 =
      Pattern.ObsMap.get state.roots_of_unary_patterns
        rule.Primitives.connected_components.(1) in
    let cc_id = ValMap.random
        random_state
        (Mods.IntMap.find_default
           ValMap.empty rule_id state.nb_rectangular_instances_by_cc) in
    let root1 =
      Option_util.unsome (-1)
        (Mods.IntSet.random random_state
           (Mods.IntMap.find_default Mods.IntSet.empty cc_id map1)) in
    let root2 =
      Option_util.unsome (-1)
        (Mods.IntSet.random random_state
           (Mods.IntMap.find_default Mods.IntSet.empty cc_id map2)) in
    let () =
      if !Parameter.debugModeOn then
        Format.printf "@[On roots:@ %i@ %i@]@." root1 root2 in
    let pattern1 = rule.Primitives.connected_components.(0) in
    let pattern2 = rule.Primitives.connected_components.(1) in
    let inj1 =
      Matching.reconstruct domain edges Matching.empty 0 pattern1 root1 in
    match inj1 with
    | None -> None,None
    | Some inj -> Matching.reconstruct domain edges inj 1 pattern2 root2,None

let pick_an_instance state random_state domain edges ?rule_id rule =
  let from_patterns () =
    Tools.array_fold_lefti
      (fun id inj_rev_roots pattern ->
         match inj_rev_roots with
         | None -> None
         | Some (inj,rev_roots) ->
           match
             IntCollection.random random_state
               (Pattern.ObsMap.get state.roots_of_patterns pattern) with
           | None -> None
           | Some root ->
             match Matching.reconstruct
                     domain edges inj id pattern root with
             | None -> None
             | Some inj' -> Some (inj',root::rev_roots))
      (Some (Matching.empty,[]))
      rule.Primitives.connected_components in
  match rule_id with
  | None -> from_patterns ()
  | Some id ->
    match Mods.IntMap.find_option id state.matchings_of_rule with
    | Some [] -> None
    | Some l -> Some (List_util.random random_state l)
    | None -> from_patterns ()

let all_injections ?excp ?unary_rate state domain edges patterna =
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
           | (Some _ | None) ->
             Pattern.ObsMap.get state.roots_of_patterns pattern, excp in
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

let adjust_rule_instances ~rule_id ?unary_rate state domain edges ccs =
  let matches = all_injections state ?unary_rate domain edges ccs in
  List.length matches,
  { state with
    matchings_of_rule =
      Mods.IntMap.add rule_id matches state.matchings_of_rule }

let adjust_unary_rule_instances ~rule_id ?max_distance state domain edges ccs=
  let pattern1 = ccs.(0) in let pattern2 = ccs.(1) in
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
                       domain edges Matching.empty 0 pattern1 root1 in
                   match inj1 with
                   | None -> out
                   | Some inj ->
                     match Matching.reconstruct
                             domain edges inj 1 pattern2 root2 with
                     | None -> out
                     | Some inj' ->
                       match max_distance with
                       | None -> (inj',None)::list,succ len
                       | Some _ ->
                         let nodes =
                           Matching.elements_with_types domain ccs inj' in
                         match Edges.are_connected ?max_distance
                                 edges nodes.(0) nodes.(1) with
                         | None -> out
                         | Some _ as p -> (inj',p)::list,succ len)
                set2)
           set1 out)
      (Pattern.ObsMap.get state.roots_of_unary_patterns pattern1)
      (Pattern.ObsMap.get state.roots_of_unary_patterns pattern2)
      ([],0) in
  len,
  { state with
    unary_candidates =
      if len = 0 then Mods.IntMap.remove rule_id state.unary_candidates
      else Mods.IntMap.add rule_id cands state.unary_candidates;
  }

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

let debug_print f state =
  let () = print_injections ?domain:None f state.roots_of_patterns in
  print_unary_injections ?domain:None f state.roots_of_unary_patterns