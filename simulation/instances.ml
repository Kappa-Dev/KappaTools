(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(* Support for picking, counting, enumerating is needed *)

type t' = My_instances.t

type t = {
  (* With rectangular approximation *)
  roots_of_patterns: IntCollection.t Pattern.ObsMap.t;

  (* Observable -> cc -> root *)
  roots_of_unary_patterns:
    Mods.IntSet.t Mods.IntMap.t Pattern.ObsMap.t;

  (* Cache for roots_of_patterns: rule -> cc -> number_instances (activity per cc) *)
  nb_rectangular_instances_by_cc: ValMap.t Mods.IntMap.t;
}

type message = unit

let send_message _ st = st

let size_rin state pattern =
  IntCollection.size (Pattern.ObsMap.get state.roots_of_patterns pattern)
let number state patterns =
    Array.fold_left
      (fun acc pattern ->  acc * (size_rin state pattern)) 1 patterns

let empty env = {
    roots_of_patterns = Pattern.Env.new_obs_map
        (Model.domain env) (fun _ -> IntCollection.create 64);
    roots_of_unary_patterns = Pattern.Env.new_obs_map
        (Model.domain env) (fun _ -> Mods.IntMap.empty);
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


(*  Break apart connected component:
    Update "roots of unary patterns"
    Easy, I should not have to rewrite this.
    Should caches be handled at this level ? I do nt think so
    and I will probably clean this.
*)
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

(* Same: not very subtle. You just propagate. *)
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

(* Most of the code is to deal with unary_instances.
   Does nothing fancy.
   Also takes the cache as an argument *)
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










(* Auxiliary *)
let number_of_unary_instances_in_cc st (pat1, pat2) = 
  let map1 = Pattern.ObsMap.get st.roots_of_unary_patterns pat1 in
  let map2 = Pattern.ObsMap.get st.roots_of_unary_patterns pat2 in
  fun cc -> 
    let set1 = Mods.IntMap.find_default Mods.IntSet.empty cc map1 in
    let set2 = Mods.IntMap.find_default Mods.IntSet.empty cc map2 in
    Mods.IntSet.size set1 * Mods.IntSet.size set2


let compute_unary_number state modified_ccs rule rule_id =

  let pat1 = rule.Primitives.connected_components.(0) in
  let pat2 = rule.Primitives.connected_components.(1) in

  let number_of_unary_instances_in_cc = 
    number_of_unary_instances_in_cc state (pat1, pat2) in

  let old_pack =
    Mods.IntMap.find_default
      ValMap.empty rule_id state.nb_rectangular_instances_by_cc in
  let new_pack =
    Hashtbl.fold
      (fun cc () i_inst ->
         let new_v = number_of_unary_instances_in_cc cc in
         if new_v = 0 then ValMap.remove cc i_inst
         else ValMap.add cc new_v i_inst)
      modified_ccs old_pack in
  let va = ValMap.total new_pack in
  let nb_rectangular_instances_by_cc =
    if va = 0 then
      Mods.IntMap.remove rule_id state.nb_rectangular_instances_by_cc
    else Mods.IntMap.add rule_id new_pack state.nb_rectangular_instances_by_cc in
  va, { state with nb_rectangular_instances_by_cc }










let pick_unary_instance_in_cc st random_state (pat1, pat2) =
  let map1 = Pattern.ObsMap.get st.roots_of_unary_patterns pat1 in
  let map2 = Pattern.ObsMap.get st.roots_of_unary_patterns pat2 in
  fun cc ->
    let root1 =
      Option_util.unsome (-1)
        (Mods.IntSet.random random_state
            (Mods.IntMap.find_default Mods.IntSet.empty cc map1)) in
    let root2 =
      Option_util.unsome (-1)
        (Mods.IntSet.random random_state
            (Mods.IntMap.find_default Mods.IntSet.empty cc map2)) in
    (root1, root2)


(* Needs picking for each component *)
let pick_a_unary_instance state random_state domain edges ~rule_id rule =
  
  let pat1 = rule.Primitives.connected_components.(0) in
  let pat2 = rule.Primitives.connected_components.(1) in

  let pick_unary_instance_in_cc = 
      pick_unary_instance_in_cc state random_state (pat1, pat2) in

  let cc_id = ValMap.random
      random_state
      (Mods.IntMap.find_default
          ValMap.empty rule_id state.nb_rectangular_instances_by_cc) in
  let root1, root2 = pick_unary_instance_in_cc cc_id in
   
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







let fold_picked_instance st random_state pats ~init f = 
  let rec aux i acc =
    if i >= Array.length pats then acc else
    match acc with
      | None -> None
      | Some acc -> 
        let pat = pats.(i) in
        let root_opt = IntCollection.random random_state
          (Pattern.ObsMap.get st.roots_of_patterns pat) in
          begin match root_opt with
          | None -> None
          | Some root ->
            let acc = f i pat root acc in
            aux (i+1) acc
          end
  in aux 0 (Some init)


let pick_an_instance state random_state domain edges pats = 
  fold_picked_instance state random_state pats ~init:(Matching.empty,[])
    (fun id pattern root (inj, rev_roots) ->
      match Matching.reconstruct domain edges inj id pattern root with
      | None -> None
      | Some inj' -> Some (inj',root::rev_roots)
    )







let process_excp pats = function
  | None -> (fun _ -> false), (-1)
  | Some (pat, root) ->
    let fixed_is = 
        pats
        |> Array.to_list
        |> List.mapi (fun i x -> (i, x))
        |> List.filter (fun (_, pat') -> Pattern.is_equal_canonicals pat pat')
        |> List.map fst in
    let sent_to_fixed_root j = List.mem j fixed_is in
    sent_to_fixed_root, root

  (* This is an inefficient and weird version for back-compatibility*)
  let fold_instances ?excp st pats ~init f =

    let sent_to_excp_root, excp_root = process_excp pats excp in

    let instances = 
      pats |> Tools.array_fold_lefti (fun i instances pat ->
        let candidates = 
          if sent_to_excp_root i then
            let c = IntCollection.create 1 in
            let () = IntCollection.add excp_root c in
            c
          else 
            Pattern.ObsMap.get st.roots_of_patterns pat in

        IntCollection.fold (fun root new_instances ->
          instances |> List.fold_left (fun new_instances instance ->
            (root :: instance) :: new_instances
          ) new_instances
        ) candidates []
      ) [[]] in
    (* Injections have been generated in the reverse order *)
    let instances = List.map List.rev instances in

    instances |> List.fold_left (fun acc instance ->
      f (Array.of_list instance) acc
    ) init


let injection_to_matching domain edges instance patterns =
  let rec aux i matching roots =
    match matching, roots with
    | None, _ -> None
    | Some matching, [] -> Some matching
    | Some matching, root::next_roots ->
      let new_acc = 
        Matching.reconstruct 
          domain edges matching i patterns.(i) root in
      aux (i+1) new_acc next_roots in
  aux 0 (Some Matching.empty) instance

let rec flatten_option_list = function
  | [] -> []
  | Some x :: xs -> x :: flatten_option_list xs
  | None :: xs -> flatten_option_list xs

(* Get rid of the rectangular approximation for nonunary rules.
   There is real work to do here. We just want to change the fold. 
   Defines a fold function that enumerate every instance.
 *)
let all_injections ?excp ?unary_rate state domain edges patterna =
  let out =
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
                    | Some new_inj -> (new_inj,root::roots) :: corrects
                  )
                 new_injs inj_list)
            cands [])
        )
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


(*


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


*)



let map_fold2 map1 map2 ~init f =
  Mods.IntMap.monadic_fold2_sparse () () 
    (fun () () key x1 x2 acc -> (), f key x1 x2 acc)
    map1 map2 init
  |> snd

let fold_unary_instances st (pat1, pat2) ~init f =
  let map1 = Pattern.ObsMap.get st.roots_of_unary_patterns pat1 in
  let map2 = Pattern.ObsMap.get st.roots_of_unary_patterns pat2 in
  map_fold2 map1 map2 ~init (fun _ set1 set2 acc ->
    Mods.IntSet.fold (fun root1 acc ->
      Mods.IntSet.fold (fun root2 acc ->
        f (root1, root2) acc
      ) set2 acc
    ) set1 acc
  )


let update_unary_candidates ~rule_id ?max_distance state domain edges ccs unary_candidates =
  let pattern1 = ccs.(0) in let pattern2 = ccs.(1) in
  let cands,len = fold_unary_instances state (pattern1, pattern2) ~init:([], 0)
    (fun (root1, root2) (list,len as out) ->
      let inj1 = Matching.reconstruct domain edges Matching.empty 0 pattern1 root1 in
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
            | Some _ as p -> (inj',p)::list,succ len
    ) in
  len,
      if len = 0 then Mods.IntMap.remove rule_id unary_candidates
      else Mods.IntMap.add rule_id cands unary_candidates






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