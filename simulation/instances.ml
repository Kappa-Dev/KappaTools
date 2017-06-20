(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)


(** Count, enumerate and pick. 
    To enumerate, no need for folding functions... 
    The rest is to be seen outside. *)


type t = {

  (* For counterfactual simulation, there would be two of these. *)
  roots : Roots.t ;

}

type mod_ccs_cache = (int, unit) Hashtbl.t

type message = unit

let send_message _ st = st


let empty env = {
  roots = Roots.empty env ;
}

let incorporate_extra_pattern state pattern matchings =
  Roots.incorporate_extra_pattern state.roots pattern matchings

let break_apart_cc state edges mod_conn ccs = { state with
  roots = Roots.break_apart_cc state.roots edges mod_conn ccs
}

let merge_cc state mod_connectivity ccs = { state with
  roots = Roots.merge_cc state.roots mod_connectivity ccs
}

let update_roots state is_add unary_ccs edges mod_connectivity pattern root =
    Roots.update_roots 
      state.roots is_add unary_ccs edges mod_connectivity pattern root



(* Compute number of instances *)

let number_of_instances st pats =
  Array.fold_left
    (fun acc pattern ->  acc * (Roots.number st.roots pattern)) 1 pats

let number_of_unary_instances_in_cc st (pat1, pat2) = 
  let map1 = Pattern.ObsMap.get st.roots.Roots.of_unary_patterns pat1 in
  let map2 = Pattern.ObsMap.get st.roots.Roots.of_unary_patterns pat2 in
  fun cc -> 
    let set1 = Mods.IntMap.find_default Mods.IntSet.empty cc map1 in
    let set2 = Mods.IntMap.find_default Mods.IntSet.empty cc map2 in
    Mods.IntSet.size set1 * Mods.IntSet.size set2



(* Pick instances *)

let pick_unary_instance_in_cc st random_state (pat1, pat2) =
  let map1 = Pattern.ObsMap.get st.roots.Roots.of_unary_patterns pat1 in
  let map2 = Pattern.ObsMap.get st.roots.Roots.of_unary_patterns pat2 in
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

(* To avoid computing more random numbers than necessary *)
let fold_picked_instance st random_state pats ~init f = 
  let rec aux i acc =
    if i >= Array.length pats then acc else
    match acc with
      | None -> None
      | Some acc -> 
        let pat = pats.(i) in
        let root_opt = IntCollection.random random_state
          (Pattern.ObsMap.get st.roots.Roots.of_patterns pat) in
          begin match root_opt with
          | None -> None
          | Some root ->
            let acc = f i pat root acc in
            aux (i+1) acc
          end
  in aux 0 (Some init)



(* Enumerate instances *)

let fold_enumerated_instances' st pats ~init f =
  let rec aux i acc =
    if i >= Array.length pats then acc else
    let pat = pats.(i) in
    let get_candidates () = Pattern.ObsMap.get st.roots.Roots.of_patterns pat in
    let acc = f i pats.(i) get_candidates acc in
    aux (i+1) acc in
  aux 0 init

(* Previous is not ok. We need to go through each possibility in order 
   This code is designed to allocate as little memory as possible. *)

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


(* This is the legitimate and efficient version *)
let fold_instances' ?excp st pats ~init f =

  let sent_to_excp_root, excp_root = process_excp pats excp in
        
  let n = Array.length pats in
  let tab = Array.make n (-1) in
  let rec aux i acc =
    if i >= n then
      f (Array.to_list tab) acc
    else
      if sent_to_excp_root i then begin tab.(i) <- excp_root ; aux (i+1) acc end else
      let ith_roots = Pattern.ObsMap.get st.roots.Roots.of_patterns pats.(i) in
      IntCollection.fold (fun r acc ->
        tab.(i) <- r ;
        aux (i + 1) acc
      ) ith_roots acc
  in aux 0 init



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
            Pattern.ObsMap.get st.roots.Roots.of_patterns pat in

        IntCollection.fold (fun root new_instances ->
          instances |> List.fold_left (fun new_instances instance ->
            (root :: instance) :: new_instances
          ) new_instances
        ) candidates []
      ) [[]] in
    (* Injections have been generated in the reverse order *)
    let instances = List.map List.rev instances in

    instances |> List.fold_left (fun acc instance ->
      f instance acc
    ) init




let map_fold2 map1 map2 ~init f =
  Mods.IntMap.monadic_fold2_sparse () () 
    (fun () () key x1 x2 acc -> (), f key x1 x2 acc)
    map1 map2 init
  |> snd

let fold_unary_instances st (pat1, pat2) ~init f =
  let map1 = Pattern.ObsMap.get st.roots.Roots.of_unary_patterns pat1 in
  let map2 = Pattern.ObsMap.get st.roots.Roots.of_unary_patterns pat2 in
  map_fold2 map1 map2 ~init (fun _ set1 set2 acc ->
    Mods.IntSet.fold (fun root1 acc ->
      Mods.IntSet.fold (fun root2 acc ->
        f (root1, root2) acc
      ) set2 acc
    ) set1 acc
  )



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
  Roots.debug_print f state.roots