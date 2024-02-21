(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  (* pat -> set of roots *)
  of_patterns: IntCollection.t Pattern.ObsMap.t;
  (* pat -> cc -> set of roots *)
  of_unary_patterns: Mods.IntSet.t Mods.IntMap.t Pattern.ObsMap.t;
}

type mod_ccs_cache = (int, unit) Hashtbl.t

let empty env =
  {
    of_patterns =
      Pattern.Env.new_obs_map (Model.domain env) (fun _ ->
          IntCollection.create 64);
    of_unary_patterns =
      Pattern.Env.new_obs_map (Model.domain env) (fun _ -> Mods.IntMap.empty);
  }

let incorporate_extra_pattern state pattern matchings =
  if IntCollection.is_empty (Pattern.ObsMap.get state.of_patterns pattern) then
    Pattern.ObsMap.set state.of_patterns pattern matchings

let add_intset_in_intmap id set map =
  if Mods.IntSet.is_empty set then
    Mods.IntMap.remove id map
  else
    Mods.IntMap.add id set map

(* Break apart connected component:
   Update "roots of unary patterns"
   Easy, I should not have to rewrite this.
   Should caches be handled at this level ? I do nt think so
   and I will probably clean this.
*)
let break_apart_cc state edges ?mod_connectivity_store = function
  | None -> ()
  | Some (origin_cc, new_cc) ->
    let () =
      match mod_connectivity_store with
      | None -> ()
      | Some mod_conn ->
        let () = Hashtbl.replace mod_conn new_cc () in
        Hashtbl.replace mod_conn origin_cc ()
    in
    Pattern.ObsMap.iteri
      (fun cc_id cc_map ->
        let oset =
          Mods.IntMap.find_default Mods.IntSet.empty origin_cc cc_map
        in
        if not (Mods.IntSet.is_empty oset) then (
          let nset, oset' =
            Mods.IntSet.partition
              (fun x -> Edges.get_connected_component x edges = Some new_cc)
              oset
          in
          Pattern.ObsMap.set state.of_unary_patterns cc_id
            (add_intset_in_intmap new_cc nset
               (add_intset_in_intmap origin_cc oset' cc_map))
        ))
      state.of_unary_patterns

(* Same: not very subtle. You just propagate. *)
let merge_cc state ?mod_connectivity_store = function
  | None -> ()
  | Some (cc1, cc2) ->
    let () =
      match mod_connectivity_store with
      | None -> ()
      | Some mod_connectivity ->
        let () = Hashtbl.replace mod_connectivity cc2 () in
        Hashtbl.replace mod_connectivity cc1 ()
    in
    Pattern.ObsMap.iteri
      (fun cc_id cc_map ->
        match Mods.IntMap.pop cc2 cc_map with
        | None, _ -> ()
        | Some set2, cc_map' ->
          let set1 = Mods.IntMap.find_default Mods.IntSet.empty cc1 cc_map in
          Pattern.ObsMap.set state.of_unary_patterns cc_id
            (add_intset_in_intmap cc1 (Mods.IntSet.union set1 set2) cc_map'))
      state.of_unary_patterns

(* Most of the code is to deal with unary_instances.
   Does nothing fancy.
   Also takes the cache as an argument *)
let update_roots state is_add unary_ccs edges mod_connectivity pattern root =
  let va = Pattern.ObsMap.get state.of_patterns pattern in
  let () =
    (if is_add then
       IntCollection.add
     else
       IntCollection.remove)
      root va
  in
  if Pattern.Set.mem pattern unary_ccs then (
    let cc_map = Pattern.ObsMap.get state.of_unary_patterns pattern in
    let cc_id =
      (* The only case where get_connected_component is None is when
         [not is_add] and [root] has just been erased! But, just
         before being erased, we know that an agent is in its own
         connected component... *)
      Option_util.unsome root (Edges.get_connected_component root edges)
    in
    let () = Hashtbl.replace mod_connectivity cc_id () in
    let set = Mods.IntMap.find_default Mods.IntSet.empty cc_id cc_map in
    let set' =
      (if is_add then
         Mods.IntSet.add
       else
         Mods.IntSet.remove)
        root set
    in
    let cc_map' = add_intset_in_intmap cc_id set' cc_map in
    Pattern.ObsMap.set state.of_unary_patterns pattern cc_map'
  )

let number r pat = IntCollection.size (Pattern.ObsMap.get r.of_patterns pat)

let print_injections ~noCounters ?domain f roots_of_patterns =
  Format.fprintf f "@[<v>%a@]"
    (Pattern.ObsMap.print Pp.space (fun pattern f roots ->
         if IntCollection.size roots > 0 then
           Format.fprintf f "@[# @[%a@] ==>@ @[%a@]@]"
             (Pattern.print ~noCounters ?domain ~with_id:true)
             pattern IntCollection.print roots))
    roots_of_patterns

let print_unary_injections ~noCounters ?domain f roots_of_patterns =
  Format.fprintf f "@[<hov>%a@]"
    (Pattern.ObsMap.print Pp.space (fun pattern f root_maps ->
         Format.fprintf f "@[# @[%a@] ==>@ @[%a@]@]"
           (Pattern.print ~noCounters ?domain ~with_id:true)
           pattern
           (Pp.set Mods.IntMap.bindings Pp.space (fun f (_cc_id, roots) ->
                Mods.IntSet.print f roots))
           root_maps))
    roots_of_patterns

let debug_print f state =
  let noCounters = true in
  let domain = None in
  let () = print_injections ~noCounters ?domain f state.of_patterns in
  print_unary_injections ~noCounters ?domain f state.of_unary_patterns

(* Useful shortcuts *)

let of_pattern pat_id state =
  try Pattern.ObsMap.get state.of_patterns pat_id
  with Not_found -> IntCollection.create 1

let of_unary_pattern pat_id state =
  try Pattern.ObsMap.get state.of_unary_patterns pat_id
  with Not_found -> Mods.IntMap.empty
