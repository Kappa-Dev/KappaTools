(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type cc_site = {
     site_link: (int * int) option;
     site_state: int option;
}
type cc_node = {
     node_type: int;
     node_sites: cc_site array;
}
type connected_component = cc_node array

let rec agents_are_compatibles a b don = function
  | [] -> true
  | (x,y)::q ->
    let o = a.(x) in let p = b.(y) in
    o.node_type = p.node_type &&
    let i_ok =
      Tools.array_fold_left2i
        (fun _ b x y ->
           b && match x.site_state,y.site_state with
           | Some a, Some b -> (a = b)
           | None, None -> true
           | (Some _ | None), _ -> false) true o.node_sites p.node_sites in
    i_ok &&
    match Tools.array_fold_left2i
            (fun _ c x y ->
               match c with
               | None -> c
               | Some todo ->
                 match x.site_link, y.site_link with
                 | (None, Some _ | Some _, None) -> None
                 | None, None -> c
                 | Some (a,s), Some (b,s') ->
                   if s <> s' then None else
                     match
                       List.find_all (fun (a',b') -> a = a' || b = b') don,
                     List.find_all (fun (a',b') -> a = a' || b = b') todo with
                     | _ :: _ :: _, _ | _, _ :: _ :: _ | [ _ ], [ _ ] -> None
                     | [a',b'], [] | [], [a',b'] ->
                       if a = a' && b = b' then c else None
                     | [],[] -> Some ((a,b)::todo)
            )
            (Some q) o.node_sites p.node_sites with
    | Some todo' -> agents_are_compatibles a b ((x,y)::don) todo'
    | _ -> false

let classify_by_type sigs mix =
  let len = Signature.size sigs in
  let out = Array.make len (0,[]) in
  let classify id ag =
    let nb,ags = out.(ag.node_type) in
    out.(ag.node_type) <- (succ nb,id::ags) in
  let () = Array.iteri classify mix in
  out

let equal cbt_a a cbt_b b =
  match Tools.array_min_equal_not_null cbt_a cbt_b with
  | None -> false
  | Some ([],ags) -> ags = []
  | Some (h1::_,ags) ->
    List.fold_left
      (fun bool ag -> bool || agents_are_compatibles a b [] [h1,ag])
      false ags

let hash_prime = 29
let coarse_hash cbt =
  Array.fold_right (fun (l,_) acc -> l + hash_prime * acc) cbt 0

type t =
  (int * (int * int list) array * connected_component) list Mods.IntMap.t

let empty = Mods.IntMap.empty

let increment_in_snapshot sigs x s =
  let cbt_x = classify_by_type sigs x in
  let hs = coarse_hash cbt_x in
  let l = Mods.IntMap.find_default [] hs s in
  let rec aux_increment = function
  | [] -> [1,cbt_x,x]
  | (n,cbt_y,y as h)::t ->
    if equal cbt_x x cbt_y y then (succ n,cbt_y,y)::t
    else h::aux_increment t in
  Mods.IntMap.add hs (aux_increment l) s

let is_counter n_id sigs =
  let ag_name = Signature.agent_of_num n_id sigs in
  (String.compare ag_name "__incr") = 0

let rec counter_value cc (nid,sid) count =
  let ag = cc.(nid) in
  Tools.array_fold_lefti
    (fun id acc si ->
      if (id = sid) then acc
      else
        match si.site_link with
        | None -> acc
        | Some x -> counter_value cc x (acc+1)) count ag.node_sites

let cc_to_user_cc sigs cc =
  let cc_without_counters =
    Array.of_list
      (Array.fold_right
         (fun ag acc ->
           if not(is_counter ag.node_type sigs) then ag::acc else acc) cc []) in
  Array.map
    (fun ag -> {
         User_graph.node_type =
           Format.asprintf "%a" (Signature.print_agent sigs) ag.node_type;
         User_graph.node_sites =
           Array.mapi (fun id si -> {
                 User_graph.site_name =
                   Format.asprintf
                     "%a" (Signature.print_site sigs ag.node_type) id;
                 User_graph.site_type =
                   let port_states =
                     (match si.site_state with
                      | None -> []
                      | Some s ->
                         [Format.asprintf
                            "%a" (Signature.print_internal_state
                                    sigs ag.node_type id)
                            s]) in
                   match si.site_link with
                   | None ->
                      User_graph.Port
                        {User_graph.port_links = []; User_graph.port_states}
                   | Some ((dn_id,_) as x) ->
                      if not(is_counter (cc.(dn_id)).node_type sigs) then
                        User_graph.Port
                          {User_graph.port_links = [x]; User_graph.port_states}
                      else User_graph.Counter (counter_value cc x 0)
                      })
                      ag.node_sites;
    })
    cc_without_counters

let export sigs s =
  Mods.IntMap.fold (fun _ l acc ->
      List_util.rev_map_append
        (fun (x,_,y) -> (x,cc_to_user_cc sigs y)) l acc) s []
