(**
  * contact_map.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2017, the 23rd of June
  * Last modification: Time-stamp: <Jul 05 2017>
  *
  * Compute strongly connected component in contact map
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type t = (Mods.IntSet.t * Mods.Int2Set.t) array array

let to_yojson a =
  let intls_to_json a =
    `List (Mods.IntSet.fold (fun b acc -> `Int b :: acc) a [])
  in
  let pairls_to_json a =
    `List
      (Mods.Int2Set.fold
         (fun (b, c) acc -> `List [ `Int b; `Int c ] :: acc)
         a [])
  in
  let array_to_json a =
    `List
      (Array.fold_left
         (fun acc (a, b) -> `List [ intls_to_json a; pairls_to_json b ] :: acc)
         [] a)
  in
  `List (Array.fold_left (fun acc t -> array_to_json t :: acc) [] a)

let of_yojson (a : Yojson.Basic.t) =
  let intls_of_json a =
    List.fold_left
      (fun acc -> function
        | `Int b -> Mods.IntSet.add b acc
        | x -> raise (Yojson.Basic.Util.Type_error ("bla1", x)))
      Mods.IntSet.empty a
  in
  let pairls_of_json a =
    List.fold_left
      (fun acc -> function
        | `List [ `Int b; `Int c ] -> Mods.Int2Set.add (b, c) acc
        | x -> raise (Yojson.Basic.Util.Type_error ("bla2", x)))
      Mods.Int2Set.empty a
  in
  let array_of_json = function
    | `List ls ->
      (match ls with
      | [ `List a; `List b ] -> intls_of_json a, pairls_of_json b
      | _ -> raise Not_found)
    | x -> raise (Yojson.Basic.Util.Type_error ("bla3", x))
  in
  match a with
  | `List array1 ->
    Tools.array_map_of_list
      (function
        | `List array2 -> Tools.array_map_of_list array_of_json array2
        | x -> raise (Yojson.Basic.Util.Type_error ("bla4", x)))
      array1
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct contact map", x))

let print_kappa ~noCounters sigs f c =
  Format.fprintf f "@[<v>%a@]"
    (Pp.array Pp.space (fun ag f intf ->
         if Signature.ports_if_counter_agent sigs ag = None || noCounters then
           Format.fprintf f "@[<hv 2>%%agent:@ %a(@[%a@])@]"
             (Signature.print_agent sigs)
             ag
             (Pp.array Pp.space (fun s f (is, ls) ->
                  if Signature.site_is_counter sigs ag s && not noCounters then
                    Format.fprintf f "@[%a%a@]"
                      (Signature.print_site sigs ag)
                      s
                      (Signature.print_counter sigs ag)
                      s
                  else
                    Format.fprintf f "@[%a%t%t@]"
                      (Signature.print_site sigs ag)
                      s
                      (fun f ->
                        if not (Mods.IntSet.is_empty is) then
                          Format.fprintf f "{@[%a@]}"
                            (Pp.set Mods.IntSet.elements Pp.space
                               (Signature.print_internal_state sigs ag s))
                            is)
                      (fun f ->
                        if not (Mods.Int2Set.is_empty ls) then
                          Format.fprintf f "@,[@[%a@]]"
                            (Pp.set Mods.Int2Set.elements Pp.space
                               (fun f (ad, sd) ->
                                 Format.fprintf f "%a.%a"
                                   (Signature.print_site sigs ad)
                                   sd
                                   (Signature.print_agent sigs)
                                   ad))
                            ls)))
             intf))
    c

let cut_at i s' l =
  let rec aux_cut_at o = function
    | [] -> None
    | (((j, s), _) as h) :: t ->
      if i = j then
        if s >= s' then
          None
        else
          Some (h :: o)
      else
        aux_cut_at (h :: o) t
  in
  aux_cut_at [] l

let get_cycles contact_map =
  let rec dfs ((known, out) as acc) path i last_s =
    if Mods.IntSet.mem i known then (
      match cut_at i last_s path with
      | None -> acc
      | Some x -> known, x :: out
    ) else (
      let known' = Mods.IntSet.add i known in
      Tools.array_fold_lefti
        (fun s acc (_, l) ->
          if s = last_s then
            acc
          else
            Mods.Int2Set.fold
              (fun ((ty, s') as x) acc -> dfs acc (((i, s), x) :: path) ty s')
              l acc)
        (known', out) contact_map.(i)
    )
  in
  let rec scan ((known, out) as acc) i =
    if i < 0 then
      out
    else
      scan
        (if Mods.IntSet.mem i known then
           acc
         else
           dfs acc [] i (-1))
        (pred i)
  in
  scan (Mods.IntSet.empty, []) (Array.length contact_map - 1)

let print_cycles sigs form contact_map =
  let o = get_cycles contact_map in
  Pp.list Pp.space
    (Pp.list Pp.empty (fun f ((ag, s), (ag', s')) ->
         Format.fprintf f "%a.%a-%a."
           (Signature.print_agent sigs)
           ag
           (Signature.print_site sigs ag)
           s
           (Signature.print_site sigs ag')
           s'))
    form o
