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

type t = ((int list) * (int*int) list) array array

let to_yojson a =
  let intls_to_json a = `List (List.map (fun b -> `Int b) a) in
  let pairls_to_json a =
    `List (List.map (fun (b,c) -> `List[`Int b;`Int c]) a) in
  let array_to_json a =
    `List (Array.fold_left
             (fun acc (a,b) ->
               (`List [(intls_to_json a);(pairls_to_json b)])::acc) [] a) in
  `List (Array.fold_left
           (fun acc t ->(array_to_json t)::acc) [] a)

let of_yojson (a:Yojson.Basic.json) =
  let intls_of_json a =
    List.map (function `Int b -> b
                     | x -> raise (Yojson.Basic.Util.Type_error("bla1",x))) a in
  let pairls_of_json a =
    List.map (function `List [`Int b;`Int c] -> (b,c)
                     | x -> raise (Yojson.Basic.Util.Type_error("bla2",x))) a in
  let array_of_json =
    function `List ls ->
             (match ls with
              | [`List a;`List b] -> ((intls_of_json a),(pairls_of_json b))
              | _ -> raise Not_found)
            |x -> raise (Yojson.Basic.Util.Type_error("bla3",x)) in
  match a with
  | `List array1 -> Tools.array_map_of_list
                      (function `List array2 ->
                                Tools.array_map_of_list array_of_json array2
                               |x ->raise (Yojson.Basic.Util.Type_error("bla4",x)))
                      array1
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct contact map",x))


let print_kappa sigs f c =
  Format.fprintf f "@[<v>%a@]"
    (Pp.array Pp.space
       (fun ag f intf ->
         if not(Signature.is_counter ag (Some sigs))||(!Parameter.debugModeOn) then
           begin
           Format.fprintf f "@[<h>%%agent: %a(%a)@]"
           (Signature.print_agent sigs) ag
           (Pp.array Pp.comma
              (fun s f (is,ls) -> Format.fprintf f "%a%a%a%a"
                  (Signature.print_site sigs ag) s
                  (Pp.list Pp.empty
                     (fun f i -> Format.fprintf f "~%a"
                         (Signature.print_internal_state sigs ag s) i)) is
                  (Pp.list Pp.empty (fun f (ad,sd) ->
                                      if (Signature.site_is_counter sigs ag s)
                                         &&not(!Parameter.debugModeOn)
                                      then ()
                                      else
                                        Format.fprintf f "!%a.%a"
                                        (Signature.print_site sigs ad) sd
                                        (Signature.print_agent sigs) ad)) ls
                  (fun f id-> if (!Parameter.debugModeOn) then ()
                              else Signature.print_counter sigs ag f id) s))
           end
           intf))
    c

let cut_at i s' l =
  let rec aux_cut_at o = function
    | [] -> None
    | ((j,s),_ as h) :: t -> if i = j then
        if s >= s' then None else Some (h::o)
      else aux_cut_at (h::o) t
  in aux_cut_at [] l

let get_cycles contact_map =
  let rec dfs (known,out as acc) path i last_s =
    if Mods.IntSet.mem i known
    then match cut_at i last_s path with
      | None -> acc
      | Some x -> known, x :: out
    else
      let known' = Mods.IntSet.add i known in
      Tools.array_fold_lefti
        (fun s acc (_,l)->
           if s = last_s then acc else
             List.fold_left
               (fun acc (ty,s' as x) -> dfs acc (((i,s),x)::path) ty s')
               acc l)
        (known',out) contact_map.(i) in
  let rec scan (known,out as acc) i =
    if i < 0 then out else
      scan
        (if Mods.IntSet.mem i known then acc else dfs acc [] i (-1))
        (pred i) in
  scan (Mods.IntSet.empty,[]) (Array.length contact_map - 1)

let print_cycles sigs form contact_map =
  let o = get_cycles contact_map in
  Pp.list Pp.space
    (Pp.list Pp.empty
       (fun f ((ag,s),(ag',s')) ->
          Format.fprintf f "%a.%a-%a."
            (Signature.print_agent sigs) ag
            (Signature.print_site sigs ag) s
            (Signature.print_site sigs ag') s'
       )
    ) form o
