(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = (unit NamedDecls.t * bool array array option) NamedDecls.t

let fold f = NamedDecls.fold (fun i n o _ -> f i n o)

let num_of_site ?agent_name site_name sign =
  let kind = match agent_name with
    | None -> "site name"
    | Some agent_name -> "site name for agent "^agent_name
  in NamedDecls.elt_id ~kind sign site_name

let site_of_num addr sign =
  try NamedDecls.elt_name sign addr
  with Invalid_argument _ -> raise Not_found

let num_of_internal_state site_id state sign =
  try
    let na,(nd,_) = sign.NamedDecls.decls.(site_id) in
    NamedDecls.elt_id ~kind:("internal state for site "^na) nd state
  with
  | Invalid_argument _ -> raise Not_found

let internal_state_of_num site_num val_num sign =
  try
    let _,(nd,_) = sign.NamedDecls.decls.(site_num) in
    fst nd.NamedDecls.decls.(val_num)
  with
  | Invalid_argument _ -> raise Not_found

let one_to_json =
  NamedDecls.to_json
    (JsonUtil.of_pair
       (NamedDecls.to_json (fun () -> `Null))
       (JsonUtil.of_option
          (fun links ->
             `List (Array.fold_right (fun a acc ->
                 `List (Array.fold_right (fun b c -> `Bool b :: c) a []) :: acc)
                 links []))))

let one_of_json =
  NamedDecls.of_json
    (JsonUtil.to_pair
       (NamedDecls.of_json (function
            | `Null -> ()
            | x -> raise (Yojson.Basic.Util.Type_error
                            ("Problematic agent signature",x))))
       (Yojson.Basic.Util.to_option
          (function
            | `List l ->
              Tools.array_map_of_list (function
                  | `List l' -> Tools.array_map_of_list (function
                      | `Bool b -> b
                      | x -> raise (Yojson.Basic.Util.Type_error
                                      ("Problematic agent signature",x)))
                      l'
                  | x -> raise (Yojson.Basic.Util.Type_error
                                  ("Problematic agent signature",x)))
                l
            | x -> raise (Yojson.Basic.Util.Type_error
                            ("Problematic agent signature",x)))))
type s = t NamedDecls.t

let size sigs = NamedDecls.size sigs
let get sigs agent_id = snd sigs.NamedDecls.decls.(agent_id)
let arity sigs agent_id = NamedDecls.size (get sigs agent_id)
let max_arity sigs =
  NamedDecls.fold (fun _ _ x a -> max x (NamedDecls.size a)) 0 sigs

let agent_of_num i sigs = NamedDecls.elt_name sigs i
let num_of_agent name sigs =
  NamedDecls.elt_id ~kind:"agent" sigs name

let id_of_site (agent_name,_ as agent_ty) site_name sigs =
  let n = num_of_agent agent_ty sigs in
  num_of_site ~agent_name site_name (get sigs n)

let site_of_id agent_id site_id sigs =
  site_of_num site_id (get sigs agent_id)

let id_of_internal_state (agent_name,_ as agent_ty) site_name state sigs =
  let n = num_of_agent agent_ty sigs in
  let sign = get sigs n in
  let site_id =
    num_of_site ~agent_name site_name sign in
  num_of_internal_state site_id state sign

let internal_state_of_id agent_id id_site id_state sigs =
  internal_state_of_num id_site id_state (get sigs agent_id)

let internal_states_number agent_id site_num sigs =
  try
    let _,(nd,_) = (get sigs agent_id).NamedDecls.decls.(site_num) in
    NamedDecls.size nd
  with
  | Invalid_argument _ -> raise Not_found

let default_internal_state agent_id site_id sigs =
  try
    let _,(nd,_) = (get sigs agent_id).NamedDecls.decls.(site_id) in
    if nd.NamedDecls.decls = [||] then None else Some 0
  with
  | Invalid_argument _ ->
    invalid_arg "Signature.default_num_value: invalid site identifier"

let rec allowed_link ag1 s1 ag2 s2 sigs =
  if ag1 > ag2 then allowed_link ag2 s2 ag1 s1 sigs
  else
    try match (get sigs ag1).NamedDecls.decls.(s1) with
      | _, (_,None) -> true
      | _, (_,Some l) -> l.(ag2-ag1).(s2)
    with
    | Invalid_argument _ ->
      invalid_arg "Signature.allowed_link: invalid site identifier"

let create contact_map t =
  let raw = NamedDecls.create t in
  let s = Array.length t in
  NamedDecls.mapi
    (fun ag ag_na -> NamedDecls.mapi
        (fun _ si_na (ints,links) ->
           if not(contact_map) then (ints, None) else
             let out =
               Array.init
                 (s-ag)
                 (fun i ->
                    Array.make (NamedDecls.size (get raw (i+ag))) false) in
             let () =
               List.iter
                 (fun ((site_name,pos as site),(agent_name,_ as agent)) ->
                    let a = num_of_agent agent raw in
                    let s = num_of_site ~agent_name site (get raw a) in
                    let () = if a >= ag then out.(a-ag).(s) <- true in
                    if List.exists
                        (fun ((x,_),(y,_)) -> x = si_na && y = ag_na)
                        (snd (snd (get raw a).NamedDecls.decls.(s))) then ()
                    else
                      raise (ExceptionDefn.Malformed_Decl
                               (Format.asprintf
                                  "No link to %s.%s from %s.%s."
                                  si_na ag_na site_name agent_name,pos)))
                 links in
             (ints,Some out)))
    raw

let print_agent sigs f ag_ty =
  Format.pp_print_string f @@ agent_of_num ag_ty sigs

let print_site sigs ag_ty f id =
  Format.pp_print_string f @@ site_of_id ag_ty id sigs
let print_internal_state sigs ag_ty site f id =
  Format.pp_print_string f @@ internal_state_of_id ag_ty site id sigs
let print_site_internal_state sigs ag_ty site f = function
  | None -> print_site sigs ag_ty f site
  | Some id ->
    Format.fprintf f "%s~%s" (site_of_id ag_ty site sigs)
      (internal_state_of_id ag_ty site id sigs)

let print_one ?sigs i f sign =
  let pp_int =
    NamedDecls.print
      ~sep:(fun _ -> ())
      (fun _ na f () -> Format.fprintf f "~%s" na) in
  let pp_link =
    match sigs with
    | None -> fun _ _ _ -> ()
    | Some sigs -> fun i f -> function
      | None -> ()
      | Some links ->
        Pp.array Pp.empty
          (fun ag -> Pp.array Pp.empty
              (fun si f b -> if b then
                  Format.fprintf f "!%a.%a"
                    (print_site sigs (i+ag)) si (print_agent sigs) (i+ag)))
          f links in
  (NamedDecls.print
     ~sep:(fun f -> Format.fprintf f ",@,")
     (fun _ name f (ints,links) ->
        Format.fprintf f "%s%a%a" name pp_int ints (pp_link i) links))
    f sign

let print f sigs =
  Format.fprintf
    f "@[<v>%a@]"
    (NamedDecls.print ~sep:Pp.space
       (fun i n f si ->
          Format.fprintf f "@[<h>%%agent: %s(%a)@]" n (print_one ~sigs i) si))
    sigs

let to_json = NamedDecls.to_json one_to_json
let of_json = NamedDecls.of_json one_of_json
