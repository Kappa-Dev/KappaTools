(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)


type kind =
  | Port of unit NamedDecls.t * bool array array option
  | Counter of int * int

type t = kind NamedDecls.t

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
    match sign.NamedDecls.decls.(site_id) with
    | _, Counter _ -> raise Not_found
    | na, Port (nd,_) ->
      NamedDecls.elt_id ~kind:("internal state for site "^na) nd state
  with
  | Invalid_argument _ -> raise Not_found

let internal_state_of_num site_num val_num sign =
  try
    match sign.NamedDecls.decls.(site_num) with
    | _, Counter _ -> raise Not_found
    | _, Port (nd,_) -> fst nd.NamedDecls.decls.(val_num)
  with
  | Invalid_argument _ -> raise Not_found

let counter_of_site site_id sign =
  try
    match sign.NamedDecls.decls.(site_id) with
    | _, Counter (x,y) -> Some (x,y)
    | _, Port _ -> None
  with
  | Invalid_argument _ -> raise Not_found

let list_counters sign =
  fold
    (fun p_id _ acc ->
      try
        match sign.NamedDecls.decls.(p_id) with
        | _, Counter _ -> p_id::acc
        | _, Port _ -> acc
      with
      | Invalid_argument _ -> raise Not_found) [] sign

let one_to_json =
  NamedDecls.to_json
    (function
      | Counter (c1,c2) -> `List [`String "Counter"; `Int c1; `Int c2]
      | Port (a,b) ->
        `List [ `String "Port";
                NamedDecls.to_json (fun () -> `Null) a;
                JsonUtil.of_option
                   (fun links ->
                      `List (Array.fold_right (fun a acc ->
                         `List (Array.fold_right
                                  (fun b l -> `Bool b::l) a [])::acc)
                           links [])) b])

let one_of_json =
  NamedDecls.of_json
    (function
      | `List [`String "Port";a;b] ->
        Port
          (NamedDecls.of_json (function
               | `Null -> ()
               | x -> raise (Yojson.Basic.Util.Type_error
                               ("Problematic agent signature",x))) a,
           Yojson.Basic.Util.to_option
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
                               ("Problematic agent signature",x))) b)
      | `List [`String "Counter"; `Int c1; `Int c2] -> Counter (c1,c2)
      | x -> raise (Yojson.Basic.Util.Type_error
                     ("Problematic agent signature",x)))

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
    match (get sigs agent_id).NamedDecls.decls.(site_num) with
    | _, Counter _ -> 0
    | _, Port (nd,_) -> NamedDecls.size nd
  with
  | Invalid_argument _ -> raise Not_found

let default_internal_state agent_id site_id sigs =
  try
    match (get sigs agent_id).NamedDecls.decls.(site_id) with
    | _, Counter _ -> None
    | _, Port (nd,_) -> if nd.NamedDecls.decls = [||] then None else Some 0
  with
  | Invalid_argument _ ->
    invalid_arg
      ("Signature.default_num_value: invalid site identifier: "^
       string_of_int site_id^" of agent "^string_of_int agent_id)

let rec allowed_link ag1 s1 ag2 s2 sigs =
  if ag1 > ag2 then allowed_link ag2 s2 ag1 s1 sigs
  else
    try match (get sigs ag1).NamedDecls.decls.(s1) with
      | _, Port (_,None) -> true
      | _, Port (_,Some l) -> l.(ag2-ag1).(s2)
      | _, Counter _ -> assert false
    with
    | Invalid_argument _ ->
      invalid_arg "Signature.allowed_link: invalid site identifier"

let counter_agent_name = "__incr"

let add_incr counters =
  let a_port = Locality.dummy_annot "a" in
  let b_port = Locality.dummy_annot "b" in
  let value_port = Locality.dummy_annot "v" in
  let zero_state = Locality.dummy_annot "z" in
  let other_state = Locality.dummy_annot "o" in
  let incr = Locality.dummy_annot counter_agent_name in
  let after = (a_port,(NamedDecls.create [||],[(b_port,incr)],None)) in
  let rev_value_lnks =
    List.fold_left
      (fun acc (ag,counts) ->
         List_util.rev_map_append (fun c -> (c,ag)) counts acc) [] counters in
  let before = (b_port,(NamedDecls.create [||],[(a_port,incr)],None)) in
  let value =
    (value_port,
     (NamedDecls.create [|(other_state,());(zero_state,())|],
      List.rev rev_value_lnks,None)) in
  let lnks = NamedDecls.create [|before;value;after|] in
  let counter_agent = (incr,lnks) in
  counter_agent

let create ~counters contact_map sigs =
  let sigs' = if counters <> [] then (add_incr counters)::sigs else sigs in
  let t = Array.of_list sigs' in
  let raw =  NamedDecls.create t in
  let s = Array.length t in
  let snd_of_third = fun (_,a,_) -> a in
  NamedDecls.mapi
    (fun ag ag_na -> NamedDecls.mapi
        (fun _ si_na (ints,links,counts) ->
           match counts with
           | Some (c1,c2) -> Counter (c1,c2)
           | None ->
             if not contact_map then Port (ints,None) else
               let out =
                 Array.init
                   (s-ag)
                   (fun i -> Array.make
                       (NamedDecls.size (snd raw.NamedDecls.decls.(i+ag)))
                       false) in
               let () =
                 List.iter
                   (fun ((site_name,pos as site),(agent_name,_ as agent)) ->
                      let a = NamedDecls.elt_id ~kind:"agent" raw agent in
                      let s = num_of_site
                          ~agent_name site (snd raw.NamedDecls.decls.(a)) in
                      let () = if a >= ag then out.(a-ag).(s) <- true in
                      if List.exists
                          (fun ((x,_),(y,_)) -> x = si_na && y = ag_na)
                          (snd_of_third
                             (snd (snd raw.NamedDecls.decls.(a)).NamedDecls.decls.(s)))
                      then ()
                      else
                        raise (ExceptionDefn.Malformed_Decl
                                 (Format.asprintf
                                    "No link to %s.%s from %s.%s."
                                    si_na ag_na site_name agent_name,pos)))
                   links in
               Port (ints,Some out)))
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
    Format.fprintf f "%s{%s}" (site_of_id ag_ty site sigs)
      (internal_state_of_id ag_ty site id sigs)
let print_counter sigs ag_ty f id =
  match (get sigs ag_ty).NamedDecls.decls.(id) with
  | _, Counter (c1,c2) -> Format.fprintf f "{=%d/+=%d}" c1 c2
  | _, Port _ -> ()

let print_one ?sigs i f sign =
  let pp_int f x =
    if NamedDecls.size x > 0 then
      Format.fprintf f "{%a}"
        (NamedDecls.print
           ~sep:Pp.space
           (fun _ na f () -> Format.fprintf f "%s" na))
        x in
  let pp_link =
    match sigs with
    | None -> fun _ _ _ -> ()
    | Some sigs -> fun i f -> function
      | None -> ()
      | Some links ->
        Format.fprintf f "[%a]"
          (Pp.array Pp.space
             (fun ag -> Pp.array Pp.space
                 (fun si f b -> if b then
                     Format.fprintf f "%a.%a"
                       (print_site sigs (i+ag)) si (print_agent sigs) (i+ag))))
          links in
  (NamedDecls.print
     ~sep:(fun f -> Format.fprintf f ",@,")
     (fun _ name f -> function
        | Counter (c1,c2) -> Format.fprintf f "%s{=%d/+=%d}" name c1 c2
        | Port (ints,links) ->
          Format.fprintf f "%s%a%a" name pp_int ints (pp_link i) links))
    f sign

let print f sigs =
  Format.fprintf
    f "@[<v>%a@]"
    (NamedDecls.print ~sep:Pp.space
       (fun i n f si ->
          Format.fprintf f "@[<h>%%agent: %s(%a)@]" n (print_one ~sigs i) si))
    sigs

let to_json sigs = NamedDecls.to_json one_to_json sigs
let of_json v =
  NamedDecls.of_json one_of_json v

let is_counter_agent sigs n_id =
  let (na,_) = sigs.NamedDecls.decls.(n_id) in na = counter_agent_name

let site_is_counter sigs ag_ty id =
  match (get sigs ag_ty).NamedDecls.decls.(id) with
  | _, Counter _ -> true
  | _, Port _ -> false
