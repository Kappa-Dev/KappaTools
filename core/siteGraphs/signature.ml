(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type 'links site_sig = {
  internal_state: unit NamedDecls.t;
  links: 'links option;
  counters_info: (int * int) option;
      (** If relevant: counter CEQ value * counter delta *)
}

type t = bool array array site_sig NamedDecls.t

let fold f = NamedDecls.fold (fun i n o _ -> f i n o)

let num_of_site ?agent_name site_name signature =
  let kind =
    match agent_name with
    | None -> "site name"
    | Some agent_name -> "site name for agent " ^ agent_name
  in
  NamedDecls.elt_id ~kind signature site_name

let site_of_num addr signature =
  try NamedDecls.elt_name signature addr
  with Invalid_argument _ -> raise Not_found

let num_of_internal_state site_id state signature =
  try
    let site_name, site_sig = signature.NamedDecls.decls.(site_id) in
    NamedDecls.elt_id
      ~kind:("internal state for site " ^ site_name)
      site_sig.internal_state state
  with Invalid_argument _ -> raise Not_found

let internal_state_of_site_id site_id val_id signature =
  try
    let site_sig = NamedDecls.elt_val signature site_id in
    NamedDecls.elt_name site_sig.internal_state val_id
  with Invalid_argument _ -> raise Not_found

let counter_of_site_id site_id signature =
  try (NamedDecls.elt_val signature site_id).counters_info
  with Invalid_argument _ -> raise Not_found

let has_counter signature =
  fold
    (fun p_id _ ok ->
      try
        let site_sig = NamedDecls.elt_val signature p_id in
        ok || not (site_sig.counters_info = None)
      with Invalid_argument _ -> raise Not_found)
    false signature

(*
let read_position p lb =
  match Yojson.Basic.from_lexbuf ~stream:true p lb with
  | `Assoc [ ("line", `Int line); ("chr", `Int chr) ]
  | `Assoc [ ("chr", `Int chr); ("line", `Int line) ] ->
    { line; chr }
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid position", x))

let write_position ob { line; chr } =
  Yojson.write_assoc ob [ "line", `Int line; "chr", `Int chr ]
*)

let one_to_json =
  NamedDecls.to_json (fun signature ->
      `Assoc
        [
          ( "internal_state",
            NamedDecls.to_json (fun () -> `Null) signature.internal_state );
          ( "links",
            JsonUtil.of_option
              (fun links ->
                `List
                  (Array.fold_right
                     (fun a acc ->
                       `List (Array.fold_right (fun b c -> `Bool b :: c) a [])
                       :: acc)
                     links []))
              signature.links );
          ( "counters_info",
            JsonUtil.of_option
              (fun (c1, c2) -> `List [ `Int c1; `Int c2 ])
              signature.counters_info );
        ])

let one_of_json : Yojson.Basic.t -> bool array array site_sig NamedDecls.t =
  NamedDecls.of_json (function
    | `Assoc [ ("internal_state", a); ("links", b); ("counters_info", c) ] ->
      {
        internal_state =
          NamedDecls.of_json
            (function
              | `Null -> ()
              | x ->
                raise
                  (Yojson.Basic.Util.Type_error
                     ("Problematic agent signature", x)))
            a;
        links =
          Yojson.Basic.Util.to_option
            (function
              | `List l ->
                Tools.array_map_of_list
                  (function
                    | `List l' ->
                      Tools.array_map_of_list
                        (function
                          | `Bool b -> b
                          | x ->
                            raise
                              (Yojson.Basic.Util.Type_error
                                 ("Problematic agent signature", x)))
                        l'
                    | x ->
                      raise
                        (Yojson.Basic.Util.Type_error
                           ("Problematic agent signature", x)))
                  l
              | x ->
                raise
                  (Yojson.Basic.Util.Type_error
                     ("Problematic agent signature", x)))
            b;
        counters_info =
          Yojson.Basic.Util.to_option
            (function
              | `List [ `Int c1; `Int c2 ] -> c1, c2
              | x ->
                raise
                  (Yojson.Basic.Util.Type_error
                     ("Problematic agent signature", x)))
            c;
      }
    | x ->
      raise (Yojson.Basic.Util.Type_error ("Problematic agent signature", x)))

type counter_agent_info = { id: int; arity: int; ports: int * int }

type s = {
  agent_sigs: t NamedDecls.t;
  counter_agent_info: counter_agent_info option;
}

let size sigs = NamedDecls.size sigs.agent_sigs
let get sigs agent_id = NamedDecls.elt_val sigs.agent_sigs agent_id
let arity sigs agent_id = NamedDecls.size (get sigs agent_id)

let max_arity sigs =
  NamedDecls.fold (fun _ _ x a -> max x (NamedDecls.size a)) 0 sigs.agent_sigs

let agent_of_num i sigs = NamedDecls.elt_name sigs.agent_sigs i

let num_of_agent name sigs =
  NamedDecls.elt_id ~kind:"agent" sigs.agent_sigs name

let id_of_site ((agent_name, _) as agent_ty) site_name sigs =
  let n = num_of_agent agent_ty sigs in
  num_of_site ~agent_name site_name (get sigs n)

let site_of_id agent_id site_id sigs = site_of_num site_id (get sigs agent_id)

let id_of_internal_state ((agent_name, _) as agent_ty) site_name state sigs =
  let n = num_of_agent agent_ty sigs in
  let signature = get sigs n in
  let site_id = num_of_site ~agent_name site_name signature in
  num_of_internal_state site_id state signature

let internal_state_of_id agent_id id_site id_state sigs =
  internal_state_of_site_id id_site id_state (get sigs agent_id)

let internal_states_number agent_id site_id sigs =
  try
    let site_sig = NamedDecls.elt_val (get sigs agent_id) site_id in
    NamedDecls.size site_sig.internal_state
  with Invalid_argument _ -> raise Not_found

let default_internal_state agent_id site_id sigs =
  try
    let site_sig = NamedDecls.elt_val (get sigs agent_id) site_id in
    if NamedDecls.size site_sig.internal_state = 0 then
      None
    else
      Some 0
  with Invalid_argument _ ->
    invalid_arg "Signature.default_num_value: invalid site identifier"

let rec allowed_link ag1 s1 ag2 s2 sigs =
  if ag1 > ag2 then
    allowed_link ag2 s2 ag1 s1 sigs
  else (
    try
      match (NamedDecls.elt_val (get sigs ag1) s1).links with
      | None -> true
      | Some l -> l.(ag2 - ag1).(s2)
    with Invalid_argument _ ->
      invalid_arg "Signature.allowed_link: invalid site identifier"
  )

let create ~counters_per_agent agent_sigs =
  {
    agent_sigs;
    counter_agent_info =
      (if counters_per_agent = [] then
         None
       else
         (* If there is a counter agent, we choose 0 for its agent id and 0 and 1 as its port ids *)
         Some { id = 0; arity = 2; ports = 0, 1 });
  }

let is_counter_agent sigs n_id =
  match sigs.counter_agent_info with
  | None -> false
  | Some agent_info -> n_id = agent_info.id

let ports_if_counter_agent sigs n_id =
  match sigs.counter_agent_info with
  | None -> None
  | Some agent_info ->
    if n_id = agent_info.id then
      Some agent_info.ports
    else
      None

let site_is_counter sigs ag_ty id =
  counter_of_site_id id (get sigs ag_ty) <> None

let get_counter_agent_info sigs =
  match sigs.counter_agent_info with
  | None -> failwith "No counter agent"
  | Some counter_agent_info -> counter_agent_info

let print_agent sigs f ag_ty =
  Format.pp_print_string f @@ agent_of_num ag_ty sigs

let print_site sigs ag_ty f id =
  Format.pp_print_string f @@ site_of_id ag_ty id sigs

let print_internal_state sigs ag_ty site f id =
  Format.pp_print_string f @@ internal_state_of_id ag_ty site id sigs

let print_site_internal_state sigs ag_ty site f = function
  | None -> print_site sigs ag_ty f site
  | Some id ->
    Format.fprintf f "%s{%s}"
      (site_of_id ag_ty site sigs)
      (internal_state_of_id ag_ty site id sigs)

let print_counter sigs ag_ty f id =
  match counter_of_site_id id (get sigs ag_ty) with
  | None -> ()
  | Some (c1, c2) -> Format.fprintf f "{=%d/+=%d}" c1 c2

let print_one ?(sigs : s option) (i : int) (f : Format.formatter)
    (signature : t) =
  let pp_int f x =
    if NamedDecls.size x > 0 then
      Format.fprintf f "{%a}"
        (NamedDecls.print ~sep:Pp.space (fun _ na f () ->
             Format.fprintf f "%s" na))
        x
  in
  let pp_link =
    match sigs with
    | None -> fun _ _ _ -> ()
    | Some sigs ->
      fun i f -> (function
        | None -> ()
        | Some links ->
          Format.fprintf f "[%a]"
            (Pp.array Pp.space (fun ag ->
                 Pp.array Pp.space (fun si f b ->
                     if b then
                       Format.fprintf f "%a.%a"
                         (print_site sigs (i + ag))
                         si (print_agent sigs) (i + ag))))
            links)
  in
  let pp_counts f = function
    | None -> ()
    | Some (c1, c2) -> Format.fprintf f "{=%d/+=%d}" c1 c2
  in
  (NamedDecls.print
     ~sep:(fun f -> Format.fprintf f ",@,")
     (fun _ name f site_sig ->
       Format.fprintf f "%s%a%a%a" name pp_int site_sig.internal_state
         (pp_link i) site_sig.links pp_counts site_sig.counters_info))
    f signature

let print f sigs =
  Format.fprintf f "@[<v>%a@]"
    (NamedDecls.print ~sep:Pp.space (fun i n f si ->
         Format.fprintf f "@[<h>%%agent: %s(%a)@]" n (print_one ~sigs i) si))
    sigs.agent_sigs

let to_json sigs = NamedDecls.to_json one_to_json sigs.agent_sigs

let of_json v =
  let agent_sigs : 'a site_sig NamedDecls.t NamedDecls.t =
    NamedDecls.of_json one_of_json v
  in
  match
    Mods.StringMap.find_option "__counter_agent" agent_sigs.NamedDecls.finder
  with
  | Some id ->
    let agent_signature = NamedDecls.elt_val agent_sigs id in
    let ports =
      ( num_of_site ("a", Loc.dummy) agent_signature,
        num_of_site ("b", Loc.dummy) agent_signature )
    in
    { agent_sigs; counter_agent_info = Some { id; arity = 2; ports } }
  | None -> { agent_sigs; counter_agent_info = None }
