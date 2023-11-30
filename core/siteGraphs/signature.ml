(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t =
  (unit NamedDecls.t * bool array array option * (int * int) option)
  NamedDecls.t

let fold f = NamedDecls.fold (fun i n o _ -> f i n o)

let num_of_site ?agent_name site_name sign =
  let kind =
    match agent_name with
    | None -> "site name"
    | Some agent_name -> "site name for agent " ^ agent_name
  in
  NamedDecls.elt_id ~kind sign site_name

let site_of_num addr sign =
  try NamedDecls.elt_name sign addr with Invalid_argument _ -> raise Not_found

let num_of_internal_state site_id state sign =
  try
    let na, (nd, _, _) = sign.NamedDecls.decls.(site_id) in
    NamedDecls.elt_id ~kind:("internal state for site " ^ na) nd state
  with Invalid_argument _ -> raise Not_found

let internal_state_of_num site_num val_num sign =
  try
    let _, (nd, _, _) = sign.NamedDecls.decls.(site_num) in
    fst nd.NamedDecls.decls.(val_num)
  with Invalid_argument _ -> raise Not_found

let counter_of_site site_id sign =
  try
    let _, (_, _, c) = sign.NamedDecls.decls.(site_id) in
    c
  with Invalid_argument _ -> raise Not_found

let has_counter sign =
  fold
    (fun p_id _ ok ->
      try
        let _, (_, _, c) = sign.NamedDecls.decls.(p_id) in
        ok || not (c = None)
      with Invalid_argument _ -> raise Not_found)
    false sign

let one_to_json =
  NamedDecls.to_json (fun (a, b, c) ->
      `List
        [
          NamedDecls.to_json (fun () -> `Null) a;
          JsonUtil.of_option
            (fun links ->
              `List
                (Array.fold_right
                   (fun a acc ->
                     `List (Array.fold_right (fun b c -> `Bool b :: c) a [])
                     :: acc)
                   links []))
            b;
          JsonUtil.of_option (fun (c1, c2) -> `List [ `Int c1; `Int c2 ]) c;
        ])

let one_of_json =
  NamedDecls.of_json (function
    | `List [ a; b; c ] ->
      ( NamedDecls.of_json
          (function
            | `Null -> ()
            | x ->
              raise
                (Yojson.Basic.Util.Type_error ("Problematic agent signature", x)))
          a,
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
                (Yojson.Basic.Util.Type_error ("Problematic agent signature", x)))
          b,
        Yojson.Basic.Util.to_option
          (function
            | `List [ `Int c1; `Int c2 ] -> c1, c2
            | x ->
              raise
                (Yojson.Basic.Util.Type_error ("Problematic agent signature", x)))
          c )
    | x ->
      raise (Yojson.Basic.Util.Type_error ("Problematic agent signature", x)))

type s = { t: t NamedDecls.t; incr: int option; incr_sites: (int * int) option }

let size sigs = NamedDecls.size sigs.t
let get sigs agent_id = snd sigs.t.NamedDecls.decls.(agent_id)
let arity sigs agent_id = NamedDecls.size (get sigs agent_id)

let max_arity sigs =
  NamedDecls.fold (fun _ _ x a -> max x (NamedDecls.size a)) 0 sigs.t

let agent_of_num i sigs = NamedDecls.elt_name sigs.t i
let num_of_agent name sigs = NamedDecls.elt_id ~kind:"agent" sigs.t name

let id_of_site ((agent_name, _) as agent_ty) site_name sigs =
  let n = num_of_agent agent_ty sigs in
  num_of_site ~agent_name site_name (get sigs n)

let site_of_id agent_id site_id sigs = site_of_num site_id (get sigs agent_id)

let id_of_internal_state ((agent_name, _) as agent_ty) site_name state sigs =
  let n = num_of_agent agent_ty sigs in
  let sign = get sigs n in
  let site_id = num_of_site ~agent_name site_name sign in
  num_of_internal_state site_id state sign

let internal_state_of_id agent_id id_site id_state sigs =
  internal_state_of_num id_site id_state (get sigs agent_id)

let internal_states_number agent_id site_num sigs =
  try
    let _, (nd, _, _) = (get sigs agent_id).NamedDecls.decls.(site_num) in
    NamedDecls.size nd
  with Invalid_argument _ -> raise Not_found

let default_internal_state agent_id site_id sigs =
  try
    let _, (nd, _, _) = (get sigs agent_id).NamedDecls.decls.(site_id) in
    if nd.NamedDecls.decls = [||] then
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
      match (get sigs ag1).NamedDecls.decls.(s1) with
      | _, (_, None, _) -> true
      | _, (_, Some l, _) -> l.(ag2 - ag1).(s2)
    with Invalid_argument _ ->
      invalid_arg "Signature.allowed_link: invalid site identifier"
  )

let add_incr counters =
  let annot = Locality.dummy in
  let a_port = "a", annot in
  let b_port = "b", annot in
  let incr = "__incr", Locality.dummy in
  let after = a_port, (NamedDecls.create [||], [ b_port, incr ], None) in
  let before_lnks =
    List.fold_right
      (fun (ag, counts) acc -> List.map (fun c -> c, ag) counts @ acc)
      counters
      [ a_port, incr ]
  in
  let before = b_port, (NamedDecls.create [||], before_lnks, None) in
  let lnks = NamedDecls.create [| after; before |] in
  let counter_agent = incr, lnks in
  counter_agent

let create ~counters contact_map sigs =
  let sigs' =
    if counters <> [] then
      add_incr counters :: sigs
    else
      sigs
  in
  let t = Array.of_list sigs' in
  let raw = NamedDecls.create t in
  let s = Array.length t in
  let snd_of_third (_, a, _) = a in
  {
    t =
      NamedDecls.mapi
        (fun ag ag_na ->
          NamedDecls.mapi (fun _ si_na (ints, links, counts) ->
              if not contact_map then
                ints, None, counts
              else (
                let out =
                  Array.init (s - ag) (fun i ->
                      Array.make
                        (NamedDecls.size (snd raw.NamedDecls.decls.(i + ag)))
                        false)
                in
                let () =
                  List.iter
                    (fun (((site_name, pos) as site), ((agent_name, _) as agent)) ->
                      let a = NamedDecls.elt_id ~kind:"agent" raw agent in
                      let s =
                        num_of_site ~agent_name site
                          (snd raw.NamedDecls.decls.(a))
                      in
                      let () = if a >= ag then out.(a - ag).(s) <- true in
                      if
                        List.exists
                          (fun ((x, _), (y, _)) -> x = si_na && y = ag_na)
                          (snd_of_third
                             (snd
                                (snd raw.NamedDecls.decls.(a)).NamedDecls.decls.(
                                s)))
                      then
                        ()
                      else
                        raise
                          (ExceptionDefn.Malformed_Decl
                             ( Format.asprintf "No link to %s.%s from %s.%s."
                                 si_na ag_na site_name agent_name,
                               pos )))
                    links
                in
                ints, Some out, counts
              )))
        raw;
    incr =
      (if counters = [] then
         None
       else
         Some 0);
    incr_sites =
      (if counters = [] then
         None
       else
         Some (0, 1));
  }

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
  match counter_of_site id (get sigs ag_ty) with
  | None -> ()
  | Some (c1, c2) -> Format.fprintf f "{=%d/+=%d}" c1 c2

let print_one ?sigs i f sign =
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
     (fun _ name f (ints, links, counts) ->
       Format.fprintf f "%s%a%a%a" name pp_int ints (pp_link i) links pp_counts
         counts))
    f sign

let print f sigs =
  Format.fprintf f "@[<v>%a@]"
    (NamedDecls.print ~sep:Pp.space (fun i n f si ->
         Format.fprintf f "@[<h>%%agent: %s(%a)@]" n (print_one ~sigs i) si))
    sigs.t

let to_json sigs = NamedDecls.to_json one_to_json sigs.t

let of_json v =
  let t = NamedDecls.of_json one_of_json v in
  let incr, incr_sites =
    match Mods.StringMap.find_option "__incr" t.NamedDecls.finder with
    | Some incr_id ->
      let incr = snd t.NamedDecls.decls.(incr_id) in
      let after = num_of_site ("a", Locality.dummy) incr in
      let before = num_of_site ("b", Locality.dummy) incr in
      Some incr_id, Some (before, after)
    | None -> None, None
  in
  { t; incr; incr_sites }

let is_counter_agent sigs n_id =
  match sigs.incr with
  | None -> false
  | Some incr_id -> n_id = incr_id

let ports_if_counter_agent sigs n_id =
  if
    match sigs.incr with
    | None -> false
    | Some incr_id -> n_id = incr_id
  then
    sigs.incr_sites
  else
    None

let site_is_counter sigs ag_ty id = counter_of_site id (get sigs ag_ty) <> None

let incr_agent sigs =
  match sigs.incr with
  | None -> failwith "No incr agent"
  | Some id ->
    (match sigs.incr_sites with
    | None -> failwith "Signature of counter inconsistent"
    | Some (before, after) -> id, 2, before, after)
