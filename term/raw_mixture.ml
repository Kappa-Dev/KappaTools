(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type internal = int option

type link = FREE | VAL of int

type agent =
  {
    a_type: int;
    a_ports: link array;
    a_ints: internal array;
  }

type t = agent list

(* we are looking for an increment agent linked to ag:
  find the agent with the 'VAL i' link value and with a different type *)
let find_ag_with_link mix i ag_ty =
  List.fold_left
    (fun acc a ->
      if ag_ty = a.a_type then acc
      else
        Tools.array_fold_lefti
          (fun pid acc' p -> match p with
             | FREE -> acc'
             | VAL i' -> if (i=i') then (a.a_type,pid)
                         else acc')
          acc a.a_ports) (-1,-1) mix

(* we are looking for the current increment agent:
  find the agent with the same link value, the same type and the same port
  then check the link value of the other port *)
let rec counter_value mix i ?limit (ag_ty,pid) count =
  try
    let incr =
      List.find
        (fun a ->
          ag_ty = a.a_type &&
            Tools.array_fold_lefti
              (fun pid' acc p ->
                (pid = pid' &&
                   (match p with
                    | FREE -> false
                    | VAL i' -> i=i')) || acc)
              false a.a_ports) mix in
    Array.fold_left
      (fun acc el ->
        match el with
        | FREE -> acc
        | VAL i' ->
           if (i=i') then acc else counter_value mix i' ?limit (ag_ty,pid) (acc+1))
      count incr.a_ports
  with Not_found ->
    if not(limit = None) then count else
      raise (ExceptionDefn.Internal_Error
               (Locality.dummy_annot
                  ("Link"^ string_of_int i ^"is broken")))

let counters_chain_length mix i limit =
  let (ag_ty,pid) = find_ag_with_link mix i (-1) in
  if ag_ty = -1 then None else
    Some (counter_value mix i ~limit (ag_ty,pid) 0)

let print_link mix sigs ag_ty f = function
  | FREE -> Format.pp_print_string f "[.]"
  | VAL i ->
     let (dst_ag,dst_port) = find_ag_with_link mix i ag_ty in
     if not(dst_ag = -1) && (Signature.is_counter dst_ag sigs) &&
          not(!Parameter.debugModeOn) then
       let counter = counter_value mix i (dst_ag,dst_port) 0 in
       Format.fprintf f "{=%d}" counter
     else Format.fprintf f "[%i]" i

let aux_pp_si sigs a s f i =
  match sigs with
  | Some sigs -> Signature.print_site_internal_state sigs a s f i
  | None ->
    match i with
    | Some i -> Format.fprintf f "%i{%i}" s i
    | None -> Format.pp_print_int f s

let print_intf with_link ?sigs mix ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      let () = Format.fprintf
          f "%t%a%a"
          (if empty then Pp.empty else Pp.space)
          (aux_pp_si sigs ag_ty i) ints.(i)
          (if with_link then print_link mix sigs ag_ty else (fun _ _ -> ()))
          ports.(i) in
      aux false (succ i) in
  aux true 0

let aux_pp_ag sigs f a =
  match sigs with
  | Some sigs -> Signature.print_agent sigs f a
  | None -> Format.pp_print_int f a

let print_agent created link ?sigs mix f ag =
  Format.fprintf f "%a(@[<h>%a@])%t"
      (aux_pp_ag sigs) ag.a_type
      (print_intf link ?sigs mix ag.a_type) (ag.a_ports, ag.a_ints)
      (fun f -> if created then Format.pp_print_string f "+")

let print ~created ?sigs f mix =
  let mix_without_counters = if (!Parameter.debugModeOn) then mix else
    List.filter
      (fun ag -> not(Signature.is_counter ag.a_type sigs)) mix in
  Pp.list
    Pp.comma
    (print_agent created true ?sigs mix)
    f mix_without_counters

let agent_to_json a =
  `Assoc
    [("type",`Int a.a_type);
     ("sites", `List
        (Array.fold_right (fun x acc ->
             (match x with FREE -> `Null|VAL i -> `Int i)::acc) a.a_ports []));
     ("internals", `List
        (Array.fold_right (fun x acc ->
             (match x with None -> `Null|Some i -> `Int i)::acc) a.a_ints []))]
let agent_of_json = function
  | (`Assoc [("type",`Int t);("sites",`List s);("internals",`List i)] |
     `Assoc [("type",`Int t);("internals",`List i);("sites",`List s)] |
     `Assoc [("internals",`List i);("type",`Int t);("sites",`List s)] |
     `Assoc [("internals",`List i);("sites",`List s);("type",`Int t)] |
     `Assoc [("sites",`List s);("internals",`List i);("type",`Int t)] |
     `Assoc [("sites",`List s);("type",`Int t);("internals",`List i)]) ->
    { a_type = t;
      a_ports =
        Tools.array_map_of_list (function
            | `Null -> FREE
            | `Int p -> VAL p
            | x ->
              raise (Yojson.Basic.Util.Type_error ("Invalid site link",x)))
          s;
      a_ints =
        Tools.array_map_of_list (function
            | `Null -> None
            | `Int p -> Some p
            | x ->
              raise (Yojson.Basic.Util.Type_error ("Invalid internal state",x)))
          i}
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid raw_agent",x))

let to_json m = `List (List.map agent_to_json m)
let of_json = function
  | `List l -> List.map agent_of_json l
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid raw_mixture",x))
