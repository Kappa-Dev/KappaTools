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

let print_link ~explicit_free f = function
  | FREE -> if explicit_free then Format.pp_print_string f "!."
  | VAL i -> Format.fprintf f "!%i" i

let aux_pp_si sigs a s f i =
  match sigs with
  | Some sigs -> Signature.print_site_internal_state sigs a s f i
  | None ->
    match i with
    | Some i -> Format.fprintf f "%i~%i" s i
    | None -> Format.pp_print_int f s

let print_intf ~explicit_free compact with_link ?sigs ag_ty f (ports,ints) =
  let rec aux empty i =
    if i < Array.length ports then
      let () = Format.fprintf
          f "%t%a%a"
          (if empty then Pp.empty
           else if compact then Pp.compact_comma else Pp.comma)
          (aux_pp_si sigs ag_ty i)
          ints.(i)
          (if with_link then print_link ~explicit_free else (fun _ _ -> ()))
          ports.(i) in
      aux false (succ i) in
  aux true 0

let aux_pp_ag sigs f a =
  match sigs with
  | Some sigs -> Signature.print_agent sigs f a
  | None -> Format.pp_print_int f a

let print_agent ~explicit_free compact created link ?sigs f ag =
  Format.fprintf f "%t%a(@[<h>%a@])"
    (fun f -> if created then Format.pp_print_string f "+")
    (aux_pp_ag sigs) ag.a_type
    (print_intf ~explicit_free compact link ?sigs ag.a_type)
    (ag.a_ports, ag.a_ints)

let print ~explicit_free ~compact ~created ?sigs f mix =
  Pp.list Pp.comma (print_agent ~explicit_free compact created true ?sigs) f mix

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
