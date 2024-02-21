(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = int * int
(** agent_id * agent_type *)

type ag = t

let make ~id ~sort = id, sort

let print ?sigs ~with_id f (i, ty) =
  match sigs with
  | Some sigs ->
    Format.fprintf f "%a%t" (Signature.print_agent sigs) ty (fun f ->
        if with_id then Format.fprintf f "/*%i*/" i)
  | None -> Format.fprintf f "n%i" i

let print_site ?sigs (i, agent) f id =
  match sigs with
  | Some sigs -> Signature.print_site sigs agent f id
  | None -> Format.fprintf f "n%is%i" i id

let print_internal ?sigs (i, agent) site f id =
  match sigs with
  | Some sigs -> Signature.print_site_internal_state sigs agent site f (Some id)
  | None -> Format.fprintf f "n%is%i~%i" i site id

let print_raw_internal ?sigs (i, agent) site f id =
  match sigs with
  | Some sigs -> Signature.print_internal_state sigs agent site f id
  | None -> Format.fprintf f "n%is%i~%i" i site id

let rename ~debug_mode inj (n_id, n_ty) =
  Renaming.apply ~debug_mode inj n_id, n_ty

let sort (_, ty) = ty
let id (id, _) = id
let compare (id1, _) (id2, _) = Mods.int_compare id1 id2
let json_dictionnary = "\"agent\":{\"id\":0,\"type\":1}"

let write_json ob a =
  JsonUtil.write_compact_pair Yojson.Basic.write_int Yojson.Basic.write_int ob a

let read_json p lb =
  JsonUtil.read_compact_pair Yojson.Basic.read_int Yojson.Basic.read_int p lb

let to_json (id, ty) = `List [ `Int id; `Int ty ]

let of_json = function
  | `List [ `Int id; `Int ty ] -> id, ty
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid agent", x))

module SetMap = SetMap.Make (struct
  type t = ag

  let compare = compare
  let print = print ?sigs:None ~with_id:true
end)
