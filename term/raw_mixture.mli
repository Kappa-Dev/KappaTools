(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type internal = int option
type link = FREE | VAL of int
type agent =
    { a_type: int; a_ports: link array; a_ints: internal array; }
type t = agent list

val copy_agent : agent -> agent

val print :
  noCounters:bool -> created:bool -> ?sigs:Signature.s ->
  Format.formatter -> t -> unit

val to_json : t -> Yojson.Basic.json
val of_json : Yojson.Basic.json -> t

type incr_t = {
    father : int Mods.DynArray.t;
    rank : (int * (bool * bool)) Mods.DynArray.t;
  (*size of the equivalence * (true - CEQ, false - CGTE) * (is_incr) array*)
   }

val union_find_counters : Signature.s option -> t -> incr_t

val find : incr_t -> int -> int
val union: incr_t -> int -> int -> unit
val create : int -> incr_t
