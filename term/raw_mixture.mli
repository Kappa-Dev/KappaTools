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
    { a_type: int; a_ports: link array; a_ints: internal array; }
type t = agent list

type snapshot
val empty_snapshot : snapshot
val increment_in_snapshot : Signature.s -> t -> snapshot -> snapshot
val output_snapshot : snapshot -> (int * t) list

val print :
  explicit_free:bool -> compact:bool -> created:bool ->
  ?sigs:Signature.s -> Format.formatter -> t -> unit

val print_dot : Signature.s -> int -> Format.formatter -> t -> unit

val to_json : t -> Yojson.Basic.json
val of_json : Yojson.Basic.json -> t
