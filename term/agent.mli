(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** A node of a site graph *)

type t = int * int
(** agent_id * agent_type *)

val compare : t -> t -> int

val sort : t -> int
val id : t -> int

val print :
  ?sigs:Signature.s -> with_id:bool -> Format.formatter -> t -> unit
val print_site :
  ?sigs:Signature.s -> t -> Format.formatter -> int -> unit
val print_internal :
  ?sigs:Signature.s -> t -> int -> Format.formatter -> int -> unit

val rename : Renaming.t -> t -> t

val to_json : t -> Yojson.Basic.json
val of_json : Yojson.Basic.json -> t
