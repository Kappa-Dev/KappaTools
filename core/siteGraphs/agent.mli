(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** A node of a site graph *)

type t = int * int
(** agent_id * agent_type *)

val make : id:int -> sort:int -> t
val compare : t -> t -> int
val sort : t -> int
val id : t -> int
val print : ?sigs:Signature.s -> with_id:bool -> Format.formatter -> t -> unit
val print_site : ?sigs:Signature.s -> t -> Format.formatter -> int -> unit

val print_internal :
  ?sigs:Signature.s -> t -> int -> Format.formatter -> int -> unit

val print_raw_internal :
  ?sigs:Signature.s -> t -> int -> Format.formatter -> int -> unit

val rename : debug_mode:bool -> Renaming.t -> t -> t
val json_dictionnary : string
val write_json : Buffer.t -> t -> unit
val read_json : Yojson.Basic.lexer_state -> Lexing.lexbuf -> t
val to_json : t -> Yojson.Basic.t
val of_json : Yojson.Basic.t -> t

module SetMap : SetMap.S with type elt = t
