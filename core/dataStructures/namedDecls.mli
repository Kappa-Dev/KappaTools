(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Stores a bunch of stuff the user gave a name to *)

type 'a t = private {
  decls: (string * 'a) array;  (** the name of the stuff * the stuff *)
  finder: int Mods.StringMap.t;
      (** [fst (fst d.decls.(StringMap.find s d.finder))] MUST be equal to [s] *)
}

val create :
  ?forbidden:Mods.StringSet.t -> (string Locality.annot * 'a) array -> 'a t

val size : 'a t -> int
val elt_name : 'a t -> int -> string
val elt_id : ?kind:string -> 'a t -> string Locality.annot -> int
val fold : (int -> string -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val mapi : (int -> string -> 'a -> 'b) -> 'a t -> 'b t

val print :
  sep:(Format.formatter -> unit) ->
  (int -> string -> Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a t ->
  unit

val debug_print :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val to_json : ('a -> Yojson.Basic.t) -> 'a t -> Yojson.Basic.t

val of_json : (Yojson.Basic.t -> 'a) -> Yojson.Basic.t -> 'a t
(** @raise Yojson.Basic.Util.Type_error if it fails *)
