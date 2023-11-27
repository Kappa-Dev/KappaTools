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
  ?forbidden:Mods.StringSet.t -> (string Loc.annoted * 'a) array -> 'a t
(** [create ~forbidden string_val_assoc] evaluates to a namedDecls.t from the string-to-variable associations [string_val_assoc] except from strings in [forbidden]. Loc info is not kept. *)
(* TODO should we remove Loc info *)

val create_from_list :
  ?forbidden:Mods.StringSet.t -> (string Loc.annoted * 'a) list -> 'a t

(* TODO see if better name, what implementation is to be kept *)
val create_no_loc : ?forbidden:Mods.StringSet.t -> (string * 'a) array -> 'a t
(** [create_no_loc] behaves the same as [create], but without the need to provide the Loc info that will be trashed *)

val size : 'a t -> int

val elt_name : 'a t -> int -> string
(** [elt_name nd i] evaluates to the name declaration of id [i] in [nd], or raises an exception if it doesn't exist *)

val elt_id : ?kind:string -> 'a t -> string Loc.annoted -> int
(** [elt_id ~kind nd (s, pos)] evaluates to the data matching declaration [s] in [nd], or if it doesn't exist, throw and exception with info about [kind] and [pos] *)

val elt_val : 'a t -> int -> 'a
(** Access data by id *)

val fold : (int -> string -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val map : (string -> 'a -> 'b) -> 'a t -> 'b t
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
