(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Injection from a pattern in the mixture *)

type t
type matching = t

val empty : t
val debug_print : Format.formatter -> t -> unit
val get : debugMode:bool -> Agent.t * int -> t -> int

val reconstruct_renaming :
  debugMode:bool -> Pattern.Env.t -> Edges.t -> Pattern.id -> int -> Renaming.t
(** [reconstruct_renaming domain graph cc root] *)

val reconstruct :
  debugMode:bool ->
  Pattern.Env.t ->
  Edges.t ->
  t ->
  int ->
  Pattern.id ->
  int ->
  t option
(** [reconstruct domain graph matching_of_previous_cc cc_id_in_rule cc root] *)

val add_cc : t -> int -> Renaming.t -> t option

val is_root_of :
  debugMode:bool -> Pattern.Env.t -> Edges.t -> Agent.t -> Pattern.id -> bool

val roots_of :
  debugMode:bool -> Pattern.Env.t -> Edges.t -> Pattern.id -> IntCollection.t

val elements_with_types :
  Pattern.Env.t -> Pattern.id array -> t -> Agent.t list array

type cache

val empty_cache : cache

val observables_from_agent :
  Pattern.Env.t ->
  Edges.t ->
  ((Pattern.id * (int * int)) list * Operator.DepSet.t) * cache ->
  Agent.t ->
  ((Pattern.id * (int * int)) list * Operator.DepSet.t) * cache
(** [observables_from_free domain graph sort agent]
    the int * int in the return list and the following ones
    is a Instantiation.concrete *)

val observables_from_free :
  debugMode:bool ->
  Pattern.Env.t ->
  Edges.t ->
  ((Pattern.id * (int * int)) list * Operator.DepSet.t) * cache ->
  Agent.t ->
  int ->
  ((Pattern.id * (int * int)) list * Operator.DepSet.t) * cache
(** [observables_from_free domain graph sort agent site] *)

val observables_from_internal :
  debugMode:bool ->
  Pattern.Env.t ->
  Edges.t ->
  ((Pattern.id * (int * int)) list * Operator.DepSet.t) * cache ->
  Agent.t ->
  int ->
  int ->
  ((Pattern.id * (int * int)) list * Operator.DepSet.t) * cache
(** [observables_from_internal domain graph sort agent site internal_state] *)

val observables_from_link :
  debugMode:bool ->
  Pattern.Env.t ->
  Edges.t ->
  ((Pattern.id * (int * int)) list * Operator.DepSet.t) * cache ->
  Agent.t ->
  int ->
  Agent.t ->
  int ->
  ((Pattern.id * (int * int)) list * Operator.DepSet.t) * cache
(** [observables_from_link domain graph sort ag site sort' ag' site'] *)

module Agent : sig
  (** An agent in a connected component *)

  type t =
    | Existing of Agent.t * int (* node, cc_id *)
    | Fresh of int * int (* type, id *)

  val rename : debugMode:bool -> int -> Renaming.t -> t -> t

  val concretize :
    debugMode:bool -> matching * int Mods.IntMap.t -> t -> int * int

  val get_type : t -> int
  val get_id : t -> int
  val is_fresh : t -> bool
  val print : ?sigs:Signature.s -> Format.formatter -> t -> unit
  val print_site : ?sigs:Signature.s -> t -> Format.formatter -> int -> unit

  val print_internal :
    ?sigs:Signature.s -> t -> int -> Format.formatter -> int -> unit

  val to_yojson : t -> Yojson.Basic.t
  val of_yojson : Yojson.Basic.t -> t
end
