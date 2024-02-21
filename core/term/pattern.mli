(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Domain to navigate in the graph *)

type link = UnSpec | Free | Link of int * int
type cc

type t = cc
(**type for domain points*)

type id

val debug_print_id : Format.formatter -> id -> unit

module ObsMap : sig
  (** Maps from patterns to something *)

  type 'a t

  val dummy : 'a -> 'a t
  val get : 'a t -> id -> 'a
  val set : 'a t -> id -> 'a -> unit
  val fold_lefti : (id -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val iteri : (id -> 'a -> unit) -> 'a t -> unit

  val print :
    ?trailing:(Format.formatter -> unit) ->
    (Format.formatter -> unit) ->
    (id -> Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit
end

module Env : sig
  type transition = private {
    next: Navigation.abstract Navigation.t;
    dst: id;  (** id of cc and also address in the Env.domain map *)
    inj: Renaming.t;  (** From dst To ("this" cc + extra edge) *)
  }

  type point

  val content : point -> cc

  val roots : point -> (int list * int) option
  (** (ids,ty) *)

  val deps : point -> Operator.DepSet.t
  val sons : point -> transition list

  type t

  val get : t -> id -> point
  val get_single_agent : int -> t -> (id * Operator.DepSet.t) option

  val get_elementary :
    debug_mode:bool ->
    t ->
    Agent.t ->
    int ->
    Navigation.abstract Navigation.arrow ->
    (id * point * Renaming.t) option

  val signatures : t -> Signature.s
  val new_obs_map : t -> (id -> 'a) -> 'a ObsMap.t
  val to_navigation : t -> id -> Navigation.abstract Navigation.t
  val print : noCounters:bool -> Format.formatter -> t -> unit
  val to_yojson : t -> Yojson.Basic.t
  val of_yojson : Yojson.Basic.t -> t
end

module PreEnv : sig
  type t
  type stat = { stat_nodes: int; stat_nav_steps: int }

  val sigs : t -> Signature.s
  val of_env : Env.t -> t
  val empty : Signature.s -> t
  val debug_print : Format.formatter -> t -> unit
end

(** {2 Create a connected component} *)
type work
(** type of a PreEnv during a pattern construction *)

val empty_cc : Signature.s -> cc

val begin_new : PreEnv.t -> work
(** Starts creation *)

val new_node : work -> int -> Agent.t * work
(** [new_node wk node_type] *)

val new_link : work -> Agent.t * int -> Agent.t * int -> work
(** [new_link wk (node, site_id) (node', site_id')] *)

val new_free : work -> Agent.t * int -> work

val new_internal_state : work -> Agent.t * int -> int -> work
(** [new_link_type work (node,site) type] *)

val finish_new :
  debug_mode:bool ->
  ?origin:Operator.rev_dep ->
  work ->
  PreEnv.t * Renaming.t * cc * id

(** {2 Use a connected component } *)

val compare_canonicals : id -> id -> int
val is_equal_canonicals : id -> id -> bool

val print_cc :
  noCounters:bool ->
  ?dotnet:bool ->
  ?full_species:bool ->
  ?sigs:Signature.s ->
  ?cc_id:id ->
  with_id:bool ->
  Format.formatter ->
  t ->
  unit

val print_cc_as_id : Signature.s -> Format.formatter -> t -> unit

val print :
  noCounters:bool ->
  ?domain:Env.t ->
  with_id:bool ->
  Format.formatter ->
  id ->
  unit
(** [print ~domain ?with_id:None form cc] *)

val id_to_yojson : id -> Yojson.Basic.t
val id_of_yojson : Yojson.Basic.t -> id
val reconstruction_navigation : t -> Navigation.abstract Navigation.t

val find_ty : cc -> int -> int
(** Abstraction leak, please do not use *)

val automorphisms : debug_mode:bool -> t -> Renaming.t list

val embeddings_to_fully_specified :
  debug_mode:bool -> Env.t -> id -> cc -> Renaming.t list

val size_of_cc : cc -> int

val fold_by_type :
  (pos:int -> agent_type:int -> (link * int) array -> 'a -> 'a) ->
  cc ->
  'a ->
  'a
(** USE WITH CARE: Break some abstraction. The array must not be
   modified and internal state [-1] means unspecified *)

val fold : (int -> (link * int) array -> 'acc -> 'acc) -> cc -> 'acc -> 'acc
(** USE WITH CARE: Break some abstraction. The array must not be
    modified and internal state [-1] means unspecified *)

type sharing_level =
  | No_sharing
  | Compatible_patterns
  | Max_sharing  (** Heuristic to use on domain construction *)

val write_sharing_level : Buffer.t -> sharing_level -> unit
(** Output a JSON value of type {!sharing_level}. *)

val string_of_sharing_level : ?len:int -> sharing_level -> string
(** Serialize a value of type {!sharing_level}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sharing_level :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sharing_level
(** Input JSON data of type {!sharing_level}. *)

val sharing_level_of_string : string -> sharing_level
(** Deserialize JSON data of type {!sharing_level}. *)

val finalize :
  debug_mode:bool ->
  sharing:sharing_level ->
  PreEnv.t ->
  Contact_map.t ->
  Env.t * PreEnv.stat

val infs : debug_mode:bool -> Signature.s -> t -> t -> t list
val matchings : debug_mode:bool -> Signature.s -> t -> t -> Renaming.t list

val merge_on_inf :
  debug_mode:bool ->
  PreEnv.t ->
  Renaming.t ->
  t ->
  t ->
  t option * (t * int * t * int * int * bool) option

val length : t -> int

module Set : SetMap.Set with type elt = id
module Map : SetMap.Map with type elt = id

val counter_value_cc : cc -> Mods.IntMap.elt * int -> int -> int
