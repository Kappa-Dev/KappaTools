(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Store definitions of agents *)

(* TODO: here, we talk about agents, but is it actually agent signatures/definitions ? while instances are in siteGraphs/agent.mli *)
(* TODO document what is internal state *)

(* TODO *)
type 'links site_sig = {
  internal_state: unit NamedDecls.t;
  links: 'links option;
  counters_info: (int * int) option;
      (** If relevant: counter CEQ value * counter delta *)
}

type t = bool array array site_sig NamedDecls.t
(** Store of one agent signature *)

(* TODO remove Loc annotations here ? *)
val num_of_site : ?agent_name:string -> string Loc.annoted -> t -> int
val site_of_num : int -> t -> string
val fold : (int -> string -> 'a -> 'a) -> 'a -> t -> 'a

val num_of_internal_state : int -> string Loc.annoted -> t -> int
(** [num_of_internal_state site_id state_name signature] *)

val internal_state_of_site_id : int -> int -> t -> string
(**[internal_state_of_site_id site_id value_id signature] *)

val counter_of_site_id : int -> t -> (int * int) option
val has_counter : t -> bool

type s
(** Store of all the agents, s as a plural *)

(* TODO See what to be kept here? *)
val create :
  counters_per_agent:(string Loc.annoted * string Loc.annoted list) list ->
  t NamedDecls.t ->
  s

val size : s -> int

val get : s -> int -> t
(** [get sigs agent_id] *)

val arity : s -> int -> int
(** [arity sigs agent_id] *)

val max_arity : s -> int
(** [max_arity sigs] returns max {!arities sigs i} *)

val num_of_agent : string Loc.annoted -> s -> int
val agent_of_num : int -> s -> string

val id_of_site : string Loc.annoted -> string Loc.annoted -> s -> int
(** [id_of_site agent_type site_name sigs] *)

val id_of_internal_state :
  string Loc.annoted -> string Loc.annoted -> string Loc.annoted -> s -> int
(** [id_of_internal_state agent_type site_name state_name sigs] *)

val internal_states_number : int -> int -> s -> int
(** [internal_state_number agent_id site_id sigs] *)

val default_internal_state : int -> int -> s -> int option

val allowed_link : int -> int -> int -> int -> s -> bool
(** [allowed_link ag1 s1 ag2 s2 sigs] evaluates to true if and only if it is allowed to create a link between site [s1] of agent [ag1] and site [s2] of agent [ag2] *)

(** {2 Counter specific} *)

(** If there are counters in the signature, we define a single agent as the
 * _counter agent_, which will be used as _dummies_ to keep track of the counter value *)

val is_counter_agent : s -> int -> bool
val ports_if_counter_agent : s -> int -> (int * int) option
val site_is_counter : s -> int -> int -> bool

type counter_agent_info = { id: int; arity: int; ports: int * int }

val get_counter_agent_info : s -> counter_agent_info
(** [counter_agent agent_sigs] *)

(** {2 I/O} *)

val print_agent : s -> Format.formatter -> int -> unit
val print_site : s -> int -> Format.formatter -> int -> unit
val print_internal_state : s -> int -> int -> Format.formatter -> int -> unit

val print_site_internal_state :
  s -> int -> int -> Format.formatter -> int option -> unit
(** [print_site_internal_state sigs agent_type site_id f state_id]
prints both the site and its internal state if it is not [None]. *)

val print_counter : s -> int -> Format.formatter -> int -> unit
val print : Format.formatter -> s -> unit
val to_json : s -> Yojson.Basic.t
val of_json : Yojson.Basic.t -> s
