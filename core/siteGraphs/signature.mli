(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Store definitions of agents *)

type t
(** Store of one agent *)

val num_of_site : ?agent_name:string -> string Locality.annot -> t -> int
val site_of_num : int -> t -> string
val fold : (int -> string -> 'a -> 'a) -> 'a -> t -> 'a

val num_of_internal_state : int -> string Locality.annot -> t -> int
(** [num_of_internal_state site_id state_name sign] *)

val internal_state_of_num : int -> int -> t -> string
val counter_of_site : int -> t -> (int * int) option
val has_counter : t -> bool

type s
(** Store of all the agents *)

val create :
  counters:(string Locality.annot * string Locality.annot list) list ->
  bool ->
  (string Locality.annot
  * (unit NamedDecls.t
    * (string Locality.annot * string Locality.annot) list
    * (int * int) option)
    NamedDecls.t)
  list ->
  s

val size : s -> int

val get : s -> int -> t
(** [get sigs agent_id] *)

val arity : s -> int -> int
(** [arity sigs agent_id] *)

val max_arity : s -> int
(** [max_arity sigs] returns max {!arities sigs i} *)

val num_of_agent : string Locality.annot -> s -> int
val agent_of_num : int -> s -> string

val id_of_site : string Locality.annot -> string Locality.annot -> s -> int
(** [id_of_site agent_type site_name sigs] *)

val id_of_internal_state :
  string Locality.annot ->
  string Locality.annot ->
  string Locality.annot ->
  s ->
  int
(** [id_of_internal_state agent_type site_name state_name sigs] *)

val internal_states_number : int -> int -> s -> int
(** [internal_state_number agent_id site_id sigs] *)

val default_internal_state : int -> int -> s -> int option

val allowed_link : int -> int -> int -> int -> s -> bool
(** [allowed_link ag1 s1 ag2 s2 sigs] *)

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
val is_counter_agent : s -> int -> bool
val ports_if_counter_agent : s -> int -> (int * int) option
val site_is_counter : s -> int -> int -> bool

val incr_agent : s -> int * int * int * int
(** id, arity, before, after *)
