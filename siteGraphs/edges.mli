(** Concrete graph implementation *)

type agent = int * int
(** agent_id * agent_type *)

type t

val empty : t

val add_agent : int -> t -> int * t
(** [add_agent agent_type graph] *)

val add_free : int -> int -> t -> t
(** [add_free agent_id site graph] *)

val add_internal : int -> int -> int -> t -> t
(** [add_internal agent_id site internal_state graph] *)

val add_link : agent -> int -> agent -> int -> t -> t
(** [add_link ag1 s1 ag2 s2 t] *)


val remove_agent : agent -> t -> t
val remove_free : int -> int -> t -> t
val remove_internal : int -> int -> t -> (t * int)
val remove_link : int -> int -> int -> int -> t -> t

val is_agent : agent -> t -> bool
(** [is_agent agent graph] *)

val is_free : int -> int -> t -> bool
(** [is_free agent_id site graph] *)

val is_internal : int -> int -> int -> t -> bool
(** [is_internal internal_state agent_id site graph] *)

val link_exists : int -> int -> int -> int -> t -> bool
(** [link_exists ag1_id site1 ag2_id site2 graph] *)

val exists_fresh : int -> int -> int -> int -> t -> int option
(** [exists_fresh ag1 site1 type_of_ag2 site2 graph] *)

val link_destination : int -> int -> t -> (agent * int) option
(** [link_destination ag site graph] *)

type path = ((agent * int) * (agent * int)) list
val empty_path : path

val rev_path : path -> path
val print_path :
  ?sigs:Signature.s -> ?graph:t -> Format.formatter -> path -> unit

val are_connected : ?candidate:path -> t -> int -> int -> int -> path option
(** [are_connected ?candidate graph x_name x y] *)

val pathes_of_interrest :
  (int -> 'a option) -> t -> int -> int -> path -> (('a*int) * path) list
(** [pathes_of_interrest is_interesting graph agent_name agent_id done_path] *)

val print : Signature.s -> Format.formatter -> t -> unit
val print_dot : Signature.s -> Format.formatter -> t -> unit
val debug_print : Format.formatter -> t -> unit
