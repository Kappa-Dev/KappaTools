(** Concrete graph implementation *)

type agent = int * int
(** agent_id * agent_type *)

type t

val empty : unit -> t

val add_agent : Signature.s -> int -> t -> int * t
(** [add_agent sigs agent_type graph] *)

val add_free : int -> int -> t -> t
(** [add_free agent_id site graph] *)

val add_internal : int -> int -> int -> t -> t
(** [add_internal agent_id site internal_state graph] *)

val add_link : agent -> int -> agent -> int -> t -> t
(** [add_link ag1 s1 ag2 s2 t] *)

val remove_agent : int -> t -> t
val remove_free : int -> int -> t -> t
val remove_internal : int -> int -> t -> t
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

val get_internal : int -> int -> t -> int
(** [get_internal ag site graph] *)

type path = ((agent * int) * (agent * int)) list
val empty_path : path

val rev_path : path -> path
val print_path :
  ?sigs:Signature.s -> ?graph:t -> Format.formatter -> path -> unit

val are_connected :
  ?candidate:path -> Signature.s -> t -> int -> int -> int -> (int * int) list
  -> int list -> int option -> bool -> path option
(** [are_connected ?candidate sigs graph x_name x y nodes_x nodes_y dist store_dist] *)

val paths_of_interest : (int -> 'a option) -> Signature.s -> t -> int ->
			  int -> path -> (('a*int) * path) list
(** [paths_of_interest
         is_interesting sigs graph agent_name agent_id done_path] *)

val build_snapshot : Signature.s -> t -> (int * Raw_mixture.t) list

val debug_print : Format.formatter -> t -> unit
