(** Concrete graph implementation *)

type agent = int * int
(** agent_id * agent_type *)

val print_agent :
  ?sigs:Signature.s -> Format.formatter -> agent -> unit
val agent_to_json : agent -> Yojson.Basic.json
val agent_of_json : Yojson.Basic.json -> agent

type t

val empty : with_connected_components : bool -> t

val copy : t -> t
(** You'd better NOT use that on the state of a simulation *)

val add_agent : Signature.s -> int -> t -> int * t
(** [add_agent sigs agent_type graph] *)

val add_free : int -> int -> t -> t
(** [add_free agent_id site graph] *)

val add_internal : int -> int -> int -> t -> t
(** [add_internal agent_id site internal_state graph] *)

val add_link : agent -> int -> agent -> int -> t -> t * (int*int) option
(** [add_link ag1 s1 ag2 s2 t]
 Some (i,j) as second returned element means cc j is now merged into cc i *)

val remove_agent : int -> t -> t
val remove_free : int -> int -> t -> t
val remove_internal : int -> int -> t -> t
val remove_link : int -> int -> int -> int -> t -> t * (int*int) option
(** Some (i,j) as second returned element means separate "new" cc j from cc i *)

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

val get_connected_component : int -> t -> int option

val in_same_connected_component : int -> int -> t -> bool

val all_agents_where : (agent -> bool) -> t -> Mods.IntSet.t

type path = ((agent * int) * (agent * int)) list
val empty_path : path
val singleton_path : agent -> int -> agent -> int -> path
val rev_path : path -> path
val print_path :
  ?sigs:Signature.s -> Format.formatter -> path -> unit

val are_connected :
  ?max_distance : int -> t -> agent list -> agent list -> path option
(** [are_connected ?max_distance graph nodes_x nodes_y] *)

val build_snapshot : Signature.s -> t -> (int * Raw_mixture.t) list

val debug_print : Format.formatter -> t -> unit
