(** Concrete graph implementation *)

type t

val empty : t

val add_free : int -> int -> int -> t -> t
(** [add_free sort_agent agent site graph] *)
val add_internal : int -> int -> int -> t -> t
(** [add_internal agent site internal_state graph] *)
val add_link : int -> int -> int -> int -> int -> int -> t -> t
(** [add_link sort_ag1 ag1 s1 sort_ag2 ag2 s2 t] *)

val remove_free : int -> int -> t -> t
val remove_internal : int -> int -> t -> (t * int)
val remove_link : int -> int -> int -> int -> t -> t

val is_free : int -> int -> t -> bool
(** [is_free agent site graph] *)
val is_internal : int -> int -> int -> t -> bool
(** [is_internal internal_state agent site graph] *)
val link_exists : int -> int -> int -> int -> t -> bool
(** [link_exists ag1 site1 ag2 site2 graph] *)
val exists_fresh : int -> int -> int -> int -> t -> int option
(** [exists_fresh ag1 site1 type_of_ag2 site2 graph] *)

type path
val empty_path : path
val rev_path : path -> path
val print_path :
  ?sigs:Signature.s -> ?graph:t -> Format.formatter -> path -> unit

val are_connected : ?candidate:path -> t -> int -> int -> path option
val pathes_of_interrest :
  (int -> 'a option) -> t -> int -> path -> (('a*int) * path) list

val print : Signature.s -> Format.formatter -> t -> unit
val print_dot : Signature.s -> Format.formatter -> t -> unit
val debug_print : Format.formatter -> t -> unit
