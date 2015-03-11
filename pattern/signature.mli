type t

val num_of_site : ?agent_name:string -> string Term.with_pos -> t -> int
val site_of_num : int -> t -> string
val fold : (int -> string -> 'a -> 'a) -> t -> 'a -> 'a

(** [num_of_internal_state site_id state_name sign] *)
val num_of_internal_state : int -> string Term.with_pos -> t -> int
val internal_state_of_num : int -> int -> t -> string

val print : Format.formatter -> t -> unit

type s

val size : s -> int
(** [get sigs agent_id] *)
val get : s -> int -> t
(** [arity sigs agent_id] *)
val arity : s -> int -> int

val num_of_agent : string Term.with_pos -> s -> int
val agent_of_num : int -> s -> string

(** [id_of_site agent_type site_name sigs] *)
val id_of_site : string Term.with_pos -> string Term.with_pos -> s -> int
val site_of_id : int -> int -> s -> string
(** [id_of_internal_state agent_type site_name state_name sigs] *)
val id_of_internal_state :
  string Term.with_pos -> string Term.with_pos ->
  string Term.with_pos -> s -> int
val internal_state_of_id : int -> int -> int -> s -> string
(** [internal_state_number agent_id site_id sigs] *)
val internal_states_number : int -> int -> s -> int
val default_num_value : int -> int -> s -> int option

val create : Ast.agent list -> s
