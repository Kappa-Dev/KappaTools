type t

val num_of_site : ?agent_name:string -> string Term.with_pos -> t -> int
val site_of_num : int -> t -> string
val fold : (int -> string -> 'a -> 'a) -> t -> 'a -> 'a

(** [num_of_internal_state site_id state_name sign] *)
val num_of_internal_state : int -> string Term.with_pos -> t -> int
val internal_state_of_num : int -> int -> t -> string

val print : Format.formatter -> t -> unit

type s

val create : Ast.agent list -> s

val size : s -> int
(** [get sigs agent_id] *)
val get : s -> int -> t
(** [arity sigs agent_id] *)
val arity : s -> int -> int

val num_of_agent : string Term.with_pos -> s -> int

(** [id_of_site agent_type site_name sigs] *)
val id_of_site : string Term.with_pos -> string Term.with_pos -> s -> int
(** [id_of_internal_state agent_type site_name state_name sigs] *)
val id_of_internal_state :
  string Term.with_pos -> string Term.with_pos ->
  string Term.with_pos -> s -> int
(** [internal_state_number agent_id site_id sigs] *)
val internal_states_number : int -> int -> s -> int
val default_internal_state : int -> int -> s -> int option

val print_agent : s -> Format.formatter -> int -> unit
val print_site : s -> int -> Format.formatter -> int -> unit
(** [print_internal_state sigs agent_type site_id f state_id] *)
val print_site_internal_state :
  s -> int -> int -> Format.formatter -> int option -> unit
