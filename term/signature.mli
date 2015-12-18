(** Store definitions of agents *)

type t (** Store of one agent *)

val num_of_site : ?agent_name:string -> string Location.annot -> t -> int
val site_of_num : int -> t -> string
val fold : (int -> string -> 'a -> 'a) -> t -> 'a -> 'a

val num_of_internal_state : int -> string Location.annot -> t -> int
(** [num_of_internal_state site_id state_name sign] *)

val internal_state_of_num : int -> int -> t -> string

type s (** Store of all the agents *)

val create : Ast.agent list -> s

val size : s -> int
val get : s -> int -> t
(** [get sigs agent_id] *)

val arity : s -> int -> int
(** [arity sigs agent_id] *)

val max_arity : s -> int
(** [max_arity sigs] returns max {arities sigs i} *)

val num_of_agent : string Location.annot -> s -> int

val id_of_site : string Location.annot -> string Location.annot -> s -> int
(** [id_of_site agent_type site_name sigs] *)

val id_of_internal_state :
  string Location.annot -> string Location.annot ->
  string Location.annot -> s -> int
(** [id_of_internal_state agent_type site_name state_name sigs] *)

val internal_states_number : int -> int -> s -> int
(** [internal_state_number agent_id site_id sigs] *)

val default_internal_state : int -> int -> s -> int option

val print_agent : s -> Format.formatter -> int -> unit
val print_site : s -> int -> Format.formatter -> int -> unit
val print_internal_state :
  s -> int -> int -> Format.formatter -> int -> unit
val print_site_internal_state :
  s -> int -> int -> Format.formatter -> int option -> unit
(** [print_site_internal_state sigs agent_type site_id f state_id]
prints both the site and its internal state if it is not [None]. *)

val print : Format.formatter -> s -> unit
