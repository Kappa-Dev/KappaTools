type t

val arity : t -> int
val num_of_site : string -> t -> int
val site_of_num : int -> t -> string
val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a

val internal_states_number : int -> t -> int
(** [num_of_internal_state site_id state_name sign] *)
val num_of_internal_state : int -> string -> t -> int
val internal_state_of_num : int -> int -> t -> string
val default_num_value : int -> t -> int option

val to_string : t -> string

val create : Ast.port list -> t
