type env
type t

type work
type node

val empty_env : Signature.t NamedDecls.t -> env
val cc_map : env -> t Mods.IntMap.t

(** {5 Create a connected component } *)
val begin_new : env -> work
val new_node :
  (work -> string (** node_type *) -> (node*work)) Term.maybe_pos
val get_site_id : (work -> node -> string -> int) Term.maybe_pos
val new_link :
  (work -> (node * int) (** node * site id *) -> (node * int) -> work)
    Term.maybe_pos
val new_free : (work -> (node * int) -> work) Term.maybe_pos
val new_internal_state : (work -> (node * int) -> string -> work) Term.maybe_pos
(** [new_link_type work (node,site) type] *)
val finish_new : work -> (env*t)

(** {5 Use a connected component } *)
val equal : t -> t -> bool
val print : env -> Format.formatter -> t -> unit
val print_dot : env -> Format.formatter -> t -> unit
