type t = ((int list) * (int*int) list) array array
(** (internal_states, (agent_type, agent_site) link_states *)

val print_kappa : Signature.s -> Format.formatter -> t -> unit
