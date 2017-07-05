


type t = ((int list) * (int*int) list) array array
(** (internal_states, (agent_type, agent_site) link_states *)

val print_kappa : Signature.s -> Format.formatter -> t -> unit


val print_cycles : Signature.s -> Format.formatter -> t -> unit

val to_yojson : t -> Yojson.Basic.json
val of_yojson : Yojson.Basic.json -> t
