(** A node of a site graph *)

type t = int * int
(** agent_id * agent_type *)

val print : ?sigs:Signature.s -> Format.formatter -> t -> unit
val to_json : t -> Yojson.Basic.json
val of_json : Yojson.Basic.json -> t
