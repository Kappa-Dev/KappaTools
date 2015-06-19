(**Graph rewriting module*)

type t (**Abstract graph*)

(**Initialisation*)
val empty : has_tracking:bool -> Environment.t -> t

val value_alg :
  get_alg:(int -> Alg_expr.t) -> Mods.Counter.t -> t -> Alg_expr.t -> Nbr.t

val value_bool :
  get_alg:(int -> Alg_expr.t) -> Mods.Counter.t -> t ->
  Alg_expr.t Ast.bool_expr -> bool


(*Connected_component.Env.t: Domain*)
val apply_rule :
  get_alg:(int -> Alg_expr.t) -> Connected_component.Env.t -> Mods.Counter.t
  -> t -> Primitives.elementary_rule -> t option

val force_rule :
  get_alg:(int -> Alg_expr.t) -> Connected_component.Env.t -> Mods.Counter.t
  -> t -> Primitives.elementary_rule ->
  (t  * Connected_component.Matching.t list option)

val print : Environment.t -> Format.formatter -> t -> unit

(** Stories *)
val add_tracked : Connected_component.t -> t -> t
val remove_tracked : Connected_component.t -> t -> t
val generate_stories : Environment.t -> t -> unit

(**Debugging*)
val print_injections :
  ?sigs:Signature.s -> Format.formatter ->
  ValMap.tree Connected_component.Map.t -> unit
val debug_print : Format.formatter -> t -> unit
