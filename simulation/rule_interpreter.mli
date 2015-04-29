type t

val empty : Environment.t -> t

val apply_rule :
  Environment.t -> Connected_component.Env.t -> Mods.Counter.t ->
  t -> Primitives.elementary_rule -> t
