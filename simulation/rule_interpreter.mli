type t

val empty : unit NamedDecls.t -> t

val apply_rule :
  Connected_component.Env.t -> t -> Primitives.elementary_rule -> t
