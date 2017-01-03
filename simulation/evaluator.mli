val do_interactive_directives :
  outputs:(Data.t -> unit) -> max_sharing:bool -> Signature.contact_map ->
  Environment.t -> Counter.t -> Rule_interpreter.t -> State_interpreter.t ->
  (((String.t * Location.t) * Ast.port list) list, Mods.StringMap.elt)
    Ast.modif_expr list ->
  Environment.t * (bool * Rule_interpreter.t * State_interpreter.t)
