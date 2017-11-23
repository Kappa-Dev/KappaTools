val string_of_binding_type:
  ?binding_type_symbol:string ->
  agent_name:string ->
  site_name:string -> string

val print_binding_type:
  Format.formatter -> ?binding_type_symbol:string ->
  agent_name:string ->
  site_name:string -> unit
