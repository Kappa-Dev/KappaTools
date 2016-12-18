type directive_unit = Time | Event

val get_compilation :
  ?unit:directive_unit -> ?max_sharing:bool -> Run_cli_args.t ->
  (Environment.t * Signature.contact_map * int list *
   (bool*bool*bool) option * bool option * string * string option *
   (Alg_expr.t * Primitives.elementary_rule * Location.t) list) *
  Counter.t * (int * Alg_expr.t) list
