val get_compilation :
  ?max_event:int -> Run_cli_args.t ->
  (Environment.t * Signature.contact_map *
   int list * (bool*bool*bool) option * bool option * Ast.formatCflow *
   (Alg_expr.t * Primitives.elementary_rule * Location.t) list) *
  Counter.t * (int * Alg_expr.t) list
