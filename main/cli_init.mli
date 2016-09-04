val get_compilation :
  ?max_e:int -> Common_args.t -> Run_cli_args.t ->
  (Environment.t * Connected_component.Env.t * Primitives.contact_map *
   int list * (bool*bool*bool) option * bool option * Ast.formatCflow *
   (Alg_expr.t * Primitives.elementary_rule * Location.t) list) *
  Counter.t * (int * Alg_expr.t) list
