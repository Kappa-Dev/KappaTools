val variables_of_result : Environment.t -> Ast.compil -> (Environment.t * Mixture.t list * (Dynamics.variable * Mods.DepSet.t * int) list)
val environment_of_result : Ast.compil -> Environment.t
val init_graph_of_result : Environment.t -> Ast.compil -> (Graph.SiteGraph.t * Environment.t)
val rules_of_result : Environment.t -> Ast.compil -> bool -> (Environment.t * Dynamics.rule list)
 
val initialize : Ast.compil -> (Environment.t * State.implicit_state * Mods.Counter.t)