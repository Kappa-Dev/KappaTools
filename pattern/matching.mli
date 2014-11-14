open Mods

val component :
  ?check_additional_edges:bool -> ?already_done:Int2Set.t -> Injection.t ->
  int -> Graph.SiteGraph.t * int -> Mixture.t ->
  (Injection.t * (int * int) list IntMap.t) option
