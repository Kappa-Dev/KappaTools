type state = {
  graph : Edges.t;
  time : float;
  event : int;
  connected_components : Mods.IntSet.t Mods.IntMap.t option;
}

val init_state : with_connected_components:bool -> state

val do_step : Signature.s -> state -> Trace.step -> state * (int * int) option
