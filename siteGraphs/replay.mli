type state = {
  graph : Edges.t;
  time : float;
  event : int;
  connected_components : Mods.IntSet.t Mods.IntMap.t;
}

val init_state : state

val do_step : Signature.s -> state -> Trace.step -> state * (int * int) option
