type state = {
  graph : Edges.t;
  time : float;
  event : int;
  connected_components : Agent.SetMap.Set.t Mods.IntMap.t option;
}

val init_state : with_connected_components:bool -> state

val do_step : Signature.s -> state -> Trace.step -> state * (int * int) option
(** @return the new state and, if the step was an unary instance of a
    binary rule, the id of the rule and the distance between its 2
    connected patterns. *)
