type new_story =
  {
    id: int;
    graph: Graph_loggers_sig.graph;
  }

type story =
  | New of new_story
  | Same_as of int

type one_compression =
  {
    log_info: StoryProfiling.StoryStats.log_info Trace.Simulation_info.t list;
    story: story
  }

val to_json: one_compression -> Yojson.Basic.json
val of_json: Yojson.Basic.json -> one_compression
