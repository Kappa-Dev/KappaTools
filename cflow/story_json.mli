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

type phase =
  | Start
  | Inprogress
  | Success
  | Faillure

type status =
       {
         phase: phase;
         message: string;
       }

val status_to_json: status -> Yojson.Basic.json
val status_of_json: Yojson.Basic.json -> status

type progress_bar =
  {
    bool: string;
    current: int;
    total: int
  }

val progress_bar_to_json: progress_bar -> Yojson.Basic.json
val progress_bar_of_json: Yojson.Basic.json -> progress_bar
