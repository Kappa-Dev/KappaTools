type current_compression_mode = Weak | Strong | Causal
type new_story = { id: int; graph: Graph_loggers_sig.graph }
type story = New of new_story | Same_as of int

type 'a one_compression = {
  log_info: 'a Trace.Simulation_info.t list;
  story_mode: current_compression_mode;
  story: story;
}

type phase = Start | Inprogress | Success | Faillure
type progress_bar = { bool: string; current: int; total: int }

type 'a message =
  | Phase of phase * string
  | Progress of progress_bar
  | Story of 'a one_compression

val message_to_json : 'a message -> Yojson.Basic.t
val message_of_json : Yojson.Basic.t -> unit message
