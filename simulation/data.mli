type flux_data = {
    flux_name : string;
    flux_start : float;
    flux_hits : int array;
    flux_fluxs : float array array;
  }
type flux_map =
  { flux_rules : string array;
    flux_data : flux_data;
    flux_end : float;
  }
type file_line = { file_line_name : string option ;
                   file_line_text : string }

type ('agent,'token) generic_snapshot   = {
    snapshot_file : string;
    snapshot_event : int;
    snapshot_agents : 'agent list;
    snapshot_tokens : 'token array; }

type snapshot =  ((int * Raw_mixture.t),(string * Nbr.t)) generic_snapshot
type distance = {
  distance_rule : int;
  distance_time : float;
  distance_length : int;
}

type t = Flux of flux_map
       | Plot of float * Nbr.t array
       | Print of file_line
       | TraceStep of Trace.step
       | Snapshot of snapshot
       | Log of string
       | UnaryDistance of distance
