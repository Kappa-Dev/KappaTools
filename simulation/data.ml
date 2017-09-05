(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type ('agent,'token) generic_snapshot = {
  snapshot_file : string;
  snapshot_event : int;
  snapshot_time : float;
  snapshot_agents : 'agent list;
  snapshot_tokens : 'token array;
}
type snapshot =
  ((int * User_graph.connected_component),(string * Nbr.t)) generic_snapshot

type flux_data = {
  flux_name : string;
  flux_kind : Primitives.flux_kind;
  flux_start : float;
  flux_hits : int array;
  flux_fluxs : float array array;
}
type flux_map = {
  flux_rules : string array;
  flux_data : flux_data;
  flux_end : float;
}

type file_line = {
  file_line_name : string option;
  file_line_text : string;
}

type t =
  | Flux of flux_map
  | DeltaActivities of int * (int * (float * float)) list
  | Plot of Nbr.t array (** Must have length >= 1 (at least [T] or [E]) *)
  | Print of file_line
  | TraceStep of Trace.step
  | Snapshot of snapshot
  | Log of string
  | Species of string * float * User_graph.connected_component

let print_snapshot ?uuid f s =
  Format.fprintf
    f "@[<v>%a%%def: \"T0\" \"%g\"@,@,%a@,%a@]"
    (Pp.option ~with_space:false (fun f x -> Format.fprintf f "# \"uuid\" : \"%i\"@," x)) uuid
    s.snapshot_time
    (Pp.list Pp.space (fun f (i,mix) ->
         Format.fprintf f "%%init: %i /*%i agents*/ @[<h>%a@]" i
           (Array.length mix)
           (User_graph.print_cc ~explicit_free:false ~compact:false) mix))
    s.snapshot_agents
    (Pp.array Pp.space (fun _ f (na,el) ->
         Format.fprintf
           f "%%init: %a %s" Nbr.print el na))
    s.snapshot_tokens

let print_dot_snapshot ?uuid f s =
  Format.fprintf
    f "@[<v>%adigraph G{@,%a@,%a}@]"
    (Pp.option ~with_space:false (fun f x -> Format.fprintf f "// \"uuid\" : \"%i\"@," x)) uuid
    (Pp.listi
       Pp.cut
       (fun i f (nb,mix) ->
          Format.fprintf f "@[<v 2>subgraph cluster%d{@," i;
          Format.fprintf
            f "counter%d [label = \"%d instance(s)\", shape=none];@,%a}@]"
            i nb (User_graph.print_dot_cc i) mix))
    s.snapshot_agents
    (Pp.array Pp.cut (fun i f (na,el) ->
         Format.fprintf
           f "token_%d [label = \"%s (%a)\" , shape=none]"
           i na Nbr.print el))
    s.snapshot_tokens
