(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

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

type ('agent,'token) generic_snapshot   = {
  snapshot_file : string;
  snapshot_event : int;
  snapshot_time : float;
  snapshot_agents : 'agent list;
  snapshot_tokens : 'token array;
}

type snapshot =  ((int * Raw_mixture.t),(string * Nbr.t)) generic_snapshot

type t =
  | Flux of flux_map
  | DeltaActivities of int * (int * (float * float)) list
  | Plot of Nbr.t array (** Must have length >= 1 (at least [T] or [E]) *)
  | Print of file_line
  | TraceStep of Trace.step
  | Snapshot of snapshot
  | Log of string
  | Species of string * float * Raw_mixture.t
