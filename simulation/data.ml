(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type snapshot = {
  snapshot_file : string;
  snapshot_event : int;
  snapshot_time : float;
  snapshot_agents : (int * User_graph.connected_component) list;
  snapshot_tokens : (string * Nbr.t) array;
}

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

let write_snapshot ob s =
  let () = Bi_outbuf.add_char ob '{' in
  let () = JsonUtil.write_field
      "snapshot_file" Yojson.Basic.write_string ob s.snapshot_file in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field
      "snapshot_event" Yojson.Basic.write_int ob s.snapshot_event in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field
      "snapshot_time" Yojson.Basic.write_float ob s.snapshot_time in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field
      "snapshot_agents"
      (JsonUtil.write_list
         (JsonUtil.write_compact_pair
            Yojson.Basic.write_int User_graph.write_connected_component))
      ob s.snapshot_agents in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field
      "snapshot_tokens"
      (JsonUtil.write_array
         (JsonUtil.write_compact_pair Yojson.Basic.write_string Nbr.write_t))
      ob s.snapshot_tokens in
  Bi_outbuf.add_char ob '}'

let read_snapshot p lb =
  let
    snapshot_file,snapshot_event,snapshot_time,snapshot_agents,snapshot_tokens =
    Yojson.Basic.read_fields
      (fun (f,e,ti,a,t) key p lb ->
         if key = "snapshot_file" then (Yojson.Basic.read_string p lb,e,ti,a,t)
         else if key = "snapshot_event" then
           (f,Yojson.Basic.read_int p lb,ti,a,t)
         else if key = "snapshot_time" then
           (f,e,Yojson.Basic.read_number p lb,a,t)
         else if key = "snapshot_agents" then
           (f,e,ti,Yojson.Basic.read_list
              (JsonUtil.read_compact_pair
                 Yojson.Basic.read_int User_graph.read_connected_component) p lb,t)
         else let () = assert (key = "snapshot_tokens") in
           (f,e,ti,a,Yojson.Basic.read_array
              (JsonUtil.read_compact_pair Yojson.Basic.read_string Nbr.read_t)
              p lb)
      )
      ("",-1,nan,[],[||]) p lb in
  {snapshot_file;snapshot_event;snapshot_time;snapshot_agents;snapshot_tokens}

let string_of_snapshot ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  let () = write_snapshot ob x in
  Bi_outbuf.contents ob

let snapshot_of_string s =
  read_snapshot (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
