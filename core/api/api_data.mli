(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type simulation_detail_output =
  ( Api_types_t.plot option,
    (string * Api_types_t.din) list,
    string list Mods.StringMap.t,
    Api_types_t.snapshot Mods.StringMap.t,
    string,
    string )
  Api_types_t.simulation_output

val api_snapshot_dot : Api_types_t.snapshot -> string
val api_snapshot_kappa : Api_types_t.snapshot -> string

val api_simulation_status :
  Api_types_t.simulation_progress ->
  simulation_detail_output ->
  Api_types_t.simulation_info

val agent_count : Api_types_t.site_graph -> int
