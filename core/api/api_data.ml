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

let api_snapshot_dot (snapshot : Api_types_t.snapshot) =
  Format.asprintf "%a@." (Data.print_dot_snapshot ?uuid:None) snapshot

let api_snapshot_kappa (snapshot : Data.snapshot) : string =
  Format.asprintf "%a@." (Data.print_snapshot ?uuid:None) snapshot

let api_simulation_status (progress : Api_types_t.simulation_progress)
    (detail : simulation_detail_output) : Api_types_t.simulation_info =
  let output : Api_types_t.simulation_info_output =
    {
      Api_types_t.simulation_output_plot =
        (match detail.Api_types_t.simulation_output_plot with
        | None -> 0
        | Some plot -> List.length plot.Data.plot_series);
      Api_types_t.simulation_output_dins =
        List.length detail.Api_types_t.simulation_output_dins;
      Api_types_t.simulation_output_file_lines =
        Mods.StringMap.size detail.Api_types_t.simulation_output_file_lines;
      Api_types_t.simulation_output_snapshots =
        Mods.StringMap.size detail.Api_types_t.simulation_output_snapshots;
      Api_types_t.simulation_output_inputs = ();
      Api_types_t.simulation_output_log_messages =
        String.length detail.Api_types_t.simulation_output_log_messages;
    }
  in
  {
    Api_types_t.simulation_info_progress = progress;
    Api_types_t.simulation_info_output = output;
  }

(* return the agent count *)
let agent_count (species : Api_types_t.site_graph) : int = Array.length species
