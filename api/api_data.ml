type simulation_detail_output =
  (Api_types_t.plot option,
   Api_types_t.flux_map list,
   string list Mods.StringMap.t,
   Api_types_t.snapshot list,
   string)
    Api_types_t.simulation_output

let api_message_errors
    ?(severity:Api_types_t.severity = `Error)
    ?(region:Api_types_t.range option)
    (message : string) : Api_types_t.message =
  { Api_types_t.message_severity = severity;
     Api_types_t.message_text = message ;
     Api_types_t.message_range = region }

let api_exception_errors
    (e : exn) : Api_types_t.errors =
  [api_message_errors
    (try  (Printexc.to_string e)
     with _ -> "unspecified exception thrown")]

let api_snapshot_dot (snapshot : Api_types_t.snapshot) =
  Format.asprintf "%a@." (Data.print_dot_snapshot ?uuid:None) snapshot

let api_snapshot_kappa (snapshot : Api_types_t.snapshot) : string =
  Format.asprintf "%a@." (Data.print_snapshot ?uuid:None) snapshot

let api_simulation_status
    (progress : Api_types_t.simulation_progress)
    (detail : simulation_detail_output) :
  Api_types_t.simulation_info =
  let output : Api_types_t.simulation_info_output =
    { Api_types_t.simulation_output_plot =
        (match detail.Api_types_t.simulation_output_plot with
        | None -> 0
        | Some plot -> List.length plot.Data.plot_series);
      Api_types_t.simulation_output_flux_maps =
        List.length detail.Api_types_t.simulation_output_flux_maps ;
      Api_types_t.simulation_output_file_lines =
        Mods.StringMap.size detail.Api_types_t.simulation_output_file_lines ;
      Api_types_t.simulation_output_snapshots =
        List.length detail.Api_types_t.simulation_output_snapshots ;
      Api_types_t.simulation_output_log_messages =
        String.length detail.Api_types_t.simulation_output_log_messages ;
    }
  in
  { Api_types_t.simulation_info_progress = progress ;
    Api_types_t.simulation_info_output = output ; }

(* return the agent count *)
let agent_count (species : Api_types_t.site_graph) : int = Array.length species
