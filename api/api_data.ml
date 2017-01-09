let api_message_errors
    ?(severity:Api_types_j.severity = `Error)
    ?(region:Api_types_j.range option = None)
    (message : string) : Api_types_j.errors =
  [{ Api_types_j.message_severity = severity;
     Api_types_j.message_text = message ;
     Api_types_j.message_range = region }]

let api_exception_errors
    (e : exn) : Api_types_j.errors =
  api_message_errors
    (try  (Printexc.to_string e)
     with _ -> "unspecified exception thrown")

let links_of_mix mix =
  snd @@ snd @@
  List.fold_left
    (fun (i,acc) a ->
       succ i,
       Tools.array_fold_lefti
         (fun j (one,two as acc) ->
            function
            | Raw_mixture.FREE -> acc
            | Raw_mixture.VAL k ->
              match Mods.IntMap.find_option k one with
              | None -> Mods.IntMap.add k (i,j) one,two
              | Some dst ->
                one,Mods.Int2Map.add dst (i,j)
                  (Mods.Int2Map.add (i,j) dst two))
         acc a.Raw_mixture.a_ports)
    (0,(Mods.IntMap.empty,Mods.Int2Map.empty)) mix

let api_mixture
    (sigs : Signature.s)
    (mix : Raw_mixture.agent list) :
  Api_types_j.site_node array =
  let links = links_of_mix mix in
  Array.mapi
    (fun i a ->
       { Api_types_j.site_node_quantity = None;
         Api_types_j.site_node_name =
           Format.asprintf "%a" (Signature.print_agent sigs) a.Raw_mixture.a_type;
         Api_types_j.site_node_sites =
           Array.mapi
             (fun j s ->
                { Api_types_j.site_name =
                    Format.asprintf
                      "%a" (Signature.print_site sigs a.Raw_mixture.a_type) j;
                  Api_types_j.site_links =
                    (match Mods.Int2Map.find_option (i,j) links with
                     | None -> []
                     | Some dst -> [dst]);
                  Api_types_j.site_states =
                    (match s with
                     | None -> []
                     | Some k ->
                       [Format.asprintf
                          "%a" (Signature.print_internal_state
                                  sigs a.Raw_mixture.a_type j) k;]);
                })
             a.Raw_mixture.a_ints;
       }
    ) (Array.of_list mix)

let label_snapshot
    (sigs : Signature.s)
    (snapshot : Data.snapshot) : Api_types_j.snapshot =
  { Api_types_j.snapshot_file = snapshot.Data.snapshot_file
  ; Api_types_j.snapshot_event = snapshot.Data.snapshot_event
  ; Api_types_j.snapshot_agents =
      List.map
        (fun (agent,mixture) -> (agent,api_mixture sigs mixture))
        snapshot.Data.snapshot_agents
  ; Api_types_j.snapshot_tokens =
      Array.map
        (fun (token,value) -> (Tools.unsome infinity (Nbr.to_float value),token))
        snapshot.Data.snapshot_tokens
  }

let api_contact_map
    (sigs : Signature.s)
    (cm : Signature.contact_map) : Api_types_j.site_node array =
  Array.mapi
    (fun ag sites ->
       { Api_types_j.site_node_quantity = None;
         Api_types_j.site_node_name =
           Format.asprintf "%a" (Signature.print_agent sigs) ag;
         Api_types_j.site_node_sites =
           Array.mapi
             (fun site (states,links) ->
                { Api_types_j.site_name =
                    Format.asprintf "%a" (Signature.print_site sigs ag) site;
                  Api_types_j.site_links = links;
                  Api_types_j.site_states =
                    List.map
                      (Format.asprintf
                         "%a"
                         (Signature.print_internal_state sigs ag site))
                      states;
                }) sites;
       })
    cm

let api_simulation_status
    (detail : Api_types_j.simulation_detail) :
  Api_types_j.simulation_info =
  let progress : Api_types_j.simulation_progress =
    detail.Api_types_j.simulation_detail_progress in
  let detail : Api_types_j.simulation_detail_output =
    detail.Api_types_j.simulation_detail_output in
  let output : Api_types_j.simulation_info_output =
    { Api_types_j.simulation_output_plot =
        (match detail.Api_types_j.simulation_output_plot with
        | None -> false
        | Some _ -> true);
      Api_types_j.simulation_output_distances =
        (match detail.Api_types_j.simulation_output_distances with
         | None -> 0
         | Some simulation_output_distances ->
           List.length simulation_output_distances);
      Api_types_j.simulation_output_flux_maps =
        List.length detail.Api_types_j.simulation_output_flux_maps ;
      Api_types_j.simulation_output_file_lines =
        List.length detail.Api_types_j.simulation_output_file_lines ;
      Api_types_j.simulation_output_snapshots =
        List.length detail.Api_types_j.simulation_output_snapshots ;
      Api_types_j.simulation_output_log_messages =
        List.length detail.Api_types_j.simulation_output_log_messages ;
    }
  in
  { Api_types_j.simulation_info_progress = progress ;
    Api_types_j.simulation_info_output = output ; }

let plot_values
    ?(separator : string = ",")
    (plot : Api_types_j.plot) : string =
  Format.asprintf "@[<v>%a@,%a@]"
    (fun f -> Format.fprintf f "@[<h>%a@]"
        (Pp.list (fun f -> Format.pp_print_string f separator)
           (fun f -> Format.fprintf f "\"%s\"")))
    plot.Api_types_j.plot_legend
    (Pp.list Pp.space
       (fun f -> Format.fprintf f "@[<h>%a@]"
           (Pp.list (fun f -> Format.pp_print_string f separator)
              (Pp.option ~with_space:false (fun f -> Format.fprintf f "%e")))))
    (List.rev plot.Api_types_j.plot_time_series)
