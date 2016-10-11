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
      Array.map (fun (token,value) -> (Nbr.to_float value,token))
        snapshot.Data.snapshot_tokens
  }

let api_contact_map
    (sigs : Signature.s)
    (cm : Primitives.contact_map) : Api_types_j.site_node array =
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

let api_errors (errors : Api_types_j.errors) : Api_types_v1_t.errors =
  List.map
    (fun (e : Api_types_t.message) ->
       { Api_types_v1_t.severity = e.Api_types_t.message_severity ;
         Api_types_v1_t.message = e.Api_types_t.message_text;
         Api_types_v1_t.range = e.Api_types_t.message_range })
    errors

let api_parameter (parameter : Api_types_v1_j.parameter) : Api_types_j.simulation_parameter =
{ Api_types_j.simulation_nb_plot = parameter.Api_types_v1_j.nb_plot ;
  Api_types_j.simulation_max_time = parameter.Api_types_v1_j.max_time ;
  Api_types_j.simulation_max_events = parameter.Api_types_v1_j.max_events ;
  Api_types_j.simulation_id = "ignore" ; }

let api_flux_map (flux_map : Api_types_j.flux_map) : Api_types_v1_j.flux_map =
  { Api_types_v1_j.flux_begin_time = flux_map.Api_types_j.flux_data.Data.flux_start;
    Api_types_v1_j.flux_end_time = flux_map.Api_types_j.flux_end ;
    Api_types_v1_j.flux_rules = Array.to_list flux_map.Api_types_j.flux_rules;
    Api_types_v1_j.flux_hits = Array.to_list flux_map.Api_types_j.flux_data.Api_types_j.flux_hits;
    Api_types_v1_j.flux_fluxs =
      List.map
        Array.to_list (Array.to_list flux_map.Data.flux_data.Data.flux_fluxs);
    Api_types_v1_j.flux_name = flux_map.Data.flux_data.Data.flux_name
  }

let api_snapshot (snapshot : Api_types_j.snapshot) : Api_types_v1_j.snapshot =
  { Api_types_v1_j.snap_file = snapshot.Api_types_j.snapshot_file ;
    Api_types_v1_j.snap_event = snapshot.Api_types_j.snapshot_event ;
    Api_types_v1_j.agents =
      List.map
        (fun (index,site_graph) ->
           (index,
            Array.map
              (fun site_node ->
                 { Api_types_v1_j.node_quantity = site_node.Api_types_j.site_node_quantity ;
                   Api_types_v1_j.node_name = site_node.Api_types_j.site_node_name ;
                   Api_types_v1_j.node_sites =
                     Array.map
                       (fun (site : Api_types_j.site) ->
                          ({ Api_types_v1_t.site_name = site.Api_types_t.site_name ;
                             Api_types_v1_t.site_links = site.Api_types_t.site_links ;
                             Api_types_v1_t.site_states = site.Api_types_t.site_states ;
                           }
                                    : Api_types_v1_j.site))
                       site_node.Api_types_j.site_node_sites; })
              site_graph))
        snapshot.Api_types_j.snapshot_agents;
    Api_types_v1_j.tokens =
      Array.to_list snapshot.Api_types_j.snapshot_tokens; }

let api_files (f : Api_types_j.file_line)  =
  { Api_types_v1_j.file_name = f.Api_types_j.file_line_name ;
    Api_types_v1_j.line = f.Api_types_j.file_line_text ; }


let api_distance (distance)  =
  { Api_types_v1_j.rule_dist = distance.Api_types_j.distance_rule ;
    Api_types_v1_j.time_dist = distance.Api_types_j.distance_time ;
    Api_types_v1_j.dist = distance.Api_types_j.distance_length ; }

let api_plot (p) =
  { Api_types_v1_j.legend = p.Api_types_j.plot_legend ;
    Api_types_v1_j.time_series =
      List.map
        (fun t ->
           { Api_types_v1_j.observation_time =
               t.Api_types_j.observable_time ;
             Api_types_v1_j.observation_values =
               t.Api_types_j.observable_values ; })
        p.Api_types_j.plot_time_series;
  }

let api_status (simulation_status : Api_types_j.simulation_status) :
  Api_types_v1_j.state =
  { Api_types_v1_j.plot =
      (match simulation_status.Api_types_j.simulation_status_plot with
      | None -> None
      | Some p -> Some (api_plot p));
    Api_types_v1_j.distances =
      (match simulation_status.Api_types_j.simulation_status_distances with
       | None -> None
       | Some distances -> Some (List.map api_distance distances)
      );
    Api_types_v1_j.time =
      simulation_status.Api_types_j.
        simulation_status_info.Api_types_j.
        simulation_info_time ;
    Api_types_v1_j.time_percentage =
      simulation_status.Api_types_j.
        simulation_status_info.Api_types_j.
        simulation_info_time_percentage ;
    Api_types_v1_j.event =
      simulation_status.Api_types_j.
        simulation_status_info.Api_types_j.
        simulation_info_event ;
    Api_types_v1_j.event_percentage =
      simulation_status.Api_types_j.
        simulation_status_info.Api_types_j.
        simulation_info_event_percentage ;
    Api_types_v1_j.tracked_events =
      simulation_status.Api_types_j.
        simulation_status_info.Api_types_j.
        simulation_info_tracked_events ;
    Api_types_v1_j.log_messages =
      simulation_status.Api_types_j.
        simulation_status_log_messages ;
    Api_types_v1_j.snapshots =
      List.map api_snapshot
        simulation_status.Api_types_j.simulation_status_snapshots ;
    Api_types_v1_j.flux_maps =
      List.map
        api_flux_map
        simulation_status.Api_types_j.simulation_status_flux_maps ;
    Api_types_v1_j.files =
      List.map
        api_files
        simulation_status.Api_types_j.simulation_status_file_lines ;
    Api_types_v1_j.is_running =
      simulation_status.Api_types_j.
        simulation_status_info.Api_types_j.
        simulation_info_is_running;
  }
