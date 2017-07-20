let api_message_errors
    ?(severity:Api_types_t.severity = `Error)
    ?(region:Api_types_t.range option)
    (message : string) : Api_types_t.errors =
  [{ Api_types_t.message_severity = severity;
     Api_types_t.message_text = message ;
     Api_types_t.message_range = region }]

let api_exception_errors
    (e : exn) : Api_types_t.errors =
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
  Api_types_t.site_node array =
  let links = links_of_mix mix in
  Array.mapi
    (fun i a ->
       {
         Api_types_t.site_node_name =
           Format.asprintf "%a" (Signature.print_agent sigs) a.Raw_mixture.a_type;
         Api_types_t.site_node_sites =
           Array.mapi
             (fun j s ->
                { Api_types_t.site_name =
                    Format.asprintf
                      "%a" (Signature.print_site sigs a.Raw_mixture.a_type) j;
                  Api_types_t.site_links =
                    (match Mods.Int2Map.find_option (i,j) links with
                     | None -> []
                     | Some dst -> [dst]);
                  Api_types_t.site_states =
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
    (snapshot : Data.snapshot) : Api_types_t.snapshot =
  { Api_types_t.snapshot_file = snapshot.Data.snapshot_file
  ; Api_types_t.snapshot_event = snapshot.Data.snapshot_event
  ; Api_types_t.snapshot_time = snapshot.Data.snapshot_time
  ; Api_types_t.snapshot_agents =
      List.map
        (fun (agent,mixture) -> (agent,api_mixture sigs mixture))
        snapshot.Data.snapshot_agents
  ; Api_types_t.snapshot_tokens =
      Array.map
        (fun (token,value) ->
           (Option_util.unsome infinity (Nbr.to_float value),token))
        snapshot.Data.snapshot_tokens
  }

let api_snapshot_site_graph
    (snapshot : Api_types_t.snapshot) : Api_types_t.site_graph =
  let tokens_sg = Array.map (fun (_,token) ->
      { Api_types_t.site_node_name = token ;
        Api_types_t.site_node_sites = [||] })
      snapshot.Api_types_t.snapshot_tokens in
  snd
    (List.fold_left
       (fun (old_offset,old_agents) (_,mixture) ->
          let new_offset = old_offset + (Array.length mixture) in
          let update_links (agent_id,site_id : int * int) =
            (agent_id+old_offset,site_id)
          in
          let update_sites site = { site with
                                    Api_types_t.site_links =
                                      List.map
                                        update_links
                                        site.Api_types_t.site_links
                                  } in
          let new_agents =
            Array.map
              (fun (node : Api_types_t.site_node)->
                 { node with
                   Api_types_t.site_node_sites =
                     Array.map
                       update_sites
                       node.Api_types_t.site_node_sites
                 }
              )
              mixture
          in
          (new_offset,Array.append old_agents new_agents)
       )
       (Array.length tokens_sg,tokens_sg)
       snapshot.Api_types_t.snapshot_agents)

let print_site_node ?link_store agid f sn =
  Format.fprintf f "%s(@[<h>%a@])"
    sn.Api_types_t.site_node_name
    (Pp.array (fun f -> Format.pp_print_string f ",")
       (fun sid f ss -> Format.fprintf f "%s%a%t" ss.Api_types_t.site_name
           (Pp.list Pp.empty (fun f i -> Format.fprintf f "~%s" i))
           ss.Api_types_t.site_states
           (fun f -> match link_store with
              | None -> ()
              | Some r ->
                Pp.list Pp.empty
                  (fun f (agid',sid') ->
                     let fid,idm = !r in
                     let lid =
                       if agid' < agid || (agid' = agid && sid' <= sid) then
                         let lid,rem = Mods.Int2Map.pop (agid,sid) idm in
                         let () = r:=(fid,rem) in
                         Option_util.unsome (-1) lid
                       else
                         let () =
                           r:=(succ fid, Mods.Int2Map.add (agid',sid') fid idm) in
                         fid in
                     Format.fprintf f "!%i" lid) f
                  ss.Api_types_t.site_links)
       ))
    sn.Api_types_t.site_node_sites

let print_site_nodes_dot nb_cc f mix =
  Pp.array
    Pp.empty
    (fun i f sn ->
       Format.fprintf
         f "node%d_%d [label = \"@[<h>%a@]\", style=filled];@,"
         nb_cc i (print_site_node ?link_store:None i) sn;
       Pp.array Pp.empty
         (fun si f ss ->
            Pp.list Pp.empty
              (fun f (di,dsi) ->
                 if di < i || (di = i && dsi <=si) then
                   Format.fprintf
                     f
                     "node%d_%d -> node%d_%d [taillabel=\"%s\", headlabel=\"%s\", dir=none];@,"
                     nb_cc i nb_cc di ss.Api_types_t.site_name
                     mix.(di).Api_types_t.site_node_sites.(dsi).Api_types_t.site_name)
              f ss.Api_types_t.site_links)
         f sn.Api_types_t.site_node_sites;
       Format.fprintf
         f "node%d_%d -> counter%d [style=invis];@," nb_cc i nb_cc)
    f mix

let api_snapshot_dot (snapshot : Api_types_t.snapshot) =
  Format.asprintf
    "@[<v>digraph G{@,%a@,%a}@]@."
    (Pp.listi
       Pp.cut
       (fun i f (nb,mix) ->
          Format.fprintf f "@[<v 2>subgraph cluster%d{@," i;
          Format.fprintf
            f "counter%d [label = \"%d instance(s)\", shape=none];@,%a}@]"
            i nb (print_site_nodes_dot i) mix))
    snapshot.Api_types_t.snapshot_agents
    (Pp.array Pp.cut (fun i f (el,na) ->
         Format.fprintf
           f
           "token_%d [label = \"%s (%f)\" , shape=none]"
           i na el))
    snapshot.Api_types_t.snapshot_tokens

let print_site_nodes f sn =
  let link_store = ref (1,Mods.Int2Map.empty) in
  Pp.array
    (fun f -> Format.pp_print_string f ",")
    (print_site_node ~link_store) f sn

let api_snapshot_kappa (snapshot : Api_types_t.snapshot) : string =
  Format.asprintf
    "@[<v>%a@,%a@]"
    (Pp.list Pp.space (fun f (i,mix) ->
         Format.fprintf f "%%init: %i @[<h>%a@]" i
           print_site_nodes mix))
    snapshot.Api_types_t.snapshot_agents
    (Pp.array Pp.space (fun _ f (el,na) ->
         Format.fprintf f "%%init: %s <- %f" na el))
    snapshot.Api_types_t.snapshot_tokens

let api_contact_map
    (sigs : Signature.s)
    (cm : Contact_map.t) : Api_types_t.site_node array =
  Array.mapi
    (fun ag sites ->
       {
         Api_types_t.site_node_name =
           Format.asprintf "%a" (Signature.print_agent sigs) ag;
         Api_types_t.site_node_sites =
           Array.mapi
             (fun site (states,links) ->
                { Api_types_t.site_name =
                    Format.asprintf "%a" (Signature.print_site sigs ag) site;
                  Api_types_t.site_links = links;
                  Api_types_t.site_states =
                    List.map
                      (fun s -> Format.asprintf
                         "%a"
                         (Signature.print_internal_state sigs ag site) s)
                      states;
                }) sites;
       })
    cm

let api_simulation_status
    (detail : Api_types_t.simulation_detail) :
  Api_types_t.simulation_info =
  let progress : Api_types_t.simulation_progress =
    detail.Api_types_t.simulation_detail_progress in
  let detail : Api_types_t.simulation_detail_output =
    detail.Api_types_t.simulation_detail_output in
  let output : Api_types_t.simulation_info_output =
    { Api_types_t.simulation_output_plot =
        (match detail.Api_types_t.simulation_output_plot with
        | None -> 0
        | Some plot -> List.length plot.Api_types_t.plot_time_series);
      Api_types_t.simulation_output_flux_maps =
        List.length detail.Api_types_t.simulation_output_flux_maps ;
      Api_types_t.simulation_output_file_lines =
        List.length detail.Api_types_t.simulation_output_file_lines ;
      Api_types_t.simulation_output_snapshots =
        List.length detail.Api_types_t.simulation_output_snapshots ;
      Api_types_t.simulation_output_log_messages =
        String.length detail.Api_types_t.simulation_output_log_messages ;
    }
  in
  { Api_types_t.simulation_info_progress = progress ;
    Api_types_t.simulation_info_output = output ; }

let plot_values
    ?(separator : string = ",")
    (plot : Api_types_t.plot) : string =
  Format.asprintf "@[<v>%a@,%a@]"
    (fun f -> Format.fprintf f "@[<h>%a@]"
        (Pp.list (fun f -> Format.pp_print_string f separator)
           (fun f -> Format.fprintf f "\"%s\"")))
    plot.Api_types_t.plot_legend
    (Pp.list Pp.space
       (fun f -> Format.fprintf f "@[<h>%a@]"
           (Pp.list (fun f -> Format.pp_print_string f separator)
              (Pp.option ~with_space:false (fun f -> Format.fprintf f "%e")))))
    (List.rev plot.Api_types_t.plot_time_series)

(* return the agent count *)
let agent_count (species : Api_types_t.site_graph) : int = Array.length species
