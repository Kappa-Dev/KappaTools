(* There are slight differences between the api datatyep as
   and the simulator datatypes.  This class serves to map
   the two implementations.
*)
module Api_types = ApiTypes_j

let plot_pg_store ~plot
                  ~file
                  ~title
                  ~descr : Pp_svg.store
  = { Pp_svg.file = file;
      Pp_svg.title = title;
      Pp_svg.descr = descr;
      Pp_svg.legend = Array.of_list plot.Api_types.legend;
      Pp_svg.points =
        List.map
          (fun (observable : Api_types.observable) ->
           (observable.Api_types.time,
            Tools.array_map_of_list
              (fun x -> Nbr.F x) observable.Api_types.values))
          plot.Api_types.observables
    }

let api_file_line (file_line : Data.file_line) : Api_types.file_line =
  { Api_types.file_name = file_line.Data.file_name
  ; Api_types.line = file_line.Data.line
  }

let api_flux_map (flux_map : Data.flux_map) : Api_types.flux_map =
  { Api_types.flux_begin_time = flux_map.Data.flux_data.Data.flux_start;
    Api_types.flux_end_time = flux_map.Data.flux_end ;
    Api_types.flux_rules = Array.to_list flux_map.Data.flux_rules;
    Api_types.flux_hits = Array.to_list flux_map.Data.flux_data.Data.flux_hits;
    Api_types.flux_fluxs =
      List.map
        Array.to_list (Array.to_list flux_map.Data.flux_data.Data.flux_fluxs);
    Api_types.flux_name = flux_map.Data.flux_data.Data.flux_name
  }

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

let api_mixture sigs mix =
  let links = links_of_mix mix in
  Array.mapi
    (fun i a ->
     { Api_types.node_quantity = None;
       Api_types.node_name =
         Format.asprintf "%a" (Signature.print_agent sigs) a.Raw_mixture.a_type;
       Api_types.node_sites =
         Array.mapi
           (fun j s ->
            { Api_types.site_name =
                Format.asprintf
                  "%a" (Signature.print_site sigs a.Raw_mixture.a_type) j;
              Api_types.site_links =
                (match Mods.Int2Map.find_option (i,j) links with
                 | None -> []
                 | Some dst -> [dst]);
              Api_types.site_states =
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

let api_snapshot sigs (snapshot : Data.snapshot) : Api_types.snapshot =
  { Api_types.snap_file = snapshot.Data.snap_file
  ; Api_types.snap_event = snapshot.Data.snap_event
  ; Api_types.agents =
      snd
        (List.fold_left
           (fun (old_offset,old_agents) (agent,mixture) ->
             let quantity = Some (float_of_int agent) in
             let mixture = Array.to_list (api_mixture sigs mixture) in
             let new_offset = old_offset + (List.length mixture) in
             let update_links (agent_id,site_id : int * int) =
               (agent_id+old_offset,site_id)
             in
             let update_sites site = { site with
               Api_types.site_links =
                 List.map
                   update_links
                   site.Api_types.site_links
             } in
             let new_agents =
               List.map
                 (fun (node : Api_types.site_node)->
                   { node with
                     Api_types.node_quantity = quantity ;
                     Api_types.node_sites =
                       Array.map
                         update_sites
                         node.Api_types.node_sites
                   }
                 )
                 mixture
             in
             (new_offset,old_agents@new_agents)
           )
           (0,[])
           snapshot.Data.agents
        )
  ; Api_types.tokens =
      List.map (fun (token,value) ->
                { Api_types.node_name = token ;
                  Api_types.node_quantity = Some (Nbr.to_float value);
                  Api_types.node_sites = Array.of_list [] })
               (Array.to_list snapshot.Data.tokens)
  }

let find_link cm (a,s) =
  let rec auxs i j = function
    | [] -> raise Not_found
    | (s',_) :: t -> if s = s' then (i,j) else auxs i (succ j) t in
  let rec auxa i = function
    | [] -> raise Not_found
    | (a',l) :: t -> if a = a' then auxs i 0 l else auxa (succ i) t in
  auxa 0 cm

let api_contact_map cm =
  let rec cut_by_agent = function
    | [] -> []
    | ((a,s),v) :: t ->
       let av,oth = List.partition (fun ((a',_),_) -> a = a') t in
       (a,(s,v)::List.map (fun ((_,s'),v') -> s',v') av)::cut_by_agent oth in
  let cm' = cut_by_agent (Export_to_KaSim.String2Map.bindings cm) in
  Tools.array_map_of_list
    (fun (ag,sites) ->
     { Api_types.node_quantity = None;
       Api_types.node_name = ag;
       Api_types.node_sites =
         Tools.array_map_of_list
           (fun (site,(states,links)) ->
            { Api_types.site_name = site;
              Api_types.site_links = List.map (find_link cm') links;
              Api_types.site_states = states;
            }) sites;
     }) cm'

let api_contactmap_site_graph
    (contactmap : Api_types.parse) : Api_types.site_graph =
  contactmap.Api_types.contact_map

let offset_site_graph
    (offset : int)
    (site_nodes : Api_types.site_node list) :
    Api_types.site_node list =
  List.map
    (fun site_node ->
      { site_node with
        Api_types.node_sites =
          Array.map
            (fun site ->
              { site with
                Api_types.site_links =
                  List.map (fun (i,j) -> (i+offset,j+offset))
                    site.Api_types.site_links })
          site_node.Api_types.node_sites
      }
    )
    site_nodes

let api_snapshot_site_graph
    (snapshot : Api_types.snapshot) : Api_types.site_graph =
  Array.of_list
    (List.concat
       [snapshot.Api_types.agents;
        let offset = List.length snapshot.Api_types.agents in
        offset_site_graph offset snapshot.Api_types.tokens])

let api_parse_is_empty (parse : Api_types.parse) =
  0 = Array.length parse.Api_types.contact_map
