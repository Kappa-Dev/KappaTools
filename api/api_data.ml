(* There are slight differences between the api datatyep as
   and the simulator datatypes.  This class serves to map
   the two implementations.
*)
module Api_types = ApiTypes_j

let plot_pg_store
    ~plot
    ~file
    ~title
    ~descr
  : Pp_svg.store
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

let plot_values
    ?(separator : string = ",")
    (plot : Api_types.plot) : string =
  String.concat "\n"
    ((String.concat
        separator
        ("time"::plot.Api_types.legend))::
     (List.map
        (fun (observable : Api_types.observable) ->
           String.concat separator
             (List.map
                (Format.sprintf "%e")
                (observable.Api_types.time
                 ::observable.Api_types.values)
             )
        )
        plot.Api_types.observables)
    )


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

let api_contact_map sigs cm =
  Array.mapi
    (fun ag sites ->
       { Api_types.node_quantity = None;
	 Api_types.node_name =
           Format.asprintf "%a" (Signature.print_agent sigs) ag;
	 Api_types.node_sites =
           Array.mapi
             (fun site (states,links) ->
		{ Api_types.site_name =
                    Format.asprintf "%a" (Signature.print_site sigs ag) site;
		  Api_types.site_links = links;
		  Api_types.site_states =
                    List.map
                      (Format.asprintf "%a" (Signature.print_internal_state sigs ag site))
                      states;
		}) sites;
       }) cm

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

(* map out *)
module EdgeMap =
  Map.Make(struct type t = (int * int) * (int * int)
    let compare = compare
  end)



let api_snapshot_kappa (snapshot : Api_types.snapshot) =
  (*let () = print_string (Api_types.string_of_snapshot snapshot) in *)
  let normalize_edge
      ((l,r) : (int * int) * (int * int)) : (int * int) * (int * int) =
    if (l < r) then (l,r) else (r,l)
  in
(*
  let format_edge
      (label : string)
      (((a,b),(c,d)) : (int * int) * (int * int)) : string =
    Format.sprintf
      "\n%s ((%d,%d),(%d,%d))\n"
      label a b c d
  in

  let debug_edge
      (label : string)
      (edge : (int * int) * (int * int)) : unit =
    print_string
      (format_edge label edge)
  in
*)

  let site_nodes : ApiTypes_t.site_node list =
    Array.to_list (api_snapshot_site_graph snapshot)
  in
  let components_index : (int * ApiTypes_t.site_node) array =
    Array.of_list
      (List.mapi (fun index site_node -> (index,site_node)) site_nodes)
  in
  let edge_index : int EdgeMap.t =
    List.fold_left
      (fun
        (index : int EdgeMap.t)
        ((agent_id,site_node) : int * ApiTypes_t.site_node) ->
        let offset : int = EdgeMap.cardinal index in
        let index_edges : (int * ((int * int) * (int * int))) list =
          List.mapi
            (fun i edge -> (i + offset,edge))
            (List.flatten
               (List.mapi
                  (fun
                    (site_id : int)
                    (site : ApiTypes_t.site) ->
                    List.map
                      (fun link -> ((agent_id,site_id),link))
                      site.ApiTypes_t.site_links
                  )
                  (Array.to_list site_node.ApiTypes_t.node_sites)
               )
            )
        in
        List.fold_left
          (fun
            (local_index : int EdgeMap.t)
            ((key,edge) : (int * ((int * int) * (int * int))))
            ->
              let edge = normalize_edge edge in
              if EdgeMap.mem edge local_index then
                local_index
              else
                (*
                let () =
                  print_string
                    (format_edge ((string_of_int key)^"!>") edge)
                in
                *)
                EdgeMap.add
                  edge
                  key
                  local_index
          )
          index
          index_edges
      )
      EdgeMap.empty
      (Array.to_list components_index)
  in
  (*
  let () =
    EdgeMap.iter
      (fun key value ->
        print_string
          (format_edge ((string_of_int value)^"->") key))
      edge_index
  in
  *)
  (* let () = print_string "(EdgeMap.cardinal edge_index)" in *)
  (* let () = print_int (EdgeMap.cardinal edge_index) in *)
  (* index the connected components *)
  let update_site
      (new_component_id : int)
      (location : int) : unit =
    let old_component_id : int =
      fst (Array.get components_index location)
    in
    Array.iteri
      (fun i (current_component_id,current_site_node) ->
         if current_component_id = old_component_id then
           Array.set
             components_index
             i
             (new_component_id,current_site_node)
         else
           ()
      )
      components_index
  in
  (* list of id's of connected components *)
  let components_ids : int list =
    Mods.IntSet.elements
      (List.fold_left
         (fun set elem -> Mods.IntSet.add (fst elem) set)
         Mods.IntSet.empty
         (Array.to_list components_index))
  in
  (* get components *)
  let agent_components id : (int * ApiTypes_t.site_node) list =
    List.map
      snd
      (List.filter
         fst
         (List.mapi
            (fun index (component_id,site_node) ->
               (component_id = id,(index,site_node))
            )
            (Array.to_list components_index)
          : (bool * (int * ApiTypes_t.site_node)) list)
       : (bool * (int * ApiTypes_t.site_node)) list)
  in
  let render_agent (_,(site_node : ApiTypes_t.site_node)) : string =
    let agent_label  = site_node.ApiTypes_t.node_name in
    let site_label =
      String.concat
        ","
        (List.map
           (fun site_node -> site_node.ApiTypes_t.site_name)
           (Array.to_list site_node.ApiTypes_t.node_sites))
    in
    Format.sprintf "%s(%s)" agent_label site_label in
  let render_component
      (connected_component : (int * ApiTypes_t.site_node) list) : string =
    String.concat
      ","
      (List.map
         (fun (agent_id,site_node) ->
            let agent_label = site_node.ApiTypes_t.node_name in
            let site_label =
              String.concat
		","
		(List.mapi
                   (fun site_id site_node ->
                      site_node.ApiTypes_t.site_name
                      ^
                      (String.concat
                         ""
                         (List.map
                            (fun link ->
                               let edge_id : (int * int) * (int * int) =
                                 normalize_edge
                                   ((agent_id,site_id),link)
                               in
                               (* let () = debug_edge "lookup" edge_id in *)
                               let link_id : int =
                                 EdgeMap.find
                                   edge_id
                                   edge_index
                               in
(*
                              let () =
                                debug_edge
                                  (string_of_int link_id)
                                  edge_id
                              in
*)
                               "!"^(string_of_int link_id)
                            )
                            site_node.ApiTypes_t.site_links))
                   )
                   (Array.to_list site_node.ApiTypes_t.node_sites))
            in
            Format.sprintf "%s(%s)" agent_label site_label
         )
         connected_component
      )
  in
  let () =
    List.iteri
      (fun index site_node ->
         Array.iter
           (fun (site : Api_types.site) ->
              List.iter
		(update_site index)
		(List.map fst site.Api_types.site_links)
           )
           site_node.Api_types.node_sites
      )
      site_nodes
  in
  String.concat
    "\n"
    (List.fold_left
       (fun
         (kappa_fragments : string list)
         (component : (int * ApiTypes_t.site_node) list) ->
         match component with
         | (_,{ Api_types.node_quantity = Some node_quantity
              ; _ })::tail ->
           (Format.sprintf "%%init: %f %s"
              node_quantity
              (match tail with
               | [] -> render_agent (List.hd component)
               | _ -> render_component component))::kappa_fragments
         | _ -> kappa_fragments

       )
       []
       (List.map
          agent_components
          components_ids))

let api_parse_is_empty (parse : Api_types.parse) =
  0 = Array.length parse.Api_types.contact_map

let api_message_errors
    ?(severity:Api_types.severity = `Error)
    (message : string) : Api_types.errors =
  [{ Api_types.severity = severity;
     Api_types.message = message ;
     Api_types.range = None }]

let api_location_errors
    ?(severity:Api_types.severity = `Error)
    ((message,location) : string Location.annot) =
  [{ Api_types.severity = severity;
     Api_types.message = message ;
     Api_types.range = Some (Location.to_range location) }]

let api_exception_errors (e : exn) =
  api_message_errors (Printexc.to_string e)

let lwt_msg (msg : string) =
  Lwt.return
    (`Left (api_message_errors msg))
let lwt_bind
    (f : 'a -> 'b Api_types.result Lwt.t)
    (result : 'a Api_types.result)
  : 'b Api_types.result Lwt.t =
  match result with
    `Left l -> Lwt.return (`Left l)
  | `Right r -> (f r)
let lwt_ignore (result : 'a Api_types.result) =
  match result with
    `Left _l -> Lwt.return_unit
  | `Right _r -> Lwt.return_unit

let eq_position l r =
  l.Api_types.chr = r.Api_types.chr
  &&
  l.Api_types.line = r.Api_types.line
let eq_range l r =
  match(l,r) with
  | (None,None) -> true
  | (Some l,Some r) ->
    eq_position l.Api_types.from_position r.Api_types.from_position
    && eq_position l.Api_types.to_position r.Api_types.to_position
  | _ -> false
let rec eq_errors  l r =
  match (l,r) with
  | ([],[]) -> true
  | (l::l_tail,r::r_tail) ->
    l.Api_types.message = r.Api_types.message
    && eq_range l.Api_types.range r.Api_types.range
    && eq_errors l_tail r_tail
  | _ -> false
