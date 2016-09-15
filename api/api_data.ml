(* There are slight differences between the api datatyep as
   and the simulator datatypes.  This class serves to map
   the two implementations.
*)
let plot_pg_store
    ~plot
    ~file
    ~title
    ~descr
  : Pp_svg.store
  = { Pp_svg.file = file;
      Pp_svg.title = title;
      Pp_svg.descr = descr;
      Pp_svg.legend = Array.of_list plot.ApiTypes_j.legend;
      Pp_svg.points =
        List.map
          (fun (observable : ApiTypes_j.observable) ->
             (observable.ApiTypes_j.observation_time ,
              Tools.array_map_of_list
                (fun x -> Nbr.F x) observable.ApiTypes_j.observation_values))
          plot.ApiTypes_j.time_series
    }

let plot_values
    ?(separator : string = ",")
    (plot : ApiTypes_j.plot) : string =
  String.concat "\n"
    ((String.concat
        separator
        ("time"::plot.ApiTypes_j.legend))::
     (List.map
        (fun (observable : ApiTypes_j.observable) ->
           String.concat separator
             (List.map
                (Format.sprintf "%e")
                (observable.ApiTypes_j.observation_time
                 ::observable.ApiTypes_j.observation_values)
             )
        )
        plot.ApiTypes_j.time_series)
    )


let api_file_line (file_line : Data.file_line) : ApiTypes_j.file_line =
  { ApiTypes_j.file_name = file_line.Data.file_name
  ; ApiTypes_j.line = file_line.Data.line
  }

let api_flux_map (flux_map : Data.flux_map) : ApiTypes_j.flux_map =
  { ApiTypes_j.flux_begin_time = flux_map.Data.flux_data.Data.flux_start;
    ApiTypes_j.flux_end_time = flux_map.Data.flux_end ;
    ApiTypes_j.flux_rules = Array.to_list flux_map.Data.flux_rules;
    ApiTypes_j.flux_hits = Array.to_list flux_map.Data.flux_data.Data.flux_hits;
    ApiTypes_j.flux_fluxs =
      List.map
        Array.to_list (Array.to_list flux_map.Data.flux_data.Data.flux_fluxs);
    ApiTypes_j.flux_name = flux_map.Data.flux_data.Data.flux_name
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
       { ApiTypes_j.node_quantity = None;
         ApiTypes_j.node_name =
           Format.asprintf "%a" (Signature.print_agent sigs) a.Raw_mixture.a_type;
         ApiTypes_j.node_sites =
           Array.mapi
             (fun j s ->
                { ApiTypes_j.site_name =
                    Format.asprintf
                      "%a" (Signature.print_site sigs a.Raw_mixture.a_type) j;
                  ApiTypes_j.site_links =
                    (match Mods.Int2Map.find_option (i,j) links with
                     | None -> []
                     | Some dst -> [dst]);
                  ApiTypes_j.site_states =
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

let api_snapshot sigs (snapshot : Data.snapshot) : ApiTypes_j.snapshot =
  { ApiTypes_j.snap_file = snapshot.Data.snap_file
  ; ApiTypes_j.snap_event = snapshot.Data.snap_event
  ; ApiTypes_j.agents =
      List.map
        (fun (agent,mixture) -> (agent,api_mixture sigs mixture))
        snapshot.Data.agents
  ; ApiTypes_j.tokens =
      List.map (fun (token,value) -> (Nbr.to_float value,token))
        (Array.to_list snapshot.Data.tokens)
  }


let find_link cm (a,s) =
  let rec auxs i j = function
    | [] -> raise Not_found
    | (s',_) :: t ->
      if s = s' then
        (i,j)
      else
        auxs i (succ j) t
  in
  let rec auxa i = function
    | [] -> raise Not_found
    | (a',l) :: t ->
      if a = a' then
        auxs i 0 l
      else auxa (succ i) t
  in
  auxa 0 cm

let api_contact_map sigs cm =
  Array.mapi
    (fun ag sites ->
       { ApiTypes_j.node_quantity = None;
         ApiTypes_j.node_name =
           Format.asprintf "%a" (Signature.print_agent sigs) ag;
         ApiTypes_j.node_sites =
           Array.mapi
             (fun site (states,links) ->
                { ApiTypes_j.site_name =
                    Format.asprintf "%a" (Signature.print_site sigs ag) site;
                  ApiTypes_j.site_links = links;
                  ApiTypes_j.site_states =
                    List.map
                      (Format.asprintf
                         "%a"
                         (Signature.print_internal_state sigs ag site))
                      states;
                }) sites;
       }) cm

let api_contactmap_site_graph
    (contactmap : ApiTypes_j.parse) : ApiTypes_j.site_graph =
  contactmap.ApiTypes_j.contact_map

let offset_site_graph
    (offset : int)
    (site_nodes : ApiTypes_j.site_node list) :
  ApiTypes_j.site_node list =
  List.map
    (fun site_node ->
       { site_node with
         ApiTypes_j.node_sites =
           Array.map
             (fun site ->
                { site with
                  ApiTypes_j.site_links =
                    List.map (fun (i,j) -> (i+offset,j+offset))
                      site.ApiTypes_j.site_links })
             site_node.ApiTypes_j.node_sites
       }
    )
    site_nodes

let api_snapshot_site_graph
    (snapshot : ApiTypes_j.snapshot) : ApiTypes_j.site_graph =
  let tokens_sg = Tools.array_map_of_list (fun (value,token) ->
      { ApiTypes_j.node_name = token ;
        ApiTypes_j.node_quantity = Some value;
        ApiTypes_j.node_sites = [||] })
      snapshot.ApiTypes_j.tokens in
  snd
    (List.fold_left
       (fun (old_offset,old_agents) (agent,mixture) ->
          let new_offset = old_offset + (Array.length mixture) in
          let update_links (agent_id,site_id : int * int) =
            (agent_id+old_offset,site_id)
          in
          let update_sites site = { site with
                                    ApiTypes_j.site_links =
                                      List.map
                                        update_links
                                        site.ApiTypes_j.site_links
                                     } in
          let new_agents =
            Array.map
              (fun (node : ApiTypes_j.site_node)->
                 { node with
                   ApiTypes_j.node_quantity = Some (float_of_int agent) ;
                   ApiTypes_j.node_sites =
                     Array.map
                       update_sites
                       node.ApiTypes_j.node_sites
                 }
              )
              mixture
          in
          (new_offset,Array.append old_agents new_agents)
       )
       (Array.length tokens_sg,tokens_sg)
       snapshot.ApiTypes_j.agents)

(* map out *)
let normalize_edge
    ((l,r) : (int * int) * (int * int)) : (int * int) * (int * int) =
  if (l < r) then (l,r) else (r,l)

module EdgeMap =
  Map.Make(struct type t = (int * int) * (int * int)
    let compare = compare
  end)
module EdgeSet =
  Set.Make(struct type t = (int * int) * (int * int)
    let compare =
      fun l r ->
        compare
          (normalize_edge l)
          (normalize_edge r)
  end)
let hash_color (l : string) : string =
  Format.sprintf "#%0x" ((Hashtbl.hash l) mod 0xffffff)

type site_node_component = { index : int ;
                             site_node : ApiTypes_t.site_node ;
                             mutable component_id : int }

let print_site_node ?link_store agid f sn =
  Format.fprintf f "%s(@[<h>%a@])"
    sn.ApiTypes_j.node_name
    (Pp.array Pp.comma
       (fun sid f ss -> Format.fprintf f "%s%a%t" ss.ApiTypes_j.site_name
           (Pp.list Pp.empty (fun f i -> Format.fprintf f "~%s" i))
           ss.ApiTypes_j.site_states
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
                         Tools.unsome (-1) lid
                       else
                         let () =
                           r:=(succ fid, Mods.Int2Map.add (agid',sid') fid idm) in
                         fid in
                     Format.fprintf f "!%i" lid) f
                  ss.ApiTypes_j.site_links)
       ))
    sn.ApiTypes_j.node_sites

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
                     nb_cc i nb_cc di ss.ApiTypes_j.site_name
                     mix.(di).ApiTypes_j.node_sites.(dsi).ApiTypes_j.site_name)
              f ss.ApiTypes_j.site_links)
         f sn.ApiTypes_j.node_sites;
       Format.fprintf
         f "node%d_%d -> counter%d [style=invis];@," nb_cc i nb_cc)
    f mix

let api_snapshot_dot (snapshot : ApiTypes_j.snapshot) =
  Format.asprintf
    "@[<v>digraph G{@,%a@,%a}@]@."
    (Pp.listi
       Pp.cut
       (fun i f (nb,mix) ->
          Format.fprintf f "@[<v 2>subgraph cluster%d{@," i;
          Format.fprintf
            f "counter%d [label = \"%d instance(s)\", shape=none];@,%a}@]"
            i nb (print_site_nodes_dot i) mix))
    snapshot.ApiTypes_j.agents
    (Pp.listi Pp.cut (fun i f (el,na) ->
         Format.fprintf
           f
           "token_%d [label = \"%s (%f)\" , shape=none]"
           i na el))
    snapshot.ApiTypes_j.tokens

let print_site_nodes f sn =
  let link_store = ref (1,Mods.Int2Map.empty) in
  Pp.array Pp.comma (print_site_node ~link_store) f sn

let api_snapshot_kappa (snapshot : ApiTypes_j.snapshot) : string =
  Format.asprintf
    "@[<v>%a@,%a@]"
    (Pp.list Pp.space (fun f (i,mix) ->
                       Format.fprintf f "%%init: %i @[<h>%a@]" i
                                      print_site_nodes mix))
    snapshot.ApiTypes_j.agents
    (Pp.list Pp.space (fun f (el,na) ->
                        Format.fprintf f "%%init: %s <- %f" na el))
    snapshot.ApiTypes_j.tokens

let api_parse_is_empty (parse : ApiTypes_j.parse) =
  0 = Array.length parse.ApiTypes_j.contact_map

let api_message_errors
    ?(severity:ApiTypes_j.severity = `Error)
    (message : string) : ApiTypes_j.errors =
  [{ ApiTypes_j.severity = severity;
     ApiTypes_j.message = message ;
     ApiTypes_j.range = None }]

let api_location_errors
    ?(severity:ApiTypes_j.severity = `Error)
    ((message,location) : string Location.annot) =
  [{ ApiTypes_j.severity = severity;
     ApiTypes_j.message = message ;
     ApiTypes_j.range = Some (Location.to_range location) }]

let api_exception_errors (e : exn) =
  api_message_errors (Printexc.to_string e)

let lwt_msg (msg : string) =
  Lwt.return
    (`Left (api_message_errors msg))
let lwt_bind
    (f : 'a -> 'b ApiTypes_j.result Lwt.t)
    (result : 'a ApiTypes_j.result)
  : 'b ApiTypes_j.result Lwt.t =
  match result with
    `Left l -> Lwt.return (`Left l)
  | `Right r -> (f r)
let lwt_ignore (result : 'a ApiTypes_j.result) =
  match result with
    `Left _l -> Lwt.return_unit
  | `Right _r -> Lwt.return_unit

let eq_position l r =
  l.ApiTypes_j.chr = r.ApiTypes_j.chr
  &&
  l.ApiTypes_j.line = r.ApiTypes_j.line
let eq_range l r =
  match(l,r) with
  | (None,None) -> true
  | (Some l,Some r) ->
    eq_position l.ApiTypes_j.from_position r.ApiTypes_j.from_position
    && eq_position l.ApiTypes_j.to_position r.ApiTypes_j.to_position
  | _ -> false
let rec eq_errors  l r =
  match (l,r) with
  | ([],[]) -> true
  | (l::l_tail,r::r_tail) ->
    l.ApiTypes_j.message = r.ApiTypes_j.message
    && eq_range l.ApiTypes_j.range r.ApiTypes_j.range
    && eq_errors l_tail r_tail
  | _ -> false
