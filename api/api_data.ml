let api_message_errors
    ?(severity:Api_types_j.severity = `Error)
    ?(region:Api_types_j.range option = None)
    (message : string) : Api_types_j.errors =
  [{ Api_types_j.message_severity = severity;
     Api_types_j.message_text = message ;
     Api_types_j.message_range = region }]

let api_exception_errors (e : exn) : Api_types_j.errors =
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

let api_snapshot (sigs : Signature.s) (snapshot : Data.snapshot) : Api_types_j.snapshot =
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
