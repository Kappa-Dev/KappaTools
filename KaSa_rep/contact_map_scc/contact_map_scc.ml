(**
  * contact_map_scc.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2017, the 23rd of June
  * Last modification: Time-stamp: <Aug 07 2017>
  *
  * Compute strongly connected component in contact map
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

(***********************************************************)
(*TYPE*)
(************************************************************)

(*
Consider the contact map (def can be extended to arbitrary
sigma-graph)
Take a new graph directed G defined as :
   + Nodes are binding sites in the contact map
   + There is an edge from (A,x) to (B,y) if and only if
     There exists y such that there is a bond between (A,y) and
     (B,y) in the contact map.

Thm: if there is an unbounded number of species, there is a
(directed) cycle in G

-> Low hanging fruit

We can inspect readability constraints to restrict the edges in G
to the realizable (that is to say that the pattern
A(x!_,z!1),B(y!1) is realizable).

-> High hanging fruit.

We can add a dedicated domain based on Floyd-Warshal closure to
compute the relation that establish that a pair of sites in the
signature may be in the same cc in a readable species.
*)

(*
You have to fold over the sites of the contact map.

For each one, you have an agent and a site, and you have to
allocate this pair in the dictionary.
You will get an identifier.

Later you can use the dictionary to get (agent, site) from the id,
and conversely, the pair from the id.
*)

(*let contact_map_converted' parameters errors contact_map store_result =
  Array.fold_left (fun (errors, store_result) node ->
      let x = node.User_graph.node_type in
      let interface = node.User_graph.node_sites in
      Array.fold_left (fun (errors, store_result) site ->
          let y = site.User_graph.site_name in
          let l2 =
            match site.User_graph.site_type with
            | User_graph.Counter _ ->
              failwith "Kasa does not deal with counters yet"
            | User_graph.Port p -> p.User_graph.port_links
          in
          (*store two maps*)

          errors, store_result
        ) (errors, store_result) interface
    ) (errors, store_result) contact_map*)

    let contact_map_converted' parameters errors handler contact_map =
      Array.fold_left (fun (errors, store_result) node ->
          let x = node.User_graph.node_type in
          let interface = node.User_graph.node_sites in
          Array.fold_left (fun (errors, store_result) site ->
              let y  = site.User_graph.site_name in
              (*list of n2*)
              let l2 = match site.User_graph.site_type with
                | User_graph.Counter _ ->
                  failwith "Kasa does not deal with counters yet"
                | User_graph.Port p -> p.User_graph.port_links
              in
              let errors, (agent_name, site_name) =
                let errors, (bool, output) =
                  Ckappa_sig.Dictionary_of_agents.allocate_bool
                    parameters
                    errors
                    Ckappa_sig.compare_unit_agent_name
                    x
                    ()
                    Misc_sa.const_unit
                    handler.Cckappa_sig.agents_dic
                in
                match bool, output with
                | _, None ->
                  Exception.warn parameters errors __POS__ Exit
                    (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_site_name)
                | _, Some (agent_name, _, _, _) ->
                  begin
                    let errors, site_dic =
                      match
                        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get
                          parameters
                          errors
                          agent_name
                          handler.Cckappa_sig.sites
                      with
                      | errors, None ->
                        Exception.warn parameters errors __POS__ Exit
                          (Ckappa_sig.Dictionary_of_sites.init ())
                      | errors, Some i -> errors, i
                    in
                    let errors, (bool, output) =
                      Ckappa_sig.Dictionary_of_sites.allocate_bool
                        parameters
                        errors
                        Ckappa_sig.compare_unit_site_name
                        (Ckappa_sig.Binding y)
                        ()
                        Misc_sa.const_unit
                        site_dic
                    in
                    let errors, site_name =
                      match bool, output with
                      | _, None | true, _ ->
                        Exception.warn parameters
                          errors __POS__ Exit Ckappa_sig.dummy_site_name
                      | _, Some (site_name, _, _, _) -> errors, site_name
                    in
                    errors, (agent_name, site_name)
                  end
              in
              (*binding nodes*)
              let errors, store_result2 =
                List.fold_left (fun (errors, store_result2) (ag, site2) ->
                    let errors, old_nodes2 =
                      match
                        Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
                          parameters
                          errors
                          (agent_name, site_name)
                          store_result2
                      with
                      | errors, None ->
                        Exception.warn parameters errors __POS__ Exit []
                      | errors, Some l -> errors, l
                    in
                    let errors, store_result =
                      Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite
                        parameters
                        errors
                        (agent_name, site_name)
                        ((ag, site2) :: old_nodes2)
                        store_result
                    in
                    let () =
                      Ckappa_sig.AgentSite_map_and_set.Map.iter
                        (fun (ag, site) l  ->
                           List.iter (fun (ag2, site2) ->
                               let l' = List.filter (fun (_,x) -> x <> site2) l2 in
                               let () =   List.iter (fun (a, s) ->
                                   let () =
                                     Loggers.fprintf (Remanent_parameters.get_logger parameters)
                                       "YES:%i" s
                                   in ()
                                 ) l'
                               in ()
                             ) l
                        ) store_result
                    in
                    let () =
                      Ckappa_sig.AgentSite_map_and_set.Map.iter
                        (fun (ag1,site1) l ->
                           List.iter (fun (ag2, site2) ->
                               Loggers.fprintf (Remanent_parameters.get_logger parameters)
                                 " [agent:%i, site:%i] -- ag:%i:site:%i \n "
                                 (Ckappa_sig.int_of_agent_name ag1)
                                 (Ckappa_sig.int_of_site_name site1)
                                 ag2
                                 site2
                             ) l
                        ) store_result
                    in
                    errors, store_result
                  ) (errors, store_result) l2
              in
              errors, store_result
            ) (errors, store_result) interface
        ) (errors, Ckappa_sig.AgentSite_map_and_set.Map.empty) contact_map

let contact_map_converted parameters errors handler contact_map store_result =
  let _ =
    contact_map_converted' parameters errors handler contact_map
  in
  Array.fold_left (fun (errors, store_result) node ->
      let x = node.User_graph.node_type in
      let interface = node.User_graph.node_sites in
      Array.fold_left (fun (errors, store_result) site ->
          let store_result1, store_result2 = store_result in
          let y  = site.User_graph.site_name in
          (*list of n2*)
          let l1,l2 = match site.User_graph.site_type with
            | User_graph.Counter _ ->
              failwith "Kasa does not deal with counters yet"
            | User_graph.Port p ->
              p.User_graph.port_states, p.User_graph.port_links
          in
          let errors, (agent_name, site_name) =
            let errors, (bool, output) =
              Ckappa_sig.Dictionary_of_agents.allocate_bool
                parameters
                errors
                Ckappa_sig.compare_unit_agent_name
                x
                ()
                Misc_sa.const_unit
                handler.Cckappa_sig.agents_dic
            in
            match bool, output with
            | _, None ->
              Exception.warn parameters errors __POS__ Exit
                (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_site_name)
            | _, Some (agent_name, _, _, _) ->
              begin
                let errors, site_dic =
                  match
                    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get
                      parameters
                      errors
                      agent_name
                      handler.Cckappa_sig.sites
                  with
                  | errors, None ->
                    Exception.warn parameters errors __POS__ Exit
                      (Ckappa_sig.Dictionary_of_sites.init ())
                  | errors, Some i -> errors, i
                in
                let errors, (bool, output) =
                  Ckappa_sig.Dictionary_of_sites.allocate_bool
                    parameters
                    errors
                    Ckappa_sig.compare_unit_site_name
                    (Ckappa_sig.Binding y)
                    ()
                    Misc_sa.const_unit
                    site_dic
                in
                let errors, site_name =
                  match bool, output with
                  | _, None | true, _ ->
                    Exception.warn parameters
                      errors __POS__ Exit Ckappa_sig.dummy_site_name
                  | _, Some (site_name, _, _, _) -> errors, site_name
                in
                errors, (agent_name, site_name)
              end
          in
          (*Nodes are binding sites*)
          let node_id =
            Graphs.node_of_int (Ckappa_sig.int_of_site_name site_name)
          in
          (*search old node*)
          let errors, old_nodes =
            match
              Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
                parameters
                errors
                (agent_name, site_name)
                store_result1
            with
            | errors, None -> Exception.warn parameters errors __POS__
                                Exit []
            | errors, Some l -> errors, l
          in
          let nodes = node_id :: old_nodes in
          let errors, store_result1 =
            Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite
              parameters
              errors
              (agent_name,site_name)
              nodes
              store_result1
          in
          (*let _ =
            Loggers.fprintf (Remanent_parameters.get_logger parameters)
              "(agent:%i, site:%i)  "
              (Ckappa_sig.int_of_agent_name agent_name)
              (Ckappa_sig.int_of_site_name site_name)
          in*)
          (*binding nodes*)
          let errors, store_result2 =
            List.fold_left (fun (errors, store_result2) (ag, site2) ->
                let errors, old_nodes2 =
                  match
                    Ckappa_sig.AgentSite_map_and_set.Map.find_option_without_logs
                      parameters
                      errors
                      (Ckappa_sig.agent_name_of_int ag,Ckappa_sig.site_name_of_int site2)
                      store_result2
                  with
                  | errors, None ->
                    Exception.warn parameters errors __POS__ Exit []
                  | errors, Some l -> errors, l
                in
                let new_nodes2 =
                  (Graphs.node_of_int site2):: old_nodes2
                in
                let errors, store_result2 =
                  Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite
                    parameters
                    errors
                    (Ckappa_sig.agent_name_of_int ag, Ckappa_sig.site_name_of_int site2)
                    new_nodes2
                    store_result2
                in
                errors, store_result2
              ) (errors, store_result2) l2
          in
          errors, (store_result1, store_result2)
        ) (errors, store_result) interface
    ) (errors, store_result) contact_map

let compute_graph_scc parameters errors contact_map_converted store_result =
  let store_result1, store_result2 = contact_map_converted in
  Ckappa_sig.AgentSite_map_and_set.Map.fold
    (fun (agent_name, site_name) nodes (errors, store_result) ->
       (*todo: check the condition of edges*)
       let errors, edges_list =
         List.fold_left (fun (errors, store_list) n1 ->
             Ckappa_sig.AgentSite_map_and_set.Map.fold
               (fun (ag2, site2) nodes' (errors, store_list) ->
                  List.fold_left (fun (errors, store_list) n2 ->
                      let () =
                        Loggers.fprintf
                          (Remanent_parameters.get_logger parameters)
                          "\nagent:%i:site:%i:node:%n-agent:%i:site:%i:node:%i\n"
                          (Ckappa_sig.int_of_agent_name agent_name)
                          (Ckappa_sig.int_of_site_name site_name)
                          (Graphs.int_of_node n1)
                          (Ckappa_sig.int_of_agent_name ag2)
                          (Ckappa_sig.int_of_site_name site2)
                          (Graphs.int_of_node n2)
                      in
                      errors, (n1, (), n2) :: store_list
                    ) (errors, store_list) nodes'
               ) store_result2 (errors, store_list)
           ) (errors, []) nodes
       in
       (*build a graph_scc*)
       let graph =
         Graphs.create parameters errors
           (fun n ->
              let () =
                Loggers.fprintf (Remanent_parameters.get_logger parameters)
                  "node_labels:%i\n"
                  (Graphs.int_of_node n)
              in
              ()
           )
           nodes
           edges_list
       in
       (*compute scc*)
       let errors, low, pre, on_stack, scc =
         Graphs.compute_scc parameters errors
           (fun () -> "")
           graph
       in
       let errors, store_result =
         Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite
           parameters
           errors
           (agent_name, site_name)
           scc
           store_result
       in
       errors, store_result
    ) store_result1 (errors, store_result)
