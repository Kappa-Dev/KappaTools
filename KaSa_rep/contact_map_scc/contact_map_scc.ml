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

let contact_map_converted parameters error handler contact_map store_result =
  (*get a list of sites*)
  let error, pair_list =
    Ckappa_sig.Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameters error
      (fun parameters error (x,y) (a,b,c) store_list ->
        error,  (a,b) :: store_list
      ) handler.Cckappa_sig.dual []
  in
  Array.fold_left (fun (error, store_result) node ->
      let x = node.User_graph.node_type in
      let interface = node.User_graph.node_sites in
      Array.fold_left (fun (error, store_result) site ->
          let y  = site.User_graph.site_name in
          (*list of n2*)
          let l2 = match site.User_graph.site_type with
            | User_graph.Counter _ ->
              failwith "Kasa does not deal with counters yet"
            | User_graph.Port p -> p.User_graph.port_links
          in
          let error, (agent_name, site_name) =
            let error, (bool, output) =
              Ckappa_sig.Dictionary_of_agents.allocate_bool
                parameters
                error
                Ckappa_sig.compare_unit_agent_name
                x
                ()
                Misc_sa.const_unit
                handler.Cckappa_sig.agents_dic
            in
            match bool, output with
            | _, None ->
              Exception.warn parameters error __POS__ Exit
                (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_site_name)
            | _, Some (agent_name, _, _, _) ->
              begin
                let error, site_dic =
                  match
                    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get
                      parameters
                      error
                      agent_name
                      handler.Cckappa_sig.sites
                  with
                  | error, None ->
                    Exception.warn parameters error __POS__ Exit
                      (Ckappa_sig.Dictionary_of_sites.init ())
                  | error, Some i -> error, i
                in
                let error, (bool, output) =
                  Ckappa_sig.Dictionary_of_sites.allocate_bool
                    parameters
                    error
                    Ckappa_sig.compare_unit_site_name
                    (Ckappa_sig.Binding y)
                    ()
                    Misc_sa.const_unit
                    site_dic
                in
                let error, site_name =
                  match bool, output with
                  | _, None | true, _ ->
                    Exception.warn parameters
                      error __POS__ Exit Ckappa_sig.dummy_site_name
                  | _, Some (site_name, _, _, _) -> error, site_name
                in
                error, (agent_name, site_name)
              end
          in
          let error, store_result =
            List.fold_left (fun (error, store_result) (agent_name2, site2) ->
                let potential_sites =
                  List.filter (fun (a,x) ->
                      (Ckappa_sig.int_of_agent_name a) = agent_name2 &&
                      (Ckappa_sig.int_of_site_name x) <> site2) pair_list
                in
                let error, store_result =
                  Ckappa_sig.Pair_AgentSite_map_and_set.Map.add_or_overwrite
                    parameters
                    error
                    ((agent_name, site_name), (x,y))
                    potential_sites
                    store_result
                in
                error, store_result
              ) (error, store_result) l2
          in
          error, store_result
        ) (error, store_result) interface
    ) (error, store_result) contact_map

let compute_graph_scc parameters errors contact_map_converted store_result =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "Graphs: \n"
  in
  Ckappa_sig.Pair_AgentSite_map_and_set.Map.fold
    (fun ((agent_name, site_name),(agent_name_string, site_name_string))
      potential_sites (errors, store_result) ->
      let errors, nodes, edges_list =
        List.fold_left (fun (errors, store_nodes, store_edges) (agent2, site2) ->
            let n1 = Graphs.node_of_int (Ckappa_sig.int_of_site_name site_name) in
            let n2 = Graphs.node_of_int (Ckappa_sig.int_of_site_name site2) in
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%s@%s -- %i@%i"
                agent_name_string
                site_name_string
                (Ckappa_sig.int_of_agent_name agent2)
                (Ckappa_sig.int_of_site_name site2)
            in
            let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
            errors, (n1 :: store_nodes), (n1, (), n2) :: store_edges
          ) (errors, [], []) potential_sites
      in
      (*build a graph_scc*)
      let graph =
        Graphs.create parameters errors
          (fun _n -> ()
           (*let () =
                 Loggers.fprintf (Remanent_parameters.get_logger parameters)
                  "node_labels:%i\n"
                  (Graphs.int_of_node n)
              in*)
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
        Ckappa_sig.AgentSiteString_map_and_set.Map.add_or_overwrite
          parameters
          errors
          (agent_name_string, site_name_string)
          scc
          store_result
      in
      errors, store_result
    ) contact_map_converted (errors, store_result)
