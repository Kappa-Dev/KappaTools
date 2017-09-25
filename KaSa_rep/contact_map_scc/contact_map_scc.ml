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

Example:
%agent:A(x,y,z)
%agent:B(x,y)
%agent:C(x,y)

%init: 100 A()
%init: 100 B()
%init: 100 C()

'r1' A(y), C(x) -> A(y!1), C(x!1)
'r2' B(x), C(y) -> B(x!1), C(y!1)
'r3' A(z), B(y) -> A(z!1), B(y!1)
'r4' A(x), B(y) -> A(x!1), B(y!1)

-> A(x!1), B(y!1), because z belongs to A, and A(z!1), B(y!1)

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

let contact_map_converted parameters errors handler contact_map store_result =
  (*Print for debug*)
  let _ =
    Loggers.fprintf (Remanent_parameters.get_logger parameters)
      "Dictionary of agents\n"
  in
  let _ =
    Ckappa_sig.Dictionary_of_agents.iter
    parameters errors
    (fun parameters errors k x a b ->
       let _ =
         Loggers.fprintf (Remanent_parameters.get_logger parameters)
           "Dictionary key:%i:value:%s\n"
           (Ckappa_sig.int_of_agent_name k)
           x
       in
       errors)
    handler.Cckappa_sig.agents_dic
  in
  let _ =
    Loggers.fprintf (Remanent_parameters.get_logger parameters)
      "Dictionary of sites\n"
  in
  let _ =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.iter
      parameters errors
      (fun parameters errors ag site_dic ->
         Ckappa_sig.Dictionary_of_sites.iter
           parameters errors
           (fun parameters errors k x a b ->
              let _ =
                Loggers.fprintf (Remanent_parameters.get_logger parameters)
                  "\nDictionary agent:%i key:%i"
                  (Ckappa_sig.int_of_agent_name ag)
                  (Ckappa_sig.int_of_site_name k);
                (match x with
                 | Ckappa_sig.Internal a ->
                   Loggers.fprintf (Remanent_parameters.get_logger parameters)
                     " Internal %s\n" a
                 | Ckappa_sig.Binding a ->
                   Loggers.fprintf (Remanent_parameters.get_logger parameters)
                     " Binding %s\n" a
                )
              in
              errors) site_dic
      )
      handler.Cckappa_sig.sites
  in
  (*convert each node in contact map to node in graph*)
  List.fold_left (fun (errors, store_result) node ->
      let x = node.Public_data.site_node_name in
      let interface = node.Public_data.site_node_sites in
      List.fold_left (fun (errors, store_result) site ->
          let y  = site.Public_data.site_name in
          (*l2 can be edges; site_name can be id*)
          (*let l1 = site.Public_data.site_states in*)
          (*list of n2*)
          let l2 = site.Public_data.site_links in
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
          let node_id =
            Graphs.node_of_int (Ckappa_sig.int_of_site_name site_name)
          in
          let errors, store_result =
            Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite
              parameters
              errors
              (agent_name,site_name)
              (node_id::[], l2)
              store_result
          in
          errors, store_result
        ) (errors, store_result) interface
    ) (errors, store_result) contact_map

let compute_graph_scc parameters errors contact_map_converted store_result =
  Ckappa_sig.AgentSite_map_and_set.Map.fold
    (fun (agent_name, site_name) (nodes, l2) (errors, store_result) ->
       (*create a new graph *)
       let errors, nodes' =
         List.fold_left (fun (errors, store_list) (ag,site) ->
             errors, (Graphs.node_of_int site) :: store_list
           ) (errors, []) l2
       in
       (*todo: check the condition of edges*)
       let errors, edges_list =
         List.fold_left (fun (erros, store_list) n1 ->
             List.fold_left (fun (errors, store_list) n2 ->
                 let s = Loggers.fprintf (Remanent_parameters.get_logger parameters)
                     "\nnode:%n-node:%i\n"
                     (Graphs.int_of_node n1)
                     (Graphs.int_of_node n2)
                 in
                 errors, (n1, s, n2) :: store_list
               ) (erros, store_list) nodes
           ) (errors, []) nodes'
       in
       let graph_scc =
         Graphs.create parameters errors
           (fun n ->
              Loggers.fprintf (Remanent_parameters.get_logger parameters)
                "node_labels:%i\n"
                (Graphs.int_of_node n)
           )
           nodes
           edges_list
       in
       let errors, store_result =
         Ckappa_sig.AgentSite_map_and_set.Map.add_or_overwrite
           parameters
           errors
           (agent_name, site_name)
           graph_scc
           store_result
       in
       errors, store_result
    ) contact_map_converted (errors, store_result)

(*let compute_graph_scc parameters error contact_map =
  let init_graph = Graphs.create parameters error
      (fun n -> ())
      []
      []
  in
  let init_dic = Dictionary_of_agent_site.init () in
  Array.fold_left
    (fun (error, store_dic, store_graph, store_node_list, edges_list) pair_array ->
      Array.fold_left
        (fun (error, store_dic, store_graph, store_node_list, edges_list)
          (state_list, pair_list)  ->
           List.fold_left
             (fun (error, store_dic, store_graph, store_node_list, edges_list) (agent, site) ->
               let error, id =
                 match
                   Dictionary_of_agent_site.allocate
                     parameters
                     error
                     Ckappa_sig.compare_unit
                     (agent, site)
                     ()
                     Misc_sa.const_unit
                     store_dic
                 with
                 | error, None -> Exception.warn parameters error __POS__ Exit 0
                 | error, Some (k, a, b, dic) -> error, k
              in
              (*convert id into node in graph *)
              let node_id = Graphs.node_of_int id in
              let node_list = node_id :: store_node_list in
              let graph =
                Graphs.create
                  parameters
                  error
                  (fun n ->
                     (*Loggers.fprintf (Remanent_parameters.get_logger parameters)
                       "node lable:%i\n"*)
                       (Graphs.int_of_node n))
                  node_list
                  edges_list
              in
              let error, low, pre, on_stack, id_list_list =
                Graphs.compute_scc parameters error
                  (fun n -> Pervasives.string_of_int n)
                  graph
              in
              error, store_dic, store_graph, store_node_list,
              edges_list
             ) (error, store_dic, store_graph, store_node_list,
                edges_list)
             pair_list
        ) (error, store_dic, store_graph, store_node_list, edges_list) pair_array
    ) (error, init_dic, init_graph, [], []) contact_map*)
