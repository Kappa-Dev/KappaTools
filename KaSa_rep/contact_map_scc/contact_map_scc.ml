(**
  * contact_map_scc.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2017, the 23rd of June
  * Last modification: Time-stamp: <Oct 27 2017>
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


type site = Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name

type node = site * site

type edge = node * node

type converted_contact_map =
  node list Ckappa_sig.PairAgentSite_map_and_set.Map.t

let add_edges
    parameters error
    (agent_name, site_name, agent_name', site_name')
    site_name_list graph =
  let edge = ((agent_name, site_name), (agent_name', site_name')) in
  let error, old =
    Ckappa_sig.PairAgentSite_map_and_set.Map.find_default_without_logs
      parameters
      error
      []
      edge
      graph
  in
  let internal_scc_decomposition =
    List.fold_left (fun old (site_name'', partners) ->
        List.fold_left (fun old (agent_name''', site_name''') ->
            let edge =
              ((agent_name', site_name''), (agent_name''', site_name'''))
            in
            edge :: old
          ) old partners
      ) old site_name_list
  in
  Ckappa_sig.PairAgentSite_map_and_set.Map.add_or_overwrite
    parameters error
    edge
    internal_scc_decomposition
    graph

let convert_contact_map parameters error contact_map =
  let graph = Ckappa_sig.PairAgentSite_map_and_set.Map.empty in
    Ckappa_sig.Agent_map_and_set.Map.fold
    (fun agent_name interface (error, graph) ->
         Ckappa_sig.Site_map_and_set.Map.fold
           (fun site_name (_, partners) (error, graph) ->
            List.fold_left
              (fun (error, graph) (agent_name', site_name') ->
                 let error, pair_opt =
                   Ckappa_sig.Agent_map_and_set.Map.find_option
                     parameters error
                     agent_name'
                     contact_map
                 in
                 match pair_opt with
                 | None -> Exception.warn parameters error __POS__ Exit graph
                 | Some interface' ->
                   let error, others =
                     Ckappa_sig.Site_map_and_set.Map.fold
                       (fun site_name (_, partners) (error, others) ->
                          error,
                          if partners = [] || site_name = site_name'
                          then
                            others
                          else
                            (site_name, partners) :: others
                       ) interface' (error,[])
                   in
                   add_edges
                     parameters error
                     (agent_name, site_name, agent_name', site_name')
                     others
                     graph
              ) (error, graph) partners
           ) interface (error, graph)
    ) contact_map (error,graph)

let mixture_of_edge
    parameters errors
    (((ag, st), (ag', st')),
      ((ag'', st''), (ag''', st'''))) =
  let _ = ag, ag''', st, st''' in
  if ag'<> ag'' || st' = st''
  then
    let errors, mixture = Preprocess.empty_mixture parameters errors in
    Exception.warn parameters errors __POS__ Exit mixture
  else
    (* TO DO build the pattern:
       A(x!1), B(x!1, y!2), C(x!2)
       ag(st!1), ag'(st'!1, st''!2), ag'''(st'''!2)
    *)
    (*get agend_id of ag(st!1)*)
    let errors, init_views =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.create
        parameters errors
        0
    in
    let fresh_agent_id = Ckappa_sig.dummy_agent_id in
    let ag_id' = Ckappa_sig.next_agent_id fresh_agent_id in
    let ag_id'' = Ckappa_sig.next_agent_id ag_id' in
    let ag_id''' = Ckappa_sig.next_agent_id ag_id'' in
    (*build bond *)
    let site_address1 =
      {
        Cckappa_sig.agent_index = fresh_agent_id;
        Cckappa_sig.site = st;
        Cckappa_sig.agent_type = ag;
      }
    in
    let errors, site_map1 =
      Ckappa_sig.Site_map_and_set.Map.add_or_overwrite
        parameters errors
        st
        site_address1
        Ckappa_sig.Site_map_and_set.Map.empty
    in
    let site_address2 =
      {
        Cckappa_sig.agent_index = ag_id';
        Cckappa_sig.site = st';
        Cckappa_sig.agent_type = ag';
      }
    in
    let errors, site_map2 =
    Ckappa_sig.Site_map_and_set.Map.add_or_overwrite
      parameters errors
      st'
      site_address2
      site_map1
    in
    let site_address3 =
      {
        Cckappa_sig.agent_index = ag_id'';
        Cckappa_sig.site = st'';
        Cckappa_sig.agent_type = ag'';
      }
    in
    let errors, site_map3 =
    Ckappa_sig.Site_map_and_set.Map.add_or_overwrite
      parameters errors
      st''
      site_address3
      site_map2
    in
    let site_address4 =
      {
        Cckappa_sig.agent_index = ag_id''';
        Cckappa_sig.site = st''';
        Cckappa_sig.agent_type = ag''';
      }
    in
    let errors, site_map4 =
    Ckappa_sig.Site_map_and_set.Map.add_or_overwrite
      parameters errors
      st'''
      site_address4
      site_map3
    in
    let errors, init_bonds =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.create
      parameters errors
      0
    in
    let errors, bonds =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set
        parameters errors
        ag_id'''
        site_map4
        init_bonds
    in
    let errors, mixture =
      errors,
      {
        Cckappa_sig.views = init_views;
        Cckappa_sig.bonds = bonds;
        Cckappa_sig.plus = [];
        Cckappa_sig.dot = [];
        Cckappa_sig.c_mixture = Ckappa_sig.EMPTY_MIX
      }
    in
    (*let errors, mixture = Preprocess.empty_mixture parameters errors in*)
    errors, mixture
(* *)

let filter_edges_in_converted_contact_map
    parameters error static dynamic
    is_reachable
    converted_contact_map
  =
  (* TO DO: remove in converted_contact_map each edge that would encode
     an unreachable patten *)
  (* Take care of propagating memoisation table, and error stream *)
  let _ = parameters, is_reachable in
  error, dynamic, converted_contact_map

let compute_graph_scc parameters errors contact_map_converted =
    let nodes, edges_list =
        Ckappa_sig.PairAgentSite_map_and_set.Map.fold
          (fun node1 potential_sites (nodes, edges) ->
             let nodes = node1::nodes in
             let edges =
               List.fold_left (fun edges node2 ->
                   (node1, node2) :: edges
                 ) edges potential_sites
             in
             nodes,edges
          ) contact_map_converted ([], [])
    in
    let n_nodes = List.length nodes in
    let nodes_array =
      Array.make n_nodes
        ((Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_site_name),
         (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_site_name))
    in
    let nodes_map = Ckappa_sig.PairAgentSite_map_and_set.Map.empty in
    let _, nodes, (errors, nodes_map) =
      List.fold_left
        (fun (i, nodes_list, (errors, map)) node ->
           nodes_array.(i) <- node;
           i+1,
           (Graphs.node_of_int i) :: nodes_list,
           Ckappa_sig.PairAgentSite_map_and_set.Map.add
             parameters errors
             node
             (Graphs.node_of_int i)
             map
        ) (0, [], (errors, nodes_map)) nodes
    in
    let errors, edges =
      List.fold_left
        (fun (errors, l) (a,b) ->
           let errors, node_opt =
            Ckappa_sig.PairAgentSite_map_and_set.Map.find_option
              parameters errors
              a
              nodes_map
           in
           let errors, node_opt' =
            Ckappa_sig.PairAgentSite_map_and_set.Map.find_option
              parameters errors
              b
              nodes_map
           in
           match node_opt,node_opt' with
           | None, _ | _, None ->
             Exception.warn parameters errors __POS__ Exit l
           | Some a, Some b -> errors, (a, (), b) :: l)
        (errors, []) edges_list
    in
    (*build a graph_scc*)
    let graph =
      Graphs.create parameters errors
        (fun _ -> ())
        nodes
        edges
       in
       (*compute scc*)
       let errors, _low, _pre, _on_stack, scc =
         Graphs.compute_scc parameters errors
           (fun () -> "")
           graph
       in
       let scc =
         List.fold_left
           (fun l a -> if List.length a < 2 then l else a::l)
           [] (List.rev scc)
       in
       let scc =
         List.rev_map
           (fun a ->
              List.rev_map
                (fun b -> nodes_array.(Graphs.int_of_node b))
                (List.rev a))
           (List.rev scc )
       in
       errors, scc
