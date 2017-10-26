(**
  * contact_map_scc.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2017, the 23rd of June
  * Last modification: Time-stamp: <Oct 26 2017>
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

let add_edges
    parameters error
    (agent_name,site_name,agent_name',site_name') (agent_name',site_name_list') graph =
  let error,old =
    Ckappa_sig.PairAgentSite_map_and_set.Map.find_default_without_logs
      parameters
      error
      []
      ((agent_name, site_name),(agent_name',site_name'))
      graph
  in
  Ckappa_sig.PairAgentSite_map_and_set.Map.add_or_overwrite
    parameters error
    ((agent_name, site_name),(agent_name',site_name'))
    (List.fold_left
       (fun old (site_name'',partners) ->
          List.fold_left
            (fun old (agent_name''',site_name''') ->
              ((agent_name',site_name''),(agent_name''',site_name'''))::old)
            old
            partners)
       old site_name_list')
    graph


let contact_map_converted parameters error handler
    (contact_map:Remanent_state.internal_contact_map) =
  let graph = Ckappa_sig.PairAgentSite_map_and_set.Map.empty in
    Ckappa_sig.Agent_map_and_set.Map.fold
    (fun agent_name interface (error,graph) ->
         Ckappa_sig.Site_map_and_set.Map.fold
           (fun  site_name (_,partners) (error,graph) ->
            List.fold_left
              (fun (error,graph) (agent_name',site_name') ->
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
                       (fun site_name (_,partners) (error,others) ->
                          error, if partners = [] || site_name=site_name'
                          then
                            others
                          else
                            (site_name,partners)::others)
                          interface'
                          (error,[])
                   in
                   add_edges
                       parameters error
                       (agent_name,site_name,agent_name',site_name')
                       (agent_name',others)
                       graph
              )
              (error,graph) partners)
           interface (error,graph))
    contact_map (error,graph)

let compute_graph_scc parameters errors kappa_handler contact_map_converted =
    let nodes, edges_list =
        Ckappa_sig.PairAgentSite_map_and_set.Map.fold
          (fun node1 potential_sites (nodes, edges) ->
             let nodes  =
               node1::nodes
             in
             let edges =
               List.fold_left
                 (fun edges node2 ->
                    (node1,node2)::edges)
                  edges potential_sites
             in
             nodes,edges)
          contact_map_converted
          ([], [])
    in
    let n_nodes = List.length nodes in
    let nodes_array =
      Array.make
        n_nodes ((Ckappa_sig.dummy_agent_name,Ckappa_sig.dummy_site_name),(Ckappa_sig.dummy_agent_name,Ckappa_sig.dummy_site_name))
    in
    let nodes_map = Ckappa_sig.PairAgentSite_map_and_set.Map.empty in
    let _, nodes, (errors, nodes_map) =
      List.fold_left
        (fun (i,nodes_list,(errors,map)) node ->
           nodes_array.(i)<-node ;
           i+1,
           (Graphs.node_of_int i)::nodes_list,
           Ckappa_sig.PairAgentSite_map_and_set.Map.add
             parameters errors
             node
             (Graphs.node_of_int i)
             map
        )
        (0,[],(errors,nodes_map))
           nodes
    in
    let errors,edges =
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
           | None,_ | _,None ->
             Exception.warn parameters errors __POS__ Exit l
           | Some a,Some b -> errors,(a,(),b)::l)
        (errors,[]) edges_list
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
