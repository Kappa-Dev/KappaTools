(**
  * contact_map_scc.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2017, the 23rd of June
  * Last modification: Time-stamp: <Nov 05 2018>
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

let add_edges parameters error (agent_name, site_name, agent_name', site_name')
    site_name_list graph =
  let edge = (agent_name, site_name), (agent_name', site_name') in
  let error, old =
    Ckappa_sig.PairAgentSite_map_and_set.Map.find_default_without_logs
      parameters error [] edge graph
  in
  let internal_scc_decomposition =
    List.fold_left
      (fun old (site_name'', partners) ->
        List.fold_left
          (fun old (agent_name''', site_name''') ->
            let edge =
              (agent_name', site_name''), (agent_name''', site_name''')
            in
            edge :: old)
          old partners)
      old site_name_list
  in
  Ckappa_sig.PairAgentSite_map_and_set.Map.add_or_overwrite parameters error
    edge internal_scc_decomposition graph

let convert_contact_map parameters error contact_map =
  let graph = Ckappa_sig.PairAgentSite_map_and_set.Map.empty in
  Ckappa_sig.Agent_map_and_set.Map.fold
    (fun agent_name interface (error, graph) ->
      Ckappa_sig.Site_map_and_set.Map.fold
        (fun site_name (_, partners) (error, graph) ->
          List.fold_left
            (fun (error, graph) (agent_name', site_name') ->
              let error, pair_opt =
                Ckappa_sig.Agent_map_and_set.Map.find_option parameters error
                  agent_name' contact_map
              in
              match pair_opt with
              | None -> Exception.warn parameters error __POS__ Exit graph
              | Some interface' ->
                let error, others =
                  Ckappa_sig.Site_map_and_set.Map.fold
                    (fun site_name (_, partners) (error, others) ->
                      ( error,
                        if partners = [] || site_name = site_name' then
                          others
                        else
                          (site_name, partners) :: others ))
                    interface' (error, [])
                in
                add_edges parameters error
                  (agent_name, site_name, agent_name', site_name')
                  others graph)
            (error, graph) partners)
        interface (error, graph))
    contact_map (error, graph)

let mixture_of_edge parameters error handler
    (((ag, st), (ag', st')), ((ag'', st''), (ag''', st'''))) =
  let _ = ag, ag''', st, st''' in
  if ag' <> ag'' || st' = st'' then (
    let error, mixture = Preprocess.empty_mixture parameters error in
    Exception.warn parameters error __POS__ Exit mixture
  ) else (
    (* build the pattern:
       A(x!1), B(x!1, y!2), C(x!2)
       ag(st!1), ag'(st'!1, st''!2), ag'''(st'''!2)
    *)
    let cckappa_only = true in
    let error, mixture =
      Build_graph.init ~cckappa_only parameters error handler
    in
    let error, ag_id, mixture =
      Build_graph.add_agent parameters error ag mixture
    in
    let error, ag_id', mixture =
      Build_graph.add_agent parameters error ag' mixture
    in
    let error, ag_id'', mixture =
      Build_graph.add_agent parameters error ag''' mixture
    in
    let error, mixture =
      Build_graph.add_site parameters error ag_id st mixture
    in
    let error, mixture =
      Build_graph.add_site parameters error ag_id' st' mixture
    in
    let error, mixture =
      Build_graph.add_site parameters error ag_id' st'' mixture
    in
    let error, mixture =
      Build_graph.add_site parameters error ag_id'' st''' mixture
    in
    let error, mixture =
      Build_graph.add_link parameters error ag_id st ag_id' st' mixture
    in
    let error, mixture =
      Build_graph.add_link parameters error ag_id' st'' ag_id'' st''' mixture
    in
    error, Build_graph.export mixture
  )

let filter_edges_in_converted_contact_map parameters error handler static
    dynamic is_reachable converted_contact_map =
  (* Take care of propagating memoisation table, and error stream *)
  (*
  In the converted site-graph, edges are of the form:
  ((A,s),(A’,s’)),((A’,s’’),(A’’,s’’’)).

  Say that the edge is potential if and only if the pattern:
  A(s!1),A’(s’!1,s’’!2),A’’(s’’’!2)
  is reachable.

  Your goal is to filter out the edges in the list that are not potential,
 before computing the strongest connected components. *)
  let error, dynamic, converted_contact_map =
    Ckappa_sig.PairAgentSite_map_and_set.Map.fold
      (fun node1 potential_sites (error, dynamic, map) ->
        let error, dynamic, potential_sites' =
          let error, dynamic, potential_sites' =
            List.fold_left
              (fun (error, dynamic, potential_sites') node2 ->
                let (ag, st), (ag', st') = node1 in
                let (ag'', st''), (ag''', st''') = node2 in
                let pattern =
                  ((ag, st), (ag', st')), ((ag'', st''), (ag''', st'''))
                in
                let error, mixture =
                  mixture_of_edge parameters error handler pattern
                in
                let error, dynamic, bool =
                  is_reachable parameters error static dynamic mixture
                in
                let error, potential_sites' =
                  if bool then
                    error, node2 :: potential_sites'
                  else
                    error, potential_sites'
                in
                error, dynamic, potential_sites')
              (error, dynamic, []) potential_sites
          in
          error, dynamic, potential_sites'
        in
        if potential_sites' <> [] then (
          let error', map =
            Ckappa_sig.PairAgentSite_map_and_set.Map.add parameters error node1
              potential_sites' map
          in
          let error =
            Exception.check_point Exception.warn parameters error error' __POS__
              Exit
          in
          error, dynamic, map
        ) else
          error, dynamic, map)
      converted_contact_map
      (error, dynamic, Ckappa_sig.PairAgentSite_map_and_set.Map.empty)
  in
  error, dynamic, converted_contact_map

let keep_list parameters error self l =
  match l with
  | [] -> error, false
  | [ x ] ->
    (match Graphs.Nodearray.unsafe_get parameters error x self with
    | error, Some true -> error, true
    | error, (Some _ | None) -> error, false)
  | _ :: _ :: _ -> error, true

let compute_graph_scc parameters error contact_map_converted =
  let error, (nodes, edges_list) =
    Ckappa_sig.PairAgentSite_map_and_set.Map.fold
      (fun node1 potential_sites (error, (nodes, edges)) ->
        let error, nodes =
          Ckappa_sig.PairAgentSite_map_and_set.Set.add_when_not_in parameters
            error node1 nodes
        in
        let error, (nodes, edges) =
          List.fold_left
            (fun (error, (nodes, edges)) node2 ->
              let error, nodes =
                Ckappa_sig.PairAgentSite_map_and_set.Set.add_when_not_in
                  parameters error node2 nodes
              in
              error, (nodes, (node1, node2) :: edges))
            (error, (nodes, edges))
            potential_sites
        in
        error, (nodes, edges))
      contact_map_converted
      (error, (Ckappa_sig.PairAgentSite_map_and_set.Set.empty, []))
  in
  let nodes = Ckappa_sig.PairAgentSite_map_and_set.Set.elements nodes in
  let n_nodes = List.length nodes in
  let nodes_array =
    Array.make n_nodes
      ( (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_site_name),
        (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_site_name) )
  in
  let error, self = Graphs.Nodearray.create parameters error n_nodes in
  let nodes_map = Ckappa_sig.PairAgentSite_map_and_set.Map.empty in
  let _, nodes, (error, nodes_map) =
    List.fold_left
      (fun (i, nodes_list, (error, map)) node ->
        nodes_array.(i) <- node;
        ( i + 1,
          Graphs.node_of_int i :: nodes_list,
          Ckappa_sig.PairAgentSite_map_and_set.Map.add parameters error node
            (Graphs.node_of_int i) map ))
      (0, [], (error, nodes_map))
      nodes
  in
  let error, (edges, self) =
    List.fold_left
      (fun (error, (l, self)) (a, b) ->
        let error, node_opt =
          Ckappa_sig.PairAgentSite_map_and_set.Map.find_option parameters error
            a nodes_map
        in
        let error, node_opt' =
          Ckappa_sig.PairAgentSite_map_and_set.Map.find_option parameters error
            b nodes_map
        in
        match node_opt, node_opt' with
        | None, _ | _, None ->
          Exception.warn parameters error __POS__ Exit (l, self)
        | Some a, Some b ->
          let error, set =
            if a = b then
              Graphs.Nodearray.set parameters error a true self
            else
              error, self
          in
          error, ((a, (), b) :: l, set))
      (error, ([], self))
      edges_list
  in
  (*build a graph_scc*)
  let error, graph = Graphs.create parameters error (fun _ -> ()) nodes edges in
  (*compute scc*)
  let error, _low, _pre, _on_stack, scc =
    Graphs.compute_scc parameters error (fun () -> "") graph
  in
  let error, scc =
    List.fold_left
      (fun (error, l) a ->
        let error, bool = keep_list parameters error self a in
        ( error,
          if bool then
            a :: l
          else
            l ))
      (error, []) (List.rev scc)
  in
  let scc =
    List.rev_map
      (fun a ->
        List.rev_map (fun b -> nodes_array.(Graphs.int_of_node b)) (List.rev a))
      (List.rev scc)
  in
  error, scc
