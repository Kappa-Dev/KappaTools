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
     There exists z such that there is a bond between (A,z) and
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

module Node_id =
struct
  type t = int * int
  let compare = compare
  let print _ _ = ()
end

module Dictionary_of_agent_site =
  (
    Dictionary.Dictionary_of_Ord (Node_id) : Dictionary.Dictionary
    with type key = int
     and type value = int * int (*agent * site*)
  )

type agent_site_dic =
  (unit, unit) Dictionary_of_agent_site.dictionary


(*
For each one, you have an agent and a site, and you have to
allocate this pair in the dictionary.
You will get an identifier.

Later you can use the dictionary to get (agent, site) from the id,
and conversely, the pair from the id.
*)

(*type t = ((int list) * (int*int) list) array array*)

let convert_contact_map_to_graph parameters error string_contact_map =
  Mods.StringSetMap.Map.iter (fun ag map ->
      Mods.StringSetMap.Map.iter (fun site (state_list, pair_list) ->
          List.iter (fun (ag2, site2) ->
              Loggers.fprintf (Remanent_parameters.get_logger parameters)
                "agent:%s:site:%s - agent:%s:site:%s\n"
                ag site
                ag2 site2
            ) pair_list
        ) map
    ) string_contact_map


let convert_int_to_nodes parameters error handler contact_map  =
  let agents_sites_dic = handler.Cckappa_sig.agents_sites_dic in
  let error, store_node_list, _ =
  Array.fold_left
      (fun (error, store_node_list, edges_list) pair_array ->
         Array.fold_left (fun (error, store_node_list, edges_list)
             (state_list, pair_list)  ->
             List.fold_left
               (fun (error, store_node_list, edges_list) (agent, site) ->
                  let error, (bool, output) =
                    Ckappa_sig.Dictionary_of_agent_site.allocate_bool
                      parameters
                      error
                      Ckappa_sig.compare_unit_agent_site
                      (agent, site)
                      ()
                      Misc_sa.const_unit
                      agents_sites_dic
                  in
                  let error, id =
                    match bool, output with
                    | _, None | true, _ ->
                      Exception.warn parameters error __POS__ Exit 0
                    | _, Some (k, _, _, _) -> error, k
                  in
                  let node_id = Graphs.node_of_int id in
                  let node_list = node_id :: store_node_list in
                  let _ =
                    Loggers.fprintf (Remanent_parameters.get_logger parameters)
                      "agent:%i:site:%i:node_id:%i\n" agent site
                      (Graphs.int_of_node node_id)
                  in
                  error, node_list, edges_list)
               (error, store_node_list, edges_list) pair_list)
           (error, store_node_list, edges_list) pair_array)
      (error, [], []) contact_map
  in
  error, store_node_list


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
