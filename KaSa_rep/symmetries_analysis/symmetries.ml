(**
   * symmetries.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 5th of December
   * Last modification: Time-stamp: <Dec 26 2016>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let declare_agent parameters error agent store_result =
Ckappa_sig.Agent_map_and_set.Map.add parameters error
  agent
  Ckappa_sig.Site_map_and_set.Map.empty
  store_result

(*-------------------------------------------------------------*)

let declare_site parameters error agent site store_result =
  let error, store_result_a =
    Ckappa_sig.Agent_map_and_set.Map.find_default
      parameters error
      Ckappa_sig.Site_map_and_set.Map.empty
      agent
      store_result
  in
  let error, store_result_a =
    Ckappa_sig.Site_map_and_set.Map.add parameters error
      site
      ([], [])
      store_result_a
  in
  Ckappa_sig.Agent_map_and_set.Map.overwrite parameters error
    agent
    store_result_a
    store_result

(*-------------------------------------------------------------*)

let add_link_in_contact_map parameters error (agent,site) (agent',site')
    store_result =
  let error, store_result_a =
    Ckappa_sig.Agent_map_and_set.Map.find_default
      parameters error
      Ckappa_sig.Site_map_and_set.Map.empty
      agent
      store_result
  in
  let error, (l, old) =
    Ckappa_sig.Site_map_and_set.Map.find_default
      parameters error
      ([], [])
      site
      store_result_a
  in
  let error, store_result'_a =
    Ckappa_sig.Site_map_and_set.Map.overwrite
      parameters error
      site
      (l, ((agent', site') :: old))
      store_result_a
  in
  Ckappa_sig.Agent_map_and_set.Map.overwrite
    parameters error
    agent
    store_result'_a
    store_result

(*----------------------------------------------------------------*)

let add_internal_state_in_contact_map parameters error (agent_type,site_name) state
    store_result =
  let error, store_result_a =
    Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs
      parameters error
      Ckappa_sig.Site_map_and_set.Map.empty
      agent_type
      store_result
  in
  let error, (old, l) =
    Ckappa_sig.Site_map_and_set.Map.find_default_without_logs parameters error
      ([],[])
      site_name
      store_result_a
  in
  let error, store_result'_a =
    Ckappa_sig.Site_map_and_set.Map.add_or_overwrite
      parameters error
      site_name
      (state::old, l) (*store state, l is a pair of (agent, site)*)
      store_result_a
  in
  Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite parameters error
    agent_type
    store_result'_a
    store_result

(*-------------------------------------------------------------*)

let init_contact_map = Ckappa_sig.Agent_map_and_set.Map.empty

let export_contact_map parameters error handler store_result =
  (*let store_result = init_contact_map in*)
  (*----------------------------------------------------------------*)
  let error, store_result =
    Ckappa_sig.Dictionary_of_agents.fold
      (fun _ _ agent_id (error,store_result) ->
         declare_agent parameters error agent_id store_result)
      handler.Cckappa_sig.agents_dic
      (error, store_result)
  in
  (*----------------------------------------------------------------*)
  let error, store_result =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.fold
      parameters error
      (fun parameters error agent_id site_dic store_result ->
         Ckappa_sig.Dictionary_of_sites.fold
           (fun _ _ site_id (error, store_result) ->
              declare_site parameters error agent_id site_id store_result)
           site_dic
           (error, store_result))
      handler.Cckappa_sig.sites
      store_result
  in
  (*----------------------------------------------------------------*)
  let error, store_result =
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.fold
      parameters error
      (fun parameters error (agent_type, site_name) state_dic store_result ->
         let error,site =
           Handler.translate_site parameters error handler agent_type site_name
         in
         match site with
         | Ckappa_sig.Binding _ -> error, store_result
         | Ckappa_sig.Internal _ ->
           Ckappa_sig.Dictionary_of_States.fold
             (fun _ ((),()) state (error,store_result) ->
                add_internal_state_in_contact_map parameters error
                  (agent_type, site_name)
                  state
                  store_result
             )
             state_dic
             (error,store_result)
      )
      handler.Cckappa_sig.states_dic
      store_result
  in
  (*----------------------------------------------------------------*)
  let error, store_result =
    Ckappa_sig.Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameters error
      (fun parameters error (agent, (site , _state))
        (agent', site', _state')store_result ->
        add_link_in_contact_map parameters error
          (agent, site)
          (agent',site')
          store_result)
      handler.Cckappa_sig.dual
      store_result
  in
  (*----------------------------------------------------------------*)
  let store_result =
    Ckappa_sig.Agent_map_and_set.Map.map
      (Ckappa_sig.Site_map_and_set.Map.map
         (fun (state_list, pair_agent_site_list) ->
            List.rev state_list, List.rev pair_agent_site_list
         )
      )
      store_result
  in
  error, store_result

(***************************************************************************)

type symmetries =
  {
    store_contact_map :
      (Ckappa_sig.Dictionary_of_States.key list *
       (Ckappa_sig.c_agent_name * Ckappa_sig.Site_map_and_set.Map.elt) list)
        Ckappa_sig.Site_map_and_set.Map.t Ckappa_sig.Agent_map_and_set.Map.t;
    store_partition_contact_map :
      Ckappa_sig.c_site_name list list
        Ckappa_sig.Agent_map_and_set.Map.t;
    store_partition_with_predicate :
      Ckappa_sig.c_site_name list list
        Ckappa_sig.Agent_map_and_set.Map.t;
  }

let init_symmetries =
  {
    store_contact_map = Ckappa_sig.Agent_map_and_set.Map.empty;
    store_partition_contact_map = Ckappa_sig.Agent_map_and_set.Map.empty;
    store_partition_with_predicate = Ckappa_sig.Agent_map_and_set.Map.empty
  }

(***************************************************************************)

let collect_partition_contact_map parameters error contact_map store_result =
  let error, store_result =
    Ckappa_sig.Agent_map_and_set.Map.fold
      (fun agent_type site_map (error, store_result) ->
         Ckappa_sig.Site_map_and_set.Map.fold
           (fun site_type (state_list, pair_list) (error, store_result) ->
              let error, old_list =
                match
                  Ckappa_sig.Agent_map_and_set.Map.find_option_without_logs
                    parameters
                    error
                    agent_type
                    store_result
                with
                | error, None -> error, []
                | error, Some l -> error, l
              in
              let error, site_list =
                List.fold_left (fun (error, current_list) (a, site_type) ->
                    error, site_type :: current_list
                  ) (error, []) pair_list
              in
              let site_list = site_type :: site_list in
              let new_site_list = site_list :: old_list in
              let error, store_result =
                Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite
                  parameters
                  error
                  agent_type
                  new_site_list
                  store_result
              in
              error, store_result
           ) site_map (error, store_result)
      ) contact_map (error, store_result)
  in
  error, store_result

(***************************************************************************)

let collect_partition_with_predicate parameters error
    partition_contact_map
    (predicate_ab: Ckappa_sig.c_site_name -> Ckappa_sig.c_site_name -> bool)
    store_result =
  let error, store_result =
    Ckappa_sig.Agent_map_and_set.Map.fold
      (fun agent_type list (error, store_result) ->
         let error, old_list =
           match
             Ckappa_sig.Agent_map_and_set.Map.find_option_without_logs
               parameters error
               agent_type
               store_result
           with
           | error, None -> error, []
           | error, Some l -> error, l
         in
         let error, new_partition_list =
           List.fold_left (fun (error, current_list) l ->
               let partition_list =
                 let rec aux acc =
                   match acc with
                   | [] | _ :: [] -> acc
                   | a :: b :: tl ->
                     if predicate_ab a b
                     then acc
                     else aux tl
                 in
                 aux l
               in
               let new_list = partition_list :: current_list in
               error, new_list
             ) (error, []) list
         in
         let error, store_result =
           Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite
             parameters
             error
             agent_type
             new_partition_list
             store_result
         in
         error, store_result
      ) partition_contact_map (error, store_result)
  in
  error, store_result



(***************************************************************************)

let print_partition_contact_map parameters error store_result =
  Ckappa_sig.Agent_map_and_set.Map.fold
    (fun agent_type l error ->
       List.fold_left (fun error l' ->
           List.fold_left (fun error site ->
               let () =
                 Loggers.fprintf (Remanent_parameters.get_logger parameters)
                   "agent_type:%i:site_type:%i\n"
                   (Ckappa_sig.int_of_agent_name agent_type)
                   (Ckappa_sig.int_of_site_name site)
               in
               error
             ) error l'
         ) error l
    ) store_result error

let print_contact_map parameters error contact_map =
  Ckappa_sig.Agent_map_and_set.Map.fold
    (fun agent site_map error ->
       Ckappa_sig.Site_map_and_set.Map.fold
         (fun site (state_list, pair_agent_site_list) error ->
            let error =
              List.fold_left (fun error (agent_name, site_name) ->
                  let () =
                    Loggers.fprintf (Remanent_parameters.get_logger parameters)
                      "agent_type:%i:site_type:%i\n"
                      (Ckappa_sig.int_of_agent_name agent_name)
                      (Ckappa_sig.int_of_site_name site_name)
                  in
                  error
                ) error (List.rev pair_agent_site_list)
            in
            let error =
              List.fold_left (fun error i ->
                  let () =
                    Loggers.fprintf (Remanent_parameters.get_logger parameters)
                      "agent_type:%i:site_type:%i:state:%i\n"
                      (Ckappa_sig.int_of_agent_name agent)
                      (Ckappa_sig.int_of_site_name site)
                      (Ckappa_sig.int_of_state_index i)
                  in
                  error
                ) error (List.rev state_list)
            in
            error
         ) site_map error
    ) contact_map error

(***************************************************************************)

let print_partition_with_predicate parameters error store_result =
  Ckappa_sig.Agent_map_and_set.Map.fold
    (fun agent_type l error ->
       List.fold_left (fun error l' ->
           List.fold_left (fun error site ->
               let () =
                 Loggers.fprintf (Remanent_parameters.get_logger parameters)
                   "agent_type:%i:site_type:%i\n"
                   (Ckappa_sig.int_of_agent_name agent_type)
                   (Ckappa_sig.int_of_site_name site)
               in
               error
             ) error l'
         ) error l
    ) store_result error

(***************************************************************************)

let detect_symmetries parameters error handler =
  let store_result = init_symmetries in
  (*-------------------------------------------------------------*)
  let error, store_contact_map =
    export_contact_map parameters error handler
      store_result.store_contact_map
  in
  (*-------------------------------------------------------------*)
  let error, store_partition_contact_map =
    collect_partition_contact_map
      parameters error
      store_contact_map
      store_result.store_partition_contact_map
  in
  (*-------------------------------------------------------------*)
  let error, store_partition_with_predicate =
    collect_partition_with_predicate
      parameters error
      store_partition_contact_map
      (=)
      store_result.store_partition_with_predicate
  in
  (*-------------------------------------------------------------*)
  let store_result =
    {
      store_contact_map = store_contact_map;
      store_partition_contact_map = store_partition_contact_map;
      store_partition_with_predicate = store_partition_with_predicate
    }
  in
  (*-------------------------------------------------------------*)
  let error =
    if Remanent_parameters.get_trace parameters
    then
      let error =
        print_partition_contact_map parameters error
          store_result.store_partition_contact_map
      in
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameters)
          "With predictate\n";
        print_partition_with_predicate
          parameters error
          store_result.store_partition_with_predicate
      in
      error
    else
      error
  in
  (*let error =
    print_contact_map parameters error
      store_result.store_contact_map
    in*)
  error, store_result
