(**
    * ode_fragmentation.ml
    * openkappa
    * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 2015, the 9th of Apirl
    * Last modification: Time-stamp: <Jul 31 2017>
    * *
    * ODE fragmentation
    *
    *
    * Copyright 2010,2011 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    *  under the terms of the GNU Library General Public License *)

let trace = false

(************************************************************************************)
(*collect modified set*)

let collect_sites_modified_set parameters error rule handler_kappa store_result
    =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
      error
      (fun parameters error _agent_id site_modif store_result ->
        if
          Ckappa_sig.Site_map_and_set.Map.is_empty
            site_modif.Cckappa_sig.agent_interface
        then
          error, store_result
        else (
          let agent_type = site_modif.Cckappa_sig.agent_name in
          (*----------------------------------------------------------------------*)
          (*collect a set of site that modified*)
          let error, site_set =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site _ (error, current_set) ->
                let error, set =
                  Ckappa_sig.Site_map_and_set.Set.add parameters error site
                    current_set
                in
                error, set)
              site_modif.Cckappa_sig.agent_interface
              (error, Ckappa_sig.Site_map_and_set.Set.empty)
          in
          (*----------------------------------------------------------------------*)
          (*PRINT at each rule*)
          let error =
            if
              Remanent_parameters.get_do_ODE_flow_of_information parameters
              && Remanent_parameters.get_trace parameters
            then (
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "Flow of information in the ODE semantics:modified sites:"
              in
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              let error, agent_string =
                try
                  Handler.string_of_agent parameters error handler_kappa
                    agent_type
                with _ ->
                  Exception.warn parameters error __POS__ Exit
                    (Ckappa_sig.string_of_agent_name agent_type)
              in
              let _ =
                Printf.fprintf stdout "\tagent_type:%s:%s\n"
                  (Ckappa_sig.string_of_agent_name agent_type)
                  agent_string
              in
              Ckappa_sig.Site_map_and_set.Set.fold
                (fun site_type error ->
                  let error, site_string =
                    try
                      Handler.string_of_site parameters error handler_kappa
                        agent_type site_type
                    with _ ->
                      Exception.warn parameters error __POS__ Exit
                        (Ckappa_sig.string_of_site_name site_type)
                  in
                  let () =
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters)
                      "\t\tsite_type:%s:%s"
                      (Ckappa_sig.string_of_site_name site_type)
                      site_string
                  in
                  let () =
                    Loggers.print_newline
                      (Remanent_parameters.get_logger parameters)
                  in
                  error)
                site_set error
            ) else
              error
          in
          (*----------------------------------------------------------------------*)
          (*get old?*)
          let error, old_set =
            match
              Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
              .unsafe_get parameters error agent_type store_result
            with
            | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
            | error, Some s -> error, s
          in
          (*new*)
          let error, new_set =
            Ckappa_sig.Site_map_and_set.Set.union parameters error site_set
              old_set
          in
          (*----------------------------------------------------------------------*)
          (*store*)
          let error, store_result =
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
              parameters error agent_type new_set store_result
          in
          error, store_result
        ))
      rule.Cckappa_sig.diff_reverse store_result
  in
  error, store_result

(************************************************************************************)
(*collect sites that are released*)

let collect_sites_bond_pair_set parameters error handler_kappa rule store_result
    =
  let bond_lhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds in
  List.fold_left
    (fun (error, store_result) (site_add1, site_add2) ->
      let store_result1, store_result2 = store_result in
      let agent_id1 = site_add1.Cckappa_sig.agent_index in
      let agent_type1 = site_add1.Cckappa_sig.agent_type in
      (*get site_address_map from bond_lhs*)
      let error, site_add_map1 =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
            parameters error agent_id1 bond_lhs
        with
        | error, None -> error, Ckappa_sig.Site_map_and_set.Map.empty
        | error, Some map -> error, map
      in
      (*get a set of sites that are bond*)
      let error, sites_bond_set1 =
        Ckappa_sig.Site_map_and_set.Map.fold
          (fun site _ (error, current_set) ->
            let error, set =
              Ckappa_sig.Site_map_and_set.Set.add parameters error site
                current_set
            in
            error, set)
          site_add_map1
          (error, Ckappa_sig.Site_map_and_set.Set.empty)
      in
      (*----------------------------------------------------------------------*)
      (*PRINT*)
      let error =
        if
          Remanent_parameters.get_do_ODE_flow_of_information parameters
          && Remanent_parameters.get_trace parameters
        then (
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "Flow of information in the ODE semantics:bond sites (first \
               agent):"
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          let error, agent_string1 =
            try
              Handler.string_of_agent parameters error handler_kappa agent_type1
            with _ ->
              Exception.warn parameters error __POS__ Exit
                (Ckappa_sig.string_of_agent_name agent_type1)
          in
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "\tagent_type:%s:%s"
              (Ckappa_sig.string_of_agent_name agent_type1)
              agent_string1
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          Ckappa_sig.Site_map_and_set.Set.fold
            (fun site_type error ->
              let error, site_string =
                try
                  Handler.string_of_site parameters error handler_kappa
                    agent_type1 site_type
                with _ ->
                  Exception.warn parameters error __POS__ Exit
                    (Ckappa_sig.string_of_site_name site_type)
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "\t\tsite_type:%s:%s"
                  (Ckappa_sig.string_of_site_name site_type)
                  site_string
              in
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              error)
            sites_bond_set1 error
        ) else
          error
      in
      (*----------------------------------------------------------------------*)
      (*compute first pair*)
      let error, store_result1 =
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
          parameters error agent_type1 sites_bond_set1 store_result1
      in
      (*----------------------------------------------------------------------*)
      (*compute second pair*)
      let agent_id2 = site_add2.Cckappa_sig.agent_index in
      let agent_type2 = site_add2.Cckappa_sig.agent_type in
      let error, site_add_map2 =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
            parameters error agent_id2 bond_lhs
        with
        | error, None -> error, Ckappa_sig.Site_map_and_set.Map.empty
        | error, Some map -> error, map
      in
      (*get a set of sites that are bond*)
      let error, sites_bond_set2 =
        Ckappa_sig.Site_map_and_set.Map.fold
          (fun site _ (error, current_set) ->
            let error, set =
              Ckappa_sig.Site_map_and_set.Set.add parameters error site
                current_set
            in
            error, set)
          site_add_map2
          (error, Ckappa_sig.Site_map_and_set.Set.empty)
      in
      (*----------------------------------------------------------------------*)
      (*PRINT*)
      let error =
        if
          Remanent_parameters.get_do_ODE_flow_of_information parameters
          && Remanent_parameters.get_trace parameters
        then (
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "Flow of information in the ODE semantics:bond sites (second \
               agent):"
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          let error, agent_string2 =
            try
              Handler.string_of_agent parameters error handler_kappa agent_type2
            with _ ->
              Exception.warn parameters error __POS__ Exit
                (Ckappa_sig.string_of_agent_name agent_type2)
          in
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "\tagent_type:%s:%s"
              (Ckappa_sig.string_of_agent_name agent_type2)
              agent_string2
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          Ckappa_sig.Site_map_and_set.Set.fold
            (fun site_type error ->
              let error, site_string =
                try
                  Handler.string_of_site parameters error handler_kappa
                    agent_type2 site_type
                with _ ->
                  Exception.warn parameters error __POS__ Exit
                    (Ckappa_sig.string_of_site_name site_type)
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "\t\tsite_type:%s:%s"
                  (Ckappa_sig.string_of_site_name site_type)
                  site_string
              in
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              error)
            sites_bond_set2 error
        ) else
          error
      in
      (*----------------------------------------------------------------------*)
      (*get old*)
      let error, old_set2 =
        match
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
          .unsafe_get parameters error agent_type2 store_result2
        with
        | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
        | error, Some s -> error, s
      in
      (*new set*)
      let error, new_set2 =
        Ckappa_sig.Site_map_and_set.Set.union parameters error sites_bond_set2
          old_set2
      in
      (*store*)
      let error, store_result2 =
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
          parameters error agent_type2 new_set2 store_result2
      in
      (*----------------------------------------------------------------------*)
      (*result*)
      (*return a pair, first pair is a first binding agent of each rule. Second
        pair is a second binding agent, it is a result of anchor*)
      let error, store_result = error, (store_result1, store_result2) in
      error, store_result)
    (error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.release

(************************************************************************************)
(*collect sites that are external*)

let collect_sites_bond_pair_set_external parameters error rule store_result =
  let bond_lhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.bonds in
  List.fold_left
    (fun (error, store_result) (site_add1, site_add2) ->
      let store_result1, store_result2 = store_result in
      let agent_id1 = site_add1.Cckappa_sig.agent_index in
      let agent_type1 = site_add1.Cckappa_sig.agent_type in
      let error, site_add_map1 =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
            parameters error agent_id1 bond_lhs
        with
        | error, None -> error, Ckappa_sig.Site_map_and_set.Map.empty
        | error, Some map -> error, map
      in
      let error, sites_bond_set1 =
        Ckappa_sig.Site_map_and_set.Map.fold
          (fun site _ (error, current_set) ->
            let error, set =
              Ckappa_sig.Site_map_and_set.Set.add parameters error site
                current_set
            in
            error, set)
          site_add_map1
          (error, Ckappa_sig.Site_map_and_set.Set.empty)
      in
      (*store*)
      let error, store_result1 =
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
          parameters error agent_type1 sites_bond_set1 store_result1
      in
      (*----------------------------------------------------------------------*)
      (*compute second pair*)
      let agent_id2 = site_add2.Cckappa_sig.agent_index in
      let agent_type2 = site_add2.Cckappa_sig.agent_type in
      let error, site_add_map2 =
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
            parameters error agent_id2 bond_lhs
        with
        | error, None -> error, Ckappa_sig.Site_map_and_set.Map.empty
        | error, Some map -> error, map
      in
      let error, sites_bond_set2 =
        Ckappa_sig.Site_map_and_set.Map.fold
          (fun site _ (error, current_set) ->
            let error, set =
              Ckappa_sig.Site_map_and_set.Set.add parameters error site
                current_set
            in
            error, set)
          site_add_map2
          (error, Ckappa_sig.Site_map_and_set.Set.empty)
      in
      let error, store_result2 =
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
          parameters error agent_type2 sites_bond_set2 store_result2
      in
      (*----------------------------------------------------------------------*)
      let error, store_result = error, (store_result1, store_result2) in
      error, store_result)
    (error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.release

(************************************************************************************)
(*collect sites from lhs rule*)

let collect_sites_lhs parameters error rule store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
      error
      (fun parameters error _agent_id agent store_result ->
        match agent with
        | Cckappa_sig.Ghost | Cckappa_sig.Unknown_agent _ -> error, store_result
        | Cckappa_sig.Dead_agent (agent, _, _, _) | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, site_list =
            Ckappa_sig.Site_map_and_set.Map.fold
              (fun site _ (error, current_list) ->
                let site_list = site :: current_list in
                error, site_list)
              agent.Cckappa_sig.agent_interface (error, [])
          in
          (*get old?*)
          let error, old_list =
            match
              Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
              .unsafe_get parameters error agent_type store_result
            with
            | error, None -> error, []
            | error, Some l -> error, l
          in
          let new_list = List.concat [ site_list; old_list ] in
          (*store*)
          let error, store_result =
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
              parameters error agent_type new_list store_result
          in
          error, store_result)
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views store_result
  in
  error, store_result

(************************************************************************************)
(*collect sites anchor set*)

let collect_sites_anchor_set parameters error handler_kappa rule
    store_sites_modified_set store_sites_bond_pair_set store_sites_lhs
    store_result =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error _agent_id agent store_result ->
      let store_result1, store_result2 = store_result in
      match agent with
      | Cckappa_sig.Ghost | Cckappa_sig.Unknown_agent _ -> error, store_result
      | Cckappa_sig.Dead_agent (agent, _, _, _) | Cckappa_sig.Agent agent ->
        let agent_type = agent.Cckappa_sig.agent_name in
        (*----------------------------------------------------------------------*)
        (*get sites that is modified*)
        let error, modified_set =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type store_sites_modified_set
          with
          | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        (*----------------------------------------------------------------------*)
        (*get a set of sites in the lhs that are bond*)
        let error, site_lhs_bond_fst_set =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type
              (fst store_sites_bond_pair_set)
          with
          | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        (*----------------------------------------------------------------------*)
        (*get a list of sites in the lsh*)
        let error, sites_lhs_list =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type store_sites_lhs
          with
          | error, None -> error, []
          | error, Some l -> error, l
        in
        (*----------------------------------------------------------------------*)
        (*first case: a site connected to a modified site*)
        let error, store_result1 =
          List.fold_left
            (fun (error, store_result) x ->
              if
                Ckappa_sig.Site_map_and_set.Set.mem x modified_set
                && Ckappa_sig.Site_map_and_set.Set.mem x site_lhs_bond_fst_set
              then (
                let store_result = snd store_sites_bond_pair_set in
                error, store_result
              ) else
                error, store_result)
            (error, store_result1) (List.rev sites_lhs_list)
        in
        (*----------------------------------------------------------------------*)
        (*second result*)
        (*get a set of anchor sites*)
        let error, anchor_set1 =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type
              (fst store_sites_bond_pair_set)
          with
          | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, anchor_set2 =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type
              (snd store_sites_bond_pair_set)
          with
          | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, anchor_set =
          Ckappa_sig.Site_map_and_set.Set.union parameters error anchor_set1
            anchor_set2
        in
        (*----------------------------------------------------------------------*)
        let error, store_result2 =
          List.fold_left
            (fun (error, store_result) x ->
              List.fold_left
                (fun (error, store_result) y ->
                  if
                    Ckappa_sig.Site_map_and_set.Set.mem x anchor_set
                    || Ckappa_sig.Site_map_and_set.Set.mem x modified_set
                       && Ckappa_sig.Site_map_and_set.Set.mem y
                            site_lhs_bond_fst_set
                  then (
                    let store_result = snd store_sites_bond_pair_set in
                    error, store_result
                  ) else
                    error, store_result)
                (error, store_result) (List.tl sites_lhs_list))
            (error, store_result2) (List.rev sites_lhs_list)
        in
        (*----------------------------------------------------------------------*)
        (*get union both result*)
        let error, get_set1 =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type store_result1
          with
          | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, get_set2 =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type store_result2
          with
          | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, union_set =
          Ckappa_sig.Site_map_and_set.Set.union parameters error get_set1
            get_set2
        in
        (*----------------------------------------------------------------------*)
        (*PRINT*)
        let error =
          if
            Remanent_parameters.get_do_ODE_flow_of_information parameters
            && Remanent_parameters.get_trace parameters
          then (
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "Flow of information in the ODE semantics:anchor sites:"
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            let error, agent_string =
              try
                Handler.string_of_agent parameters error handler_kappa
                  agent_type
              with _ ->
                Exception.warn parameters error __POS__ Exit
                  (Ckappa_sig.string_of_agent_name agent_type)
            in
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "\tagent_type:%s:%s"
                (Ckappa_sig.string_of_agent_name agent_type)
                agent_string
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            Ckappa_sig.Site_map_and_set.Set.fold
              (fun site_type error ->
                let error, site_string =
                  try
                    Handler.string_of_site parameters error handler_kappa
                      agent_type site_type
                  with _ ->
                    Exception.warn parameters error __POS__ Exit
                      (Ckappa_sig.string_of_site_name site_type)
                in
                let () =
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameters)
                    "\t\tsite_type:%s:%s"
                    (Ckappa_sig.string_of_site_name site_type)
                    site_string
                in
                let () =
                  Loggers.print_newline
                    (Remanent_parameters.get_logger parameters)
                in
                error)
              union_set error
          ) else
            error
        in
        (*----------------------------------------------------------------------*)
        (*result*)
        let error, store_result = error, (store_result1, store_result2) in
        error, store_result)
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.views store_result

(************************************************************************************)
(*collect internal flow*)

let cartesian_prod_eq i a b =
  let rec loop a acc =
    match a with
    | [] -> List.rev acc
    | x :: xs ->
      loop xs
        (List.rev_append
           (List.rev
              (List.fold_left
                 (fun acc y ->
                   if x <> y then
                     (i, x, y) :: acc
                   else
                     acc)
                 [] b))
           acc)
  in
  loop a []

let collect_internal_flow parameters error handler_kappa rule store_sites_lhs
    store_sites_modified_set store_sites_anchor_set store_result =
  (*----------------------------------------------------------------------*)
  let add_link error agent_type (site_list, set) store_result =
    let result =
      Ode_fragmentation_type.Internal_flow_map.Map.add agent_type
        (site_list, set) store_result
    in
    error, result
  in
  (*----------------------------------------------------------------------*)
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error _agent_id agent store_result ->
      let store_result1, store_result2 = store_result in
      match agent with
      | Cckappa_sig.Ghost | Cckappa_sig.Unknown_agent _ -> error, store_result
      | Cckappa_sig.Dead_agent (agent, _, _, _) | Cckappa_sig.Agent agent ->
        let agent_type = agent.Cckappa_sig.agent_name in
        (*let agent_type_modif = agent_modif.agent_name in*)
        (*get modified set*)
        let error, modified_set =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type store_sites_modified_set
          with
          | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        (*get anchor set*)
        let error, anchor_set1 =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type
              (fst store_sites_anchor_set)
          with
          | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, anchor_set2 =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type
              (snd store_sites_anchor_set)
          with
          | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, anchor_set =
          Ckappa_sig.Site_map_and_set.Set.union parameters error anchor_set1
            anchor_set2
        in
        (*----------------------------------------------------------------------*)
        (*first result: site -> modified site*)
        let error, site_list =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type store_sites_lhs
          with
          | error, None -> error, []
          | error, Some l -> error, l
        in
        (*------------------------------------------------------------------------------*)
        (*PRINT*)
        let error =
          if
            Remanent_parameters.get_do_ODE_flow_of_information parameters
            && Remanent_parameters.get_trace parameters
          then (
            let _ =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "Flow of information in the ODE semantics:internal flow (first \
                 case):\n"
            in
            let modified_list =
              Ckappa_sig.Site_map_and_set.Set.elements modified_set
            in
            let cartesian_output =
              cartesian_prod_eq agent_type site_list modified_list
            in
            let error =
              List.fold_left
                (fun error (agent_type, site_type, site_modif) ->
                  let error, agent_string =
                    try
                      Handler.string_of_agent parameters error handler_kappa
                        agent_type
                    with _ ->
                      Exception.warn parameters error __POS__ Exit
                        (Ckappa_sig.string_of_agent_name agent_type)
                  in
                  let error, site_string =
                    try
                      Handler.string_of_site parameters error handler_kappa
                        agent_type site_type
                    with _ ->
                      Exception.warn parameters error __POS__ Exit
                        (Ckappa_sig.string_of_site_name site_type)
                  in
                  let error, site_modif_string =
                    Handler.string_of_site parameters error handler_kappa
                      agent_type site_modif
                  in
                  let () =
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters)
                      "Flow of information in the ODE semantics:Internal flow\n\
                       -agent_type:%s:%s:site_type:%s:%s -> \
                       agent_type:%s:%s:site_type_modified:%s:%s"
                      (Ckappa_sig.string_of_agent_name agent_type)
                      agent_string
                      (Ckappa_sig.string_of_site_name site_type)
                      site_string
                      (Ckappa_sig.string_of_agent_name agent_type)
                      agent_string
                      (Ckappa_sig.string_of_site_name site_modif)
                      site_modif_string
                  in
                  let () =
                    Loggers.print_newline
                      (Remanent_parameters.get_logger parameters)
                  in
                  error)
                error cartesian_output
            in
            error
          ) else
            error
        in
        (*------------------------------------------------------------------------------*)
        (*PRINT second internal flow*)
        let error =
          if
            Remanent_parameters.get_do_ODE_flow_of_information parameters
            && Remanent_parameters.get_trace parameters
          then (
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "Flow of information in the ODE semantics:internal flow \
                 (second case):"
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            let anchor_list =
              Ckappa_sig.Site_map_and_set.Set.elements anchor_set
            in
            let cartesian_output =
              cartesian_prod_eq agent_type site_list anchor_list
            in
            let error =
              List.fold_left
                (fun error (agent_type, site_type, site_anchor) ->
                  let error, agent_string =
                    try
                      Handler.string_of_agent parameters error handler_kappa
                        agent_type
                    with _ ->
                      Exception.warn parameters error __POS__ Exit
                        (Ckappa_sig.string_of_agent_name agent_type)
                  in
                  let error, site_string =
                    try
                      Handler.string_of_site parameters error handler_kappa
                        agent_type site_type
                    with _ ->
                      Exception.warn parameters error __POS__ Exit
                        (Ckappa_sig.string_of_site_name site_type)
                  in
                  let error, site_anchor_string =
                    try
                      Handler.string_of_site parameters error handler_kappa
                        agent_type site_anchor
                    with _ ->
                      Exception.warn parameters error __POS__ Exit
                        (Ckappa_sig.string_of_site_name site_anchor)
                  in
                  let () =
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters)
                      "Flow of information in the ODE semantics:Internal flow\n\
                       -agent_type:%s:%s:site_type:%s:%s -> \
                       agent_type:%s:%s:site_type_anchor:%s:%s"
                      (Ckappa_sig.string_of_agent_name agent_type)
                      agent_string
                      (Ckappa_sig.string_of_site_name site_type)
                      site_string
                      (Ckappa_sig.string_of_agent_name agent_type)
                      agent_string
                      (Ckappa_sig.string_of_site_name site_anchor)
                      site_anchor_string
                  in
                  let () =
                    Loggers.print_newline
                      (Remanent_parameters.get_logger parameters)
                  in
                  error)
                error cartesian_output
            in
            error
          ) else
            error
        in
        (*------------------------------------------------------------------------------*)
        let error, store_result1 =
          add_link error agent_type (site_list, modified_set) store_result1
        in
        (*------------------------------------------------------------------------------*)
        let error, store_result2 =
          add_link error agent_type (site_list, anchor_set) store_result2
        in
        (*------------------------------------------------------------------------------*)
        let error, store_result = error, (store_result1, store_result2) in
        error, store_result)
    rule.Cckappa_sig.rule_lhs.Cckappa_sig.views store_result

(************************************************************************************)
(*collect external flow*)

let cartesian_prod_external i anchor_set i' bond_fst_list bond_snd_set =
  let anchor_list = Ckappa_sig.Site_map_and_set.Set.elements anchor_set in
  let rec loop anchor_list acc =
    match anchor_list with
    | [] -> List.rev acc
    | x :: xs ->
      loop xs
        (List.rev_append
           (List.rev
              (List.fold_left
                 (fun acc y ->
                   if
                     Ckappa_sig.Site_map_and_set.Set.mem x anchor_set
                     && Ckappa_sig.Site_map_and_set.Set.mem x bond_snd_set
                   then
                     (i, x, i', y) :: acc
                   else
                     acc)
                 [] bond_fst_list))
           acc)
  in
  loop anchor_list []

let collect_external_flow parameters error handler_kappa rule
    store_sites_bond_pair_set_external store_sites_anchor_set store_result =
  (*------------------------------------------------------------------------------*)
  let add_link error (agent_type1, agent_type2)
      (anchor_set, bond_fst_set, bond_snd_set) store_result =
    let result =
      Ode_fragmentation_type.External_flow_map.Map.add (agent_type1, agent_type2)
        (anchor_set, bond_fst_set, bond_snd_set)
        store_result
    in
    error, result
  in
  (*------------------------------------------------------------------------------*)
  List.fold_left
    (fun (error, store_result) (site_add1, site_add2) ->
      let agent_type1 = site_add1.Cckappa_sig.agent_type in
      let agent_type2 = site_add2.Cckappa_sig.agent_type in
      (*------------------------------------------------------------------------------*)
      (*get sites that are bond on the lhs*)
      let error, bond_fst_set =
        match
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
          .unsafe_get parameters error agent_type1
            (fst store_sites_bond_pair_set_external)
        with
        | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
        | error, Some s -> error, s
      in
      (*------------------------------------------------------------------------------*)
      let error, bond_snd_set =
        match
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
          .unsafe_get parameters error agent_type2
            (snd store_sites_bond_pair_set_external)
        with
        | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
        | error, Some s -> error, s
      in
      (*------------------------------------------------------------------------------*)
      (*get anchor set*)
      let error, anchor_set1 =
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold
          parameters error
          (fun parameters error _agent_type site_set old_set ->
            let error, set =
              Ckappa_sig.Site_map_and_set.Set.union parameters error site_set
                old_set
            in
            error, set)
          (fst store_sites_anchor_set)
          Ckappa_sig.Site_map_and_set.Set.empty
      in
      let error, anchor_set2 =
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold
          parameters error
          (fun parameters error _agent_type site_set old_set ->
            let error, set =
              Ckappa_sig.Site_map_and_set.Set.union parameters error site_set
                old_set
            in
            error, set)
          (snd store_sites_anchor_set)
          Ckappa_sig.Site_map_and_set.Set.empty
      in
      let error, anchor_set =
        Ckappa_sig.Site_map_and_set.Set.union parameters error anchor_set1
          anchor_set2
      in
      (*------------------------------------------------------------------------------*)
      (*PRINT External flow*)
      let bond_fst_list =
        Ckappa_sig.Site_map_and_set.Set.elements bond_fst_set
      in
      let cartesian_output =
        cartesian_prod_external agent_type2 anchor_set agent_type1 bond_fst_list
          bond_snd_set
      in
      let error =
        if
          Remanent_parameters.get_do_ODE_flow_of_information parameters
          && Remanent_parameters.get_trace parameters
        then (
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "Flow of information in the ODE semantics:external flow:"
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          List.fold_left
            (fun error (agent_type, anchor_site_type, agent_type', site_modif) ->
              let error, agent_string =
                try
                  Handler.string_of_agent parameters error handler_kappa
                    agent_type
                with _ ->
                  Exception.warn parameters error __POS__ Exit
                    (Ckappa_sig.string_of_agent_name agent_type)
              in
              let error, agent_string' =
                try
                  Handler.string_of_agent parameters error handler_kappa
                    agent_type'
                with _ ->
                  Exception.warn parameters error __POS__ Exit
                    (Ckappa_sig.string_of_agent_name agent_type')
              in
              let error, anchor_site_type_string =
                try
                  Handler.string_of_site parameters error handler_kappa
                    agent_type anchor_site_type
                with _ ->
                  Exception.warn parameters error __POS__ Exit
                    (Ckappa_sig.string_of_site_name anchor_site_type)
              in
              let error, site_modif_string =
                try
                  Handler.string_of_site parameters error handler_kappa
                    agent_type' site_modif
                with _ ->
                  Exception.warn parameters error __POS__ Exit
                    (Ckappa_sig.string_of_site_name site_modif)
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "Flow of information in the ODE semantics:External flow:\n\
                   -agent-type:%s:%s:site_type_anchor:%s:%s -> \
                   agent_type:%s:%s:site_type_modified:%s:%s"
                  (Ckappa_sig.string_of_agent_name agent_type)
                  agent_string
                  (Ckappa_sig.string_of_site_name anchor_site_type)
                  anchor_site_type_string
                  (Ckappa_sig.string_of_agent_name agent_type')
                  agent_string'
                  (Ckappa_sig.string_of_site_name site_modif)
                  site_modif_string
              in
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              error)
            error cartesian_output
        ) else
          error
      in
      (*------------------------------------------------------------------------------*)
      let error, store_result =
        add_link error (agent_type1, agent_type2)
          (anchor_set, bond_fst_set, bond_snd_set)
          store_result
      in
      error, store_result)
    (error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.release

(************************************************************************************)
(*RULE*)

let scan_rule parameters error handler_kappa rule store_result =
  (*----------------------------------------------------------------------*)
  (*modified set*)
  let error, store_sites_modified_set =
    collect_sites_modified_set parameters error rule handler_kappa
      store_result.Ode_fragmentation_type.store_sites_modified_set
  in
  (*----------------------------------------------------------------------*)
  (*collect sites that are release*)
  let error, store_sites_bond_pair_set =
    collect_sites_bond_pair_set parameters error handler_kappa rule
      store_result.Ode_fragmentation_type.store_sites_bond_pair_set
  in
  (*----------------------------------------------------------------------*)
  (*collects sites that are external*)
  let error, store_sites_bond_pair_set_external =
    collect_sites_bond_pair_set_external parameters error rule
      store_result.Ode_fragmentation_type.store_sites_bond_pair_set_external
  in
  (*----------------------------------------------------------------------*)
  (*collect sites from lhs rule*)
  let error, store_sites_lhs =
    collect_sites_lhs parameters error rule
      store_result.Ode_fragmentation_type.store_sites_lhs
  in
  (*----------------------------------------------------------------------*)
  (*collect anchor sites*)
  let error, store_sites_anchor_set =
    collect_sites_anchor_set parameters error handler_kappa rule
      store_sites_modified_set store_sites_bond_pair_set store_sites_lhs
      store_result.Ode_fragmentation_type.store_sites_anchor_set
  in
  (*----------------------------------------------------------------------*)
  (*collect internal flow: site -> modified/anchor site*)
  let error, store_internal_flow =
    collect_internal_flow parameters error handler_kappa rule store_sites_lhs
      store_sites_modified_set store_sites_anchor_set
      store_result.Ode_fragmentation_type.store_internal_flow
  in
  (*----------------------------------------------------------------------*)
  (*collect external flow : a -> b , if 'a' is an anchor site or 'b' is a
    modified site*)
  let error, store_external_flow =
    collect_external_flow parameters error handler_kappa rule
      store_sites_bond_pair_set_external store_sites_anchor_set
      store_result.Ode_fragmentation_type.store_external_flow
  in
  (*----------------------------------------------------------------------*)
  ( error,
    {
      Ode_fragmentation_type.store_sites_modified_set;
      Ode_fragmentation_type.store_sites_bond_pair_set;
      Ode_fragmentation_type.store_sites_bond_pair_set_external;
      Ode_fragmentation_type.store_sites_lhs;
      Ode_fragmentation_type.store_sites_anchor_set;
      Ode_fragmentation_type.store_internal_flow;
      Ode_fragmentation_type.store_external_flow;
    } )

(************************************************************************************)
(*RULES*)

let scan_rule_set parameters error handler_kappa compiled =
  let error, init_store_sites_modified_set =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create
      parameters error 0
  in
  let error, init =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create
      parameters error 0
  in
  let init_store_sites_bond_pair_set = init, init in
  let init_store_sites_bond_pair_set_external = init, init in
  let error, init_store_sites_lhs =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create
      parameters error 0
  in
  let init_store_sites_anchor = init, init in
  let init_internal1 = Ode_fragmentation_type.Internal_flow_map.Map.empty in
  let init_internal2 = Ode_fragmentation_type.Internal_flow_map.Map.empty in
  let init_external = Ode_fragmentation_type.External_flow_map.Map.empty in
  let init_ode =
    {
      Ode_fragmentation_type.store_sites_modified_set =
        init_store_sites_modified_set;
      Ode_fragmentation_type.store_sites_bond_pair_set =
        init_store_sites_bond_pair_set;
      Ode_fragmentation_type.store_sites_bond_pair_set_external =
        init_store_sites_bond_pair_set_external;
      Ode_fragmentation_type.store_sites_lhs = init_store_sites_lhs;
      Ode_fragmentation_type.store_sites_anchor_set = init_store_sites_anchor;
      Ode_fragmentation_type.store_internal_flow =
        init_internal1, init_internal2;
      Ode_fragmentation_type.store_external_flow = init_external;
    }
  in
  (*----------------------------------------------------------------------*)
  let error, store_result =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameters error
      (fun parameters error rule_id rule store_result ->
        (*----------------------------------------------------------------------*)
        (*PRINT*)
        let _ =
          if
            Remanent_parameters.get_do_ODE_flow_of_information parameters
            && Remanent_parameters.get_trace parameters
          then (
            let parameters = Remanent_parameters.update_prefix parameters "" in
            (*Print at each rule:*)
            let error, rule_string =
              try Handler.string_of_rule parameters error compiled rule_id
              with _ ->
                Exception.warn parameters error __POS__ Exit
                  (Ckappa_sig.string_of_rule_id rule_id)
            in
            let () = Printf.fprintf stdout "%s\n" rule_string in
            error
          ) else
            error
        in
        (*----------------------------------------------------------------------*)
        let error, store_result =
          scan_rule parameters error handler_kappa
            rule.Cckappa_sig.e_rule_c_rule store_result
        in
        error, store_result)
      compiled.Cckappa_sig.rules init_ode
  in
  error, store_result
