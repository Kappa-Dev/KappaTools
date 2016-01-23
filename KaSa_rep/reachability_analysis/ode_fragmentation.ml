(**
    * ode_fragmentation.ml
    * openkappa
    * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 2015, the 9th of Apirl
    * Last modification:
    * *
    * ODE fragmentation
    *
    *
    * Copyright 2010,2011 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    *  under the terms of the GNU Library General Public License *)

open Int_storage
open Cckappa_sig
open Printf
open Ode_fragmentation_type

let warn parameter mh message exn default =
  Exception.warn parameter mh (Some "ODE fragmentation") message exn
                 (fun () -> default)

let trace = false

(************************************************************************************)
(*collect modified set*)

let collect_sites_modified_set parameter error rule handler_kappa store_result =
  let error, store_result =
    AgentMap.fold parameter error
      (fun parameter error agent_id site_modif store_result ->
        if Site_map_and_set.Map.is_empty site_modif.agent_interface
        then
          error, store_result
        else
          let agent_type = site_modif.agent_name in
          (*----------------------------------------------------------------------*)
          (*collect a set of site that modified*)
          let error, site_set =
            Site_map_and_set.Map.fold
              (fun site _ (error, current_set) ->
                let error, set =
                  Site_map_and_set.Set.add parameter error site current_set
                in
                error, set
              )
              site_modif.agent_interface
              (error, Site_map_and_set.Set.empty)
          in
          (*----------------------------------------------------------------------*)
          (*PRINT at each rule*)
          let _ =
            if Remanent_parameters.get_do_ODE_flow_of_information parameter
            then
              if Remanent_parameters.get_trace parameter
              then
                let () =
                  Loggers.fprintf (Remanent_parameters.get_logger parameter)
                    "Flow of information in the ODE semantics:modified sites:"
                in
		let () =
		  Loggers.print_newline (Remanent_parameters.get_logger parameter)
		in
                let error, agent_string =
                  try
                    Handler.string_of_agent parameter error handler_kappa agent_type
                  with
                    _ -> warn parameter error (Some "line 67") Exit
                      (string_of_int agent_type)
                in
                let _ =
                  fprintf stdout "\tagent_type:%i:%s\n" agent_type agent_string
                in
                Site_map_and_set.Set.iter (fun site_type ->
                  let error, site_string =
                    try
                      Handler.string_of_site parameter error handler_kappa agent_type
                        site_type
                    with
                      _ -> warn parameter error (Some "line 79") Exit
                        (string_of_int site_type)
                  in
                  let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "\t\tsite_type:%i:%s" site_type site_string
		  in
		  Loggers.print_newline (Remanent_parameters.get_logger parameter)
                ) site_set
              else ()
          in
          (*----------------------------------------------------------------------*)
          (*get old?*)
          let error, old_set =
            match AgentMap.unsafe_get parameter error agent_type store_result with
            | error, None -> error, Site_map_and_set.Set.empty
            | error, Some s -> error, s
          in
          (*new*)
          let error, new_set =
            Site_map_and_set.Set.union parameter error site_set old_set
          in
          (*----------------------------------------------------------------------*)
          (*store*)
          let error, store_result =
            AgentMap.set
              parameter
              error
              agent_type
              new_set
              store_result
          in
          error, store_result
      )
      rule.diff_reverse
      store_result
  in
  error, store_result

(************************************************************************************)
(*collect sites that are released*)

let collect_sites_bond_pair_set parameter error handler_kappa rule store_result =
  let bond_lhs = rule.rule_lhs.bonds in
  List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
    let store_result1, store_result2 = store_result in
    let agent_id1 = site_add1.agent_index in
    let agent_type1 = site_add1.agent_type in
    (*get site_address_map from bond_lhs*)
    let error, site_add_map1 =
      match AgentMap.unsafe_get parameter error agent_id1 bond_lhs with
      | error, None -> error, Site_map_and_set.Map.empty
      | error, Some map -> error, map
    in
    (*get a set of sites that are bond*)
    let error, sites_bond_set1 =
      Site_map_and_set.Map.fold
        (fun site _ (error, current_set) ->
          let error, set =
            Site_map_and_set.Set.add
              parameter
              error
              site
              current_set
          in
          error, set
        ) site_add_map1 (error, Site_map_and_set.Set.empty)
    in
    (*----------------------------------------------------------------------*)
    (*PRINT*)
    let _ =
      if Remanent_parameters.get_do_ODE_flow_of_information parameter
      then
        if Remanent_parameters.get_trace parameter
        then
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "Flow of information in the ODE semantics:bond sites (first agent):"
          in
	  let () =
	    Loggers.print_newline (Remanent_parameters.get_logger parameter)
	  in
          let error, agent_string1 =
            try
              Handler.string_of_agent parameter error handler_kappa agent_type1
            with
              _ -> warn parameter error (Some "line 158") Exit (string_of_int agent_type1)
          in
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameter) "\tagent_type:%i:%s" agent_type1 agent_string1
          in
	  let () =
	    Loggers.print_newline (Remanent_parameters.get_logger parameter)
	  in
          Site_map_and_set.Set.iter (fun site_type ->
            let error, site_string =
              try
                Handler.string_of_site parameter error handler_kappa agent_type1
                  site_type
              with
                _ -> warn parameter error (Some "line 169") Exit (string_of_int site_type)
            in
            let () =
	      Loggers.fprintf (Remanent_parameters.get_logger parameter) "\t\tsite_type:%i:%s" site_type site_string
	    in
	    Loggers.print_newline (Remanent_parameters.get_logger parameter)
          ) sites_bond_set1
        else ()
    in
    (*----------------------------------------------------------------------*)
    (*compute first pair*)
    let error, store_result1 =
      AgentMap.set
        parameter
        error
        agent_type1
        sites_bond_set1
        store_result1
    in
    (*----------------------------------------------------------------------*)
    (*compute second pair*)
    let agent_id2 = site_add2.agent_index in
    let agent_type2 = site_add2.agent_type in
    let error, site_add_map2 =
      match AgentMap.unsafe_get parameter error agent_id2 bond_lhs with
      | error, None -> error, Site_map_and_set.Map.empty
      | error, Some map -> error, map
    in
    (*get a set of sites that are bond*)
    let error, sites_bond_set2 =
      Site_map_and_set.Map.fold
        (fun site _ (error, current_set) ->
          let error, set =
            Site_map_and_set.Set.add
              parameter
              error
              site
              current_set
          in
          error, set
        ) site_add_map2 (error, Site_map_and_set.Set.empty)
    in
    (*----------------------------------------------------------------------*)
    (*PRINT*)
    let _ =
      if Remanent_parameters.get_do_ODE_flow_of_information parameter
      then
        if Remanent_parameters.get_trace parameter
        then
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "Flow of information in the ODE semantics:bond sites (second agent):"
          in
	  let () =
	    Loggers.print_newline (Remanent_parameters.get_logger parameter)
	  in
          let error, agent_string2 =
            try
              Handler.string_of_agent parameter error handler_kappa agent_type2
            with
              _ -> warn parameter error (Some "line 223") Exit (string_of_int agent_type2)
          in
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameter) "\tagent_type:%i:%s" agent_type2 agent_string2
          in
	  let () =
	    Loggers.print_newline (Remanent_parameters.get_logger parameter)
	  in
          Site_map_and_set.Set.iter (fun site_type ->
            let error, site_string =
              try
                Handler.string_of_site parameter error handler_kappa agent_type2
                  site_type
              with
                _ -> warn parameter error (Some "line 234") Exit (string_of_int site_type)
            in
            let () =
	      Loggers.fprintf (Remanent_parameters.get_logger parameter) "\t\tsite_type:%i:%s" site_type site_string
	    in
	    Loggers.print_newline (Remanent_parameters.get_logger parameter)
          ) sites_bond_set2
        else ()
    in
    (*----------------------------------------------------------------------*)
    (*get old*)
    let error, old_set2 =
      match AgentMap.unsafe_get parameter error agent_type2 store_result2 with
      | error, None -> error, Site_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*new set*)
    let error, new_set2 =
      Site_map_and_set.Set.union parameter error sites_bond_set2 old_set2
    in
    (*store*)
    let error, store_result2 =
      AgentMap.set
        parameter
        error
        agent_type2
        new_set2
        store_result2
    in
    (*----------------------------------------------------------------------*)
    (*result*)
    (*return a pair, first pair is a first binding agent of each rule. Second
      pair is a second binding agent, it is a result of anchor*)
    let error, store_result =
      error, (store_result1, store_result2)
    in
    error, store_result
  ) (error, store_result) rule.actions.release

(************************************************************************************)
(*collect sites that are external*)

let collect_sites_bond_pair_set_external parameter error rule store_result =
  let bond_lhs = rule.rule_lhs.bonds in
  List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
    let store_result1, store_result2 = store_result in
    let agent_id1 = site_add1.agent_index in
    let agent_type1 = site_add1.agent_type in
    let error, site_add_map1 =
      match AgentMap.unsafe_get parameter error agent_id1 bond_lhs with
      | error, None -> error, Site_map_and_set.Map.empty
      | error, Some map -> error, map
    in
    let error, sites_bond_set1 =
      Site_map_and_set.Map.fold
        (fun site _ (error, current_set) ->
          let error, set =
            Site_map_and_set.Set.add
              parameter
              error
              site
              current_set
          in
          error, set
        ) site_add_map1 (error, Site_map_and_set.Set.empty)
    in
    (*store*)
    let error, store_result1 =
      AgentMap.set
        parameter
        error
        agent_type1
        sites_bond_set1
        store_result1
    in
    (*----------------------------------------------------------------------*)
    (*compute second pair*)
    let agent_id2 = site_add2.agent_index in
    let agent_type2 = site_add2.agent_type in
    let error, site_add_map2 =
      match AgentMap.unsafe_get parameter error agent_id2 bond_lhs with
      | error, None -> error, Site_map_and_set.Map.empty
      | error, Some map -> error, map
    in
    let error, sites_bond_set2 =
      Site_map_and_set.Map.fold
        (fun site _ (error, current_set) ->
          let error, set =
            Site_map_and_set.Set.add
              parameter
              error
              site
              current_set
          in
          error, set
        ) site_add_map2 (error, Site_map_and_set.Set.empty)
    in
    let error, store_result2 =
      AgentMap.set
        parameter
        error
        agent_type2
        sites_bond_set2
        store_result2
    in
    (*----------------------------------------------------------------------*)
    let error, store_result =
      error, (store_result1, store_result2)
    in
    error, store_result
  ) (error, store_result) rule.actions.release

(************************************************************************************)
(*collect sites from lhs rule*)

let collect_sites_lhs parameter error rule store_result =
  let error, store_result =
    AgentMap.fold parameter error
      (fun parameter error agent_id agent store_result ->
        match agent with
        | Ghost | Unknown_agent _ -> error, store_result
        | Dead_agent (agent, _, _, _)
        | Agent agent ->
          let agent_type = agent.agent_name in
          let error, site_list =
            Site_map_and_set.Map.fold
              (fun site _ (error, current_list) ->
                let site_list = site :: current_list in
                error, site_list
              ) agent.agent_interface (error, [])
          in
          (*get old?*)
          let error, old_list =
            match AgentMap.unsafe_get parameter error agent_type store_result with
            | error, None -> error, []
            | error, Some l -> error, l
          in
          let new_list = List.concat [site_list; old_list] in
          (*store*)
          let error, store_result =
            AgentMap.set
              parameter
              error
              agent_type
              new_list (*FIXME: new_list?*)
              store_result
          in
          error, store_result
      ) rule.rule_lhs.views store_result
  in
  error, store_result

(************************************************************************************)
(*collect sites anchor set*)

let collect_sites_anchor_set parameter error handler_kappa rule
    store_sites_modified_set
    store_sites_bond_pair_set
    store_sites_lhs
    store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_id agent store_result ->
      let store_result1, store_result2 = store_result in
      match agent with
      | Ghost | Unknown_agent _ -> error, store_result
      | Dead_agent (agent, _, _, _)
      | Agent agent ->
        let agent_type = agent.agent_name in
        (*----------------------------------------------------------------------*)
        (*get sites that is modified*)
        let error, modified_set =
          match AgentMap.unsafe_get parameter error agent_type
            store_sites_modified_set
          with
          | error, None -> error, Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        (*----------------------------------------------------------------------*)
        (*get a set of sites in the lhs that are bond*)
        let error, site_lhs_bond_fst_set =
          match AgentMap.unsafe_get parameter error agent_type
            (fst store_sites_bond_pair_set)
          with
          | error, None -> error, Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        (*----------------------------------------------------------------------*)
        (*get a list of sites in the lsh*)
        let error, sites_lhs_list =
          match AgentMap.unsafe_get parameter error agent_type store_sites_lhs with
          | error, None -> error, []
          | error, Some l -> error, l
        in
        (*----------------------------------------------------------------------*)
        (*first case: a site connected to a modified site*)
        let error, store_result1 =
          List.fold_left (fun (error, store_result) x ->
            begin
              if Site_map_and_set.Set.mem x modified_set &&
                Site_map_and_set.Set.mem x site_lhs_bond_fst_set
              then
                let store_result =
                  snd store_sites_bond_pair_set
                in
                error, store_result
              else
                error, store_result
            end
          ) (error, store_result1) (List.rev sites_lhs_list)
        in
        (*----------------------------------------------------------------------*)
        (*second result*)
        (*get a set of anchor sites*)
        let error, anchor_set1 =
          match AgentMap.unsafe_get parameter error agent_type
            (fst store_sites_bond_pair_set)
          with
          | error, None -> error, Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, anchor_set2 =
          match AgentMap.unsafe_get parameter error agent_type
            (snd store_sites_bond_pair_set)
          with
          | error, None -> error, Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, anchor_set =
          Site_map_and_set.Set.union parameter error anchor_set1 anchor_set2
        in
        (*----------------------------------------------------------------------*)
        let error, store_result2 =
          List.fold_left (fun (error, store_result) x ->
            List.fold_left (fun (error, store_result) y ->
              begin
                if Site_map_and_set.Set.mem x anchor_set ||
                  Site_map_and_set.Set.mem x modified_set &&
                  Site_map_and_set.Set.mem y site_lhs_bond_fst_set
                then
                  let store_result =
                    snd store_sites_bond_pair_set
                  in
                  error, store_result
                else
                  error, store_result
              end
            ) (error, store_result) (List.tl sites_lhs_list)
          ) (error, store_result2) (List.rev sites_lhs_list)
        in
        (*----------------------------------------------------------------------*)
        (*get union both result*)
        let error, get_set1 =
          match AgentMap.unsafe_get parameter error agent_type store_result1 with
          | error, None -> error, Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, get_set2 =
          match AgentMap.unsafe_get parameter error agent_type store_result2 with
          | error, None -> error, Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, union_set =
          Site_map_and_set.Set.union parameter error get_set1 get_set2
        in
        (*----------------------------------------------------------------------*)
        (*PRINT*)
        let _ =
          if Remanent_parameters.get_do_ODE_flow_of_information parameter
          then
            if Remanent_parameters.get_trace parameter
            then
              let () =
                Loggers.fprintf (Remanent_parameters.get_logger parameter)
                  "Flow of information in the ODE semantics:anchor sites:"
              in
	      let () =
		Loggers.print_newline (Remanent_parameters.get_logger parameter)
	      in
	      let error, agent_string =
                try
                  Handler.string_of_agent parameter error handler_kappa agent_type
                with
                  _ -> warn parameter error (Some "line 510") Exit
                    (string_of_int agent_type)
              in
              let () =
                Loggers.fprintf (Remanent_parameters.get_logger parameter) "\tagent_type:%i:%s" agent_type agent_string
              in
	      let () =
		Loggers.print_newline (Remanent_parameters.get_logger parameter)
	      in
              Site_map_and_set.Set.iter (fun site_type ->
                let error, site_string =
                  try
                    Handler.string_of_site parameter error handler_kappa agent_type
                      site_type
                  with
                    _ -> warn parameter error (Some "line 522") Exit
                      (string_of_int site_type)
                in
                let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)  "\t\tsite_type:%i:%s" site_type site_string in
		Loggers.print_newline (Remanent_parameters.get_logger parameter)
              ) union_set
            else ()
        in
        (*----------------------------------------------------------------------*)
        (*result*)
        let error, store_result =
          error, (store_result1, store_result2)
        in
        error, store_result
    ) rule.rule_lhs.views store_result

(************************************************************************************)
(*collect internal flow*)

let cartesian_prod_eq i a b =
  let rec loop a acc =
    match a with
      | [] -> List.rev acc
      | x :: xs ->
        loop xs (List.rev_append (List.rev (List.fold_left (fun acc y ->
          if x <> y
          then (i, x, y) :: acc
          else acc
        ) [] b)) acc)
  in
  loop a []

let collect_internal_flow parameter error handler_kappa rule
    store_sites_lhs
    store_sites_modified_set
    store_sites_anchor_set
    store_result =
  (*----------------------------------------------------------------------*)
  let add_link agent_type (site_list, set) store_result =
    let result =
      Internal_flow_map.Map.add agent_type (site_list, set) store_result
    in
    error, result
  in
  (*----------------------------------------------------------------------*)
  AgentMap.fold parameter error
    (fun parameter error agent_id agent store_result ->
      let store_result1, store_result2 = store_result in
      match agent with
      | Ghost | Unknown_agent _ -> error, store_result
      | Dead_agent (agent, _, _, _)
      | Agent agent ->
        let agent_type = agent.agent_name in
        (*let agent_type_modif = agent_modif.agent_name in*)
        (*get modified set*)
        let error, modified_set =
          match AgentMap.unsafe_get parameter error agent_type
            store_sites_modified_set
          with
          | error, None -> error, Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        (*get anchor set*)
        let error, anchor_set1 =
          match AgentMap.unsafe_get parameter error agent_type
            (fst store_sites_anchor_set)
          with
          | error, None -> error, Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, anchor_set2 =
          match AgentMap.unsafe_get parameter error agent_type
            (snd store_sites_anchor_set)
          with
          | error, None -> error, Site_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error, anchor_set =
          Site_map_and_set.Set.union parameter error anchor_set1 anchor_set2
        in
        (*----------------------------------------------------------------------*)
        (*first result: site -> modified site*)
        let error, site_list =
          match AgentMap.unsafe_get parameter error agent_type store_sites_lhs with
          | error, None -> error, []
          | error, Some l -> error, l
        in
        (*------------------------------------------------------------------------------*)
        (*PRINT*)
        let _ =
          if Remanent_parameters.get_do_ODE_flow_of_information parameter
          then
            if Remanent_parameters.get_trace parameter
            then
              let _ =
                Loggers.fprintf (Remanent_parameters.get_logger parameter)
                  "Flow of information in the ODE semantics:internal flow (first case):\n"
              in
              let modified_list =
                Site_map_and_set.Set.elements modified_set
              in
              let cartesian_output =
                cartesian_prod_eq agent_type site_list modified_list
              in
              let _ =
                List.iter (fun (agent_type, site_type, site_modif) ->
                  let error, agent_string =
                    try
                      Handler.string_of_agent parameter error handler_kappa agent_type
                    with
                      _ -> warn parameter error (Some "line 631") Exit
                        (string_of_int agent_type)
                  in
                  let error, site_string =
                    try
                      Handler.string_of_site parameter error handler_kappa agent_type
                        site_type
                    with
                      _ -> warn parameter error (Some "line 639") Exit
                        (string_of_int site_type)
                  in
                  let error, site_modif_string =
                    Handler.string_of_site parameter error handler_kappa agent_type
                      site_modif
                  in
                  let () =
		    Loggers.fprintf (Remanent_parameters.get_logger parameter)
                    "Flow of information in the ODE semantics:Internal flow\n-agent_type:%i:%s:site_type:%i:%s -> agent_type:%i:%s:site_type_modified:%i:%s"
                    agent_type agent_string
                    site_type site_string
                    agent_type agent_string
                    site_modif site_modif_string
		  in
		  Loggers.print_newline (Remanent_parameters.get_logger parameter)
                ) cartesian_output
              in
              ()
        in
        (*------------------------------------------------------------------------------*)
        (*PRINT second internal flow*)
        let _ =
          if Remanent_parameters.get_do_ODE_flow_of_information parameter
          then
            if Remanent_parameters.get_trace parameter
            then
              let () =
                Loggers.fprintf (Remanent_parameters.get_logger parameter)
                  "Flow of information in the ODE semantics:internal flow (second case):"
              in
	      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
              let anchor_list =
                Site_map_and_set.Set.elements anchor_set
              in
              let cartesian_output =
                cartesian_prod_eq agent_type site_list anchor_list
              in
              let _ =
                List.iter (fun (agent_type, site_type, site_anchor) ->
                  let error, agent_string =
                    try
                      Handler.string_of_agent parameter error handler_kappa agent_type
                    with
                      _ -> warn parameter error (Some "line 678") Exit
                        (string_of_int agent_type)
                  in
                  let error, site_string =
                    try
                      Handler.string_of_site parameter error handler_kappa agent_type
                        site_type
                    with
                      _ -> warn parameter error (Some "line 687") Exit
                        (string_of_int site_type)
                  in
                  let error, site_anchor_string =
                    try
                      Handler.string_of_site parameter error handler_kappa agent_type
                        site_anchor
                    with
                      _ -> warn parameter error (Some "line 695") Exit
                        (string_of_int site_anchor)
                  in
                  let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "Flow of information in the ODE semantics:Internal flow\n-agent_type:%i:%s:site_type:%i:%s -> agent_type:%i:%s:site_type_anchor:%i:%s"
                    agent_type agent_string
                    site_type site_string
                    agent_type agent_string
                    site_anchor site_anchor_string
		  in
		  Loggers.print_newline (Remanent_parameters.get_logger parameter)
                ) cartesian_output
              in
              ()
        in
        (*------------------------------------------------------------------------------*)
        let error, store_result1 =
          add_link agent_type (site_list, modified_set) store_result1
        in
        (*------------------------------------------------------------------------------*)
        let error, store_result2 =
          add_link agent_type (site_list, anchor_set) store_result2
        in
        (*------------------------------------------------------------------------------*)
        let error, store_result =
          error, (store_result1, store_result2)
        in
        error, store_result
    ) rule.rule_lhs.views
    store_result

(************************************************************************************)
(*collect external flow*)

let cartesian_prod_external i anchor_set i' bond_fst_list bond_snd_set =
  let anchor_list = Site_map_and_set.Set.elements anchor_set in
  let rec loop anchor_list acc =
    match anchor_list with
      | [] -> List.rev acc
      | x :: xs ->
        loop xs (List.rev_append (List.rev (List.fold_left (fun acc y ->
          if Site_map_and_set.Set.mem x anchor_set &&
            Site_map_and_set.Set.mem x bond_snd_set
          then (i, x, i', y) :: acc
          else acc
        ) [] bond_fst_list)) acc)
  in
  loop anchor_list []

let collect_external_flow parameter error handler_kappa rule
    store_sites_bond_pair_set_external
    store_sites_anchor_set
    store_result =
  (*------------------------------------------------------------------------------*)
  let add_link (agent_type1, agent_type2) (anchor_set, bond_fst_set, bond_snd_set)
      store_result =
    let result =
      External_flow_map.Map.add (agent_type1, agent_type2)
        (anchor_set, bond_fst_set, bond_snd_set) store_result
    in
    error, result
  in
  (*------------------------------------------------------------------------------*)
  List.fold_left (fun (error, store_result) (site_add1, site_add2) ->
    let agent_type1 = site_add1.agent_type in
    let agent_type2 = site_add2.agent_type in
    (*------------------------------------------------------------------------------*)
    (*get sites that are bond on the lhs*)
    let error, bond_fst_set =
      match AgentMap.unsafe_get parameter error agent_type1
        (fst store_sites_bond_pair_set_external)
      with
      | error, None -> error, Site_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*------------------------------------------------------------------------------*)
    let error, bond_snd_set =
      match AgentMap.unsafe_get parameter error agent_type2
        (snd store_sites_bond_pair_set_external)
      with
      | error, None -> error, Site_map_and_set.Set.empty
      | error, Some s -> error, s
    in
    (*------------------------------------------------------------------------------*)
    (*get anchor set*)
    let error, anchor_set1 =
      AgentMap.fold parameter error
        (fun parameter error agent_type site_set old_set ->
          let error, set =
            Site_map_and_set.Set.union
              parameter
              error
              site_set
              old_set
          in
          error, set
        ) (fst store_sites_anchor_set) Site_map_and_set.Set.empty
    in
    let error, anchor_set2 =
      AgentMap.fold parameter error
        (fun parameter error agent_type site_set old_set ->
          let error, set =
            Site_map_and_set.Set.union
              parameter
              error
              site_set
              old_set
          in
          error, set
        )(snd store_sites_anchor_set) Site_map_and_set.Set.empty
    in
    let error, anchor_set =
      Site_map_and_set.Set.union
        parameter
        error
        anchor_set1
        anchor_set2
    in
    (*------------------------------------------------------------------------------*)
    (*PRINT External flow*)
    let bond_fst_list = Site_map_and_set.Set.elements bond_fst_set in
    let cartesian_output =
      cartesian_prod_external
        agent_type2
        anchor_set
        agent_type1
        bond_fst_list
        bond_snd_set
    in
    let _ =
      if Remanent_parameters.get_do_ODE_flow_of_information parameter
      then
        if Remanent_parameters.get_trace parameter
        then
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "Flow of information in the ODE semantics:external flow:"
          in
	  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter)
	    in
	  List.iter (fun (agent_type, anchor_site_type, agent_type', site_modif) ->
            let error, agent_string =
              try
                Handler.string_of_agent parameter error handler_kappa agent_type
              with
                _ -> warn parameter error (Some "line 835") Exit (string_of_int agent_type)
            in
            let error, agent_string' =
              try
                Handler.string_of_agent parameter error handler_kappa agent_type'
              with
                _ -> warn parameter error (Some "line 841") Exit (string_of_int agent_type')
            in
            let error, anchor_site_type_string =
              try
                Handler.string_of_site parameter error handler_kappa agent_type
                  anchor_site_type
              with
                _ -> warn parameter error (Some "line 848") Exit
                  (string_of_int anchor_site_type)
            in
            let error, site_modif_string =
              try
                Handler.string_of_site parameter error handler_kappa agent_type'
                  site_modif
              with
                _ -> warn parameter error (Some "line 856") Exit (string_of_int site_modif)
            in
            let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "Flow of information in the ODE semantics:External flow:\n-agent-type:%i:%s:site_type_anchor:%i:%s -> agent_type:%i:%s:site_type_modified:%i:%s"
              agent_type agent_string
              anchor_site_type anchor_site_type_string
              agent_type' agent_string'
              site_modif site_modif_string in
	    Loggers.print_newline (Remanent_parameters.get_logger parameter)
          ) cartesian_output
        else ()
    in
    (*------------------------------------------------------------------------------*)
    let error, store_result =
      add_link (agent_type1, agent_type2) (anchor_set, bond_fst_set, bond_snd_set)
        store_result
    in
    error, store_result
  ) (error, store_result) rule.actions.release

(************************************************************************************)
(*RULE*)

let scan_rule parameter error handler_kappa rule store_result =
  (*----------------------------------------------------------------------*)
  (*modified set*)
  let error, store_sites_modified_set =
    collect_sites_modified_set
      parameter
      error
      rule
      handler_kappa
      store_result.store_sites_modified_set
  in
  (*----------------------------------------------------------------------*)
  (*collect sites that are release*)
  let error, store_sites_bond_pair_set =
    collect_sites_bond_pair_set
      parameter
      error
      handler_kappa
      rule
      store_result.store_sites_bond_pair_set
  in
  (*----------------------------------------------------------------------*)
  (*collects sites that are external*)
  let error, store_sites_bond_pair_set_external =
    collect_sites_bond_pair_set_external
      parameter
      error
      rule
      store_result.store_sites_bond_pair_set_external
  in
  (*----------------------------------------------------------------------*)
  (*collect sites from lhs rule*)
  let error, store_sites_lhs =
    collect_sites_lhs
      parameter
      error
      rule
      store_result.store_sites_lhs
  in
  (*----------------------------------------------------------------------*)
  (*collect anchor sites*)
  let error, store_sites_anchor_set =
    collect_sites_anchor_set
      parameter
      error
      handler_kappa
      rule
      store_sites_modified_set
      store_sites_bond_pair_set
      store_sites_lhs
      store_result.store_sites_anchor_set
  in
  (*----------------------------------------------------------------------*)
  (*collect internal flow: site -> modified/anchor site*)
  let error, store_internal_flow =
    collect_internal_flow
      parameter
      error
      handler_kappa
      rule
      store_sites_lhs
      store_sites_modified_set
      store_sites_anchor_set
      store_result.store_internal_flow
  in
  (*----------------------------------------------------------------------*)
  (*collect external flow : a -> b , if 'a' is an anchor site or 'b' is a
    modified site*)
  let error, store_external_flow =
    collect_external_flow
      parameter
      error
      handler_kappa
      rule
      store_sites_bond_pair_set_external
      store_sites_anchor_set
      store_result.store_external_flow
  in
  (*----------------------------------------------------------------------*)
  error,
  {
    store_sites_modified_set           = store_sites_modified_set;
    store_sites_bond_pair_set          = store_sites_bond_pair_set;
    store_sites_bond_pair_set_external = store_sites_bond_pair_set_external;
    store_sites_lhs                    = store_sites_lhs;
    store_sites_anchor_set             = store_sites_anchor_set;
    store_internal_flow                = store_internal_flow;
    store_external_flow                = store_external_flow;
  }

(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler_kappa compiled =
  let error, init_store_sites_modified_set    = AgentMap.create parameter error 0 in
  let error, init                             = AgentMap.create parameter error 0 in
  let init_store_sites_bond_pair_set          = (init, init) in
  let init_store_sites_bond_pair_set_external = (init, init) in
  let error, init_store_sites_lhs             = AgentMap.create parameter error 0 in
  let init_store_sites_anchor                 = (init, init) in
  let init_internal1                          = Internal_flow_map.Map.empty in
  let init_internal2                          = Internal_flow_map.Map.empty in
  let init_external                           = External_flow_map.Map.empty in
  let init_ode =
    {
      store_sites_modified_set  = init_store_sites_modified_set;
      store_sites_bond_pair_set = init_store_sites_bond_pair_set;
      store_sites_bond_pair_set_external = init_store_sites_bond_pair_set_external;
      store_sites_lhs        = init_store_sites_lhs;
      store_sites_anchor_set = init_store_sites_anchor;
      store_internal_flow    = init_internal1, init_internal2;
      store_external_flow    = init_external;
    }
  in
  (*----------------------------------------------------------------------*)
  let error, store_result =
    Nearly_inf_Imperatif.fold parameter error
      (fun parameter error rule_id rule store_result ->
        (*----------------------------------------------------------------------*)
        (*PRINT*)
        let _ =
          if Remanent_parameters.get_do_ODE_flow_of_information parameter
          then
            let parameter =
              Remanent_parameters.update_prefix parameter ""
            in
            if Remanent_parameters.get_trace parameter
            then
              (*Print at each rule:*)
              let error, rule_string =
                try
                  Handler.string_of_rule parameter error handler_kappa compiled rule_id
                with
                  _ -> warn parameter error (Some "line 1010") Exit (string_of_int rule_id)
              in
              fprintf stdout "%s\n" rule_string
            else ()
        in
        (*----------------------------------------------------------------------*)
        let error, store_result =
          scan_rule
            parameter
            error
            handler_kappa
            rule.e_rule_c_rule
            store_result
        in
        error, store_result
      )
      compiled.rules
      init_ode
  in
  error, store_result

(************************************************************************************)
(*UTILITIES FUNCTIONS*)

(*------------------------------------------------------------------------------*)
(* A list of site*)
(*
let get_site_common_list parameter error agent_type store_sites_common =
  let error, get_sites =
    AgentMap.unsafe_get
      parameter
      error
      agent_type
      store_sites_common
  in
  let site_list =
    match get_sites with
      | None -> []
      | Some s -> s
  in site_list

(*------------------------------------------------------------------------------*)
(* A set of site*)

let get_site_common_set parameter error agent_type store_sites_common =
  let error, get_set =
    AgentMap.unsafe_get
      parameter
      error
      agent_type
      store_sites_common
  in
  let set =
    match get_set with
      | None -> SiteSet.Set.empty
      | Some s -> s
  in set

(*------------------------------------------------------------------------------*)
(* A set of anchor site*)

let anchor_set parameter error agent_type store_sites_anchor1 store_sites_anchor2 =
  let anchor_set1 =
    get_site_common_set
      parameter
      error
      agent_type
      store_sites_anchor1
  in
  let anchor_set2 =
    get_site_common_set
      parameter
      error
      agent_type
      store_sites_anchor2
  in
  SiteSet.Set.union parameter error anchor_set1 anchor_set2

(*------------------------------------------------------------------------------*)
(* A set of anchors site (combine two cases) fold*)

let get_anchor_common parameter error store_sites_anchor =
  AgentMap.fold
     parameter
     error
     (fun parameter error agent_type site_set old_set ->
        SiteSet.Set.union
	  parameter
	  error
          site_set
          old_set
      )
     store_sites_anchor
     SiteSet.Set.empty

let fold_anchor_set parameter error store_sites_anchor_set1 store_sites_anchor_set2 =
  let error, anchor_set1 = get_anchor_common parameter error store_sites_anchor_set1 in
  let error, anchor_set2 = get_anchor_common parameter error store_sites_anchor_set2 in
  let error,anchor_set =
    SiteSet.Set.union
      parameter
      error
      anchor_set1
      anchor_set2
  in anchor_set

(***********************************************************************************)
(*MODIFIED SITES*)

let collect_sites_modified_set parameter error rule handler store_sites_modified_set =
  let error, store_sites_modified_set =
    Quick_Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id site_modif store_sites_modified_set ->
        let agent_type = site_modif.agent_name in
        if SiteSet.Map.is_empty
          site_modif.agent_interface
        then
          error, store_sites_modified_set
        else
          (*get a pair of (site_set, value)*)
          let site_set, error  =
            SiteSet.Map.fold (fun site _ (current_set, error) ->
              (*get a set of site*)
              let error, set =
                SiteSet.Set.add parameter error site current_set
              in
              (set, error)
            ) site_modif.agent_interface
              (SiteSet.Set.empty, error)
          in
          (*------------------------------------------------------------------------------*)
          (*print*)
          let _ =
            SiteSet.Set.iter (fun site_type ->
              let error, agent_type_string =
                Handler.string_of_agent parameter error handler agent_type
              in
              let error, site_type_string =
                Handler.string_of_site parameter error handler agent_type site_type
              in
              fprintf stdout "Flow of information in the ODE semantics:agent_type:%i:%s:site_type_modified:%i:%s\n"
                agent_type agent_type_string
                site_type site_type_string
            ) site_set
          in
          (*------------------------------------------------------------------------------*)
          (*store only site_set*)
          let error, store_sites_modified_set =
            AgentMap.set
              parameter
              error
              agent_type
              site_set
              store_sites_modified_set
          in
          error, store_sites_modified_set
      )
      rule.diff_reverse
      store_sites_modified_set
  in error, store_sites_modified_set

(************************************************************************************)
(*BINDING SITES - SET*)

(*------------------------------------------------------------------------------*)
(*++ element in a pair of site that are bond*)

(*without adding the old result*)
let collect_store_bond_set_each_rule parameter error bond_lhs
    site_address store_sites_bond_set =
  let agent_id = site_address.agent_index in
  let agent_type = site_address.agent_type in
   let error, site_address_map =
     Quick_Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_id
      bond_lhs
   in
  let site_address =
    match site_address_map with
      | None -> SiteSet.Map.empty
      | Some s -> s
  in
  (*get a set of site that are bond*)
  let error,sites_bond_set =
    SiteSet.Map.fold
      (fun site _ (error,current_set) ->
          SiteSet.Set.add
            parameter
	    error
	    site
            current_set)
      site_address
      (error,SiteSet.Set.empty)
  in
  (*store*)
  let error, store_sites_bond_set =
    AgentMap.set
      parameter
      error
      agent_type
      sites_bond_set
      store_sites_bond_set
  in error, store_sites_bond_set

(*combine with the old_result*)
let collect_store_bond_set parameter error bond_lhs site_address store_sites_bond_set =
  let agent_id = site_address.agent_index in
  let agent_type = site_address.agent_type in
  let error, site_address_map =
    Quick_Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_id
      bond_lhs
  in
  let site_address =
    match site_address_map with
      | None -> SiteSet.Map.empty
      | Some s -> s
  in
  (*get a set of site that are bond*)
  let error,sites_bond_set =
    SiteSet.Map.fold
      (fun site _ (error,current_set) ->
       SiteSet.Set.add parameter error
         site
         current_set)
      site_address
      (error,SiteSet.Set.empty)
  in
  (*get old*)
  let old_set =
    get_site_common_set
      parameter
      error
      agent_type
      store_sites_bond_set
  in
  let error, result_set =
    SiteSet.Set.union parameter error
      sites_bond_set
      old_set
  in
  (*store*)
  let error, store_sites_bond_set =
    AgentMap.set
      parameter
      error
      agent_type
      result_set
      store_sites_bond_set
  in error, store_sites_bond_set

(*------------------------------------------------------------------------------*)
(*-- collect binding sites in the lhs with: site -> site*)

(*Use for collecting anchor site*)
let collect_sites_bond_pair_set parameter error bond_lhs
    site_address_1
    site_address_2
    store_sites_bond_set_1
    store_sites_bond_set_2
    store_sites_bond_pair_set =
  (*the first binding agent, check at each rule, and not combine with the old result*)
  let error, store_sites_bond_set_1 =
    collect_store_bond_set_each_rule
      parameter
      error
      bond_lhs
      site_address_1
      store_sites_bond_set_1
  in
  (*the second binding agent, it is a result of anchor, combine with the old result*)
  let error, store_sites_bond_set_2 =
    collect_store_bond_set
      parameter
      error
      bond_lhs
      site_address_2
      store_sites_bond_set_2
  in
  let error, store_sites_bond_pair_set =
    (store_sites_bond_set_1, store_sites_bond_set_2)
  in
  error, store_sites_bond_pair_set

(*-- collect binding sites in the lhs with: site -> site*)

let result_sites_bond_pair_set parameter error bond_lhs release
    store_sites_bond_pair_set =
  List.fold_left (fun (error, store_sites_bond_pair_set)
    (site_address_1, site_address_2) ->
      let error, store_sites_bond_pair_set =
        error, collect_sites_bond_pair_set
          parameter
          error
          bond_lhs
          site_address_1
          site_address_2
          (fst store_sites_bond_pair_set)
          (snd store_sites_bond_pair_set)
          store_sites_bond_pair_set
      in
      error, store_sites_bond_pair_set)
    (error, store_sites_bond_pair_set)
    release

(*------------------------------------------------------------------------------*)
(*-- collect binding sites in the lhs with: site -> site*)

(*use for external flow; collect binding site at each rule and not combine
  them with old result*)

let collect_sites_bond_pair_set_external parameter error bond_lhs
    site_address_1
    site_address_2
    store_sites_bond_set_1
    store_sites_bond_set_2
    store_sites_bond_pair_set =
  let _ = fprintf stdout "print collect_sites\n"
  in
  let error, store_sites_bond_set_1 =
    collect_store_bond_set_each_rule
      parameter
      error
      bond_lhs
      site_address_1
      store_sites_bond_set_1
  in
  let error, store_sites_bond_set_2 =
    collect_store_bond_set_each_rule
      parameter
      error
      bond_lhs
      site_address_2
      store_sites_bond_set_2
  in
  let error, store_sites_bond_pair_set =
    (store_sites_bond_set_1, store_sites_bond_set_2)
  in
  error, store_sites_bond_pair_set

(*-- collect binding sites in the lhs with: site -> site*)

let result_sites_bond_pair_set_external parameter error bond_lhs release
    store_sites_bond_pair_set =
  List.fold_left (fun (error, store_sites_bond_pair_set)
    (site_address_1, site_address_2) ->
      let _ = fprintf stdout "print result_sites\n"
      in
      let error, store_sites_bond_pair_set =
        error, collect_sites_bond_pair_set_external
          parameter
          error
          bond_lhs
          site_address_1
          site_address_2
          (fst store_sites_bond_pair_set)
          (snd store_sites_bond_pair_set)
          store_sites_bond_pair_set
      in
      error, store_sites_bond_pair_set)
    (error, store_sites_bond_pair_set)
    release

(************************************************************************************)
(*SITES LHS: get site of each agent in each rule*)

let store_sites_lhs parameter error rule store_sites_lhs =
  let error, store_sites_lhs =
    Quick_Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent store_sites_lhs ->
        match agent with
       | Ghost | Unknown_agent _ -> error, store_sites_lhs
       | Agent agent | Dead_agent (agent,_,_,_) ->
          let agent_type = agent.agent_name in
          let site_list =
            SiteSet.Map.fold
              (fun site _ current_list ->
                site :: current_list)
              agent.agent_interface []
          in
          (*store*)
          let error, sites_list =
            AgentMap.set
              parameter
              error
              agent_type
              site_list
              store_sites_lhs
          in
          error, sites_list
      )
      rule.rule_lhs.views
      store_sites_lhs
  in error, store_sites_lhs

(************************************************************************************)
(*ANCHOR SITES*)

let collect_sites_anchor_set parameter error handler get_rule
    store_sites_modified_set
    store_sites_bond_pair_set
    store_sites_lhs
    store_sites_anchor_set =
  Quick_Nearly_inf_Imperatif.fold
    parameter
    error
    (fun parameter error agent_id agent store_sites_anchor_set ->
      match agent with
        | Unknown_agent _ | Ghost -> error, store_sites_anchor_set
	| Dead_agent (agent,_,_,_) | Agent agent ->
          let agent_type = agent.agent_name in
          (*get a set of modified site in the rule lhs*)
          let modified_set =
            get_site_common_set
              parameter
              error
              agent_type
              store_sites_modified_set
          in
          (*get a set of sites in the rule lhs that are bond (the first
            agent that has site that are bond*)
          let site_lhs_bond_fst_set =
            get_site_common_set
              parameter
              error
              agent_type
              (fst store_sites_bond_pair_set)
          in
          (*get a list of sites in the rule lhs*)
          let site_lhs_list =
            get_site_common_list
              parameter
              error
              agent_type
              store_sites_lhs
          in
          (*get a set of anchor sites from the first case and second case*)
          let error, anchor_set =
            anchor_set
              parameter
              error
              agent_type
              (fst store_sites_anchor_set)
              (snd store_sites_anchor_set)
          in
          (*first case: a site connected to a modified site*)
          let error, anchor_set1 =
            let rec aux acc =
              match acc with
                | [] -> error, (fst store_sites_anchor_set)
                | x :: tl ->
                  if not (SiteSet.Set.is_empty modified_set &&
                            SiteSet.Set.is_empty site_lhs_bond_fst_set)
                  then
                    begin
                      if SiteSet.Set.mem x modified_set &&
                        SiteSet.Set.mem x site_lhs_bond_fst_set
                      then
                        let anchor = snd store_sites_bond_pair_set in
                        error, anchor
                      else aux tl
                    end
                  else error, (fst store_sites_anchor_set) (*if both sets are empty then do nothing*)
            in aux (List.rev site_lhs_list)
          in
          (* second case: at least two sites, one of them belong to an anchor/modified
             site*)
          let error, anchor_set2 =
            match (List.rev site_lhs_list) with
              | [] | [_] -> error, (snd store_sites_anchor_set)
              | x :: tl ->
                let rec aux to_visit =
                  match to_visit with
                    | [] -> error, (snd store_sites_anchor_set)
                    | y :: tl' ->
                      if not (SiteSet.Set.is_empty modified_set ||
                                SiteSet.Set.is_empty anchor_set &&
                                SiteSet.Set.is_empty site_lhs_bond_fst_set)
                      then
                        begin
                          if SiteSet.Set.mem x anchor_set ||
                            SiteSet.Set.mem x modified_set &&
                            SiteSet.Set.mem y site_lhs_bond_fst_set
                          then
                            let anchor = snd store_sites_bond_pair_set in
                            error, anchor
                          else aux tl'
                        end
                      else error, (snd store_sites_anchor_set)
                in aux tl
          in
          (*PRINT*)
          let _ =
            (*get a set of anchor1*)
            let error, out_anchor_set1 =
              AgentMap.unsafe_get
                parameter
                error
                agent_type
                anchor_set1
            in
            let get_anchor_set1 =
              match out_anchor_set1 with
                | None -> SiteSet.Set.empty
                | Some s -> s
            in
            (*get a set of anchor2*)
            let error, out_anchor_set2 =
              AgentMap.unsafe_get
                parameter
                error
                agent_type
                anchor_set2
            in
            let get_anchor_set2 =
              match out_anchor_set2 with
                | None -> SiteSet.Set.empty
                | Some s -> s
            in
            (*do the union of two set*)
            let error,final_anchor_set =
              SiteSet.Set.union
		parameter
		error
                get_anchor_set1
                get_anchor_set2
            in
            (*---------------------------------------------------------------------------*)
            (*print the final set*)
            let l = SiteSet.Set.elements final_anchor_set in
            match l with
              | [] -> ()
              | _ as l' ->
                let error, agent_type_string =
                  Handler.string_of_agent parameter error handler agent_type
                in
                Printf.fprintf stdout
                  "Flow of information in the ODE semantics:agent_type:%i:%s:"
                  agent_type agent_type_string; (*TODO*)

                print_string "anchor_type:";
                print_list l'
          in
          (*result*)
          error, (anchor_set1, anchor_set2)
     ) get_rule.rule_lhs.views
          store_sites_anchor_set

(************************************************************************************)
(*INTERNAL FLOW*)

(*cartesian product: remove the self binding.
  For example: A(x,y) where both 'x,y' are modified sites.
  Two lists:
  - site_list (x,y)
  - modified_list (x,y)
  Then remove the self-binding at (x,x) and (y,y)
*)

let cartesian_prod_eq i a b =
  let rec loop a acc =
    match a with
      | [] -> List.rev acc
      | x :: xs ->
        loop xs (List.rev_append (List.rev (List.fold_left (fun acc y ->
          if x <> y
          then (i, x, y) :: acc
          else acc
        ) [] b)) acc)
  in
  loop a []

(*------------------------------------------------------------------------------*)
(*compute internal_flow: site -> modified site*)

let internal_flow_lhs_modified parameter error agent_type
    store_sites_lhs
    store_sites_modified_set =
  let result_modified_list = SiteSet.Set.elements store_sites_modified_set in
  let site_lhs =
    get_site_common_list
      parameter
      error
      agent_type
      store_sites_lhs
  in
  match site_lhs with
    | [] | [_] -> []
    | _ -> cartesian_prod_eq
      agent_type
      site_lhs
      result_modified_list

(*------------------------------------------------------------------------------*)
(*compute internal_flow: site -> anchor site*)

let internal_flow_lhs_anchor parameter error agent_type
    store_sites_lhs
    anchor_set =
  let site_lhs =
    get_site_common_list
      parameter
      error
      agent_type
      store_sites_lhs
  in
  let anchor_list = SiteSet.Set.elements anchor_set in
  match site_lhs with
    | [] | [_] -> []
    | _ -> cartesian_prod_eq
      agent_type
      site_lhs
      anchor_list

(*------------------------------------------------------------------------------*)
(*INTERNAL FLOW*)

let collect_internal_flow parameter error handler get_rule
    store_sites_lhs
    store_sites_modified_set
    store_sites_anchor_set
    store_internal_flow =
  Quick_Nearly_inf_Imperatif.fold
    parameter
    error
    (fun parameter error agent_id agent store_internal_flow ->
      match agent with
        | Unknown_agent _ | Ghost -> error, store_internal_flow
	| Dead_agent (agent,_,_,_)
	| Agent agent ->
          let agent_type = agent.agent_name in
          let modified_set =
            get_site_common_set
              parameter
              error
              agent_type
              store_sites_modified_set
          in
          let error,anchor_set =
            anchor_set
              parameter
              error
              agent_type
              (fst store_sites_anchor_set)
              (snd store_sites_anchor_set)
          in
          (*1st: site -> modified site*)
          let get_internal_flow1 =
            internal_flow_lhs_modified
              parameter
              error
              agent_type
              store_sites_lhs
              modified_set
          in
          (*store*)
          let error, internal_flow1 =
            AgentMap.set
              parameter
              error
              agent_type
              get_internal_flow1
              (fst store_internal_flow)
          in
          (*------------------------------------------------------------------------------*)
          (*PRINT*)
          let _ =
            List.iter (fun (agent_type, site_type, modified_site_type) ->
              let error, agent_type_string =
                Handler.string_of_agent parameter error handler agent_type
              in
              let error, site_type_string =
                Handler.string_of_site parameter error handler agent_type site_type
              in
              let error, modified_site_type_string =
                Handler.string_of_site parameter error handler agent_type modified_site_type
              in
              fprintf stdout "Flow of information in the ODE semantics:Internal flow:\n- agent_type:%i:%s:site_type:%i:%s -> agent_type:%i::%s:modified_type:%i:%s\n"
                agent_type agent_type_string
                site_type site_type_string
                agent_type agent_type_string
                modified_site_type modified_site_type_string
            ) get_internal_flow1
          in
          (*------------------------------------------------------------------------------*)
          (*2nd: site -> anchor site*)
          let get_internal_flow2 =
            internal_flow_lhs_anchor
              parameter
              error
              agent_type
              store_sites_lhs
              anchor_set
          in
          (*store*)
          let error, internal_flow2 =
            AgentMap.set
              parameter
              error
              agent_type
              get_internal_flow2
              (snd store_internal_flow)
          in
          (*----------------------------------------------------------------------------*)
          (*PRINT*)
          let _ =
            List.iter (fun (agent_type, site_type, y) ->
              let error, agent_type_string =
                Handler.string_of_agent parameter error handler agent_type
              in
              let error, site_type_string =
                Handler.string_of_site parameter error handler agent_type site_type
              in
              fprintf stdout "Flow of information in the ODE semantics:Internal flow:\n- agent_type:%i:%s:site_type:%i:%s -> agent_type:%i:%s:anchor_type:%i\n"
                agent_type agent_type_string
                site_type site_type_string
                agent_type agent_type_string
                y
            ) get_internal_flow2
          in
          (*result*)
          error, (internal_flow1, internal_flow2)
    ) get_rule.rule_lhs.views
    store_internal_flow

(************************************************************************************)
(*EXTERNAL FLOW:
  A binding between two agents:
  agent with an anchor -> agent with a modified site.
  For example: A(x), B(x)
  where 'x' of A is an anchor, and bind to 'x' of B ('x' is a modified site).
  => A(x) -> B(x)
*)

(*TODO: define castesian for set type*)

let cartesian_prod_external i anchor_set i' bond_fst_list bond_snd_set =
  let anchor_list = SiteSet.Set.elements anchor_set in
  let rec loop anchor_list acc =
    match anchor_list with
      | [] -> List.rev acc
      | x :: xs ->
        loop xs (List.rev_append (List.rev (List.fold_left (fun acc y ->
          if not (SiteSet.Set.is_empty anchor_set &&
                    SiteSet.Set.is_empty bond_snd_set)
          then
            begin
              if SiteSet.Set.mem x anchor_set &&
                SiteSet.Set.mem x bond_snd_set
              then (i, x, i', y) :: acc
              else acc
            end
          else List.rev acc (*FIXME*)
        ) [] bond_fst_list)) acc)
  in
  loop anchor_list []

let collect_external_flow parameter error release
    store_sites_bond_pair_set_external
    store_sites_anchor_set1
    store_sites_anchor_set2
    store_external_flow =
  List.fold_left (fun (error, store_external_flow) (site_address_1, site_address_2) ->
    let agent_type_1 = site_address_1.agent_type in
    let agent_type_2 = site_address_2.agent_type in
    let _ =
      fprintf stdout "agent_type1:%i\n" agent_type_1
    in
    (*collect site that are bond in the lhs; the first element in a pair*)
    let bond_fst_set =
      get_site_common_set
        parameter
        error
        agent_type_1
        (fst store_sites_bond_pair_set_external)
    in
    (*collect site that are bond in the lhs; the second element in a pair*)
    let bond_snd_set =
      get_site_common_set
        parameter
        error
        agent_type_2
        (snd store_sites_bond_pair_set_external)
    in
    (*get a set of anchor sites*)
    let anchor_set =
      fold_anchor_set
        parameter
        error
        store_sites_anchor_set1
        store_sites_anchor_set2
    in
    let bond_fst_list = SiteSet.Set.elements bond_fst_set in
    let store_result =
      cartesian_prod_external
        agent_type_2
        anchor_set
        agent_type_1
	bond_fst_list
        bond_snd_set
    in
    (*FIXME:PRINT*)
    let _ =
      let rec aux acc =
        match acc with
        | [] -> acc
        | (agent_type,x,agent_type',y) :: tl ->
          fprintf stdout
            "Flow of information in the ODE semantics:External flow:\n- agent_type:%i:anchor_type:%i -> agent_type:%i:modified_type:%i\n"
            agent_type x agent_type' y;
          aux tl
      in aux store_result
    in
    (*------------------------------------------------------------------------------*)
    (*PRINT*)
    (*let _ =
      List.iter (fun (agent_type_x, site_type_x, agent_type_y, site_type_y) ->
        (*let error, agent_type_x_string =
          Handler.string_of_agent parameter error handler agent_type_x
        in
        let error, agent_type_y_string =
          Handler.string_of_agent parameter error handler agent_type_y
        in
        let error, site_type_x_string =
          Handler.string_of_site parameter error handler agent_type_x site_type_x
        in
        let error, site_type_y_string =
          Handler.string_of_site parameter error handler agent_type_y site_type_y
        in*)
        fprintf stdout "Flow of information in the ODE semantics:External flow:\n- agent_type:%i:anchor_type:%i -> agent_type:%i:modified_type:%i\n"
          agent_type_x site_type_x
          agent_type_y site_type_y
      ) collect_external_flow
    in*)
    (*------------------------------------------------------------------------------*)
    error, store_result)
    (error, store_external_flow)
    release

(************************************************************************************)
(*RULE*)

let scan_rule parameter error handler get_rule ode_class =
  let release = get_rule.actions.release in
  let bond_lhs = get_rule.rule_lhs.bonds in
  (*------------------------------------------------------------------------------*)
  (*a) collect modified sites*)
  let error, store_sites_modified_set =
    collect_sites_modified_set
      parameter
      error
      get_rule
      handler
      ode_class.store_sites_modified_set
  in
  (*------------------------------------------------------------------------------*)
  (*b) collect binding sites*)
  let error, store_sites_bond_pair_set =
    result_sites_bond_pair_set
      parameter
      error
      bond_lhs
      release
      ode_class.store_sites_bond_pair_set
  in
  let error, store_sites_bond_pair_set_external =
    result_sites_bond_pair_set_external
      parameter
      error
      bond_lhs
      release
      ode_class.store_sites_bond_pair_set_external
  in
  (*------------------------------------------------------------------------------*)
  (*c) collect sites from the lhs rule at each rule without combining their sites*)
  let error, store_sites_lhs =
    store_sites_lhs
      parameter
      error
      get_rule
      ode_class.store_sites_lhs
  in
  (*------------------------------------------------------------------------------*)
  (*d) 1st: anchor sites (first case): A(x), B(x): 'x' of A is a modified site,
    'x' of A bind to 'x' of B => B(x) is an anchor site;
    - 2nd: collect anchor sites (second case): a site connected to a site in an
    agent with an anchor, the second agent should contain at least an anchor on
    another site. For example: A(x,y), B(x): Agent A where site x is an
    anchor/modified, y bind to x in agent B. Site x of B is an anchor.
  *)
  let error, store_sites_anchor_set =
    collect_sites_anchor_set
      parameter
      error
      handler
      get_rule
      store_sites_modified_set
      store_sites_bond_pair_set
      store_sites_lhs
      ode_class.store_sites_anchor_set
  in
  (*------------------------------------------------------------------------------*)
  (*e) compute internal_flow: site -> modified/anchor site*)
  let error, store_internal_flow =
    collect_internal_flow
      parameter
      error
      handler
      get_rule
      store_sites_lhs
      store_sites_modified_set
      store_sites_anchor_set
      ode_class.store_internal_flow
  in
  (*------------------------------------------------------------------------------*)
  (*f) external flow: a -> b, if 'a': anchor site or 'b':modified site*)
  (*FIXME*)
  let error, store_external_flow =
    let _ =
      fprintf stdout "external flow\n"
    in
    collect_external_flow
      parameter
      error
      release
      store_sites_bond_pair_set_external
      (fst store_sites_anchor_set)
      (snd store_sites_anchor_set)
      ode_class.store_external_flow
  in
  (*------------------------------------------------------------------------------*)
  (*return value of ode_class*)
  error,
  {
    store_sites_modified_set            = store_sites_modified_set;
    store_sites_bond_pair_set           = store_sites_bond_pair_set;
    store_sites_bond_pair_set_external  = store_sites_bond_pair_set_external;
    store_sites_lhs                     = store_sites_lhs;
    store_sites_anchor_set              = store_sites_anchor_set;
    store_internal_flow                 = store_internal_flow;
    store_external_flow                 = store_external_flow
  }

(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler compiled =
  let error, init           = AgentMap.create parameter error 0 in
  let init_pair             = (init, init) in
  let error, init_lhs       = AgentMap.create parameter error 0 in
  let error, init_internal1 = AgentMap.create parameter error 0 in
  let error, init_internal2 = AgentMap.create parameter error 0 in
  let error, init_external  = AgentMap.create parameter error 0 in
  (*init state of ode_class*)
  let init_ode =
    {
      store_sites_modified_set            = init;
      store_sites_bond_pair_set           = init_pair;
      store_sites_bond_pair_set_external  = init_pair;
      store_sites_lhs                     = init_lhs;
      store_sites_anchor_set              = (init, init);
      store_internal_flow                 = (init_internal1, init_internal2);
      store_external_flow                 = init_external
    }
  in
  let error, ode_class =
    AgentMap.fold
      parameter error
      (fun parameter error rule_id rule ode_class ->
        (*map rule of type int to rule of type string*)
        let error, rule_string =
          Handler.string_of_rule parameter error handler compiled rule_id
        in
	fprintf stdout "Flow of information in the ODE semantics:%s\n" rule_string;
        scan_rule
          parameter
          error
          handler
          rule.e_rule_c_rule
          ode_class
      ) compiled.rules init_ode
  in
  error, ode_class

(************************************************************************************)
(*MAIN*)

let ode_fragmentation parameter error handler compiled =
  let error, result =
    scan_rule_set parameter error handler compiled
  in
  error, result
*)
