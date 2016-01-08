(**
  * bdu_fixpoint_iteration.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 9th of October
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Cckappa_sig
open Bdu_analysis_type
open SetMap
open Mvbdu_sig
open Boolean_mvbdu
open Memo_sig
open Site_map_and_set
open Covering_classes_type
open Bdu_build
open Fifo
open Printf
open Mvbdu_wrapper

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Bdu_fixpoint_iteration") message exn (fun () -> default) 

let local_trace = false

let dump_channel parameter  f =
  if local_trace ||  Remanent_parameters.get_trace parameter
  then f (Remanent_parameters.get_log parameter)

let dump_formatter parameter  f =
  if local_trace ||  Remanent_parameters.get_trace parameter
  then f (Remanent_parameters.get_formatter parameter) 

(************************************************************************************)
(*update bdu:
  - (bdu_X U bdu_creation) U [\rho[update_views] | \rho \in bdu_X (inter) bdu_test views]
*)

(*Xn intersection with bdu_test and modif and then union with X_n*)

let compute_bdu_update_aux parameter handler error bdu_test list_a bdu_X =
  (*do the intersection X_n and bdu_test*)
  let error, handler, bdu_inter =
    Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error bdu_X bdu_test
  in
  (*redefine with modification list*)
  let error, handler, bdu_redefine = 
    Mvbdu_wrapper.Mvbdu.mvbdu_redefine parameter handler error bdu_inter list_a
  in
  (*do the union of bdu_redefine and bdu_X*)
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_redefine bdu_X
  in
  error, handler, bdu_result

(************************************************************************************)
(*bdu update function for views*)

let compute_bdu_update_views parameter handler error bdu_test list_a bdu_X =
  let error, handler, bdu_result =
    compute_bdu_update_aux
      parameter
      handler
      error
      bdu_test
      list_a
      bdu_X
  in
  error, handler, bdu_result

(************************************************************************************)
(*bdu update function deal with agent creation*)

let compute_bdu_update_creation parameter handler error bdu_creation bdu_X =
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_creation bdu_X
  in
  error, handler, bdu_result

(************************************************************************************)
(*bdu update function deal with side effects*)

let compute_bdu_update_side_effects parameter handler error bdu_test list_a bdu_X =
  let error, handler, bdu_result =
    compute_bdu_update_aux
      parameter
      handler
      error
      bdu_test
      list_a
      bdu_X
  in
  error, handler, bdu_result
    
(************************************************************************************)
(*write a function add update(c) into working list*)

let add_update_to_wl parameter error handler_kappa compiled agent_type cv_id 
    store_covering_classes_modification_update_full wl =
  let error, (_, s1) = 
    match Int2Map_CV_Modif.Map.find_option (agent_type, cv_id)
      store_covering_classes_modification_update_full
    with
    | None -> error, ([], Site_map_and_set.Set.empty)
    | Some (l, s) -> error, (l, s)
  in 
  (*-----------------------------------------------------------------------*)
  (*print working list information*)
  let _ =
    if (Remanent_parameters.get_dump_reachability_analysis_wl parameter) 
    then
      let parameters_cv =
        Remanent_parameters.update_prefix parameter ""
      in
      if (Remanent_parameters.get_trace parameters_cv)
      then Printf.fprintf (Remanent_parameters.get_log parameters_cv) "\n";
      dump_channel parameter
        (fun stderr ->
          let _ = 
            let error, agent_string =
              Handler.string_of_agent parameter error handler_kappa agent_type
            in
            Printf.fprintf stderr "deal with agent_type:%i:%s:covering_class_id:%i:\n"
              agent_type agent_string cv_id;
            Printf.fprintf stderr "add:\n";
            Site_map_and_set.Set.iter 
	    (fun rule_id ->
              (*mapping rule_id of type int -> string*)
              let error, rule_id_string =
                Handler.string_of_rule parameter error handler_kappa
                  compiled rule_id
              in
              Printf.fprintf stderr " (%s)\n" rule_id_string) s1;
            Printf.fprintf stderr "inside a working list."
          in
          ())
    else ()
  in
  (*-----------------------------------------------------------------------*)
  Site_map_and_set.Set.fold (fun rule_id (error, wl) ->
    let error, wl = IntWL.push parameter error rule_id wl in
    error, wl
  ) s1 (error, wl)
    
(************************************************************************************)
(*fixpoint*)

(*from rule_id get bdu_creation_map, bdu_test_map, and modif_list_map*)

let collect_bdu_creation_and_modif_list
    parameter
    error
    rule_id
    store_proj_bdu_creation_restriction_map
    store_proj_modif_list_restriction_map
    store_proj_bdu_test_restriction_map
    =
  let error, bdu_creation_map = 
    match Map_final_creation_bdu.Map.find_option rule_id
      store_proj_bdu_creation_restriction_map
    with
    | None -> error, Map_agent_type_creation_bdu.Map.empty
    | Some map -> error, map
  in
  let error, modif_list_map =
    match Map_final_modif_list.Map.find_option rule_id
      store_proj_modif_list_restriction_map
    with
    | None -> error, Map_agent_id_modif_list.Map.empty
    | Some map -> error, map
  in
   let error, bdu_test_map = 
     match 
       Map_final_test_bdu.Map.find_option rule_id 
         store_proj_bdu_test_restriction_map
     with
     | None -> error, Map_agent_id_test_bdu.Map.empty
     | Some map -> error, map
   in
   error, (bdu_creation_map, modif_list_map, bdu_test_map)

(*form rule_id get bdu_potential_map, and modif_list_map (when state of the site is free)*)

let collect_bdu_potential_and_list parameter error rule_id
    store_proj_bdu_potential_restriction_map
    store_proj_potential_list_restriction_map =
  let error, bdu_potential_map =
    match Map_final_potential_bdu.Map.find_option rule_id 
      store_proj_bdu_potential_restriction_map 
    with
    | None -> error, Map_agent_type_potential_bdu.Map.empty
    | Some map -> error, map
  in
  let error, potential_list_map =
    match Map_final_potential_list.Map.find_option rule_id
      store_proj_potential_list_restriction_map
    with
    | None -> error, Map_agent_type_potential_list.Map.empty
    | Some map -> error, map
  in
  error, (bdu_potential_map, potential_list_map)

(************************************************************************************)
(*check is_enable*)

let is_enable parameter handler error bdu_false 
    rule_id bdu_proj_views store_bdu_update_map =
  let is_enable =
    Map_triple_views.Map.for_all
      (fun (agent_id, agent_type, cv_id)  bdu_test ->
        let error, bdu_X =
          match Map_bdu_update.Map.find_option (agent_type, cv_id) store_bdu_update_map
          with
          | None -> error, bdu_false
          | Some bdu -> error, bdu
        in
        (*do the intersection of bdu_test and bdu_X*)
        let error, handler, bdu_inter =
          Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error bdu_test bdu_X
        in
        (*check is enable*)
        if Mvbdu_wrapper.Mvbdu.equal bdu_inter bdu_false
        then false
        else true
      ) bdu_proj_views
  in
  error, is_enable

(************************************************************************************)
(*compute view that has new view and new bond*)

let compute_new_views parameter handler error
    handler_kappa 
    compiled
    agent_type
    cv_id 
    is_new_view 
    store_covering_classes_modification_update_full
    wl_tl
    store_result =
  let error, (handler, new_wl, store_result) =
    if is_new_view
    then
      let error, new_wl =
        add_update_to_wl
          parameter
          error
          handler_kappa
          compiled
	  agent_type
	  cv_id 
          store_covering_classes_modification_update_full
          wl_tl
      in
      error, (handler, new_wl, store_result)
    else
      error, (handler, wl_tl, store_result)
  in
  error, (handler, new_wl, store_result)

(************************************************************************************)
(*compute views that is enabled*)

let compute_views_enabled parameter handler error
    handler_kappa
    compiled
    bdu_true 
    bdu_false
    rule_id
    bdu_test_map
    bdu_creation_map 
    modif_list_map 
    bdu_potential_map
    potential_list_map
    wl_tl
    store_covering_classes_modification_update_full
    bdu_proj_views
    store_bdu_update_map =
  (*-----------------------------------------------------------------------*)
  (* add_link should collect the list/set of (agent_type,cv_id) for which
     something has changed, so that add_update_to_wl can focus on these
     pairs *)
  let add_link handler (agent_type, cv_id) bdu_update store_result =
    let error, bdu_old =
      match Map_bdu_update.Map.find_option (agent_type, cv_id) store_result
      with
      | None -> error, bdu_false
      | Some old -> error, old
    in
    let error, handler, bdu_union =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_update bdu_old
    in  
    (*-----------------------------------------------------------------------*)
    if Mvbdu_wrapper.Mvbdu.equal bdu_union bdu_old
    then
      error, handler, false, bdu_union, store_result
    else
      let error, handler, bdu_diff =
	Mvbdu_wrapper.Mvbdu.mvbdu_xor parameter handler error bdu_union bdu_old
      in
      let store_result =
        Map_bdu_update.Map.add (agent_type, cv_id) bdu_union store_result
      in
      error, handler, true, bdu_diff, store_result
  in
  (*-----------------------------------------------------------------------*)
  (*deal with views*)
  let error, (handler, wl_tl, store_result) =
    Map_triple_views.Map.fold
      (fun (agent_id, agent_type, cv_id) _ (error, (handler, wl_tl, store_result)) ->
        (*-----------------------------------------------------------------------*)
        (*print working list information*)
        let _ = 
          if (Remanent_parameters.get_dump_reachability_analysis_wl parameter) 
          then
            let parameters_cv =
              Remanent_parameters.update_prefix parameter ""
            in
            if (Remanent_parameters.get_trace parameters_cv)
            then 
              let error, agent_string =
                Handler.string_of_agent parameter error handler_kappa agent_type
              in
              Printf.fprintf (Remanent_parameters.get_log parameters_cv) "\n";
              dump_channel parameter 
                (fun stderr -> Printf.fprintf stderr
                  "\t at agent_id:%i:agent_type:%i:%s:covering_classes_id:%i (working list output):"
                  agent_id agent_type agent_string cv_id)
          else ()
        in 
        (*-----------------------------------------------------------------------*)
        (*print different of views information*)
        let _ = 
          if (Remanent_parameters.get_dump_reachability_analysis_diff parameter) 
          then
            let parameters_cv =
              Remanent_parameters.update_prefix parameter ""
            in
            if (Remanent_parameters.get_trace parameters_cv)
            then 
              let error, agent_string =
                Handler.string_of_agent parameter error handler_kappa agent_type
              in
              Printf.fprintf (Remanent_parameters.get_log parameters_cv) "\n";
              dump_channel parameter 
                (fun stderr -> Printf.fprintf stderr
                  "\t at agent_id:%i:agent_type:%i:%s:covering_classes_id:%i (different views output):"
                  agent_id agent_type agent_string cv_id)
            else ()
        in 
        (*-----------------------------------------------------------------------*)
        let error, bdu_X =
          match Map_bdu_update.Map.find_option (agent_type, cv_id) store_result with
          | None -> error, bdu_false
          | Some bdu -> error, bdu
        in
        let error, bdu_test =
          match Map_triple_views.Map.find_option (agent_id, agent_type, cv_id) bdu_test_map
          with
          | None -> error, bdu_true
          | Some bdu -> error, bdu
        in
        (*list of list_a in modification*)
        let error, list =
          match Map_agent_id_modif_list.Map.find_option agent_id modif_list_map with
          | None -> error, []
          | Some l -> error, l
        in
        let error, handler, bdu_update =
          List.fold_left (fun _ list_a ->
            compute_bdu_update_views
              parameter
              handler
              error
              bdu_test
              list_a
              bdu_X
          ) (error, handler, bdu_false) list
        in
        (*-----------------------------------------------------------------------*)
        let error, handler, is_new_view, bdu_diff, store_result =
          add_link handler (agent_type, cv_id) bdu_update store_result
        in
        (*-----------------------------------------------------------------------*)
        (*print working list of new view*)
        let _ =
          if is_new_view 
          then
            let _ =
              (*print information of working list*)
              begin
                if (Remanent_parameters.get_dump_reachability_analysis_wl parameter) 
                then
                  let parameters_cv =
                    Remanent_parameters.update_prefix parameter ""
                  in
                  if (Remanent_parameters.get_trace parameters_cv)
                  then
                    dump_channel parameter (fun stderr ->
	              Printf.fprintf stderr "\n\t\tdiscovering a new views (working list output).\n")
                  else ()
              end
            in
            (*print information of different views*)
            let _ =
              if (Remanent_parameters.get_dump_reachability_analysis_diff parameter) 
              then
                let parameters_cv =
                  Remanent_parameters.update_prefix parameter ""
                in
                if (Remanent_parameters.get_trace parameters_cv)
                then
                  dump_channel parameter (fun stderr ->
	            Printf.fprintf stderr "\t\tdiscovering a new views (different views output):\n";
                    Mvbdu_wrapper.Mvbdu.print stderr "" bdu_diff)
                else ()
            in
            ()    
          (*-----------------------------------------------------------------------*)
          (*deal with the case boolean return false*)
          else
            let _ =
              (*print information of working list*)
              begin
                if (Remanent_parameters.get_dump_reachability_analysis_wl parameter) 
                then
                  let parameters_cv =
                    Remanent_parameters.update_prefix parameter ""
                  in
                  if (Remanent_parameters.get_trace parameters_cv)
                  then
                    dump_channel parameter (fun stderr -> Printf.fprintf stderr
                      "\n\t\tnothing has changed (working list output).\n")
                  else ()
              end
            in
            (*print information of different views*)
            let _ =
              if (Remanent_parameters.get_dump_reachability_analysis_diff parameter) 
              then
                let parameters_cv =
                  Remanent_parameters.update_prefix parameter ""
                in
                if (Remanent_parameters.get_trace parameters_cv)
                then
                  dump_channel parameter (fun stderr -> Printf.fprintf stderr
                    "\t\tnothing has changed (different views output).\n")
                else ()
            in
            ()
        in      
        (*-----------------------------------------------------------------------*)
        let error, (handler, new_wl, store_result) =
          compute_new_views
            parameter
            handler
            error
            handler_kappa
            compiled
	    agent_type
	    cv_id
            is_new_view
            store_covering_classes_modification_update_full
            wl_tl
            store_result
        in
        error, (handler, new_wl, store_result)          
      )
      bdu_proj_views
      (error, (handler, wl_tl, store_bdu_update_map))
  in
  (*-----------------------------------------------------------------------*)
  (*start to deal with agent creation*)
  let error, (handler, wl_tl, store_result) =
    Map_agent_type_creation_bdu.Map.fold
      (fun (agent_type, cv_id) bdu_creation (error, (handler, wl_tl, store_result)) ->
       let error, bdu_X =
	  match Map_bdu_update.Map.find_option (agent_type, cv_id) store_result
	  with
	  | None -> error, bdu_false
	  | Some bdu -> error, bdu
	in
        let error, handler, bdu_update =
          compute_bdu_update_creation
            parameter
            handler
            error
            bdu_creation
            bdu_X
	in
	let error, handler, is_new_view, bdu_diff, store_result =
          add_link handler (agent_type, cv_id) bdu_update store_result
        in
        (*-----------------------------------------------------------------------*)
        let error, (handler, wl_tl, store_result) =
          compute_new_views
            parameter
            handler
            error
            handler_kappa
            compiled
	    agent_type
	    cv_id 
            is_new_view
            store_covering_classes_modification_update_full
            wl_tl
            store_result
        in
        error, (handler, wl_tl, store_result)          
      )
      bdu_creation_map
      (error, (handler, wl_tl, store_result))
  in 
  (*-----------------------------------------------------------------------*)
  (*start fo deal with side effects*)
  let error, (handler, wl_tl, store_result) =
    Map_agent_type_potential_bdu.Map.fold 
      (* JF: to do use a fold2 *)
      (fun (agent_type, cv_id) bdu_test (error, (handler, wl_tl, store_result)) ->
        let error, bdu_X =
	  match Map_bdu_update.Map.find_option (agent_type, cv_id) store_result
	  with
	  | None -> error, bdu_false
	  | Some bdu -> error, bdu
	in
        let error, list = 
          match
            Map_agent_type_potential_list.Map.find_option agent_type potential_list_map
          with
          | None -> error, []
          | Some l -> error, l
        in
        let error, handler, bdu_update =
          List.fold_left (fun _ list_a ->
            let error, handler, bdu_update =
              compute_bdu_update_side_effects
                parameter
                handler
                error
                bdu_test
                list_a
                bdu_X
            in
            error, handler, bdu_update
          ) (error, handler, bdu_false) list
        in
	let error, handler, is_new_view, bdu_diff, store_result =
          add_link handler (agent_type, cv_id) bdu_update store_result
        in
        (*-----------------------------------------------------------------------*)
        let error, (handler, wl_tl, store_result) =
          compute_new_views
            parameter
            handler
            error
            handler_kappa
            compiled
	    agent_type
	    cv_id
            is_new_view
            store_covering_classes_modification_update_full
            wl_tl
            store_result
        in
        error, (handler, wl_tl, store_result)          
      )
      bdu_potential_map
      (*store_proj_bdu_views*)
      (error, (handler, wl_tl, store_result))
  in 
  error, (handler, wl_tl, store_result)

(************************************************************************************)
(*fixpoint iteration with/without initial state*)

let collect_bdu_fixpoint_with_init parameter handler error 
    handler_kappa
    compiled
    bdu_true
    bdu_false
    (wl_creation:Fifo.IntWL.WSet.elt list * Fifo.IntWL.WSet.elt list *
       Fifo.IntWL.WSet.t)
    store_proj_bdu_creation_restriction_map
    store_proj_modif_list_restriction_map
    store_proj_bdu_test_restriction_map
    store_proj_bdu_potential_restriction_map
    store_proj_potential_list_restriction_map
    store_bdu_test_restriction_map
    store_proj_bdu_views
    store_covering_classes_modification_update_full
    store_bdu_init_restriction_map
    =
  (*-----------------------------------------------------------------------*)
  let add_link parameter handler (agent_type, cv_id) bdu store_result =
    let bdu_old =
      Map_bdu_update.Map.find_default bdu_false (agent_type, cv_id) store_result
    in
    let error, handler, bdu_union =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_old bdu
    in
    let result_map =
      Map_bdu_update.Map.add (agent_type, cv_id) bdu_union store_result
    in
    error, result_map
  in
  (*-----------------------------------------------------------------------*)
  (*in case having initial state the bdu_iter will be the union of bdu_init
    and bdu_iter*)
  let error, store_bdu_fixpoint_init_map =
    Map_creation_bdu.Map.fold
      (fun (agent_type, rule_id, cv_id) bdu (error, store_result) ->
        let error, store_result =
          add_link parameter handler (agent_type, cv_id) bdu store_result
        in
        error, store_result
      )
      store_bdu_init_restriction_map
      (error, Map_bdu_update.Map.empty)
  in
  (*-----------------------------------------------------------------------*)
  (*add update(c) into working list*)
  let error, wl_init_creation = 
    Map_bdu_update.Map.fold 
      (fun (agent_type, cv_id) _ (error, wl_init_creation) -> 
	add_update_to_wl 
          parameter
          error 
          handler_kappa
          compiled
          agent_type
          cv_id 
          store_covering_classes_modification_update_full
          wl_init_creation)
      store_bdu_fixpoint_init_map
      (error, wl_creation)
  in
  (*-----------------------------------------------------------------------*)
  (*iterate function*)
  let rec aux acc_wl (error, handler, store_bdu_fixpoint_init_map) =
    if IntWL.is_empty acc_wl
    then
      error, (handler, store_bdu_fixpoint_init_map)
    else
      (*-----------------------------------------------------------------------*)
      (*pop the first element (rule_id) in this working list*)
      let error, (rule_id_op, wl_tl) = IntWL.pop parameter error acc_wl in
      match rule_id_op with
      | None -> 
        warn parameter error (Some "888") Exit (handler, store_bdu_fixpoint_init_map)
      | Some rule_id ->
        (*----------------------------------------------------------------------*)
        (*compute bdu_creation, bdu_test and modif_list for this rule_id*)
        (*output of rule that is enabled*)
    	let _ = 
          if (Remanent_parameters.get_dump_reachability_analysis_iteration parameter) 
          then
            let parameters_cv =
              Remanent_parameters.update_prefix parameter ""
            in
            if (Remanent_parameters.get_trace parameters_cv)
            then 
              (*mapping rule_id of type int -> string*)
              let error, rule_id_string =
                Handler.string_of_rule parameter error handler_kappa
                  compiled rule_id
              in
              Printf.fprintf (Remanent_parameters.get_log parameters_cv) "\n";
              dump_channel parameter 
                (fun stderr -> Printf.fprintf stderr "try applying %s (iteration output):" rule_id_string)
          else ()
        in       
        (*--------------------------------------------------------------------*)
        (*output of working list*)
    	let _ = 
          if (Remanent_parameters.get_dump_reachability_analysis_wl parameter)
          then
            let parameters_cv =
              Remanent_parameters.update_prefix parameter ""
            in
            if (Remanent_parameters.get_trace parameters_cv)
            then 
              (*mapping rule_id of type int -> string*)
              let error, rule_id_string =
                Handler.string_of_rule parameter error handler_kappa
                  compiled rule_id
              in
              Printf.fprintf (Remanent_parameters.get_log parameters_cv) "\n";
              dump_channel parameter 
                (fun stderr -> Printf.fprintf stderr "try applying %s (working list output)." rule_id_string)
          else ()
        in
        (*--------------------------------------------------------------------*)
        (*output of different bdu*)
    	let _ = 
          if (Remanent_parameters.get_dump_reachability_analysis_diff parameter) 
          then
            let parameters_cv =
              Remanent_parameters.update_prefix parameter ""
            in
            if (Remanent_parameters.get_trace parameters_cv)
            then
              (*mapping rule_id of type int -> string*)
              let error, rule_id_string =
                Handler.string_of_rule parameter error handler_kappa
                  compiled rule_id
              in
              Printf.fprintf (Remanent_parameters.get_log parameters_cv) "\n";
              dump_channel parameter 
                (fun stderr -> Printf.fprintf stderr "try applying %s (different views output)." rule_id_string)
          else ()
        in
        (*--------------------------------------------------------------------*)
        let error, bdu_proj_views =
	  match Map_rule_id_views.Map.find_option rule_id store_proj_bdu_views with
	  | None -> error, Map_triple_views.Map.empty
	  | Some m -> error, m
	in
        (*--------------------------------------------------------------------*)
        let error, (bdu_creation_map, modif_list_map, bdu_test_map) =
          collect_bdu_creation_and_modif_list
            parameter
            error
            rule_id
            store_proj_bdu_creation_restriction_map
            store_proj_modif_list_restriction_map
            store_proj_bdu_test_restriction_map
        in
        (*--------------------------------------------------------------------*)
        let error, (bdu_potential_map, potential_list_map) =
          collect_bdu_potential_and_list
            parameter
            error
            rule_id
            store_proj_bdu_potential_restriction_map
            store_proj_potential_list_restriction_map
        in
        (*--------------------------------------------------------------------*)
        (*is for all bdu_test satisfy a covering_class?*)
        let error, is_enable = 
          is_enable
            parameter
            handler
            error
            bdu_false
            rule_id
            bdu_proj_views
            store_bdu_fixpoint_init_map
        in
        (*-----------------------------------------------------------------------*)
        begin
          if is_enable
          then
            (*-----------------------------------------------------------------------*)
            (*output of rule that is enabled*)
            let _ =
              if (Remanent_parameters.get_dump_reachability_analysis_iteration parameter) 
              then
                let parameters_cv =
                  Remanent_parameters.update_prefix parameter ""
                in
                if (Remanent_parameters.get_trace parameters_cv)
                then
                  dump_channel parameter 
                    (fun stderr -> Printf.fprintf stderr "\n\t\"enabled\"")
                else
                  ()
            in
            (*-----------------------------------------------------------------------*)
            let error, (handler, new_wl, store_new_result) =
              compute_views_enabled
                parameter
                handler
                error
                handler_kappa
                compiled
                bdu_true
                bdu_false
		rule_id 
		bdu_proj_views
                bdu_creation_map
                modif_list_map
                bdu_potential_map
                potential_list_map
                wl_tl
                store_covering_classes_modification_update_full
                bdu_proj_views
                store_bdu_fixpoint_init_map
            in
            aux new_wl (error, handler, store_new_result)
          else
            (*-----------------------------------------------------------------------*)
            (*output of rule that is disabled*)
            let _ =
              if (Remanent_parameters.get_dump_reachability_analysis_iteration parameter) 
              then
                let parameters_cv =
                  Remanent_parameters.update_prefix parameter ""
                in
                if (Remanent_parameters.get_trace parameters_cv)
                then
                  dump_channel parameter 
                    (fun stderr -> Printf.fprintf stderr "\n\t\"disabled\"") 
                else ()
            in
            (*-----------------------------------------------------------------------*)
            aux wl_tl (error, handler, store_bdu_fixpoint_init_map)
        end
  in
  (*start with init_map and union with initial state*)
  aux wl_init_creation (error, handler, store_bdu_fixpoint_init_map)

(************************************************************************************)
(*final fixpoint iteration*)

let collect_bdu_fixpoint_map parameter handler error 
    handler_kappa
    compiled
    wl_creation
    store_proj_bdu_creation_restriction_map
    store_proj_modif_list_restriction_map
    store_proj_bdu_test_restriction_map
    store_proj_bdu_potential_restriction_map
    store_proj_potential_list_restriction_map
    store_bdu_test_restriction_map
    store_proj_bdu_views
    store_covering_classes_modification_update_full
    store_bdu_init_restriction_map
    =
  let error, handler, bdu_false = 
    Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error
  in 
  let error, handler, bdu_true = 
    Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error 
  in
  (*-----------------------------------------------------------------------*)
  (*fixpoint*)
  let error, (handler, store_bdu_fixpoint_map) =
     collect_bdu_fixpoint_with_init
       parameter
       handler
       error
       handler_kappa
       compiled
       bdu_true
       bdu_false
       wl_creation
       store_proj_bdu_creation_restriction_map
       store_proj_modif_list_restriction_map
       store_proj_bdu_test_restriction_map
       store_proj_bdu_potential_restriction_map
       store_proj_potential_list_restriction_map
       store_bdu_test_restriction_map
       store_proj_bdu_views
       store_covering_classes_modification_update_full
       store_bdu_init_restriction_map
  in
  error, (handler, store_bdu_fixpoint_map)

(************************************************************************************)
(*BACKUP: DO NOT DELETE ME*)

(*side effects in the case of half break*)

(*check the reverse binding: B.x - A.x *)

(*let store_new_result_hb_map
    parameter
    error 
    half_break_map
    store_covering_classes_modification_update
    store_contact_map
    store_result_map
    =
 let add_link (agent_type, cv_id) rule_id_effect store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
	([], Site_map_and_set.Set.empty)
        (agent_type, cv_id) store_result in
    let errorm, current_set =
      Site_map_and_set.Set.add parameter error rule_id_effect old
    in
    let error, new_set =
      Site_map_and_set.Set.union parameter error current_set old
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id)
        (l, new_set) store_result
    in
    error, result
 in
 Int2Map_HalfBreak_effect.Map.fold
   (fun (agent_type_eff, site_type_eff) (l1, l2) (error, store_result) ->
     List.fold_left (fun (error, store_result) (rule_id_eff, state) ->
       Int2Map_syn.Map.fold
         (fun rule_id set (error, store_result) ->
           Set_pair.Set.fold 
             (fun ((agent_type1, site_type1, state1),(agent_type2,site_type2, state2))
               (error, store_result) ->
                 if state = state2
                 then
                   if agent_type1 = agent_type_eff &&
                     site_type1 = site_type_eff
                   then
                     Int2Map_CV_Modif.Map.fold
                       (fun (agent_type, cv_id) 
                         (l', rule_id_set) (error, store_result) ->
                           Site_map_and_set.Set.fold
                             (fun rule_id_update (error, current_result) ->
                               if agent_type = agent_type2
                               then
                                 let error, result =
                                   add_link (agent_type, cv_id) rule_id_eff
                                     current_result
                                 in
                                 error, result
                               else
                                 error, current_result
                             ) rule_id_set (error, store_result)
                       ) store_covering_classes_modification_update
                       (error, store_result)
                   else
                     error, store_result
                 else
                   error, store_result
             ) set (error, store_result)
         ) store_contact_map (error, store_result)
     ) (error, store_result) l2
   ) half_break_map (error, store_result_map)
 *)   

(************************************************************************************)
(*side effects in the case of remove*)

(*let store_new_result_remove_map
  parameter
  error
  remove_map
  store_covering_classes_modification_update
  store_contact_map
  store_result_map
    =
  let add_link (agent_type, cv_id) rule_id_effect store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
	([], Site_map_and_set.Set.empty)
        (agent_type, cv_id) store_result in
    let errorm, current_set =
      Site_map_and_set.Set.add parameter error rule_id_effect old
    in
    let error, new_set =
      Site_map_and_set.Set.union parameter error current_set old
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id)
        (l, new_set) store_result
    in
    error, result
  in
  Int2Map_Remove_effect.Map.fold
    (fun (agent_type_eff, site_type_eff) (l1, l2) (error, store_result) ->
      List.fold_left (fun (error, store_result) rule_id_eff ->
        Int2Map_syn.Map.fold
          (fun rule_id set (error, store_result) ->
            Set_pair.Set.fold
              (fun ((agent_type1, site_type1, state1),(agent_type2, site_type2, state2))
                (error, store_result) ->
                  if agent_type1 = agent_type_eff && 
                    site_type1 = site_type_eff
                  then
                    let error, store_result =
                      Int2Map_CV_Modif.Map.fold
                        (fun (agent_type, cv_id) (l', rule_id_set)
                          (error, store_result) ->
                            let error, store_result =
                              Site_map_and_set.Set.fold
                                (fun rule_id_update (error, current_result) ->
                                  if agent_type = agent_type2
                                  then
                                    add_link (agent_type, cv_id) rule_id_eff current_result
                                  else
                                    error, current_result
                                ) rule_id_set (error, store_result)
                            in
                            error, store_result
                        ) store_covering_classes_modification_update
                        (error, store_result)
                    in
                    error, store_result
                  else
                    error, store_result
              ) set (error, store_result)
          ) store_contact_map (error, store_result)
      ) (error, store_result) l2
    ) remove_map (error, store_result_map)
  *)

(************************************************************************************)
(*combine the result of half break and remove*)

(*let collect_hb_remove_map parameter error 
    store_side_effects
    store_covering_classes_modification_update
    store_contact_map
    store_result_map
    =
  let add_link error (agent_type, cv_id) rule_id_set store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
        ([], Site_map_and_set.Set.empty) (agent_type, cv_id) store_result
    in
    let error, union =
      Site_map_and_set.Set.union parameter error
        rule_id_set old
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id) (l, union) store_result
    in
    error, result
  in
  let (half_break_map, remove_map) = store_side_effects in
  let error, store_hb_map =
    store_new_result_hb_map
      parameter
      error
      half_break_map
      store_covering_classes_modification_update
      store_contact_map
      Int2Map_CV_Modif.Map.empty
  in
  let error, store_remove_map =
    store_new_result_remove_map
      parameter
      error
      remove_map
      store_covering_classes_modification_update
      store_contact_map
      Int2Map_CV_Modif.Map.empty
  in
  Int2Map_CV_Modif.Map.fold2_with_logs
    (fun parameter error str str_opt exn ->
      let error, _ = warn parameter error str_opt exn Not_found
      in
      error
    )
    parameter
    error
    (*exists in 'a t*)
    (fun parameter error (agent_type, cv_id) (l1, s1) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) s1 store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error (agent_type, cv_id) (l2, s2) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) s2 store_result
      in
      error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_type, cv_id) (l1, s1) (l2, s2) store_result ->
      let error', union = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 331") Exit in
      let error, store_result =
        add_link error (agent_type, cv_id) union store_result
      in
      error, store_result
    )
    store_hb_map
    store_remove_map
    store_result_map
  *)    

(************************************************************************************)
(*combine the result of update(c) and half break and remove side
  effects. This is final update function for side effect*)

(*let collect_update_hb_remove_map parameter error 
    store_side_effects
    store_contact_map
    store_covering_classes_modification_update
    store_result_map
    =
  let add_link error (agent_type, cv_id) rule_id_set store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
        ([], Site_map_and_set.Set.empty) (agent_type, cv_id) store_result
    in
    let error, union =
      Site_map_and_set.Set.union parameter error
        rule_id_set old
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id) (l, union) store_result
    in
    error, result
  in
  let error, store_hb_remove_map =
    collect_hb_remove_map
      parameter
      error
      store_side_effects
      store_covering_classes_modification_update
      store_contact_map
      Int2Map_CV_Modif.Map.empty
  in
  Int2Map_CV_Modif.Map.fold2_with_logs
    (fun parameter error str str_opt exn ->
      let error, _ = warn parameter error str_opt exn Not_found
      in
      error
    )
    parameter
    error
    (*exists in 'a t*)
    (fun parameter error (agent_type, cv_id) (l1, s1) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) s1 store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error (agent_type, cv_id) (l2, s2) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) s2 store_result
      in
      error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_type, cv_id) (l1, s1) (l2, s2) store_result ->
      let error', union = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 331") Exit in
      let error, store_result =
        add_link error (agent_type, cv_id) union store_result
      in
      error, store_result
    )
    store_hb_remove_map
    store_covering_classes_modification_update
    store_result_map
  *)
(************************************************************************************)
(*compute view that has new view and new bond*)

(*let compute_new_views parameter handler error
    agent_type
    cv_id 
    is_new_view 
    store_covering_classes_modification_update_full
    wl_tl
    store_result =
  let error, (handler, new_wl, store_result) =
    if is_new_view
    then
      let error, new_wl =
        add_update_to_wl
          parameter
          error
	  agent_type
	  cv_id 
          store_covering_classes_modification_update_full
          wl_tl
      in
      error, (handler, new_wl, store_result)
    (*REMARK: will be used later when changed to module and use dynamic contact map*)
    (* begin
       if is_new_bond
       then
       let error, new_wl =
       add_update_to_wl
       parameter
       error
       agent_type 
       cv_id 
       store_new_result_map
       wl_tl
       in
       error, (handler, new_wl, store_result)
       else
       let error, new_wl =
       add_update_to_wl
       parameter
       error
       agent_type
       cv_id 
       store_covering_classes_modification_update
       wl_tl
       in
       error, (handler, new_wl, store_result)
       end*)
    else
      error, (handler, wl_tl, store_result)
  in
  error, (handler, new_wl, store_result)*)
