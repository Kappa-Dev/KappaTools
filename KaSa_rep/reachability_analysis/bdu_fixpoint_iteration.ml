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

(************************************************************************************)
(*it is an enable rule when the intersection between bdu_test and
  bdu_remanent is different than empty set*)

let is_bdu_test_enable parameter handler error bdu_false bdu_test bdu_X = (*CHECK*)
  (*if bdu_test is empty*)
  if Mvbdu_wrapper.Mvbdu.equal bdu_test bdu_false
  then
    (*then it is an enable rule*)
    error, true
  else
    (*if bdu_test is not empty, then do the intersection with bdu_X*)
    begin
      let error, handler, bdu_inter =
        Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error bdu_test bdu_X
      in
      (*then test the enable of this result*)
      if not (Mvbdu_wrapper.Mvbdu.equal bdu_inter bdu_false)
      then 
        (*if it is empty then it is not enable*)
        error, true
      else
        error, false        
    end  

(************************************************************************************)
(*update bdu:
  - (bdu_X U bdu_creation) U [\rho[update_views] | \rho \in bdu_X (inter) bdu_test views]
*)

(*Xn intersection with bdu_test and modif and then union with X_n*)

let compute_bdu_update_test parameter handler error bdu_test list_a bdu_X =
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

(*REMOVE:per creation*)
let compute_bdu_update_creation parameter handler error bdu_creation bdu_X' =
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_X' bdu_creation
  in
  error, handler, bdu_result

(*the final update*)
let compute_bdu_update parameter handler error bdu_test list_a bdu_creation bdu_X =
  (*to the first one with bdu_test*)
  let error, handler, bdu_Xn = 
    compute_bdu_update_test
      parameter
      handler
      error
      bdu_test
      list_a
      bdu_X
  in
  (*do the union per creation*)
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_Xn bdu_creation
  in
  error, handler, bdu_result

(************************************************************************************)
(*a bond is discovered for the first time*)

(*rule that has the views that are tested and bond: if one can apply this
  rule, then add rule of side effects into the update(c), and then add
  update(c') to the working list.*)
 
(*let collect_rhs_bond parameter error rule_id rule store_result =
  List.fold_left (fun (error, set) (site_add1, site_add2) ->
    if Map_site_address.Set.mem (site_add1, site_add2) set
    then error, set
    else
      error, Map_site_address.Set.add (site_add1, site_add2) set
  ) (error, store_result) rule.actions.bind*)

let store_test_has_bond_rhs parameter error rule_id rule store_result =
  let add_link rule_id set store_result =
    let (l, old) =
      match
	Map_test_bond.Map.find_option rule_id store_result
      with Some (l,old) -> l, old
      | None -> [], Map_site_address.Set.empty
    in
    let result_map = 
      Map_test_bond.Map.add rule_id (l, set) store_result
    in
    error, result_map
  in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_id agent site_add_map store_result ->
      match agent with
      | Ghost -> error, store_result
      | Unknown_agent _ | Dead_agent _ ->
	 warn parameter error (Some "line 156, rhs should not have dead agents")
           Exit store_result
      | Agent agent ->
        let agent_type = agent.agent_name in
        let error, set =
          Site_map_and_set.Map.fold
            (fun site site_add2 (error, set) ->
              let site_add1 = Cckappa_sig.build_address agent_id agent_type site in
              if Map_site_address.Set.mem (site_add1, site_add2) set
              then error, set
              else
                error, Map_site_address.Set.add (site_add1, site_add2) set
            ) site_add_map (error, Map_site_address.Set.empty)
        in
        let error, store_result =
          add_link rule_id set store_result
        in
        error, store_result
    ) rule.rule_lhs.views rule.rule_rhs.bonds store_result

(*write a function add update(c) into working list*)

let add_update_to_wl parameter error store_covering_classes_modification_update wl =
  Int2Map_CV_Modif.Map.fold
    (fun (agent_type, cv_id) (l1, s1) (error, store_wl) ->
      let error, result =
        Site_map_and_set.Set.fold (fun rule_id (error, wl) ->
          let error, wl = IntWL.push parameter error rule_id wl
          in
          error, wl
        ) s1 (error, store_wl)
      in
      error, result
    ) store_covering_classes_modification_update (error, wl)

(************************************************************************************)
(*fixpoint*)

(*Testing*)
open Remanent_parameters_sig

let print_list rule_id l = 
  let rec aux acc =
    match acc with
    | [] -> []
    | (site, state) :: tl -> 
      fprintf stdout "rule_id:%i:site:%i:state:%i\n" rule_id site state; aux tl
  in
  aux l

let print_set parameter set =
  if Map_site_address.Set.is_empty set
  then fprintf stdout "empty\n"
  else
  Map_site_address.Set.iter (fun (site_add1, site_add2) ->
    fprintf parameter.log 
      "{agent_id:%i; agent_type:%i; site_type:%i} -- "
      site_add1.Cckappa_sig.agent_index site_add1.Cckappa_sig.agent_type 
      site_add1.Cckappa_sig.site;
    fprintf parameter.log 
      "{agent_id:%i; agent_type:%i; site_type:%i} \n"
      site_add2.Cckappa_sig.agent_index site_add2.Cckappa_sig.agent_type 
      site_add2.Cckappa_sig.site
  ) set

(*from rule_id get the bdu_creation, bdu_test, and modif_list*)

let collect_bdu_creation_and_modif_list parameter bdu_true error rule_id
    store_proj_bdu_creation_restriction_map
    store_proj_modif_list_restriction_map
    =
  let error, bdu_creation =
    match 
      Map_final_creation_bdu.Map.find_option rule_id store_proj_bdu_creation_restriction_map
    with
    | None -> error, bdu_true
    | Some (l, (ag, bdu_creation)) -> error, bdu_creation
  in
  let  error, modif_list = 
    match
      Map_final_modif_list.Map.find_option rule_id store_proj_modif_list_restriction_map
    with
    | None -> error, []
    | Some (l, (x, l')) -> error, l'
  in
  error, (bdu_creation, modif_list)
  
(*fixpoint iteration*)

let collect_bdu_update_map parameter handler error 
    rule 
    wl_creation
    store_proj_bdu_creation_restriction_map
    store_proj_modif_list_restriction_map
    store_proj_bdu_test_restriction_map
    store_bdu_test_restriction_map
    store_test_has_bond_rhs
    store_covering_classes_modification_update
    =
  let error, handler, bdu_false = 
    Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error 
  in 
  let error, handler, bdu_true = 
    Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error 
  in
  (*-----------------------------------------------------------------------*)
  let add_link (agent_type, cv_id) bdu_update store_result =
    let error, (l, bdu_old) =
      match
	Map_bdu_update.Map.find_option (agent_type, cv_id) store_result
      with
        Some (l, old) -> error, (l, old)
      | None -> error, ([], bdu_false)
    in
    let error, handler, bdu_union =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_update bdu_old
    in
    (*-----------------------------------------------------------------------*)
    (*test if bdu_union = bdu_old*)
    if Mvbdu_wrapper.Mvbdu.equal bdu_union bdu_old
    then
      (*then false, do not add bdu_update, store bdu_old*)
      error, false, store_result
    else
      (*-----------------------------------------------------------------------*)
      (*add bdu_update*)
      let result_map =
        Map_bdu_update.Map.add (agent_type, cv_id) (l, bdu_union) store_result
      in
      error, true, result_map
  in
  (*-----------------------------------------------------------------------*)
  (*iterate function over a working list*)
  let rec aux acc_wl (error, store_bdu_update_map) =
    if IntWL.is_empty acc_wl
    then
      error, store_bdu_update_map
    else
      (*-----------------------------------------------------------------------*)
      (*pop the first element (rule_id) in this working list*)
      let error, (rule_id_op, wl_tl) =
        IntWL.pop parameter error acc_wl
      in
      match rule_id_op with
      | None -> error, store_bdu_update_map
      | Some rule_id ->
        (*-----------------------------------------------------------------------*)
        (*compute bdu_creation, bdu_test and modif_list for this rule_id*)
        let error, (bdu_creation, modif_list) =
          collect_bdu_creation_and_modif_list
            parameter
            bdu_true
            error
            rule_id
            store_proj_bdu_creation_restriction_map
            store_proj_modif_list_restriction_map
        in
        (*build_list from modif_list*)
        let error, handler, list_a =
          Mvbdu_wrapper.Mvbdu.build_list
            parameter
            handler
            error
            modif_list
        in
        (*-----------------------------------------------------------------------*)
        (*is for all bdu_test sastify in a covering_class*)
        let is_enable = 
          Map_test_bdu.Map.for_all
            (fun (agent_id, agent_type, rule_id', cv_id) (_, (_, _)) ->
              let error, is_bdu_test_empty, bdu_test =
                match 
                  Map_final_test_bdu.Map.find_option rule_id 
                    store_proj_bdu_test_restriction_map
                with
                | None -> error, true, bdu_true (*if bdu_test is empty*)
                | Some (_, (_, bdu_test)) -> error, false, bdu_test
              in
              (*-----------------------------------------------------------------------*)
              (*search bdu_X inside a store_result_map*)
              let error, bdu_X =
                match Map_bdu_update.Map.find_option (agent_type, cv_id)
                  store_bdu_update_map
                with
                | None -> error, bdu_false (*the initial state of bdu_iterate is bdu_false*)
                | Some (l, bdu) -> error, bdu
              in
              (*-----------------------------------------------------------------------*)
              (*do the intersection of the bdu_test and bdu_iterate*)
              let error, handler, bdu_inter =
                Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error bdu_test bdu_X
              in
              (*-----------------------------------------------------------------------*)
              (*let _ =
                fprintf stdout "rule_id:%i\nbdu_test is\n" rule_id;
                Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_test;
                fprintf stdout "\nbdu_inter is\n";
                Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_inter;
              in*)
              (*it is an enable rule when the intersection of bdu_test and
                bdu_iter is different than empty; or it is an enable when
                bdu_test is empty*)

              (*CHECK*)
              if is_bdu_test_empty
              then
                true
              else
                if Mvbdu_wrapper.Mvbdu.equal bdu_inter bdu_false
                then false
                else true
              (*-----------------------------------------------------------------------*)
              (*it is an enable rule when the intersection of bdu_test and
                bdu_iter is different than empty; or it is an enable when
                bdu_test is empty*)
              (*if (*(Mvbdu_wrapper.Mvbdu.equal bdu_test bdu_true) ||*)
                not (Mvbdu_wrapper.Mvbdu.equal bdu_inter bdu_false)
              then 
                let _ =
                  fprintf stdout "yes, it is an enable rule\n"
                in
                true
              else 
                let _ =
                  fprintf stdout "no, it is not an enable rule\n"
                in
                false*)
            ) store_bdu_test_restriction_map
        in
        (*-----------------------------------------------------------------------*)
        if is_enable
        then
          (*do a fold inside bdu_test of the covering_class and then do the update*)
          let error, (new_wl, store_new_result) =
            Map_test_bdu.Map.fold 
              (fun (agent_id, agent_type, rule_id', cv_id) (l1, (_, _)) 
              (error, (store_wl, store_update_map)) ->
                (*return new_wl and store_result,then use it to iterate out
                  of this function*)
                let error, bdu_test = 
                  match 
                    Map_final_test_bdu.Map.find_option rule_id 
                      store_proj_bdu_test_restriction_map
                  with
                  | None -> error, bdu_true (*bdu_test is empty*)
                  | Some (l, (ag_id, bdu_test)) -> error, bdu_test
                in
                (*-----------------------------------------------------------------------*)
                (*search bdu_X inside a store_result_map*)
                let error, bdu_X =
                  match Map_bdu_update.Map.find_option (agent_type, cv_id)
                    store_update_map
                  with
                  | None -> error, bdu_false (*the initial state is bdu_false*)
                  | Some (l, bdu) -> error, bdu
                in
                (*-----------------------------------------------------------------------*)
                let error, handler, bdu_update =
                  compute_bdu_update
                    parameter
                    handler
                    error
                    bdu_test
                    list_a
                    bdu_creation
                    bdu_X
                in
                (*-----------------------------------------------------------------------*)
                let error, is_new_view, store_result =
                  add_link (agent_type, cv_id) bdu_update store_update_map
                in
                (*-----------------------------------------------------------------------*)
                if is_new_view
                then
                  (*let _ =
                    fprintf stdout "Yes, it is a new view\nbdu_update:\n";
                    Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_update
                  in*)
                   (*add update(c) into wl_tl*)
                   let error, new_wl =
                     add_update_to_wl
                       parameter
                       error
                       store_covering_classes_modification_update
                       wl_tl
                   in
                   error, (new_wl, store_result)
                 else
                   (*let _ =
                     fprintf stdout "No, it is not a new view\n";
                     Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_update
                   in*)
                   error, (store_wl, store_result)
              ) store_bdu_test_restriction_map (error, (wl_tl, store_bdu_update_map))
          in
          (*-----------------------------------------------------------------------*)
          (*iterate with new function*)
          aux new_wl (error, store_new_result)
        else
          (*let _ = fprintf stdout "No, it is not an enable rule(final)\n" in*)
          aux wl_tl (error, store_bdu_update_map)
  in
  aux wl_creation (error, Map_bdu_update.Map.empty)
