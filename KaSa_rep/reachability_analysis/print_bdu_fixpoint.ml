(**
  * print_bdu_fixpoint.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 13th of October
  * Last modification: 
  * 
  * Print relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Printf
open Bdu_analysis_type
open Remanent_parameters_sig
open Cckappa_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU creation") message exn (fun () -> default)  

let trace = false

(************************************************************************************)

(*let print_test_bonds parameter error result =
  let b, map = result in
  if b 
  then
    (*There is bond on the rhs*)
    Map_test_bond.Map.iter (fun rule_id (l1, ( s2)) ->
      if l1 <> []
      then ()
      else
        ();
      let _ =
        fprintf parameter.log "rule_id:%i\n" rule_id
      in
      Map_site_address.Set.iter (fun (site_add1, site_add2) ->
        fprintf parameter.log 
          "{agent_id:%i; agent_type:%i; site_type:%i} -- "
          site_add1.Cckappa_sig.agent_index site_add1.Cckappa_sig.agent_type 
          site_add1.Cckappa_sig.site;
        fprintf parameter.log 
          "{agent_id:%i; agent_type:%i; site_type:%i} \n"
          site_add2.Cckappa_sig.agent_index site_add2.Cckappa_sig.agent_type 
          site_add2.Cckappa_sig.site     
      ) s2
    ) map
  else
    fprintf stdout "There is no bond on the rhs\n"*)

(*let print_test_bonds parameter error result =
  let (b, set) = result in
  Map_site_address.Set.iter (fun (site_add1, site_add2) ->
    fprintf parameter.log 
      "{agent_id:%i; agent_type:%i; site_type:%i} -- "
      site_add1.Cckappa_sig.agent_index site_add1.Cckappa_sig.agent_type 
      site_add1.Cckappa_sig.site;
    fprintf parameter.log 
      "{agent_id:%i; agent_type:%i; site_type:%i} \n"
      site_add2.Cckappa_sig.agent_index site_add2.Cckappa_sig.agent_type 
      site_add2.Cckappa_sig.site     
  ) set*)

let print_test_bonds parameter error result =
  let (b, map) = result in
  fprintf stdout "%b\n" b;
  Map_test_bond.Map.iter (fun rule_id set ->
    fprintf stdout "rule_id:%i" rule_id;
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
  ) map

let print_test_bond_map parameter error set =
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

(************************************************************************************)

let print_new_wl_side_effect parameter error result =
  Int2Map_CV_Modif.Map.iter
    ( fun (x, y) (_, s2) ->
      let _ =
        fprintf parameter.log
          "agent_type:%i:covering_class_id:%i:@set of rule_id:\n" x y
      in
      Site_map_and_set.Set.iter
        (fun rule_id ->
          fprintf parameter.log "rule_id:%i\n" rule_id
        ) s2
    ) result
  
(************************************************************************************)

let print_bdu_update_map parameter error result =
  Map_bdu_update.Map.iter (fun (agent_type, cv_id) bdu_update ->
    let _ =
      fprintf parameter.log "agent_type:%i:cv_id:%i\n" agent_type cv_id
    in
    Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_update
  ) result   
  
(************************************************************************************)
(*main print*)

let print_bdu_fixpoint parameter error result =
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Fixpoint iteration :\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "** New update function when discovered sites that are bond on the rhs for the first time:\n";
    print_new_wl_side_effect
      parameter
      error
      result.store_new_wl_side_effect
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "** Discovered sites that are bond on the rhs for the first time:\n";
    print_test_bonds
      parameter
      error
      result.store_test_has_bond_rhs
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
    "** Fixpoint iteration function:\n";
    print_bdu_update_map
      parameter
      error
      result.store_bdu_fixpoint_map    
  in
  error
