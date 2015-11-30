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
open Print_bdu_build_map
open Remanent_parameters_sig
open Cckappa_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU creation") message exn (fun () -> default)  

let trace = false

(************************************************************************************)

let print_test_bonds parameter error result =
  Map_test_bond.Map.iter (fun rule_id (l1, s2) ->
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
  ) result

(************************************************************************************)

let print_bdu_update_map parameter error result =
  Map_bdu_update.Map.iter (fun (agent_type, cv_id) (l1, bdu_update) ->
    if l1 <> []
    then ()
    else ();
    let _ =
      fprintf parameter.log "agent_type:%i:cv_id:%i\n" agent_type cv_id
    in
    Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_update
  ) result
    
    (*List.iter (fun bdu_update ->
      let _ =
        let _ = Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_update in
        fprintf parameter.log "\n"
      in
      ()
    ) l2
  ) result*)

(*let print_list l =
  let rec aux acc =
    match acc with
    | [] -> []
    | (site, state) :: tl -> fprintf stdout "site:%i:state:%i\n" site state; aux tl
  in aux l

let print_triple_test parameter error result =
  let (l, l') = result in
  let _ = 
    fprintf parameter.log "bdu_creation\n";
    Map_creation_bdu_ag.Map.iter (fun agent_type (l1, bdu_creation) ->
      if l1 <> [] then () else ();
      fprintf parameter.log "bdu_creation\nagent_type:%i:\n" agent_type;
      Mvbdu_wrapper.Mvbdu.print parameter.log "" bdu_creation
    ) l
  in
  Map_modif_ag.Map.iter (fun agent_id (l1, modif_list) ->
    if l1 <> [] then () else ();
    fprintf parameter.log "modif list:agent_id:%i\n" agent_id;
    let _ = print_list modif_list in
    ()
  ) l'*)
  
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
      "** Discovered sites that are bond on the rhs for the first time:\n";
    print_test_bonds
      parameter
      error
      result.store_test_has_bond_rhs
  in
  (*let _ =
  fprintf (Remanent_parameters.get_log parameter)
    "** triple test:\n";
    print_triple_test
      parameter
      error
      result.store_triple_test
  in*)
  let _ =
  fprintf (Remanent_parameters.get_log parameter)
    "** Fixpoint iteration function:\n";
    print_bdu_update_map
      parameter
      error
      result.store_bdu_update_map    
  in
  error
