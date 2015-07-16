 (**
  * side_effect.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 23th of Feburary
  * Last modification: 
  * 
  * Compute the side effect 
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Int_storage
open Cckappa_sig
open Printf

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Side effect") message exn
                 (fun () -> default)                

let trace = false

(************************************************************************************)
(*TYPE*)

module AgentMap = Quick_Nearly_inf_Imperatif
type site = int
type set = Site_map_and_set.set
type 'a map      = 'a Site_map_and_set.map
type agent_index = int

(*site effect*)
type know_unbinding = (agent_index * agent_name * site * 
                         agent_index * agent_name * site) list
    
(*deletion*)
type document_del   = (agent_index * agent_name * site) map
type undocument_del = (agent_index * agent_name * site) map
type deletion       = document_del AgentMap.t * undocument_del AgentMap.t


type side_effect =
    {
      store_half_break : set AgentMap.t;
      store_unbinding  : know_unbinding;
      store_remove_map : deletion
    }

(************************************************************************************)

let empty_set = Site_map_and_set.empty_set
let empty_map = Site_map_and_set.empty_map
let add_set = Site_map_and_set.add_set
let union = Site_map_and_set.union

let sprintf_list l =
  let acc = ref "{" in
  List.iteri (fun i x ->
    acc := !acc ^
      if i <> 0
      then sprintf "; %i" x
      else sprintf "%i" x
  ) l;
  !acc ^ "}"
    
let print_list l =
  let output = sprintf_list l in
  fprintf stdout "%s\n" output

(************************************************************************************)
(*compute site effect*)

(*unknow unbinding*)
let collect_half_break parameter error handler store_half_break half_break =
  List.fold_left (fun (error, store_half_break) (site_add, state) ->
    (*get information of site and agent_type in site_address*)
    let site = site_add.site in
    let agent_type = site_add.agent_type in
    let error, (min, max) = (*TO BE USED*)
      match state with
        | None ->
          begin
            let error, value_state =
              Misc_sa.unsome
                (Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
                   parameter
                   error
                   (agent_type, site)
                   handler.states_dic)
                (fun error -> warn parameter error (Some "line 782")
                  Exit (Dictionary_of_States.init ()))
            in
            let error, last_entry =
              Dictionary_of_States.last_entry parameter error value_state in
            error, (1, last_entry)
          end
        | Some interval -> error, (interval.min, interval.max)
    in
    (*get the old one*)
    let error, out_old =
      AgentMap.unsafe_get parameter error agent_type
        store_half_break in
    let old_set =
      match out_old with
        | None -> empty_set
        | Some s -> s
    in
    (*new set*)
    let error, set =
      add_set
        parameter
        error
        site
        old_set
    in
    let error, new_set =
      Site_map_and_set.union
        parameter
        error
        set
        old_set
    in
    (*store*)
    AgentMap.set
      parameter
      error
      agent_type
      new_set
      store_half_break
  )(error, store_half_break) half_break

(*------------------------------------------------------------------------------*)
(*know unbinding*)

let collect_know_binding error store_unbinding release =
  let error, store_unbinding =
    List.fold_left (fun (error, store_unbinding) (site_add_1, site_add_2) ->
      (*get the first binding information*)
      let agent_index_1 = site_add_1.agent_index in
      let agent_type_1 = site_add_1.agent_type in
      let site_1 = site_add_1.site in
      (*get the second binding information*)
      let agent_index_2 = site_add_2.agent_index in
      let agent_type_2 = site_add_2.agent_type in
      let site_2 = site_add_2.site in
      (*store unbinding information*)
      let unbinding_list =
        (agent_index_1, agent_type_1, site_1, 
         agent_index_2, agent_type_2, site_2) :: store_unbinding
      in error, unbinding_list
    )(error, store_unbinding) release
  in
  error, store_unbinding

(*------------------------------------------------------------------------------*)
(*collect remove actions-it is one of the property of side effect*)

(*collect document site*)
let collect_document_site parameter error index agent agent_type store_doc =
  let site_map =
    Site_map_and_set.fold_map
      (fun site _ current_map ->
        let error, site_map =
          Site_map_and_set.add_map
            parameter
            error
            site
            (index, agent_type, site)
            current_map
        in
        site_map
      )
      agent.agent_interface empty_map
  in
  let error, old =
    AgentMap.unsafe_get
      parameter
      error
      agent_type
      store_doc
  in
  let old_map =
    match old with
      | None -> empty_map
      | Some m -> m
  in
  let error, final_map =
    Site_map_and_set.union_map
      parameter
      error
      old_map
      site_map  
  in
  AgentMap.set
    parameter
    error
    agent_type
    final_map
    store_doc
    
(*------------------------------------------------------------------------------*)
(*collect undocument site*)

let collect_undocument_site parameter error index agent_type list_undoc store_undoc =
  let undoc_map =
    List.fold_left (fun current_map site ->
      let error, site_map =
        Site_map_and_set.add_map
          parameter
          error
          site
          (index, agent_type, site)
          current_map
      in site_map
    ) empty_map list_undoc
  in
  let error, old = 
    AgentMap.unsafe_get
      parameter
      error
      agent_type
      store_undoc
  in
  let old_map =
    match old with
      | None -> empty_map
      | Some m -> m
  in
  let error, final_map =
    Site_map_and_set.union_map
      parameter
      error
      old_map
      undoc_map          
  in
  AgentMap.set
    parameter
    error
    agent_type
    final_map
    store_undoc

(*------------------------------------------------------------------------------*)

let collect_remove parameter error store_remove_map remove =
  List.fold_left (fun (error, store_remove_map) (index, agent, list_undoc) ->
    let agent_type = agent.agent_name in
    let error, document_site =
      collect_document_site
        parameter
        error
        index agent agent_type (fst store_remove_map)
    in
    let error, undocument_site =
      collect_undocument_site
        parameter error index agent_type list_undoc
        (snd store_remove_map)
    in
    error, (document_site, undocument_site)      
  ) (error, store_remove_map) remove

(************************************************************************************)
(*RULE*)

let scan_rule parameter error handler rule store_side_effect =
  (*------------------------------------------------------------------------------*)
  (*compute side effects - half break actions*)
  let error, store_half_break =
    collect_half_break parameter error
      handler
      store_side_effect.store_half_break
      rule.actions.half_break
  in
  (*------------------------------------------------------------------------------*)
  (*compute side effects - whole break, release actions*)
  let error, store_unbinding =
    collect_know_binding error 
      store_side_effect.store_unbinding
      rule.actions.release
  in
  (*------------------------------------------------------------------------------*)
  (*compute side effects - remove action*)
  let error, store_remove_map =
    collect_remove
      parameter
      error
      store_side_effect.store_remove_map
      rule.actions.remove
  in
  (*------------------------------------------------------------------------------*)
  (*result*)
  error,
  {
    store_half_break = store_half_break;
    store_unbinding  = store_unbinding;
    store_remove_map = store_remove_map
  }

(************************************************************************************)
(*RULES*)

let create_map parameter error n_agents =
  AgentMap.create parameter error n_agents

let scan_rule_set parameter error handler rules =
  let n_agents = handler.nagents in
  let error, init_half_break = create_map parameter error n_agents in
  let error, init_remove_doc = create_map parameter error n_agents in
  let error, init_remove_undoc = create_map parameter error n_agents in
  let init_remove_map   = (init_remove_doc, init_remove_undoc) in
 (*------------------------------------------------------------------------------*)
  (*init state of covering class*)
  let init_class =
    {
      store_half_break = init_half_break;
      store_unbinding  = [];
      store_remove_map = init_remove_map
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, store_side_effect =
    Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule store_result ->
        scan_rule
          parameter
          error
          handler
          rule.e_rule_c_rule
          store_result
      )
      rules
      init_class
  in error, store_side_effect
    
(************************************************************************************)   
(*PRINT*)

let print_halfbreak parameter error store_half_break =
  AgentMap.print
    error
    (fun error parameter set ->
      let is_empty = Site_map_and_set.is_empty_set set in
      if not is_empty
      then
	let _ =
	  fprintf stdout "Side-effect:half_break:\n";
          let l = Site_map_and_set.elements set in
          fprintf stdout "Side_effect:half_break:site_type:";
          print_list l
	in
	error
      else
	error
    )
    parameter
    store_half_break
     
(*------------------------------------------------------------------------------*)

let print_unbinding store_unbinding =
  fprintf stdout "Side-effect:unbinding:\n";
  let rec aux acc =
    match acc with
      | [] -> ()
      | (i,a,x,u,b,y) :: tl ->
        fprintf stdout
          "agent_id:%i:agent_type:%i:site_type:%i -> agent_id:%i:agent_type:%i:site_type:%i\n"
          i a x u b y;
        aux tl
  in aux store_unbinding

(*------------------------------------------------------------------------------*)

let print_remove parameter error store_remove =
  (*print document*)
  let _ =
    AgentMap.print error
      (fun error parameter map ->
	let is_empty = Site_map_and_set.is_empty_map map in
	if not is_empty
	then
          let _ =
	    fprintf stdout "Side-effect:deletion:\n";
            fprintf stdout "Side-effect:deletion:document_site:\n";
            Site_map_and_set.iter_map (fun k (i,a,s) ->
              let _ =
		fprintf stdout "agent_id:%i:agent_type:%i:site_type:%i\n"
                  i a s
              in
              ()
            ) map
	  in error
	else
	  error
      )
      parameter
      (fst store_remove)
  in
  (*print undocument*)
  AgentMap.print error
    (fun error parameter map ->
      let is_empty = Site_map_and_set.is_empty_map map in
      if not is_empty
      then
	let _ =
	  fprintf stdout "Side-effect:deletion:\n";
          fprintf stdout "Side-effect:deletion:undocument_site:\n";
          Site_map_and_set.iter_map (fun k (i,a,s) ->
            let _ =
              fprintf stdout "agent_id:%i:agent_type:%i:site_type:%i\n"
              i a s
            in
            ()) map
	in error
      else
	error
    )
    parameter
    (snd store_remove)

(*------------------------------------------------------------------------------*)

let print_result parameter error result =
  let _ =
    print_halfbreak parameter error result.store_half_break
  in
  let _ =
    print_unbinding result.store_unbinding
  in
  let _ =
    print_remove parameter error result.store_remove_map
  in
  ()

(************************************************************************************)   
(*MAIN*)

let side_effect parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = 
    scan_rule_set parameter error handler cc_compil.rules in
  let _ = print_result parameter error result in
  error, result
