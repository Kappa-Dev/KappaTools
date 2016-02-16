(**
   * agent_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 30th of January
   * Last modification:
   *
   * Abstract domain to record live rules
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Rule domain") message exn
    (fun () -> default)

let local_trace = false
  
module Int2Map_Agent =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = int
         let compare = compare
        end
       ))

type domain_static_information =
  {
    agents_test    : int list Int2Map_Agent.Map.t;
    agents_created : int list Int2Map_Agent.Map.t;
    agents_test_rule : int list Int2Map_Agent.Map.t
  }
    
let add_link parameter error rule_id agent_id store_result =
  let error, old_list =
    match Int2Map_Agent.Map.find_option_without_logs parameter error
      rule_id store_result
    with
    | error, None -> error, []
    | error, Some l -> error, l
  in
  let new_list = agent_id :: old_list in
  let error, store_result =
    Int2Map_Agent.Map.add_or_overwrite
      parameter error rule_id new_list store_result
  in
  error, store_result

let collect_agents_test parameter error rule_id rule store_result =
  let error, store_result =
    Bdu_analysis_type.AgentMap.fold parameter error
      (fun parameter error agent_id agent store_result ->
        match agent with
        | Cckappa_sig.Unknown_agent _
        | Cckappa_sig.Ghost -> error, store_result
        | Cckappa_sig.Dead_agent _ -> warn parameter error (Some "line 49") Exit store_result
        | Cckappa_sig.Agent agent ->
          let error, store_result =
            add_link parameter error rule_id agent_id store_result
          in
          error, store_result
      ) rule.Cckappa_sig.rule_lhs.Cckappa_sig.views store_result
  in
  error, store_result

(*a map from agent_id to rule_id*)

let collect_agents_test_rule parameter error rule_id rule store_result =
  let error, store_result =
    Bdu_analysis_type.AgentMap.fold parameter error
      (fun parameter error agent_id agent store_result ->
        match agent with
        | Cckappa_sig.Unknown_agent _
        | Cckappa_sig.Ghost -> error, store_result
        | Cckappa_sig.Dead_agent _ -> warn parameter error (Some "line 49") Exit store_result
        | Cckappa_sig.Agent agent ->
          let error, store_result =
            add_link parameter error agent_id rule_id store_result
          in
          error, store_result
      ) rule.Cckappa_sig.rule_lhs.Cckappa_sig.views store_result
  in
  error, store_result

let collect_agents_created parameter error rule_id rule store_result =
  let error, store_result =
    List.fold_left (fun (error, store_result) (agent_id, agent_type) ->
      let error, store_result =
        add_link parameter error rule_id agent_id store_result
      in
      error, store_result
    ) (error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.creation
  in
  error, store_result

let scan_rule parameter error rule_id rule store_result =
  let error, agents_test =
    collect_agents_test
      parameter
      error
      rule_id
      rule
      store_result.agents_test
  in
  let error, agents_created =
    collect_agents_created
      parameter
      error
      rule_id
      rule
      store_result.agents_created
  in
  let error, agents_test_rule =
    collect_agents_test_rule
      parameter
      error
      rule_id
      rule
      store_result.agents_test_rule
  in
  error, 
  {
    agents_test = agents_test;
    agents_created = agents_created;
    agents_test_rule = agents_test_rule
  }

let init =
  let init_test    = Int2Map_Agent.Map.empty in
  let init_created = Int2Map_Agent.Map.empty in
  let init_test_rule = Int2Map_Agent.Map.empty in
  let init = 
    {
      agents_test    = init_test;
      agents_created = init_created;
      agents_test_rule = init_test_rule
    }
  in
  init

let scan_rule_set parameter error compile rules =
  let error, store_result =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule store_result ->
        scan_rule
          parameter
          error
          rule_id
          rule.Cckappa_sig.e_rule_c_rule
          store_result
      ) rules init
  in
  error, store_result

let print_agents_test result =
  Int2Map_Agent.Map.iter (fun rule_id l ->
    List.iter (fun agent_id ->
      Printf.fprintf stdout "TEST rule_id:%i:agent_id:%i\n" rule_id agent_id
    ) l      
  ) result

let print_agents_created result =
  Int2Map_Agent.Map.iter (fun rule_id l ->
    List.iter (fun agent_id ->
      Printf.fprintf stdout "CREATED rule_id:%i:agent_id:%i\n" rule_id agent_id
    ) l      
  ) result

let main parameter error compil =
  let error, store_result =
    scan_rule_set
      parameter
      error
      compil
      compil.Cckappa_sig.rules
  in
  let _ = print_agents_test store_result.agents_test in  
  let _ = print_agents_created store_result.agents_created in
  error, store_result
