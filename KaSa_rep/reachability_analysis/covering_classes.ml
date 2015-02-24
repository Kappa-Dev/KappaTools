 (**
  * covering_classes.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 23th of Feburary
  * Last modification: 
  * 
  * Compute the relations between the left hand site of a rule and its sites.
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering classes") message exn (fun () -> default)

let local_trace = false
                    
let empty_classes parameter error handler =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, agent_test  =
    Covering_classes_type.AgentMap.create parameter error n_agents in
  let error, covering_classes =
    Covering_classes_type.SiteMap.create parameter error (n_agents, (0,0)) in
  error,
  {
    Covering_classes_type.agent_test = agent_test;
    Covering_classes_type.covering_classes  = covering_classes;
  }

let add_generic get set parameter error agent_id key map = (*FIXME*)
  let error, old_agent =
    match get paramter error key map with
    | error, None -> Int_storage.Quick_Nearly_inf_Imperatif.create
                       parameter error 0
    | error, Some x -> error, x
  in
  let error, old_label_set =
    match Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get
            parameter error old_agent with
    | error, None -> error, Covering_classes.Lables.empty
    | error, Some x -> error, x
  in
  let error, new_label_set =
    Covering_classes.Labels.add_set
      parameter error agent_id old_label_set in
  if new_label_set == old_label_set
  then error, map
  else
    let error, new_agent =
      Int_storage.Quick_Nearly_inf_Imperatif.set
        parameter error new_label_set old_agent
    in
    set parameter error key new_agent map

let add_agent parameters error agent_id agent_type =
  let _ = Misc_sa.trace parameters (fun () ->
                              "agent_type:" ^ (string_of_int agent_type) ^
                                "\n")
  in
  add_generic Covering_classes.AgentMap.unsafe_get Covering_classes.AgentMap.set
              parameters error agent_id agent_type
                                      
let add_site parameters error agent_id agent_type site_id site_id_modif =
  let _ = Misc_sa.trace parameters
                        (fun () ->
                         if site_id == site_id_modif
                         then
                           "agent_type:" ^ (string_of_int agent_id) ^
                             ", covering_class: \{site_type:" ^ (string_of_int site_id) ^
                               "\}\n"
                         else
                           "agent_type:" ^ (string_of_int agent_id) ^
                             ", covering_class:\{site_type:" ^ (string_of_int site_id) ^
                               ", site_type_modif:" ^ (string_of_int site_id_modif) ^
                                 "\}\n")
  in
  add_generic Covering_classes.SiteMap.unsafe_get Covering_classes.SiteMap.set
              parameters error agent_id (agent_type, (site_id, site_id_modif))

(*Compute covering class*)
let scan_rule parameter error handler rule classes =
  let viewlhs    = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  let agent_test = covering_classes.Covering_classes_type.agent_test in
  let covering_classes = covering_classes.Covering_classes_type.covering_classes in
  let _ = Misc_sa.trace parameter (fun () -> "TEST\n") in
  let error, (agent_test, covering_classes) =
    Int_storage.Quick_Nearly_inf_Imperatif.fold2_common
      parameter error
    (fun parameter error agent_id agent (agent_test, covering_classes) ->
       match agent with
       | Cckappa_sig.Ghost -> error, (agent_test, covering_classes)
       | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, agent_test =
            add_agent parameter agent_id agent_type agent_test in
          let error, covering_classes =
            Cckappa_sig.Site_map_and_set.fold_map
              (fun site site_modif covering_classes ->
               add_site parameter agent_id agent_type site site_modif
                        covering_classes)
              agent.Cckappa_sig.agent_interface
          in
          error, (agent_test, covering_classes)
    )
    viewlhs rule.Cckappa_sig.diff_reverse covering_classes
  in
  error,
  { classes with
    Covering_classes_type.agent_test = agent_test;
    Covering_classes_type.covering_classes = covering_classes;
  }

let scan_rule_set parameter error handler rules =
  let error, init = empty_classes parameter error handler in
  Int_storage.Nearly_inf_Imperatif.fold
    parameter error
    (fun parameter error rule_id rule classes ->
     let _ = Misc_sa.trace parameter
      (fun () -> "Rule " ^ (string_of_int rule_id) ^ "\n") in
     scan_rule parameter error handler rule.Cckappa_sig.e_rule_c_rule classes
    ) rules init
      
let covering_classes parameters error handler cc_compil =
  let _ = Misc_sa.trace parameters (fun () -> "Covering_classes \n") in
  let error, covering_classes =
    scan_rule_set parameters error handler cc_compil.Cckappa_sig.rules
