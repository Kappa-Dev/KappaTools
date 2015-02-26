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
  let error, covering_classes =
    Covering_classes_type.SiteMap.create parameter error (n_agents, 0) in
  error,
  {
    Covering_classes_type.covering_classes  = covering_classes
  }

let add_generic get set parameter error rule_id agent_id key map =
  let error, old_agent =
    match get parameter error key map with
    | error, None -> Int_storage.Quick_Nearly_inf_Imperatif.create
                       parameter error 0
    | error, Some x -> error, x
  in
  let error, old_label_set =
    match Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get
            parameter error rule_id old_agent with
    | error, None -> error, Covering_classes_type.Labels.empty
    | error, Some x -> error, x
  in
  let error, new_label_set =
    Covering_classes_type.Labels.add_set parameter error agent_id old_label_set in
  if new_label_set == old_label_set
  then error, map
  else
    let error, new_agent =
      Int_storage.Quick_Nearly_inf_Imperatif.set
        parameter error rule_id new_label_set old_agent
    in
    set parameter error key new_agent map

let add_site parameters error rule_id agent_id agent_type site_id =
  let _ = Misc_sa.trace parameters
          (fun () ->
             "covering_class: site_type:" ^ (string_of_int site_id) ^ "\n")
  in
  add_generic Covering_classes_type.SiteMap.unsafe_get Covering_classes_type.SiteMap.set parameters error rule_id agent_id (agent_type, site_id)

(*Compute covering class*)
let scan_rule parameter error handler rule_id rule classes =
  let viewslhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  let rule_diff = rule.Cckappa_sig.diff_reverse in
  let covering_classes = classes.Covering_classes_type.covering_classes in
  let _ = Misc_sa.trace parameter (fun () -> "TEST\n") in
  let error, covering_classes =
    Int_storage.Quick_Nearly_inf_Imperatif.fold2_common
      parameter error
    (fun parameter error agent_id agent site_modif covering_classes ->
       match agent with
       | Cckappa_sig.Ghost -> error, covering_classes
       | Cckappa_sig.Agent agent ->
		   let agent_type = agent.Cckappa_sig.agent_name in
	      Cckappa_sig.Site_map_and_set.fold_map (
			  fun site _ (error, site_modif) ->
				  (error, site_modif)
		   )	 
	      agent.Cckappa_sig.agent_interface (error, covering_classes)
		  (*
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, site_modif =
            Cckappa_sig.Site_map_and_set.fold_map
              (fun site (error, site_modif) ->
               add_site parameter error rule_id agent_id agent_type site site_modif              )
              agent.Cckappa_sig.agent_interface (error, site_modif)
          in
          let error, covering_classes =
            Cckappa_sig.Site_map_and_set.fold_map 
              (fun site (error, covering_classes) ->
               if Cckappa_sig.Site_map_and_set.exists (fun site_modif ->
                          site = site_modif) site_modif
               then error, covering_classes
               else
                 add_site parameter error rule_id agent_id agent_type site
                          covering_classes
              ) agent.Cckappa_sig.agent_interface (error, covering_classes)
          in*)
          (*(error, covering_classes)*)
    ) viewslhs rule_diff covering_classes
  in
  error,
  {
    classes with
    Covering_classes_type.covering_classes = covering_classes
  }

let scan_rule_set parameter error handler rules =
  let error, init = empty_classes parameter error handler in
  Int_storage.Nearly_inf_Imperatif.fold
    parameter error
    (fun parameter error rule_id rule classes ->
     let _ = Misc_sa.trace parameter
      (fun () -> "Rule " ^ (string_of_int rule_id) ^ "\n") in
     scan_rule parameter error handler rule_id rule.Cckappa_sig.e_rule_c_rule classes
    ) rules init
      
let covering_classes parameters error handler cc_compil =
  let _ = Misc_sa.trace parameters (fun () -> "Covering_classes \n") in
  (*let error, covering_classes =*)
    scan_rule_set parameters error handler cc_compil.Cckappa_sig.rules
