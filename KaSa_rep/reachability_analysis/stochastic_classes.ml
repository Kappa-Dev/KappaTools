(**
  * stochastic_classes.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 13th of March
  * Last modification: 
  * 
  * Compute the relations between sites in an agent.
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Stochastic_classes") message exn
                 (fun () -> default)

let trace = false
              
let empty_classes parameter error handler =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, stochastic_classes =
    Stochastic_classes_type.AgentMap.create parameter error n_agents in
  error,
  {
    Stochastic_classes_type.stochastic_classes = stochastic_classes
  }
                   
let add_generic parameter error agent_id key map =
  let get_agent =
    Stochastic_classes_type.AgentMap.unsafe_get
      parameter
      error
      key
      map
  in
  let error, old_agent =
    match get_agent with
      | error, None ->
         Stochastic_classes_type.AgentMap.create parameter error 0
      | error, Some a -> error, a
  in
  let error, new_agent=
    Stochastic_classes_type.AgentMap.set
      parameter
      error
      agent_id
      old_agent
      map
  in new_agent

let add_agent parameter error agent_id agent_type =
  add_generic
    parameter
    error
    agent_id
    agent_type

let agent_creation parameter error viewsrhs agent_modif_plus creation =
  List.fold_left (fun (error, agent_modif_plus) (agent_id, agent_type) ->
    let error, get_agent =
      Stochastic_classes_type.AgentMap.get
        parameter
        error
        agent_id
        viewsrhs
    in
    match get_agent with
      | None -> warn parameter error (Some "line 242") Exit agent_modif_plus
      | Some Cckappa_sig.Ghost -> error, agent_modif_plus
      | Some Cckappa_sig.Agent agent ->
         error,
	 add_agent parameter error
                   agent_id
                   agent_type
                   agent_modif_plus)
                 (error, agent_modif_plus) creation
                 
let add_sites_class parameter error agent_type sites_list stochastic_classes =
  match sites_list with
  | [] -> error, stochastic_classes
  | _ ->
     (*add the normal sites without union*)
     let new_list = (List.rev sites_list) ::[] in
     Stochastic_classes_type.AgentMap.set
       parameter
       error
       agent_type
       new_list
       stochastic_classes
       
let rec agent_sites parameter error agent_id agent_type agent viewsrhs creation =
  let error, init =
    Stochastic_classes_type.AgentMap.create parameter error 0 in
  let error, agent_modif_plus =
    Stochastic_classes_type.AgentMap.create parameter error 0 in
  match creation with
  | [] ->
     (* get the list of sites associated with agent_type, or empty if
     it does not exist *)
     let error, get_agent_sites =
       Stochastic_classes_type.AgentMap.unsafe_get
         parameter
         error
         agent_type
         init
     in
     let agent_sites_list =
       match get_agent_sites with
       | None -> []
       | Some sites -> sites
     in
     (*collect all sites in an agent interface*)
     let sites_list =
       Cckappa_sig.Site_map_and_set.fold_map
         (fun site _ current_list ->
          site :: current_list)
         agent.Cckappa_sig.agent_interface agent_sites_list
     in
     let _ = print_string "\nsites_list:"; Union_find.print_list sites_list;
             print_string "\n"
     in
     sites_list
  | (agent_id2, agent_type2) :: tl ->
     let error, get_agent_rhs =
       Stochastic_classes_type.AgentMap.get
         parameter
         error
         agent_id2
         viewsrhs
     in
     match get_agent_rhs with
     | None | Some Cckappa_sig.Ghost -> []
     | Some Cckappa_sig.Agent agent2 ->
        let _ =
          let error, get_agent2_sites =
            Stochastic_classes_type.AgentMap.unsafe_get
              parameter
              error
              agent_type2 (*agent_type*)
              init
          in
          let agent2_sites_list =
            match get_agent2_sites with
            | None -> []
            | Some sites -> sites
          in
          let sites_list =
            Cckappa_sig.Site_map_and_set.fold_map
              (fun site _ current_sites ->
               site :: current_sites)
              agent2.Cckappa_sig.agent_interface
              agent2_sites_list
          in
          let _ = print_string "\nsites_list_new:";
                  Union_find.print_list sites_list;
                  print_string "\n"
          in
          sites_list
        in agent_sites parameter error agent_id agent_type agent viewsrhs tl
    
let scan_rule parameter error handler rule classes =
  let viewslhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  let viewsrhs = rule.Cckappa_sig.rule_rhs.Cckappa_sig.views in
  let creation = rule.Cckappa_sig.actions.Cckappa_sig.creation in
  let error, agent_modif_plus =
    Stochastic_classes_type.AgentMap.create parameter error 0 in
  let stochastic_classes = classes.Stochastic_classes_type.stochastic_classes in
  let error, stochastic_classes =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent stochastic_classes ->
       match agent with
       | Cckappa_sig.Ghost -> error, stochastic_classes
       | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          (*get all the sites in the lhs and rhs if there is a new created
	  agent. *)
          let sites_list =
            agent_sites
              parameter
              error
              agent_id
              agent_type
              agent
              viewsrhs
              creation
	  in
          (*store all sites associated with agent_type*)
          let error, stochastic_classes =
            add_sites_class
              parameter
              error
              agent_type
              sites_list
              stochastic_classes
          in
          error, stochastic_classes
      ) viewslhs stochastic_classes
  in
  error,
  {
    Stochastic_classes_type.stochastic_classes = stochastic_classes
  }
  
let scan_rule_set parameter error handler rules =
  let error, init = empty_classes parameter error handler in
  (*map each agent to a stochastic classes*)
  let error, agent_map =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule classes ->
       scan_rule
         parameter
         error
         handler
         rule.Cckappa_sig.e_rule_c_rule
         classes
      ) rules init
  in
  let error, init = Stochastic_classes_type.AgentMap.create
                      parameter error 0 in
  let error, result =
    Stochastic_classes_type.AgentMap.fold
      parameter
      error
      (fun parameter error id classes init ->
       	let error, eq_classes =
          Union_find.union_dic parameter error classes in
	(*store the result*)
	Stochastic_classes_type.AgentMap.set
          parameter error id eq_classes init)
      agent_map.Stochastic_classes_type.stochastic_classes
      init
  in
  error, result

let print_remanent_t parameter error result =
  Stochastic_classes_type.AgentMap.print
    error
    (fun error parameter dic ->
      let _ = Stochastic_classes_type.Dictionary_of_Stochastic_classes.print
        parameter
        error
        (fun parameter error elt l _ _ ->
          let _ = Printf.printf "Stochastic_class_id:%i:" elt in
          let _ =
            print_string "site_type:{";
            let rec print_list l =
              match l with
                | [] -> ()
                | h :: [] -> print_int h; print_string "}"
                | h :: tl ->
                  let _ = print_int h; print_string "," in
                  print_list tl in
            print_list l
          in
          let _ = print_newline () in
          error
        ) dic.Stochastic_classes_type.dic_union
      in error )
    parameter
    result
     
let stochastic_classes parameter error handler cc_compil =
  let parameter =  Remanent_parameters.update_prefix parameter "agent_type:" in 
  let error, result =
    scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules
  in
  let _ = print_remanent_t parameter error result in
  error, result
