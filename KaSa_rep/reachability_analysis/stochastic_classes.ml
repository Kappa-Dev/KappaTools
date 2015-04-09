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
  
let scan_rule parameter error handler rule classes =
  let viewslhs = rule.Cckappa_sig.rule_lhs.Cckappa_sig.views in
  let viewsrhs = rule.Cckappa_sig.rule_rhs.Cckappa_sig.views in
  let creation = rule.Cckappa_sig.actions.Cckappa_sig.creation in
  let stochastic_class = classes.Stochastic_classes_type.stochastic_class in
  let error,stochastic_class_rhs = Stochastic_classes_type.AgentMap.create parameter error 0 in
  (*creation*)
  let error, stochastic_class_rhs =
    List.fold_left
      (fun (error, stochastic_class_rhs) (agent_id, agent_type) ->
	let error, agent =
	  Stochastic_classes_type.AgentMap.get
	    parameter
	    error
	    agent_id
	    viewsrhs
	in
	match agent with
	  | None
	  | Some Cckappa_sig.Ghost -> error, stochastic_class_rhs
	  | Some Cckappa_sig.Agent agent ->
             (*let _ = Printf.fprintf stdout "AGENT:pos:%i:%i\n" agent_id agent_type in*)
             let sites_list =
               Cckappa_sig.Site_map_and_set.fold_map
                 (fun site _ current_list ->
                  site :: current_list)
                 agent.Cckappa_sig.agent_interface []
             in
             let error, old_list =
               Stochastic_classes_type.AgentMap.unsafe_get
                 parameter
                 error
                 agent_type
                 stochastic_class_rhs
             in
             let old_list =
               match old_list with
               | None -> []
               | Some old_list -> old_list
             in
             let new_list = List.concat [sites_list;old_list] in
             (*let _ = print_string "OLD:"; Union_find.print_list old_list;
                     print_string "\n"
             in
             let _ = print_string "NEW_LIST:"; Union_find.print_list new_list;
                     print_string "\n"
             in*)
             let error, stochastic_class_rhs =
               Stochastic_classes_type.AgentMap.set
                 parameter
                 error
                 agent_type
                 new_list
                 stochastic_class_rhs
             in
             error, stochastic_class_rhs
      ) (error, stochastic_class_rhs) creation
  in
  let error, stochastic_classes =
    Stochastic_classes_type.AgentMap.fold (*FIXME*)
      parameter error
      (fun parameter error agent_id agent (*stochastic_class_rhs*) stochastic_class ->
       (*check if there is no creation agent*)
       (*let is_empty_list l =
         match l with
         | [] -> true
         | _ -> false
       in*)
       match agent with
       | Cckappa_sig.Ghost -> error, stochastic_class
       | Cckappa_sig.Agent agent ->
	  let agent_type = agent.Cckappa_sig.agent_name in
          (*let _ = Printf.fprintf stdout "AGENT:pos:%i:%i\n" agent_id agent_type in*)
          let sites_list =
            Cckappa_sig.Site_map_and_set.fold_map
              (fun site _ current_list ->
               site :: current_list)
              agent.Cckappa_sig.agent_interface []
          in
          let error, old_list =
            Stochastic_classes_type.AgentMap.unsafe_get
              parameter
              error
              agent_type
              stochastic_class
          in
          let old_list =
            match old_list with
            | None -> []
            | Some old_list -> old_list
          in
          let new_list = List.concat [sites_list;old_list] in
          (*let _ = print_string "OLD:"; Union_find.print_list old_list;
                  print_string "\n"
          in
          let _ = print_string "NEW_LIST:"; Union_find.print_list new_list;
                  print_string "\n"
          in*)
          let error, stochastic_class_lhs =
            Stochastic_classes_type.AgentMap.set
              parameter
              error
              agent_type
              new_list
              stochastic_class
          in
          error, stochastic_class_lhs
      ) viewslhs (*stochastic_class_rhs*) stochastic_class
  in
  error,
  {
    Stochastic_classes_type.stochastic_class = stochastic_class;
  }

let empty_stochastic_classes parameter error handler =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, empty_stochastic = 
    Stochastic_classes_type.AgentMap.create parameter error n_agents in
  error, 
  {
    Stochastic_classes_type.stochastic_class = empty_stochastic;
  }

let print_t parameter error result =
  Stochastic_classes_type.AgentMap.print
    error
    (fun error parameter l ->
      let _ =
	print_string "site_type:{";
        let rec print_list l  =
          match l with
          | [] -> print_string "empty"
          | h :: [] -> print_int h; print_string "}"
          | h :: tl ->
             let _ = print_int h; print_string "," in
             print_list tl in
        print_list l
      in
      let _ = print_newline () in error) parameter result
    
let scan_rule_set parameter error handler rules =
  let error, empty_stochastic_type = empty_stochastic_classes parameter error handler in
  let error, init_stochastic = Stochastic_classes_type.AgentMap.create parameter error 0 in
  (*map each agent to a stochastic classes*)
  let error, agent_map =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule list ->
       let _ = Printf.fprintf stdout "- DO with rule:%i\n" rule_id in
       let error, map =
         scan_rule
           parameter
           error
           handler
           rule.Cckappa_sig.e_rule_c_rule
           list
       in
       let error, result =
         Stochastic_classes_type.AgentMap.fold
           parameter error
           (fun parameter error id sites_list store_union ->
            let _ = Printf.fprintf stdout "- DO Agent:%i\n" id in
            let array = Array.of_list (List.rev sites_list) in (*FIXME:array start from 0*)
            let _ = print_string "ARRAY:";
                    Array.iter (fun i -> Printf.fprintf stdout "%i\n" i) array;
                    print_string "\n"
            in
            let error, union_array =
              Union_find.union_dic parameter error array sites_list
            in
            let (union_map,a) = Union_find.eq_classes_map parameter error union_array
            in
            (*fold to list*)
            let union_list =
              Cckappa_sig.Site_map_and_set.fold_map
                (fun k union _ -> union) union_map []
            in
            let _ = print_string "- UNION_MAP:";
                    Union_find.print_list union_list;
                    print_string "\n"
            in
            (*store*)
            let error, result =
              Stochastic_classes_type.AgentMap.set
                parameter
                error
                id
                union_list
                store_union
            in error, result
           )
           map.Stochastic_classes_type.stochastic_class
           init_stochastic
       in
       error,
       {Stochastic_classes_type.stochastic_class = result}
      ) rules empty_stochastic_type
  in
  error, agent_map.Stochastic_classes_type.stochastic_class
    
let stochastic_classes parameter error handler cc_compil =
  let parameter =  Remanent_parameters.update_prefix parameter "agent_type:" in 
  let error, result =
    scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules
  in
  let _ = print_t parameter error result in
  error, result
