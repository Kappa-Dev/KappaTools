 (**
  * translate_sig.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  * 
  * Creation: 2010, the 12th of August
  * Last modification: 2014, the 9th of December 
  * * 
  * Pretty printing of token library
  *  
  * Copyright 2010 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
     Exception.warn parameters mh (Some "Translate_sig") message exn (fun () -> default) 
  
let trace = false
let local_trace = false 
   
let string_of_port port = "[state_min:"^(string_of_int port.Cckappa_sig.site_state.Cckappa_sig.min)^";state_max:"^(string_of_int port.Cckappa_sig.site_state.Cckappa_sig.max)^"]"

let print_agent parameters error handler agent =
     match agent with 
    | Cckappa_sig.Agent agent -> 
       let parameters = Remanent_parameters.update_prefix parameters ("agent_type_"^(string_of_int agent.Cckappa_sig.agent_name)^":") in
       Cckappa_sig.Site_map_and_set.fold_map
         (fun a b error ->  
          let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%ssite_type_%i->state:%s\n" (Remanent_parameters.get_prefix parameters) a (string_of_port b)  in 
          error)
         agent.Cckappa_sig.agent_interface
            error
    | Cckappa_sig.Ghost -> 
      let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%sGhost\n" (Remanent_parameters.get_prefix parameters) in 
      error  

let print_diffagent parameters error handler agent =
  let parameters = Remanent_parameters.update_prefix parameters ("agent_type_"^(string_of_int agent.Cckappa_sig.agent_name)^":") in
  Cckappa_sig.Site_map_and_set.fold_map
   (fun a b error -> 
    let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%ssite_type_%i->state:%s\n" (Remanent_parameters.get_prefix parameters) a (string_of_port b) in 
    error)
   agent.Cckappa_sig.agent_interface
   error
      
let print_mixture parameters error handler mixture =
    let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%s\n" (Remanent_parameters.get_prefix parameters) in 
    let error = 
      Int_storage.Quick_Nearly_inf_Imperatif.print
       error 
       (fun error parameters a -> 
        let _ = print_agent parameters error handler a in 
            error
       ) 
       (Remanent_parameters.update_prefix parameters "agent_id_") 
       mixture.Cckappa_sig.views 
    in 
    let error = 
      Int_storage.Quick_Nearly_inf_Imperatif.print
        error
        (fun error parameters a -> 
         let error =
           Cckappa_sig.Site_map_and_set.fold_map 
              (fun k a error -> 
                let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%ssite_type_%i->agent_id_%i.site_type_%i\n" (Remanent_parameters.get_prefix parameters) k a.Cckappa_sig.agent_index a.Cckappa_sig.site
                in error
                ) 
               a
              error
          in error)
        (Remanent_parameters.update_prefix parameters "bonds:agent_id_") 
        mixture.Cckappa_sig.bonds
    in 
    let error =
      List.fold_left 
        (fun error (i,j) -> 
          let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%i+%i" i j in 
          error 
        )
        error
        mixture.Cckappa_sig.plus
    in 
     let error = 
       List.fold_left 
        (fun error (i,j) -> 
          let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%i.%i" i j 
          in  error )
        error 
        mixture.Cckappa_sig.dot
    in 
       error

let print_diffview parameters error handler diff =
    let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%s\n" (Remanent_parameters.get_prefix parameters) in 
    let error = 
      Int_storage.Quick_Nearly_inf_Imperatif.print
       error 
       (fun error parameters a -> 
          let _ = print_diffagent parameters error handler a in 
            error 
       ) 
       (Remanent_parameters.update_prefix parameters "agent_id_") 
       diff 
    in 
      error
  
 let rec print_short_alg parameters error handler alg = 
    match alg with 
     |Ast.BIN_ALG_OP(Term.MULT,a1,a2),_ -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(" in
        let error = print_short_alg parameters error handler a1 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "*" in
        let error = print_short_alg parameters error handler a2 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) ")" in
         error  
     | Ast.BIN_ALG_OP(Term.SUM,a1,a2),_ -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(" in
        let error = print_short_alg parameters error handler a1 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "+" in
        let error = print_short_alg parameters error handler a2 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) ")" in
         error  
     | Ast.BIN_ALG_OP(Term.DIV,a1,a2),_ -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(" in
        let error = print_short_alg parameters error handler a1 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "/" in
        let error = print_short_alg parameters error handler a2 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) ")" in
         error   
     | Ast.BIN_ALG_OP(Term.MINUS,a1,a2),_ -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(" in
        let error = print_short_alg parameters error handler a1 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "-" in
        let error = print_short_alg parameters error handler a2 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) ")" in
         error   
     | Ast.BIN_ALG_OP(Term.POW,a1,a2),_ -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(" in
        let error = print_short_alg parameters error handler a1 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "**" in
        let error = print_short_alg parameters error handler a2 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) ")" in
         error   
     | Ast.BIN_ALG_OP(Term.MODULO,a1,a2),_ -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(" in
        let error = print_short_alg parameters error handler a1 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "mod" in
        let error = print_short_alg parameters error handler a2 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) ")" in
         error  
     | Ast.UN_ALG_OP(Term.LOG,a1),_ -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(log(" in
        let error = print_short_alg parameters error handler a1 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "))" in
         error  
     | Ast.UN_ALG_OP(Term.SQRT,a1),_ -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(sqrt(" in
        let error = print_short_alg parameters error handler a1 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "))" in
         error 
     | Ast.UN_ALG_OP(Term.EXP,a1),_ -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(exp(" in
        let error = print_short_alg parameters error handler a1 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "))" in
         error 
     | Ast.UN_ALG_OP(Term.SINUS,a1),_ -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(sin(" in
        let error = print_short_alg parameters error handler a1 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "))" in
         error 
     | Ast.UN_ALG_OP(Term.COSINUS,a1),_ -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(cos(" in
        let error = print_short_alg parameters error handler a1 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "))" in
         error 
   (*  | Ast.UN_ALG_OP(Term.ABS,a1),_ -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(abs(" in
        let error = print_short_alg parameters error handler a1 in 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "))" in
         error  *)
     | Ast.EMAX,_  -> 
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "#Event_MAX#" in
         error
     | Ast.TMAX,_ -> 
         let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "#Time_MAX#" in
           error
     | Ast.UN_ALG_OP(Term.TAN,a1),_ -> 
          let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(tan(" in
          let error = print_short_alg parameters error handler a1 in 
          let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "))" in
         error  
     | Ast.STATE_ALG_OP Term.TIME_VAR,_ ->   
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "#TIME#" in
        error 
     | Ast.STATE_ALG_OP Term.EVENT_VAR,_ ->   
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "#EVENT#" in
        error 
   
     | Ast.OBS_VAR s,_ -> 
       let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "(OBS(%s))" s in
       error

     | Ast.CONST(Nbr.F(f)),_ ->
       let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%f " f in 
       error
         
      (*MOD: add print integer at compilation variables*)
      | Ast.CONST(Nbr.I(i)),_ ->
       let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%d " i in 
       error

     | Ast.UN_ALG_OP _,_
     | Ast.BIN_ALG_OP _,_
     | Ast.STATE_ALG_OP _,_
     | Ast.CONST _,_
     | Ast.TOKEN_ID _,_ 
     | Ast.KAPPA_INSTANCE _,_
     | Ast.PLOTNUM,_ ->  (*to do*) error 
    (* | Ast.INFINITY _ -> 
      let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "+oo" in 
         error *)
    (* | _ -> (*to do*)
       error *)
   
 let print_var parameters error handler var =
   let s = var.Cckappa_sig.e_id in
   let _ = 
     if s <> "" 
     then Printf.fprintf (Remanent_parameters.get_log parameters) "%s: " var.Cckappa_sig.e_id 
   in 
     print_short_alg parameters error handler (var.Cckappa_sig.c_variable,(Lexing.dummy_pos,Lexing.dummy_pos))

        
 let print_variables parameters error handler var =
   Int_storage.Nearly_inf_Imperatif.print_var_f
       error 
       (fun error parameters var ->
        let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%s" (Remanent_parameters.get_prefix parameters) in
           print_var parameters error handler var)
       parameters var

 let print_signatures parameters error handler signature = error

 let print_bond parameters relation (add1,add2) = 
   Printf.fprintf 
     (Remanent_parameters.get_log parameters) 
     "%s(agent_id_%d,agent_type_%d)@site_type_%d%s(agent_id_%d,agent_type_%d)@site_type_%d\n" 
     (Remanent_parameters.get_prefix parameters) 
     add1.Cckappa_sig.agent_index 
     add1.Cckappa_sig.agent_type
     add1.Cckappa_sig.site
     relation
     add2.Cckappa_sig.agent_index 
     add2.Cckappa_sig.agent_type
     add2.Cckappa_sig.site

 let print_half_bond parameters relation (add1,_) = 
   Printf.fprintf 
     (Remanent_parameters.get_log parameters) 
     "%s(agent_id_%d,agent_type_%d)@site_type_%d\n" 
     (Remanent_parameters.get_prefix parameters) 
     add1.Cckappa_sig.agent_index
     add1.Cckappa_sig.agent_type
     add1.Cckappa_sig.site
     
 let print_remove parameters (index,agent,list) =
   let _ = Printf.fprintf 
     (Remanent_parameters.get_log parameters) 
     "%s(agent_id_%d,agent_type_%d)\n" 
     (Remanent_parameters.get_prefix parameters)
     index
     agent.Cckappa_sig.agent_name
   in 
   let parameters_doc =  Remanent_parameters.update_prefix parameters "documented_site:" in
    let _ = 
      Cckappa_sig.Site_map_and_set.iter_map
        (fun site _ -> 
            Printf.fprintf 
              (Remanent_parameters.get_log parameters_doc)
	      "%s(agent_id_%d,agent_type_%d)@site_type_%d\n"
              (Remanent_parameters.get_prefix parameters_doc)
              index
              agent.Cckappa_sig.agent_name 
              site)
         agent.Cckappa_sig.agent_interface
   in 
   let parameters =  Remanent_parameters.update_prefix parameters "undocumented_site:" in
   let _ = 
     List.iter   
       (fun site  -> 
         Printf.fprintf 
           (Remanent_parameters.get_log parameters) 
           "%s(agent_id_%d,agent_type_%d)@site_type_%d\n"
           (Remanent_parameters.get_prefix parameters)
           index
           agent.Cckappa_sig.agent_name 
           site)
       list
   in  ()
   
   let print_created_agent parameters (index,agent) =
     Printf.fprintf 
       (Remanent_parameters.get_log parameters) 
       "%s(agent_id_%d,agent_type_%d)\n" 
       (Remanent_parameters.get_prefix parameters)
       index
       agent
    
   let print_actions parameters error handler actions =
   let parameters_unbinding =  Remanent_parameters.update_prefix parameters "unbinding:" in 
   let _ = List.iter (print_bond parameters_unbinding "....") (List.rev actions.Cckappa_sig.release) in
   let parameters_half_unbinding =  Remanent_parameters.update_prefix parameters "1/2unbinding:" in
   let _ = List.iter (print_half_bond parameters_half_unbinding "....") (List.rev actions.Cckappa_sig.half_break) in 
   let parameters_removal =  Remanent_parameters.update_prefix parameters "deletion:" in 
   let _ = List.iter (print_remove parameters_removal) (List.rev actions.Cckappa_sig.remove) in
   let parameters_creation =  Remanent_parameters.update_prefix parameters "creation:" in 
   let _ = List.iter (print_created_agent  parameters_creation) (List.rev actions.Cckappa_sig.creation) in
   let parameters_binding =  Remanent_parameters.update_prefix parameters "binding:" in 
   let _ = List.iter (print_bond parameters_binding "----") (List.rev actions.Cckappa_sig.bind) in
    error
   
   let print_rule parameters error handler rule =
     let parameters_lhs =  Remanent_parameters.update_prefix parameters "lhs:" in 
     let error = print_mixture parameters_lhs error handler rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs in
     let parameters_rhs =  Remanent_parameters.update_prefix parameters "rhs:" in 
     let error = print_mixture parameters_rhs error handler rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_rhs in
     let parameters_lhsdiff =  Remanent_parameters.update_prefix parameters "direct:" in 
     let error = print_diffview parameters_lhsdiff error handler rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.diff_direct in
     let parameters_rhsdiff =  Remanent_parameters.update_prefix parameters "reverse:" in 
     let error = print_diffview parameters_rhsdiff error handler rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.diff_reverse in
     let parameters_actions =  Remanent_parameters.update_prefix parameters "actions:" in 
     let error = print_actions parameters_actions error handler rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.actions in
     error 
   
   let print_rules parameters error handler rules =
     Int_storage.Nearly_inf_Imperatif.print_var_f
       error
       (fun error parameters rule ->
        print_rule parameters error handler rule)
       parameters
       rules

 let print_observables parameters error handler obs = error
 
 let print_init parameters error handler init =
(*  let parameters_init =  Remanent_parameters.update_prefix parameters "coef:" in
  let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%s%i\n" parameters_init.Remanent_parameters_sig.prefix init.Cckappa_sig.e_init_factor in *)
  let parameters_rhs =  Remanent_parameters.update_prefix parameters "mixture:" in 
  let error = print_mixture parameters_rhs error handler init.Cckappa_sig.e_init_c_mixture  in 
   error 
   
 let print_inits parameters error handler init = 
   Int_storage.Nearly_inf_Imperatif.print
       error 
       (fun error parameters init -> 
           print_init parameters error handler init) 
       parameters
       init 

 let print_perturbations parameters error handler perturbations = error  
   
 let print_compil parameters error handler compil = 
   let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "%s" (Remanent_parameters.get_prefix parameters) in 
   let parameters' =  Remanent_parameters.update_prefix parameters "variables:" in
   let error = print_variables parameters' error handler compil.Cckappa_sig.variables in 
   let parameters' =  Remanent_parameters.update_prefix parameters "signature:" in 
   let error = print_signatures parameters' error handler compil.Cckappa_sig.signatures in 
   let parameters' =  Remanent_parameters.update_prefix parameters "rules:" in 
   let error = print_rules parameters' error handler compil.Cckappa_sig.rules in 
   let parameters' =  Remanent_parameters.update_prefix parameters "observables:" in 
   let error = print_observables parameters' error handler compil.Cckappa_sig.observables in 
   let parameters' =  Remanent_parameters.update_prefix parameters "initial_states:" in 
   let error = print_inits parameters' error handler compil.Cckappa_sig.init in 
   let parameters' =  Remanent_parameters.update_prefix parameters "perturbations:" in 
   let error = print_perturbations parameters' error handler compil.Cckappa_sig.perturbations in 
     error 
    
