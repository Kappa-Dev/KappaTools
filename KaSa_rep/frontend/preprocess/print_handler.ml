 (**
  * print_handler.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2011, the 17th of January
  * Last modification: 2011, the 30th of March 
  * * 
  * Pretty printing of Ckappa handler 
  * 
  *  
  * Copyright 2010,2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed    
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Print_handler") message exn (fun () -> default) 
  
let trace = false
let local_trace = false 
  

let print_state parameters state = 
  match state with 
    | Ckappa_sig.Internal a -> Printf.fprintf parameters.Remanent_parameters_sig.log "%s%s" parameters.Remanent_parameters_sig.prefix a
    | Ckappa_sig.Binding Cckappa_sig.Free -> Printf.fprintf parameters.Remanent_parameters_sig.log "%sfree" parameters.Remanent_parameters_sig.prefix 
    | Ckappa_sig.Binding Cckappa_sig.Lnk_type (a,b) -> Printf.fprintf parameters.Remanent_parameters_sig.log "%s%d@%d" parameters.Remanent_parameters_sig.prefix a b 

let print_site parameters site = 
  match site with 
    | Ckappa_sig.Internal a -> Printf.fprintf parameters.Remanent_parameters_sig.log "%s%s(internal state)" parameters.Remanent_parameters_sig.prefix a
    | Ckappa_sig.Binding a -> Printf.fprintf parameters.Remanent_parameters_sig.log "%s%s(binding state)" parameters.Remanent_parameters_sig.prefix a 

let print_handler parameters error handler = 
  let log = parameters.Remanent_parameters_sig.log in 
  let _ = Printf.fprintf log "%s:\n" parameters.Remanent_parameters_sig.prefix in 
  let parameters_agent = Remanent_parameters.update_prefix parameters "agents:" in 
  let _ = Printf.fprintf log "%s \n" parameters_agent.Remanent_parameters_sig.prefix in
  let print_f print_aux =   
    (fun parameters error i site () () -> 
      let parameters = Remanent_parameters.update_prefix parameters ((string_of_int i)^"->") in 
      let _ = print_aux parameters site in 
      let _ = Printf.fprintf parameters.Remanent_parameters_sig.log "\n" in 
       error)
  in 
  let error = 
    Ckappa_sig.Dictionary_of_agents.print 
      parameters_agent
      error
      (fun parameters error i agent_name () () -> 
        let _ = Printf.fprintf parameters_agent.Remanent_parameters_sig.log "%s%d:%s\n" parameters_agent.Remanent_parameters_sig.prefix i agent_name
        in error)
      handler.Cckappa_sig.agents_dic 
  in 
  let parameters_sites = Remanent_parameters.update_prefix parameters "sites:" in 
  let _ = Printf.fprintf log "%s \n" parameters_sites.Remanent_parameters_sig.prefix in
  let error = 
    Int_storage.Nearly_inf_Imperatif.print  
      error 
      (fun error parameters a -> 
        let _ = Ckappa_sig.Dictionary_of_sites.print parameters error (print_f print_site) a in error)
      parameters_sites
      handler.Cckappa_sig.sites 
  in
  let parameters_states = Remanent_parameters.update_prefix parameters "states:" in 
  let _ = Printf.fprintf log "%s \n" parameters_states.Remanent_parameters_sig.prefix in
  let error = 
    Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.print  
      error 
      (fun error parameters a -> 
          Cckappa_sig.Dictionary_of_States.print parameters error (print_f print_state) a)
      parameters_states
      handler.Cckappa_sig.states_dic 
  in  
  let parameters_duals = Remanent_parameters.update_prefix parameters "duals:" in 
  let _ = Printf.fprintf log "%s \n" parameters_duals.Remanent_parameters_sig.prefix in
  let error = 
    Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.print  
      error 
      (fun error parameters (a,b,c) -> 
          let _ = Printf.fprintf log "%s%i,%i,%i\n" parameters.Remanent_parameters_sig.prefix a b c in 
          error)
      parameters_duals
      handler.Cckappa_sig.dual 
  in  
  error 
  
let dot_of_contact_map parameters (error:Exception.method_handler) handler = 
    let error,parameters_dot = 
          Remanent_parameters.open_contact_map_file parameters error
    in 
    let _ = Printf.fprintf parameters_dot.Remanent_parameters_sig.log "%s%s\n" Headers.dot_comment Headers.head_contact_map_in_dot in 
    let _ = Printf.fprintf parameters_dot.Remanent_parameters_sig.log "graph G{ \n" in 
    let _ = 
      Ckappa_sig.Dictionary_of_agents.print 
        parameters_dot
        error
        (fun parameters_dot error i agent_name () () -> 
          let _ = Printf.fprintf parameters_dot.Remanent_parameters_sig.log "subgraph cluster%d {\n" i in 
          let error,site_dic = 
             Misc_sa.unsome 
              (Int_storage.Nearly_inf_Imperatif.get parameters_dot error i handler.Cckappa_sig.sites)
              (fun error -> warn parameters_dot error (Some "line 103") Exit (Ckappa_sig.Dictionary_of_sites.init ()))  
          in
          let error = 
            Ckappa_sig.Dictionary_of_sites.print 
              parameters_dot
              error 
              (fun parameters_dot error j site () () -> 
                let _ = 
                  match site 
                  with 
                      | Ckappa_sig.Internal site_name -> 
                          Printf.fprintf 
                              parameters_dot.Remanent_parameters_sig.log 
                              "   %d.%d [style = filled label = \"%s\" shape =%s color = %s size = \"5\"]\n"
                              i
                              j 
                              site_name
                              parameters_dot.Remanent_parameters_sig.contact_map_output.Remanent_parameters_sig.internal_site_shape
                              parameters_dot.Remanent_parameters_sig.contact_map_output.Remanent_parameters_sig.internal_site_color
                      | Ckappa_sig.Binding site_name ->
                        Printf.fprintf 
                              parameters_dot.Remanent_parameters_sig.log 
                              "   %d.%d [style = filled label = \"%s\" shape =%s color = %s size = \"5\"]\n"
                              i
                              j 
                              site_name
                              parameters_dot.Remanent_parameters_sig.contact_map_output.Remanent_parameters_sig.binding_site_shape
                              parameters_dot.Remanent_parameters_sig.contact_map_output.Remanent_parameters_sig.binding_site_color
                    in 
                          
                          error)
              site_dic 
          in 
          let error,n_sites = Ckappa_sig.Dictionary_of_sites.last_entry parameters_dot error site_dic  in
          let color = 
            Misc_sa.fetch_array 
              n_sites 
              parameters_dot.Remanent_parameters_sig.contact_map_output.Remanent_parameters_sig.agent_color_array
              parameters_dot.Remanent_parameters_sig.contact_map_output.Remanent_parameters_sig.agent_color_def
          in 
          let shape = 
            Misc_sa.fetch_array 
              n_sites 
              parameters_dot.Remanent_parameters_sig.contact_map_output.Remanent_parameters_sig.agent_shape_array
              parameters_dot.Remanent_parameters_sig.contact_map_output.Remanent_parameters_sig.agent_shape_def
          in 
          let _ = Printf.fprintf 
              parameters_dot.Remanent_parameters_sig.log 
              "label =  \"%s\";  shape = %s; color = %s \n"
              agent_name
              shape
              color 
          in 
          
          let _ = Printf.fprintf parameters_dot.Remanent_parameters_sig.log "} ; \n" in 
			error)	
      handler.Cckappa_sig.agents_dic 
    in 
    let _ = 
            Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.iter                         
              parameters_dot
              error 
              (fun parameters_dot error (i,(j,k)) (i',j',k') -> 
                if i<i' or (i=i' && j<=j') 
                then 
                  let _ = 
                    Printf.fprintf parameters_dot.Remanent_parameters_sig.log "%d.%d -- %d.%d\n" i j i' j' 
                  in error 
                else
                  error)
              handler.Cckappa_sig.dual  
          in               
    let _ = Printf.fprintf parameters_dot.Remanent_parameters_sig.log "}\n" in 
    let error,parameters = Remanent_parameters.close_file parameters_dot error 
    in error 
    
