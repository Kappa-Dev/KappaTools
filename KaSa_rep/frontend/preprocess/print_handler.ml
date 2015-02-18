 (**
  * print_handler.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2011, the 17th of January
  * Last modification: 2015, the 05th of February
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
    | Ckappa_sig.Internal a -> Printf.fprintf (Remanent_parameters.get_log parameters) "%s%s" (Remanent_parameters.get_prefix parameters) a
    | Ckappa_sig.Binding Cckappa_sig.Free -> Printf.fprintf (Remanent_parameters.get_log parameters) "%sfree" (Remanent_parameters.get_prefix parameters) 
    | Ckappa_sig.Binding Cckappa_sig.Lnk_type (a,b) -> Printf.fprintf (Remanent_parameters.get_log parameters) "%sagent_type:%d@site_type:%d" (Remanent_parameters.get_prefix parameters) a b

let print_site parameters site = 
  match site with 
  | Ckappa_sig.Internal a -> Printf.fprintf (Remanent_parameters.get_log parameters) "%s%s(internal state)" (Remanent_parameters.get_prefix parameters) a
    | Ckappa_sig.Binding a -> Printf.fprintf (Remanent_parameters.get_log parameters) "%s%s(binding state)" (Remanent_parameters.get_prefix parameters) a 

let print_handler parameters error handler = 
  let log = (Remanent_parameters.get_log parameters) in 
  let _ = Printf.fprintf log "%s\n" (Remanent_parameters.get_prefix parameters) in
  let parameters_agent = Remanent_parameters.update_prefix parameters "agents:" in
  let _ = Printf.fprintf log "%s \n" (Remanent_parameters.get_prefix parameters_agent) in 
  let print_f print_aux =   
    (fun parameters error i site () () -> 
     let parameters = Remanent_parameters.update_prefix parameters ("site_type:"^(string_of_int i)^"->") in
      let _ = print_aux parameters site in 
      let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "\n" in
       error)
  in
  let print_state_f print_aux =   
    (fun parameters error i state () () -> 
     let parameters = Remanent_parameters.update_prefix parameters ("state_id:"^(string_of_int i)^"->") in
     let _ =   print_aux parameters state in
      let _ = Printf.fprintf (Remanent_parameters.get_log parameters) "\n" in 
       error)
  in
  let error = 
    Ckappa_sig.Dictionary_of_agents.print
      parameters_agent
      error
      (fun parameters error i agent_name () () -> 
       let _ = Printf.fprintf (Remanent_parameters.get_log parameters_agent) "%sagent_type:%d:%s\n" (Remanent_parameters.get_prefix parameters_agent) i agent_name
        in error)
      handler.Cckappa_sig.agents_dic
  in 
  let parameters_sites = Remanent_parameters.update_prefix parameters "sites:" in
  let _ = Printf.fprintf log "%s \n" (Remanent_parameters.get_prefix parameters_sites) in
  let error = 
    Int_storage.Nearly_inf_Imperatif.print_site_f
      error
      (fun error parameters a ->
       let _ = Ckappa_sig.Dictionary_of_sites.print parameters error (print_f print_site) a in error)
      parameters_sites
      handler.Cckappa_sig.sites
  in
  let parameters_states = Remanent_parameters.update_prefix parameters "states:" in 
  let _ = Printf.fprintf log "%s \n" (Remanent_parameters.get_prefix parameters_states) in
  let error = 
    Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.print
      error 
      (fun error parameters a ->
       Cckappa_sig.Dictionary_of_States.print parameters error (print_state_f print_state) a)
      parameters_states
      handler.Cckappa_sig.states_dic
  in  
  let parameters_duals = Remanent_parameters.update_prefix parameters "duals:" in 
  let _ = Printf.fprintf log "%s \n" (Remanent_parameters.get_prefix parameters_duals) in 
  let error = 
    Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.print
      error 
      (fun error parameters (a,b,c) -> 
          let _ = Printf.fprintf log "%sagent_type:%i,site_type:%i,state_id:%i\n" (Remanent_parameters.get_prefix parameters) a b c in
          error)
      parameters_duals
      handler.Cckappa_sig.dual 
  in  
  error 
  
let dot_of_contact_map parameters (error:Exception.method_handler) handler = 
    let error,parameters_dot = 
          Remanent_parameters.open_contact_map_file parameters error
    in 
    let _ = Printf.fprintf (Remanent_parameters.get_log parameters_dot) "%s%s\n" Headers.dot_comment Headers.head_contact_map_in_dot in 
    let _ = Printf.fprintf (Remanent_parameters.get_log parameters_dot) "graph G{ \n" in 
    let _ = 
      Ckappa_sig.Dictionary_of_agents.print 
        parameters_dot
        error
        (fun parameters_dot error i agent_name () () -> 
          let _ = Printf.fprintf (Remanent_parameters.get_log parameters_dot) "subgraph cluster%d {\n" i in 
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
                              (Remanent_parameters.get_log parameters_dot) 
                              "   %d.%d [style = filled label = \"%s\" shape =%s color = %s size = \"5\"]\n"
                              i
                              j 
                              site_name
                              (Remanent_parameters.get_internal_site_shape parameters_dot)
                              (Remanent_parameters.get_internal_site_color parameters_dot)
                      | Ckappa_sig.Binding site_name ->
                        Printf.fprintf 
                              (Remanent_parameters.get_log parameters_dot) 
                              "   %d.%d [style = filled label = \"%s\" shape =%s color = %s size = \"5\"]\n"
                              i
                              j 
                              site_name
                              (Remanent_parameters.get_binding_site_shape parameters_dot)
                              (Remanent_parameters.get_binding_site_color parameters_dot)
                in 
                error)
              site_dic 
          in 
          let error,n_sites = Ckappa_sig.Dictionary_of_sites.last_entry parameters_dot error site_dic  in
          let color = 
            Misc_sa.fetch_array 
              n_sites 
              (Remanent_parameters.get_agent_color_array parameters_dot)
              (Remanent_parameters.get_agent_color_def parameters_dot)
          in 
          let shape = 
            Misc_sa.fetch_array 
              n_sites 
              (Remanent_parameters.get_agent_shape_array parameters_dot)
              (Remanent_parameters.get_agent_shape_def parameters_dot)
          in 
          let _ = 
	    Printf.fprintf 
              (Remanent_parameters.get_log parameters_dot)
              "label =  \"%s\";  shape = %s; color = %s \n"
              agent_name
              shape
              color 
          in 
          let _ = Printf.fprintf (Remanent_parameters.get_log parameters_dot) "} ; \n" in 
			error)	
      handler.Cckappa_sig.agents_dic 
    in 
    let _ = 
            Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.iter                         
              parameters_dot
              error 
              (fun parameters_dot error (i,(j,k)) (i',j',k') -> 
                if i<i' || (i=i' && j<=j') 
                then 
                  let _ = 
                    Printf.fprintf (Remanent_parameters.get_log parameters_dot) "%d.%d -- %d.%d\n" i j i' j' 
                  in error 
                else
                  error)
              handler.Cckappa_sig.dual  
          in               
    let _ = Printf.fprintf (Remanent_parameters.get_log parameters_dot) "}\n" in 
    let error,parameters = Remanent_parameters.close_file parameters_dot error 
    in error 
    
