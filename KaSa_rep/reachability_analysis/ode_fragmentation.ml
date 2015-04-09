(**
    * ode_classes.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 2015, the 9th of Apirl
    * Last modification: 
    * * 
    * ODE fragmentation
    * 
    *  
    * Copyright 2010,2011 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    *  under the terms of the GNU Library General Public License *)

let warn parameter mh message exn default =
  Exception.warn parameter mh (Some "ODE fragmentation") message exn
                 (fun () -> default)

let add_site parameter error agent_type sites_list site_modif_plus =
  let error, old =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      site_modif_plus
  in
  let old_list =
    match old with
    | None -> []
    | Some sites -> sites
  in
  let new_list = List.concat [sites_list; old_list] in
  Int_storage.Nearly_inf_Imperatif.set
    parameter
    error
    agent_type
    new_list
    site_modif_plus
    
let scan_rule_sites_modified parameter error handler rule sites_modif_plus =
  (*collect sites that are modified*)
  let error, sites_modif_plus =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error agent_id site_modif sites_modif_plus ->
       (*if there is no sites modified then do nothing*)
       if Cckappa_sig.Site_map_and_set.is_empty_map
            site_modif.Cckappa_sig.agent_interface
       then error, sites_modif_plus
       else
         let sites_list =
           Cckappa_sig.Site_map_and_set.fold_map
             (fun site _ current_list ->
              site :: current_list) site_modif.Cckappa_sig.agent_interface []
         in
         let agent_type = site_modif.Cckappa_sig.agent_name in
         let error, sites_modif_plus =
           add_site
             parameter
             error
             agent_type
             sites_list
             sites_modif_plus
         in error, sites_modif_plus
      )
      rule.Cckappa_sig.diff_reverse sites_modif_plus
  in error, sites_modif_plus
       
  (*collect sites that is an anchor in the first case: a site connected to
    a site in an agent with a modified site *)
  (*let bonds = rule.Cckappa_sig.actions.Cckappa_sig.bind in
  let error, anchors_fst =    
    List.fold_left (fun (error, anchors_fst) (site_address, site_address')
                   ) (error, anchors_fst) bonds*)
    
      
let scan_rule parameter error handler rules =
  let error, init = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let error, agent_map =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule store_ode ->
       scan_rule_sites_modified (*FIXME*)
         parameter
         error
         handler
         rule.Cckappa_sig.e_rule_c_rule
         store_ode
      ) rules init
  in
  let error, result =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error id list init ->
       (*let error, join_list = (*TODO*)
         join parameter error list in*)
       Int_storage.Nearly_inf_Imperatif.set
         parameter
         error
         id
         list
         init)
      agent_map
      init
  in error, result

let print_t parameter error result =
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter l ->
     let _ =
       print_string "site_type_modified:{";
       let rec print_list l =
         match l with
         | [] -> print_string "empty"
         | h :: [] -> print_int h; print_string "}"
         | h :: tl ->
            let _ = print_int h; print_string "," in
            print_list tl
       in
       print_list l
     in
     let _ = print_newline () in
     error) parameter result

let ode_fragmentation parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule parameter error handler cc_compil.Cckappa_sig.rules in
  let _ = print_t parameter error result in
  error, result
  
