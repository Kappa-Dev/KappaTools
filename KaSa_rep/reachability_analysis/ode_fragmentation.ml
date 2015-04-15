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

let sprintf_list l =
  let acc = ref "{" in
  List.iteri (fun i x ->
              acc := !acc ^
                       if i <> 0
                       then Printf.sprintf "; %d" x
                       else Printf.sprintf "%d" x
             ) l;
  !acc ^ "}"
           
let print_list l =
  let output = sprintf_list l in
  Printf.fprintf stdout "%s\n" output

let add_site_modified parameter error agent_type sites_list store_sites_modified =
  let error, old =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_modified
  in
  let old_list =
    match old with
    | None -> []
    | Some sites -> sites
  in
  (*store*)
  let new_list = List.concat [sites_list; old_list] in
  Int_storage.Nearly_inf_Imperatif.set
    parameter
    error
    agent_type
    new_list
    store_sites_modified

let collect_sites_modified parameter error rule store_sites_modified =
  let error, store_sites_modified =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error agent_id site_modif store_sites_modified ->
       let agent_type = site_modif.Cckappa_sig.agent_name in
       (*if there is no sites modified then do nothing*)
       if Cckappa_sig.Site_map_and_set.is_empty_map
            site_modif.Cckappa_sig.agent_interface
       then error, store_sites_modified
       else
         (*collect sites in an agent interface*)
         let sites_list =
           Cckappa_sig.Site_map_and_set.fold_map
             (fun site _ current_list ->
              site :: current_list) site_modif.Cckappa_sig.agent_interface []
         in
         (*map those sites with agent_type and store it into a store_sites_modified*)
           let error, store_sites_modified =
           add_site_modified
             parameter
             error
             agent_type
             sites_list
             store_sites_modified
           in
           error, store_sites_modified
      )
      rule.Cckappa_sig.diff_reverse store_sites_modified
  in error, store_sites_modified
  
let collect_sites_bond_2 parameter error site_address bond_rhs store_sites_bond_2 =
  (*store_sites_bond_2 is a set of sites that are bond, it is taken from a list of sites
  in rule rhs that are bond.*) 
  let agent_id = site_address.Cckappa_sig.agent_index in
  let agent_type = site_address.Cckappa_sig.agent_type in
  let site = site_address.Cckappa_sig.site in
  (*let _ = print_string "AGENT_ID:"; print_int agent_id;
          print_string "\n" in
  let _ = print_string "AGENT_TYPE:"; print_int agent_type
          ; print_string "\n"
  in
  let _ = print_string "SITE:"; print_int site; print_string "\n" in*)
  (*get sites_address map from bond_rhs*)
  let error, site_address_map =
    Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_id
      bond_rhs
  in
  let site_address =
    match site_address_map with
    | None -> Cckappa_sig.Site_map_and_set.empty_map
    | Some s -> s
  in
  (*build sites that are bond into list*)
  let sites_bond_list =
    Cckappa_sig.Site_map_and_set.fold_map
      (fun site _ current_list ->
       site :: current_list) site_address []
  in
  (*get the old_list in store_sites_bond_2*)
  let error, old_list =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_bond_2
  in
  let old_list =
    match old_list with
    | None -> []
    | Some old_list -> old_list
  in                                   
  (*store*)
  let new_list = List.concat [sites_bond_list; old_list] in
  (*let _ = print_string "NEW_LIST:";
    print_list new_list; print_string "\n" in*)
  let error, store_sites_bond_2 =
    Int_storage.Nearly_inf_Imperatif.set
      parameter
      error
      agent_type
      new_list
      store_sites_bond_2
  in error, store_sites_bond_2

let collect_store_bond_1 parameter error site_address store_sites_modified store_sites_bond_1 =
  (*store_sites_bond1 is a set of sites that are bond, it is taken from a
  list of sites in a set of sites that is modified.*)    
  let agent_id = site_address.Cckappa_sig.agent_index in
  let agent_type = site_address.Cckappa_sig.agent_type in
  let site = site_address.Cckappa_sig.site in
  (*let _ = print_string "AGENT_ID:"; print_int agent_id;
          print_string "\n" in
  let _ = print_string "AGENT_TYPE:"; print_int agent_type
          ; print_string "\n"
  in
  let _ = print_string "SITE:"; print_int site; print_string "\n" in*)
  (*get site_address list *)
  let error, get_sites_list =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_modified
  in
  let sites_list =
    match get_sites_list with
    | None -> []
    | Some s -> s
  in
  (*get the old_list in store_sites_bond_1*)
  let error, old_list =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_bond_1
  in
  let old_list =
    match old_list with
    | None -> []
    | Some old_list -> old_list
  in
  (*store*)
  let new_list = List.concat [sites_list; old_list] in
  (*let _ = print_string "NEW_LIST:";
          print_list new_list; print_string "\n" in*)
  let error, store_sites_bond_1 =
    Int_storage.Nearly_inf_Imperatif.set
      parameter
      error
      agent_type
      new_list
      store_sites_bond_1
  in
  error, store_sites_bond_1
              
(*anchor first case*)
let collect_sites_bond_pair_1 parameter error rule site_address_modified site_address store_sites_modified store_sites_bond_1 store_sites_bond_2 store_sites_bond_pair =
  (*store_sites_bond_pair_1:(site_adress_modified, site_address*)
  (*a) collect sites that are modified *)
  let error, store_sites_modified =
    collect_sites_modified
      parameter
      error
      rule
      store_sites_modified
  in
  (*b) collect sites that are bond where sites are taken from a set of
  sites that are modified*)
  let error, store_sites_bond_1 =
    collect_store_bond_1
      parameter
      error
      site_address_modified
      store_sites_modified
      store_sites_bond_1
  in
  (*c) collect sites that are bond where sites are taken from a set of
  sites in the rule rhs that are bond*)
  let bond_rhs = rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds in
  let error, store_sites_bond_2 =
    collect_sites_bond_2
      parameter
      error
      site_address
      bond_rhs
      store_sites_bond_2
  in
  let error, store_sites_bond_pair =
    (store_sites_bond_1, store_sites_bond_2)    
  in
  error, store_sites_bond_pair

let collect_sites_bond_pair_2 parameter error rule site_address site_address_modified store_sites_modified store_sites_bond_1 store_sites_bond_2 store_sites_bond_pair =
  (*store_sites_bond_pair_2:(site_adress, site_address_modified)*)
  (*a) collect sites that are modified *)
  let error, store_sites_modified =
    collect_sites_modified
      parameter
      error
      rule
      store_sites_modified
  in
  (*b) collect sites that are bond where sites are taken from a set of
  sites that are modified*)
  let error, store_sites_bond_1 =
    collect_store_bond_1
      parameter
      error
      site_address_modified
      store_sites_modified
      store_sites_bond_1
  in
  (*c) collect sites that are bond where sites are taken from a set of
  sites in the rule rhs that are bond*)
  let bond_rhs = rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds in
  let error, store_sites_bond_2 =
    collect_sites_bond_2
      parameter
      error
      site_address
      bond_rhs
      store_sites_bond_2
  in
  let error, store_sites_bond_pair =
    (store_sites_bond_2, store_sites_bond_1)    
  in
  error, store_sites_bond_pair

module AgentMap = Int_storage.Nearly_inf_Imperatif

type sites_ode = (int list AgentMap.t * int list AgentMap.t)

type ode_frag =
    {
      store_sites_bond_pair_1 : sites_ode;
      store_sites_bond_pair_2 : sites_ode
    }

let scan_rule parameter error handler rule ode_class =
  let bind = rule.Cckappa_sig.actions.Cckappa_sig.bind in
  (*create the init*)
  let error, init_store_sites_modified =
    Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let error, init_store_sites_bond_1 =
    Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let error, init_store_sites_bond_2 =
    Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let error, store_sites_bond_pair_1 =
    List.fold_left (fun (error, store_sites_bond_pair)
      (site_address_modified, site_address) ->
        error, collect_sites_bond_pair_1
          parameter
          error
          rule
            site_address_modified
              site_address
              init_store_sites_modified
              init_store_sites_bond_1
              init_store_sites_bond_2
              store_sites_bond_pair
    )(error, ode_class.store_sites_bond_pair_1) bind
  in
  let error, store_sites_bond_pair_2 =
    List.fold_left (fun (error, store_sites_bond_pair)
      (site_address, site_address_modified) ->
        error, collect_sites_bond_pair_2
          parameter
          error
          rule
            site_address
              site_address_modified
              init_store_sites_modified
              init_store_sites_bond_1
              init_store_sites_bond_2
              store_sites_bond_pair
    )(error, ode_class.store_sites_bond_pair_2) bind
  in
  error,
  {
    store_sites_bond_pair_1 = store_sites_bond_pair_1;
    store_sites_bond_pair_2 = store_sites_bond_pair_2
  }
    
let scan_rule_set parameter error handler rules =
  let error, init =
    Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let init_pair = (init, init) in
  let init_ode = 
    {store_sites_bond_pair_1 = init_pair;
     store_sites_bond_pair_2 = init_pair}
  in
  let error, ode_class =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule ode_class ->
       let _ = Printf.fprintf stdout "- DO rule_id:%i\n" rule_id in
       scan_rule
         parameter
         error
         handler
         rule.Cckappa_sig.e_rule_c_rule
         ode_class
      ) rules init_ode
  in
  error, ode_class
          
let print_modified parameter error result =
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter l ->
     let _ =
       print_string "site_type_modified:";
       print_list l
     in
     error) parameter result

let print_bond parameter error result =
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter l ->
     let _ =
       print_string "site_type_bond:";
       print_list l
     in
     error) parameter result

let print_pair_1 parameter error (result, result') = 
  let p1 = print_modified parameter error result in
  let p2 = print_bond parameter error result' in
  p1, p2

let print_pair_2 parameter error (result, result') = 
  let p1 = print_bond parameter error result in
  let p2 = print_modified parameter error result' in
  p1, p2

let print_ode parameter error {store_sites_bond_pair_1; store_sites_bond_pair_2} =
  let p1 = print_pair_1 parameter error store_sites_bond_pair_1 in
  let p2 = print_pair_2 parameter error store_sites_bond_pair_2 in
  p1; p2
          
let ode_fragmentation parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules in
  let _ = print_ode parameter error result in
  error, result
  
