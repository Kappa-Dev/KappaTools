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
                 
(************************************************************************************)
(*MODIFIED SITES*)

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

(************************************************************************************)
(*ANCHOR SITES*)
              
(*++ first element in a pair of site that are bond*)

let collect_store_bond_1 parameter error site_address
                         store_sites_modified
                         store_sites_bond_modified
  =
  (*store_sites_bond_modified is a set of sites that are bond, it is taken from a
  list of sites in a set of sites that is modified.*)
  let agent_type = site_address.Cckappa_sig.agent_type in
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
  let error, old_list =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_bond_modified
  in
  let old_list =
    match old_list with
    | None -> []
    | Some s -> s
  in
  let new_list = List.concat [sites_list; old_list] in
  let error, store_sites_bond_modified =
    Int_storage.Nearly_inf_Imperatif.set
      parameter
      error
      agent_type
      new_list
      store_sites_bond_modified
  in
  error, store_sites_bond_modified
           
(*------------------------------------------------------------------------------*)
(*++ second element in a pair of site that are bond*)

let collect_store_bond_2 parameter error bond_rhs
                         site_address
                         store_sites_bond
  =
  let agent_id = site_address.Cckappa_sig.agent_index in
  let agent_type = site_address.Cckappa_sig.agent_type in
  let error, site_address_map =
    Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_id
      bond_rhs
  in
  let site_adress =
    match site_address_map with
    | None -> Cckappa_sig.Site_map_and_set.empty_map
    | Some s -> s
  in
  let sites_bond_list =
    Cckappa_sig.Site_map_and_set.fold_map
      (fun site _ current_list -> site :: current_list) site_adress []
  in
  let error, old_list =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_bond
  in
  let old_list =
    match old_list with
    | None -> []
    | Some s -> s
  in
  let new_list = List.concat [sites_bond_list; old_list] in
  let error, store_sites_bond =
    Int_storage.Nearly_inf_Imperatif.set
      parameter
      error
      agent_type
      new_list
      store_sites_bond
  in
  error, store_sites_bond

(*------------------------------------------------------------------------------*)
(*+ pair of site that are bond*)

let collect_sites_bond_pair parameter error bond_rhs
                            store_sites_modified
                            site_address_modified
                            site_address
                            store_sites_bond_modified
                            store_sites_bond
                            store_sites_bond_pair
  =
  (*anchor first case:
  A site connected to a site in an agent with a modified site*)
  let error, store_sites_bond_modified =
    collect_store_bond_1
      parameter
      error
      site_address_modified
      store_sites_modified
      store_sites_bond_modified
  in
  let error, store_sites_bond =
    collect_store_bond_2
      parameter
      error
      bond_rhs
      site_address
      store_sites_bond    
  in
  let error, store_sites_bond_pair =
    (store_sites_bond_modified, store_sites_bond)
  in
  error, store_sites_bond_pair

(*------------------------------------------------------------------------------*)
(*-- collect binding sites in the rhs with: modified site -> bind site*)

let store_sites_bond_pair parameter error bond_rhs bind
                          store_sites_modified
                          store_sites_bond_pair
  =
  let error, store_sites_bond_pair =
    List.fold_left (fun (error, store_sites_bond_pair)
                        (site_address_modified, site_address) ->
                    let error, store_sites_bond_pair =
                      error, collect_sites_bond_pair
                               parameter
                               error
                               bond_rhs
                               store_sites_modified
                               site_address_modified
                               site_address
                               (fst store_sites_bond_pair)
                               (snd store_sites_bond_pair)
                               store_sites_bond_pair                        
                    in
                    error, store_sites_bond_pair
                   )
                   (error, store_sites_bond_pair) bind
  in
  error, store_sites_bond_pair

(*------------------------------------------------------------------------------*)
(*- collect anchor sites*)

let store_sites_anchor parameter error init store_sites_bond_pair =
  (*get the old_list*)
  let error, store_sites_anchor =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_type a old_list_t ->
       let error, get_old_list =
         Int_storage.Nearly_inf_Imperatif.unsafe_get
           parameter
           error
           agent_type
           old_list_t
       in
       let old_list =
         match get_old_list with
         | None -> []
         | Some s -> s
       in
       let new_list = List.concat [a; old_list] in
       let error, anchor =
         Int_storage.Nearly_inf_Imperatif.set
           parameter
           error
           agent_type
           new_list
           old_list_t
       in error, anchor
      )
      (snd store_sites_bond_pair)
      init
  in
  error, store_sites_anchor
  
              
(************************************************************************************)
(*TYPE*)

module AgentMap = Int_storage.Nearly_inf_Imperatif

type sites_ode = (int list AgentMap.t * int list AgentMap.t)

type ode_frag =
    {
      store_sites_modified  : int list AgentMap.t;
      store_sites_bond_pair : sites_ode;
      store_sites_anchor    : int list AgentMap.t
    }
      
(************************************************************************************)   
(*RULE*)

let scan_rule parameter error handler rule ode_class =
  let bind = rule.Cckappa_sig.actions.Cckappa_sig.bind in
  let bond_rhs = rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds in
  (*------------------------------------------------------------------------------*)
  (*a)collect modified sites*)
  let error, store_sites_modified =
    collect_sites_modified
      parameter
      error
      rule
      ode_class.store_sites_modified
  in
  (*------------------------------------------------------------------------------*)
  (*b) collect anchor sites: a site connected to a site in an agent with a
  modified site*)
  let error, store_sites_bond_pair =
    store_sites_bond_pair
      parameter
      error
      bond_rhs
      bind
      store_sites_modified
      (ode_class.store_sites_bond_pair)
  in
  let error, store_sites_anchor =
    let error, init = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
    store_sites_anchor
      parameter
      error
      init
      store_sites_bond_pair
  in
  (*------------------------------------------------------------------------------*)
  (*return value of ode_class*)
  error,
  {
    store_sites_modified = store_sites_modified;
    store_sites_bond_pair = store_sites_bond_pair;
    store_sites_anchor = store_sites_anchor
  }
    
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler rules =
  let error, init =
    Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let init_pair = (init, init) in
  (*init state of ode_class*)
  let init_ode =
    {
      store_sites_modified  = init;
      store_sites_bond_pair = init_pair;
      store_sites_anchor    = init
    }
  in
  let error, ode_class =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule ode_class ->
       scan_rule
         parameter
         error
         handler
         rule.Cckappa_sig.e_rule_c_rule
         ode_class
      ) rules init_ode
  in
  error, ode_class
           
(************************************************************************************)
(*PRINT*)

let print_modified parameter error result =
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter l ->
     let _ =
       print_string "site_type_modified:";
       print_list l
     in
     error) parameter result
    
let print_anchor parameter error result =
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter l ->
     let _ =
       print_string "site_type_anchor:";
       print_list l
     in
     error) parameter result


(************************************************************************************)
(*MAIN PRINT*)
    
let print_ode parameter error
              { store_sites_modified;
                store_sites_bond_pair;
                store_sites_anchor
              } =
  let _ = Printf.fprintf stdout "* Sites that are modified:\n" in
  let m = print_modified parameter error store_sites_modified in
  m;
  let _ = Printf.fprintf stdout "* Anchor sites:\n" in
  let a = print_anchor parameter error store_sites_anchor in
  a

    
(************************************************************************************)     
(*MAIN*)

let ode_fragmentation parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules in
  let _ = print_ode parameter error result in
  error, result
