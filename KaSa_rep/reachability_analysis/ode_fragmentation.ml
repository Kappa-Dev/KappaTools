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
  
let collect_sites_bond_2 parameter error bond_rhs site_address store_sites_bond_2 =
  (*store_sites_bond_2 is a set of sites that are bond, it is taken from a list of sites
  in rule rhs that are bond.*) 
  let agent_id = site_address.Cckappa_sig.agent_index in
  let agent_type = site_address.Cckappa_sig.agent_type in
  let site = site_address.Cckappa_sig.site in
  (*let _ = Printf.fprintf stdout "agent_pos:%i:agent_type:%i:site:%i\n"
    agent_id agent_type site 
  in*)
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
    print_list new_list; print_string "\n"
  in*)
  let error, store_sites_bond_2 =
    Int_storage.Nearly_inf_Imperatif.set
      parameter
      error
      agent_type
      new_list
      store_sites_bond_2
  in error, store_sites_bond_2

let collect_store_bond_1 parameter error site_address store_sites_modified store_sites_bond_1 =
  (*store_sites_bond_1 is a set of sites that are bond, it is taken from a
  list of sites in a set of sites that is modified.*)    
  let agent_id = site_address.Cckappa_sig.agent_index in
  let agent_type = site_address.Cckappa_sig.agent_type in
  let site = site_address.Cckappa_sig.site in
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
  let error, store_sites_bond_1 =
    Int_storage.Nearly_inf_Imperatif.set
      parameter
      error
      agent_type
      new_list
      store_sites_bond_1
  in
  error, store_sites_bond_1
              
(*anchor first case:
  A site connected (bond) to a site in an agent with a modified site*)
let collect_sites_bond_pair_1 parameter error rule bond_rhs site_address_modified site_address store_sites_modified store_sites_bond_1 store_sites_bond_2 store_sites_bond_pair =
  (*store_sites_bond_pair_1:(site_address_modified, site_address*)
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
  let error, store_sites_bond_2 =
    collect_sites_bond_2
      parameter
      error
      bond_rhs
      site_address
      store_sites_bond_2
  in
  let error, store_sites_bond_pair =
    (store_sites_bond_1, store_sites_bond_2)    
  in
  error, store_sites_bond_pair

(*anchor second case*)
 (*a) collect sites that are bond, where sites are taken from a set of
  sites in the rule rhs that are bond*)
let collect_sites_bond_rhs parameter error rule bond_rhs site_address_1 site_address_2 store_sites_bond_rhs_1 store_sites_bond_rhs_2 store_sites_bond_rhs =
  (*first agent that are bond in a pair of agents*)
  let error, store_sites_bond_1 =
    collect_sites_bond_2
      parameter
      error
      bond_rhs
      site_address_1
      store_sites_bond_rhs_1
  in
  (*second agent that are bond in a pair of agents*)
  let error, store_sites_bond_2 =
    collect_sites_bond_2
      parameter
      error
      bond_rhs
      site_address_2
      store_sites_bond_rhs_2
  in
  let error, store_sites_bond_rhs =
    (store_sites_bond_1, store_sites_bond_2)
  in error, store_sites_bond_rhs

(*ode_frag type*)
module AgentMap = Int_storage.Nearly_inf_Imperatif

type sites_ode = (int list AgentMap.t * int list AgentMap.t)

type ode_frag =
    {
      store_sites_modified : int list AgentMap.t; (*Information*)
      store_sites_bond_pair_1 : sites_ode;
      store_sites_anchor_1 : int list AgentMap.t; (*Information*)
      store_sites_bond_rhs : sites_ode;
      (*external flow (a, b) if a is an anchor or b is a modified sites*)
      store_external_flow : (int list * int list)
    }

let scan_rule parameter error handler rule ode_class =
  let bind = rule.Cckappa_sig.actions.Cckappa_sig.bind in
  let bond_rhs = rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds in
  (*create the init*)
  let error, init_store_sites_modified =
    Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  (*FOR information about modified sites*)
  let error, store_sites_modified =
    collect_sites_modified
      parameter
      error
      rule
        ode_class.store_sites_modified
  in
  (*a)collect anchor sites in the first case*)
  let error, store_sites_bond_pair_1 =
    List.fold_left (fun (error, store_sites_bond_pair)
      (site_address_modified, site_address) ->
        error, collect_sites_bond_pair_1
          parameter
          error
          rule
            bond_rhs
            site_address_modified
              site_address
              init_store_sites_modified
              (fst store_sites_bond_pair)
              (snd store_sites_bond_pair)
              store_sites_bond_pair
    )(error, ode_class.store_sites_bond_pair_1) bind
  in
  (*b)collect anchor sites in the second case*)
  (*TODO:check the conditions*)
  (*return binding sites*)
  let error, store_sites_bond_rhs =
    List.fold_left (fun (error, store_sites_bond_rhs)
      (site_address_1, site_address_2) ->
        error, collect_sites_bond_rhs
          parameter
          error
          rule
            bond_rhs
            site_address_1
              site_address_2
              (fst store_sites_bond_rhs)
              (snd store_sites_bond_rhs)
              store_sites_bond_rhs
    )
      (error, ode_class.store_sites_bond_rhs) bind
  in
  (*d) External flow: link between 'a' and 'b'. If 'a' is an anchor or 'b'
  is a modified site*)
  let store_external_flow =
    (*folding a set of modified sites into a list of sites*)
    let error, get_sites_modified =
      Int_storage.Nearly_inf_Imperatif.fold
        parameter
        error
        (fun parameter error k a b ->
         (*new_list*)
         let sites = List.concat [a; b] in
         error, sites
        )
        (fst store_sites_bond_pair_1)
        []
    in
    (*folding a set of anchor sites into a list of sites (first case)*)
    let error, get_sites_anchor =
      Int_storage.Nearly_inf_Imperatif.fold
        parameter
        error
        (fun parameter error k a b ->
         let sites = List.concat [a; b] in
         error, sites)
        (snd store_sites_bond_pair_1)
        []
    in
    (*store the external flow by checking the condition above*)
    (*check if 'a' is a member of a list of anchor sites
      or 'b' is a member of a list of modified sites*)
    let store_external_flow =
      let store_anchor =
        List.fold_left (fun old_list a ->
                        let is_mem_anchor =
                          List.mem a get_sites_anchor in
                        if is_mem_anchor
                        then List.concat [[a];old_list]
                        else []
                       )
                       [] get_sites_anchor
      in
      let store_modified =
        List.fold_left (fun old_list b ->
                        let is_mem_modified =
                          List.mem b get_sites_modified in
                        if is_mem_modified
                        then List.concat [[b];old_list]
                        else []
                       )
                       [] get_sites_modified
      in
      (store_anchor, store_modified)
    in
    store_external_flow
  in
  (*return value of ode_class*)
  error,
  {
    store_sites_modified = store_sites_modified;
    store_sites_bond_pair_1 = store_sites_bond_pair_1;
    store_sites_anchor_1 = (snd store_sites_bond_pair_1);
    store_sites_bond_rhs = store_sites_bond_rhs;
    store_external_flow = store_external_flow
  }
    
let scan_rule_set parameter error handler rules =
  let error, init =
    Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let init_pair = (init, init) in
  (*init state of ode_class*)
  let init_ode =
    {
      store_sites_modified = init;
      store_sites_bond_pair_1 = init_pair;
      store_sites_anchor_1 = init;
      store_sites_bond_rhs = init_pair;
      store_external_flow = [], []
    }
  in
  let error, ode_class =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule ode_class ->
       (*let _ = Printf.fprintf stdout "- DO rule_id:%i\n" rule_id in*)
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

let print_anchor_1 parameter error result =
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter l ->
     let _ =
       print_string "site_type_anchor:";
       print_list l
     in
     error) parameter result

let print_external_flow (a,b) =
  match a, b with
  | [], [] | _, [] | [], _ -> ()
  | a, b ->
     let output = sprintf_list a in
     let a = Printf.fprintf stdout "anchor:%s -> " output in
     let b = Printf.fprintf stdout "modified:"; print_list b in
     a;b

(*let print_external_flow result =
  match result with
  | [] -> []
  | (a,b)::tl ->
     let _ = Printf.fprintf stdout "anchor:%i -> modified:%i\n" a b in
     tl*)
     
(*TODO: do not print as a list, print as a pair of information*)

(*print functions of anchor sites in the first case*)
let print_pair_1 parameter error (result, result') = 
  let _ = Printf.fprintf stdout "+ First element in a pair of agents:\n" in
  let p1 = print_modified parameter error result in
  let _ = Printf.fprintf stdout "+ Second element in a pair of agents:\n" in
  let p2 = print_bond parameter error result' in
  p1, p2

let print_rhs_pair parameter error (result, result') =
  let _ = Printf.fprintf stdout "+ First element in a pair of agents:\n" in
  let p1 = print_bond parameter error result in
  let _ = Printf.fprintf stdout "+ Second element in a pair agents:\n" in
  let p2 = print_bond parameter error result' in
  p1, p2

(*print function of ode_class*)
let print_ode parameter error
              {store_sites_modified; store_sites_bond_pair_1;
               store_sites_anchor_1; store_sites_bond_rhs;
               store_external_flow
              } =
  let _ = Printf.fprintf stdout "* Sites that are modified:\n" in
  let m = print_modified parameter error store_sites_modified in
  m;
  let _ = Printf.fprintf stdout "* Anchor sites in the first case:\n" in
  let _ = Printf.fprintf stdout "- (site_address_modified, site_address)\n" in
  let p1 = print_pair_1 parameter error store_sites_bond_pair_1 in
  p1;
  let _ = Printf.fprintf stdout "- Anchor sites :\n" in
  let a = print_anchor_1 parameter error store_sites_anchor_1 in
  a;
  let _ = Printf.fprintf stdout "* Anchor sites in the second case:\n" in
  let _ = Printf.fprintf stdout "- (site_address,site_address)\n" in
  let p2 = print_rhs_pair parameter error store_sites_bond_rhs in
  p2;
  let _ = Printf.fprintf stdout "- External flow :\n" in
  let f = print_external_flow store_external_flow in
  f
          
let ode_fragmentation parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules in
  let _ = print_ode parameter error result in
  error, result
