(**
    * ode_fragmentation.ml
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
(*TYPE*)

module AgentMap = Int_storage.Nearly_inf_Imperatif

type sites_ode = (int list AgentMap.t * int list AgentMap.t)
type pair_flow = (int * int) list  (*TODO: change to (int t * int t) list*)
  
type ode_frag =
    {
      store_sites_modified   : int list AgentMap.t;
      store_sites_bond_pair  : sites_ode;
      store_sites_rhs        : int list AgentMap.t;
      store_sites_anchor1    : int list AgentMap.t;
      store_sites_anchor2    : int list AgentMap.t;
      store_internal_flow    : (pair_flow * pair_flow);
      store_external_flow    : pair_flow;
      store_sites_address    :
        (Cckappa_sig.site_address * Cckappa_sig.site_address) (*TEST*)
    }
                 
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
              site :: current_list)
             site_modif.Cckappa_sig.agent_interface []
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
                         
(*------------------------------------------------------------------------------*)
(*++ element in a pair of site that are bond*)

let collect_store_bond parameter error bond_rhs
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
  (*get old_list*)
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
  (*store*)
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
(*-- collect binding sites in the rhs with: site -> site*)

(*+ pair of site that are bond*)      
let collect_sites_bond_pair parameter error bond_rhs
                           site_address_1
                           site_address_2
                           store_sites_bond_1
                           store_sites_bond_2
                           store_sites_bond_pair
  =
  let error, store_sites_bond_1 =
    collect_store_bond
      parameter
      error
      bond_rhs
      site_address_1
      store_sites_bond_1
  in
  let error, store_sites_bond_2 =
    collect_store_bond
      parameter
      error
      bond_rhs
      site_address_2
      store_sites_bond_2
  in
  let error, store_sites_bond_pair =
    (store_sites_bond_1, store_sites_bond_2)
  in
  error, store_sites_bond_pair
           
(*-- collect binding sites in the rhs with: site -> site*)
let store_sites_bond_pair parameter error bond_rhs bind
                           store_sites_bond_pair
  =
  let error, store_sites_bond_pair =
    List.fold_left (fun (error, store_sites_bond_pair)
                        (site_address_1, site_address_2) ->
                    let error, store_sites_bond_pair =
                      error, collect_sites_bond_pair
                               parameter
                               error
                               bond_rhs
                               site_address_1
                               site_address_2
                               (fst store_sites_bond_pair)
                               (snd store_sites_bond_pair)
                               store_sites_bond_pair
                    in
                    error, store_sites_bond_pair
                   )
                   (error, store_sites_bond_pair)
                   bind
  in
  error, store_sites_bond_pair

(*-FIXME*)
let store_sites_address error bind store_sites_address =
  List.fold_left (fun  store_sites_address
    (site_address_1, site_address_2) ->
      let store_sites_address =
        (site_address_1, site_address_2)
      in
      store_sites_address
  )
    store_sites_address
    bind

           
(************************************************************************************)
(*SITES RHS*)

let store_sites_rhs parameter error rule store_sites_rhs =
  let error, store_sites_rhs =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent store_sites_rhs ->
        match agent with
       | Cckappa_sig.Ghost -> error, store_sites_rhs
       | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          (*get sites_list in a combine format, A(x), A(y) -> A(x,y)*)
          let get_sites_list =
            let site_list =
              Cckappa_sig.Site_map_and_set.fold_map
                (fun site _ current_list ->
                 site :: current_list) agent.Cckappa_sig.agent_interface []
            in
            let error, old_list =
              Int_storage.Nearly_inf_Imperatif.unsafe_get
                parameter
                error
                agent_type
                store_sites_rhs
            in
            let old_list =
              match old_list with
              | None -> []
              | Some s -> s
            in
            (*store*)
            let new_list = List.concat [site_list; old_list] in
            let error, sites_list =
              Int_storage.Nearly_inf_Imperatif.set
                parameter
                error
                agent_type
                new_list
                store_sites_rhs
            in
            error, sites_list
          in get_sites_list
      )
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
      store_sites_rhs
  in
  error, store_sites_rhs

(************************************************************************************)   
(*RETURN A LIST FROM A SET *)

(*------------------------------------------------------------------------------*)
(* A set of RHS site -> a list of RHS site*)

let site_rhs_list parameter error agent_type store_sites_rhs =
  let error, get_sites_rhs =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_rhs
  in
  let site_rhs_list =
    match get_sites_rhs with
      | None -> []
      | Some s -> s
  in
  site_rhs_list

(*------------------------------------------------------------------------------*)
(* A set of modified site -> a list of modified site*)

let modified_list parameter error agent_type store_sites_modified =
  let error, get_sites_modified = 
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_modified
  in
  let modified_list =
    match get_sites_modified with
      | None -> []
      | Some s -> s
  in
  modified_list
   
(*------------------------------------------------------------------------------*)
(* A set of anchors site -> a list of anchors site: (combine two cases)*)

let anchor_list parameter error agent_type store_sites_anchor1 store_sites_anchor2 =
  let error, get_anchor_list1 =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_anchor1
  in
  let anchor_list1 =
    match get_anchor_list1 with
      | None -> []
      | Some s -> s
  in
  let error, get_anchor_list2 =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_anchor2
  in
  let anchor_list2 =
    match get_anchor_list2 with
      | None -> []
      | Some s -> s
  in
  let anchor_list =
    List.concat [anchor_list1; anchor_list2]
  in
  anchor_list

(*------------------------------------------------------------------------------*)
(*A common function return a list by using folding in a set*)

let get_sites_list_common parameter error store_sites_common =
  let error, get_sites =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_type sites old_list ->
        let sites_list = List.concat [sites; old_list] in
        error, sites_list
      )
      store_sites_common []
  in
  get_sites

(*------------------------------------------------------------------------------*)
(*ANCHOR *)

let anchor_list_common parameter error store_sites_anchor1 store_sites_anchor2 =
  let get_sites_anchor1 =
    get_sites_list_common parameter error store_sites_anchor1
  in
  let get_sites_anchor2 =
    get_sites_list_common parameter error store_sites_anchor2
  in
  let anchor_list =
    List.concat [get_sites_anchor1; get_sites_anchor2]
  in
  anchor_list

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
  (*b) collect binding sites*)
  let error, store_sites_bond_pair =
    store_sites_bond_pair
      parameter
      error
      bond_rhs
      bind
      ode_class.store_sites_bond_pair
  in
  (*------------------------------------------------------------------------------*)
  (*c)collect sites from the rhs rule*)
  let error, store_sites_rhs =
    store_sites_rhs
      parameter
      error
      rule
      ode_class.store_sites_rhs
  in
  (*------------------------------------------------------------------------------*)
  (*FIXME*)
  (*d)Anchor sites(first case): A(x,y), B(x,y): 'x' of A is modified site,
    'x' of B is a modified site, 'y' of A bind to 'y' of B => B(y) and A(y)
    are anchor sites *)
  let error, store_sites_anchor1 =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent store_sites_anchor1 ->
        match agent with
          | Cckappa_sig.Ghost -> error, store_sites_anchor1
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            (*get a list of modified site in the rule rhs*)
            let modified_list =
              modified_list parameter error agent_type store_sites_modified
            in
            (*get a list of sites in the rule rhs*)
            let site_rhs_list =
              site_rhs_list parameter error agent_type store_sites_rhs
            in
            (*- get a list of sites in the rule rhs that are bond (fst
              agent that has site that are bond*)
            let error, get_site_rhs_bond_fst =
              Int_storage.Nearly_inf_Imperatif.unsafe_get
                parameter
                error
                agent_type
                (fst store_sites_bond_pair)
            in
            let site_rhs_bond_fst_list =
              match get_site_rhs_bond_fst with
                | None -> []
                | Some s -> s
            in
            (*compute anchor site*)
            let error, sites_list =
              let rec aux acc =
                match acc with
                  | [] | [_] -> error, store_sites_anchor1
                  | x :: tl ->
                    if List.mem x modified_list
                    then
                      let rec aux' acc' =
                        match acc' with
                          | [] -> error, store_sites_anchor1
                          | y :: tl' ->
                            (*FIXME*)
                            if List.mem y site_rhs_bond_fst_list
                            then
                              let a = snd store_sites_bond_pair in
                              error, a
                            else
                              aux' tl'
                      in
                      aux' tl
                    else
                      aux tl
              in
              aux (List.rev site_rhs_list)
            in
            error, sites_list
      )
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
      ode_class.store_sites_anchor1
  in
  (*------------------------------------------------------------------------------*)
  (*e) collect anchor sites (second case): a site connected to a site in an
    agent with an anchor, the second agent should contain at least an anchor on
    another site. For example: A(x,y), B(x): Agent A where site x is an
    anchor, y bind to x in agent B. Site x of B is an anchor.*)
  let error, store_sites_anchor2 =
    Int_storage.Quick_Nearly_inf_Imperatif.fold 
      parameter
      error
      (fun parameter error agent_id agent store_sites_anchor ->
       match agent with
       | Cckappa_sig.Ghost -> error, store_sites_anchor
       | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          (*get a list of anchor sites from the first case and second case*)
          let anchor_list =
            anchor_list parameter error agent_type store_sites_anchor1 
              store_sites_anchor
          in
          (*get a list of site from the rhs of rule*)
          let site_rhs_list =
            site_rhs_list parameter error agent_type store_sites_rhs
          in
          (*get a list of site that are bond in the rhs (snd element)*)
          let error, get_site_rhs_bond =
            Int_storage.Nearly_inf_Imperatif.unsafe_get
              parameter
              error
              agent_type
              (snd store_sites_bond_pair)
          in
          let site_rhs_bond_list =
            match get_site_rhs_bond with
            | None -> []
            | Some s -> s
          in
          (*compute anchor site in the second case*)
          let error, sites_list =
            let rec aux acc =
              match acc with
                | [] | [_]-> error, store_sites_anchor
                | x :: tl ->
                  if List.mem x anchor_list
                  then
                    let rec aux' acc' =
                      match acc' with
                        | [] -> error, store_sites_anchor
                        | y :: l' ->
                          (*check y bind to another site z, return this site z *)
                          if List.mem y site_rhs_bond_list
                          then
                            let a = fst store_sites_bond_pair in
                            error, a
                          else aux' l'
                    in aux' tl
                  else aux tl
            in aux site_rhs_list
          in
          error, sites_list
      )
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
      ode_class.store_sites_anchor2
  in  
  (*------------------------------------------------------------------------------*)
  (*f)internal flow: a -> b, if 'a': site tested; 'b':modified/anchor site*)
  let store_internal_flow =
    let store_internal_flow =
      (*get sites that are tested. Sites on the rhs? FIXME*)
      let get_sites_tested =
        get_sites_list_common parameter
          error store_sites_rhs
      in
      (*get modified sites*)
      let get_sites_modified =
        get_sites_list_common parameter error store_sites_modified
      in
      (*get anchor sites*)
      let get_sites_anchor =
        anchor_list_common parameter error store_sites_anchor1 store_sites_anchor2
      in
      (*compute internal_flow*)
      (*- site_tested -> modified site*)
      let internal_flow1 =
        let rec aux acc =
          match acc with
            | [] | [_] -> []
            | x :: tl ->
              if List.mem x get_sites_modified
              then
                let rec aux' acc' =
                  match acc' with
                    | [] -> []
                    | y :: tl' ->
                      (*FIXME:
                        check if site_type and modified_site are the same then return empty*)
                      if x = y
                      then
                        aux' tl'
                      else
                        (y, x) :: aux' tl'
                in
                aux' tl
              else aux tl
        in
        aux (List.rev get_sites_tested)
      in
      (*- site_tested -> anchor site*)
      let internal_flow2 =
        let rec aux acc =
          match acc with
            | [] | [_] -> []
            | x :: tl ->
              if List.mem x get_sites_anchor
              then
                let rec aux' acc' =
                  match acc' with
                    | [] -> []
                    | y :: tl' ->
                      (*FIXME:
                        check if site_type and anchor_site are the same then return empty*)
                      if x = y
                      then aux' tl'
                      else
                      (y, x) :: aux' tl'
                in
                aux' tl
              else
                aux tl
        in
        aux get_sites_tested
      in
      (*- combine the result*)
      let internal_flow =
        (internal_flow1, internal_flow2)
      in
      internal_flow
    in
    store_internal_flow
  in
  (*------------------------------------------------------------------------------*)
  (*g)external flow: a -> b, if 'a': anchor site or 'b':modified site*)
  let store_external_flow =
    let store_external_flow =
      (*get a list of anchor sites*)
      let get_sites_anchor = 
        anchor_list_common parameter error store_sites_anchor1 store_sites_anchor2
      in
      (*get a list of modified sites that are bond*)
      let get_sites_modified =
        get_sites_list_common parameter error store_sites_modified
      in
      let external_flow =
        let rec aux acc acc' =
          match acc, acc' with
          | [], [] | [], _ | _, [] -> []
          | x :: tl, y :: tl' -> (x, y) :: aux tl tl'
        in
        aux get_sites_anchor get_sites_modified
      in
      external_flow
    in
    store_external_flow
  in
  (*------------------------------------------------------------------------------*)
  (*TEST*)
  let store_sites_address =
    store_sites_address
      error
      bind
      ode_class.store_sites_address
  in
  (*------------------------------------------------------------------------------*)
  (*return value of ode_class*)
  error,
  {
    store_sites_modified   = store_sites_modified;
    store_sites_bond_pair  = store_sites_bond_pair;
    store_sites_rhs        = store_sites_rhs;
    store_sites_anchor1    = store_sites_anchor1;
    store_sites_anchor2    = store_sites_anchor2;
    store_internal_flow    = store_internal_flow;
    store_external_flow    = store_external_flow;
    store_sites_address    = store_sites_address
  }
    
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler rules =
  let error, init =
    Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let error, init' =
   Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let init_pair = (init, init) in
  let init_build = Cckappa_sig.build_address 0 0 0 in
  (*init state of ode_class*)
  let init_ode =
    {
      store_sites_modified   = init;
      store_sites_bond_pair  = init_pair;
      store_sites_rhs        = init;
      store_sites_anchor1    = init;
      store_sites_anchor2    = init;
      store_internal_flow    = ([], []);
      store_external_flow    = [];
      store_sites_address    = (init_build, init_build) (*TEST*)
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

let print_anchor_pair parameter error (result, result') =
  let a = 
    print_anchor parameter error result
  in
  let a2 =
    print_anchor parameter error result'
  in
  a, a2
   
let print_rhs parameter error result =(*REMOVE*)
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter l ->
     let _ =
       print_string "site_type:";
       print_list l
     in
     error) parameter result

(*site_tested -> modified site*)
let print_internal_flow1 result =
  let rec aux acc =
    match acc with
      | [] -> acc
      | (x,y) :: tl ->
        Printf.fprintf stdout "site_type:%i -> modified_type:%i\n" x y;
        aux tl
  in aux result

(*site_tested -> anchor site*)
let print_internal_flow2 result =
  let rec aux acc =
    match acc with
      | [] -> acc
      | (x,y) :: tl ->
        Printf.fprintf stdout "site_type:%i -> anchor_type:%i\n" x y; (*FIXME*)
        aux tl
  in aux result
    
let print_internal_flow (result, result') =
  let p1 = print_internal_flow1 result in
  let p2 = print_internal_flow2 result' in
  p1, p2

let print_external_flow result =
  let rec aux acc =
    match acc with
    | [] -> acc
    | (x,y) :: tl ->
       Printf.fprintf stdout "anchor_type:%i -> modified_type:%i\n" x y;
       aux tl
  in aux result

let print_sites_address (result, result') = (*TEST*)
  let s1 =
    Printf.fprintf stdout "agent_index:%i:site:%i:agent_type:%i\n"
      result.Cckappa_sig.agent_index result.Cckappa_sig.site result.Cckappa_sig.agent_type
  in
  let s2 =
    Printf.fprintf stdout "agent_index':%i:site':%i:agent_type':%i\n"
      result'.Cckappa_sig.agent_index result'.Cckappa_sig.site result'.Cckappa_sig.agent_type
  in
  s1, s2 

(************************************************************************************)
(*MAIN PRINT*)
    
let print_ode parameter error
              { store_sites_modified;
                store_sites_bond_pair;
                store_sites_rhs;
                store_sites_anchor1;
                store_sites_anchor2;
                store_internal_flow;
                store_external_flow;
                store_sites_address
              } =
  let _ = Printf.fprintf stdout "* Sites that are modified:\n" in
  let m = print_modified parameter error store_sites_modified in
  m;
  let _ = Printf.fprintf stdout "* Anchor sites:\n" in
  let a = print_anchor parameter error store_sites_anchor1 in
  a;
  let _ = Printf.fprintf stdout "* Anchor sites 2:\n" in
  let a2 = print_anchor parameter error store_sites_anchor2 in
  a2;
  let _ = Printf.fprintf stdout "* Internal flow:\n" in
  let i = print_internal_flow store_internal_flow in
  i;
  let _ = Printf.fprintf stdout "* External flow:\n" in
  let e = print_external_flow store_external_flow in
  e;
  let _ = Printf.fprintf stdout "* Store sites address:\n" in
  let s = print_sites_address store_sites_address in
  s
    
(************************************************************************************)     
(*MAIN*)

let ode_fragmentation parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules in
  let _ = print_ode parameter error result in
  error, result
