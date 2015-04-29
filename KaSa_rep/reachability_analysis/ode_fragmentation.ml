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

type set = Cckappa_sig.Site_map_and_set.set

type sites_ode = (set AgentMap.t * set AgentMap.t)

type pair_int_flow =
    ((Cckappa_sig.agent_name * int *  int) list) AgentMap.t

type pair_ext_flow =
    ((Cckappa_sig.agent_name * int * Cckappa_sig.agent_name * int) list)

type ode_frag =
    {
      store_sites_modified_set   : set AgentMap.t;
      store_sites_bond_pair_set  : sites_ode;
      store_sites_rhs            : int list AgentMap.t;
      store_sites_anchor_set1    : set AgentMap.t;
      store_internal_flow1       : pair_int_flow;
      store_sites_anchor_set2    : set AgentMap.t;
      store_internal_flow2       : pair_int_flow;
      store_external_flow        : pair_ext_flow
    }
                 
(************************************************************************************)
(*MODIFIED SITES*)

let add_site_modified parameter error agent_type site_set store_sites_modified_set =
  let error, get_old =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_modified_set
  in
  let old_set =
    match get_old with
      | None -> Cckappa_sig.Site_map_and_set.empty_set
      | Some s -> s
  in
  let error, new_set = Cckappa_sig.Site_map_and_set.union
    parameter
    error
    site_set
    old_set
  in
  Int_storage.Nearly_inf_Imperatif.set
    parameter
    error
    agent_type
    new_set
    store_sites_modified_set     

let collect_sites_modified_set parameter error rule store_sites_modified_set =
  let error, store_sites_modified_set =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id site_modif store_sites_modified_set ->
        let agent_type = site_modif.Cckappa_sig.agent_name in
        if Cckappa_sig.Site_map_and_set.is_empty_map
          site_modif.Cckappa_sig.agent_interface
        then
          error, store_sites_modified_set
        else
          (*collect site and return a set *)
          let site_set =
            Cckappa_sig.Site_map_and_set.fold_map
              (fun site _ current_set ->
                let error, set =
                  Cckappa_sig.Site_map_and_set.add_set
                    parameter
                    error
                    site
                    current_set
                in
                set
              )
              site_modif.Cckappa_sig.agent_interface
              Cckappa_sig.Site_map_and_set.empty_set
          in
          (*store*)
          let error, store_sites_modified_set =
            add_site_modified
              parameter
              error
              agent_type
              site_set
              store_sites_modified_set
          in
          error, store_sites_modified_set
      )
      rule.Cckappa_sig.diff_reverse store_sites_modified_set
  in
  error, store_sites_modified_set

(************************************************************************************)
(*BINDING SITES - SET TEST*)

(*------------------------------------------------------------------------------*)
(*++ element in a pair of site that are bond*)

let collect_store_bond_set parameter error bond_rhs site_address store_sites_bond_set =
  let agent_id = site_address.Cckappa_sig.agent_index in
  let agent_type = site_address.Cckappa_sig.agent_type in
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
  (*get a set of site that are bond*)
  let sites_bond_set =
    Cckappa_sig.Site_map_and_set.fold_map
      (fun site _ current_set ->
        let error, set =
          Cckappa_sig.Site_map_and_set.add_set
            parameter
            error
            site
            current_set
        in
        set
      )
      site_address Cckappa_sig.Site_map_and_set.empty_set
  in
  (*get old set*)
  let error, get_old_set =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_bond_set
  in
  let old_set =
    match get_old_set with
      | None -> Cckappa_sig.Site_map_and_set.empty_set
      | Some s -> s
  in
  (*store*)
  let error, new_set = Cckappa_sig.Site_map_and_set.union
    parameter
    error
    sites_bond_set
    old_set
  in
  let error, store_sites_bond_set =
    Int_storage.Nearly_inf_Imperatif.set
      parameter
      error
      agent_type
      new_set
      store_sites_bond_set
  in error, store_sites_bond_set

(*------------------------------------------------------------------------------*)
(*-- collect binding sites in the rhs with: site -> site*)

let collect_sites_bond_pair_set parameter error bond_rhs
    site_address_1
    site_address_2
    store_sites_bond_set_1
    store_sites_bond_set_2
    store_sites_bond_pair_set
    =
  let error, store_sites_bond_set_1 =
    collect_store_bond_set
      parameter
      error
      bond_rhs
      site_address_1
      store_sites_bond_set_1
  in
  let error, store_sites_bond_set_2 =
    collect_store_bond_set
      parameter
      error
      bond_rhs
      site_address_2
      store_sites_bond_set_2
  in
  let error, store_sites_bond_pair_set =
    (store_sites_bond_set_1, store_sites_bond_set_2)
  in
  error, store_sites_bond_pair_set

(*-- collect binding sites in the rhs with: site -> site*)
let store_sites_bond_pair_set parameter error bond_rhs bind 
    store_sites_bond_pair_set
    =
  List.fold_left (fun (error, store_sites_bond_pair_set)
    (site_address_1, site_address_2) ->
      let error, store_sites_bond_pair_set =
        error, collect_sites_bond_pair_set
          parameter
          error
          bond_rhs
          site_address_1
          site_address_2
          (fst store_sites_bond_pair_set)
          (snd store_sites_bond_pair_set)
          store_sites_bond_pair_set
      in
      error, store_sites_bond_pair_set
  )
    (error, store_sites_bond_pair_set)
    bind

(************************************************************************************)
(*BINDING SITES*)
                         
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
(* A set of anchors site -> a list of anchors site: (combine two cases)*)

let anchor_set parameter error agent_type store_sites_anchor1 store_sites_anchor2 =
  let error, get_anchor_set1 =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_anchor1
  in
  let anchor_set1 =
    match get_anchor_set1 with
      | None -> Cckappa_sig.Site_map_and_set.empty_set
      | Some s -> s
  in
  let error, get_anchor_set2 =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_anchor2
  in
  let anchor_set2 =
    match get_anchor_set2 with
      | None -> Cckappa_sig.Site_map_and_set.empty_set
      | Some s -> s
  in
  let error, anchor_set =
    Cckappa_sig.Site_map_and_set.union
      parameter
      error
      anchor_set1
      anchor_set2
  in
  anchor_set

(************************************************************************************)   
(*RULE*)

let scan_rule parameter error handler rule ode_class =
  let bind = rule.Cckappa_sig.actions.Cckappa_sig.bind in
  let bond_rhs = rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds in
  (*------------------------------------------------------------------------------*)
  (*a)collect modified sites*)
  let error, store_sites_modified_set =
    collect_sites_modified_set
      parameter
      error
      rule
      ode_class.store_sites_modified_set
  in
  (*------------------------------------------------------------------------------*)
  (*b) collect binding sites*)
  let error, store_sites_bond_pair_set =
    store_sites_bond_pair_set
      parameter
      error
      bond_rhs
      bind
      ode_class.store_sites_bond_pair_set
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
  (*d)Anchor sites(first case): A(x,y), B(x): 'x' of A is a modified site,
    'y' of A bind to 'y' of B => B(y) is an anchor site*)
  let error, store_sites_anchor_set1 =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent store_sites_anchor_set1 ->
        match agent with
          | Cckappa_sig.Ghost -> error, store_sites_anchor_set1
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            (*get a list of modified site in the rule rhs*)
            let error, get_modified_set =
              Int_storage.Nearly_inf_Imperatif.unsafe_get
                parameter
                error
                agent_type
                store_sites_modified_set
            in
            let modified_set =
              match get_modified_set with
                | None -> Cckappa_sig.Site_map_and_set.empty_set
                | Some s -> s
            in
            (*- get a list of sites in the rule rhs that are bond (fst
              agent that has site that are bond*)
            let error, get_site_rhs_bond_fst =
              Int_storage.Nearly_inf_Imperatif.unsafe_get
                parameter
                error
                agent_type
                (fst store_sites_bond_pair_set)
            in
            let site_rhs_bond_fst_set =
              match get_site_rhs_bond_fst with
                | None -> Cckappa_sig.Site_map_and_set.empty_set
                | Some s -> s
            in
            (*get a list of sites in the rule rhs*)
            let site_rhs_list =
              site_rhs_list parameter error agent_type store_sites_rhs
            in
            (*compute anchor site*)
            let error, sites_list =
              let rec aux acc =
                match acc with
                  | [] | [_] -> error, store_sites_anchor_set1
                  | x :: tl ->
                    let rec aux' acc' =
                      match acc' with
                        | [] -> error, store_sites_anchor_set1
                        | y :: tl' ->
                          if Cckappa_sig.Site_map_and_set.mem_set
                            x modified_set &&
                            Cckappa_sig.Site_map_and_set.mem_set
                            y site_rhs_bond_fst_set
                          then
                            let a = snd store_sites_bond_pair_set in
                            error, a
                          else aux' tl'
                    in aux' tl
              in aux (List.rev site_rhs_list)
            in
            error, sites_list
       )
            rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
              ode_class.store_sites_anchor_set1
       in
  (*------------------------------------------------------------------------------*)
  (*e) collect anchor sites (second case): a site connected to a site in an
    agent with an anchor, the second agent should contain at least an anchor on
    another site. For example: A(x,y), B(x): Agent A where site x is an
    anchor, y bind to x in agent B. Site x of B is an anchor.*)
  let error, store_sites_anchor_set2 =
    Int_storage.Quick_Nearly_inf_Imperatif.fold 
      parameter
      error
      (fun parameter error agent_id agent store_sites_anchor_set2 ->
       match agent with
       | Cckappa_sig.Ghost -> error, store_sites_anchor_set2
       | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          (*get a list of anchor sites from the first case and second case*)
          let anchor_set =
            anchor_set parameter error agent_type store_sites_anchor_set1
              store_sites_anchor_set2
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
              (fst store_sites_bond_pair_set)
          in
          let site_rhs_bond_set =
            match get_site_rhs_bond with
            | None -> Cckappa_sig.Site_map_and_set.empty_set
            | Some s -> s
          in
          (*compute anchor site*)
          let error, sites_list =
            let rec aux acc =
              match acc with
                | [] | [_] -> error, store_sites_anchor_set2
                | x :: tl ->
                  let rec aux' acc' =
                    match acc' with
                      | [] -> error, store_sites_anchor_set2
                      | y :: tl' ->
                        if Cckappa_sig.Site_map_and_set.mem_set
                          x anchor_set && 
                          Cckappa_sig.Site_map_and_set.mem_set
                         y site_rhs_bond_set
                        then
                          let a = snd store_sites_bond_pair_set in
                          error, a
                        else
                          aux' tl'
                  in
                  aux' tl
            in
            aux (List.rev site_rhs_list)
          in
          error, sites_list
      )
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
      ode_class.store_sites_anchor_set2 (*FIXME*)
      (*store_sites_anchor1*)
  in
  (*------------------------------------------------------------------------------*)
  (*f)compute internal_flow: site -> modified site*)
  let error, store_internal_flow1 =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent store_internal_flow1 ->
        match agent with
          | Cckappa_sig.Ghost -> error, store_internal_flow1
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            (*get a list of modified site*)
            let error, get_modified_set =
              Int_storage.Nearly_inf_Imperatif.unsafe_get
                parameter
                error
                agent_type
                store_sites_modified_set
            in
            let modified_set =
              match get_modified_set with
                | None -> Cckappa_sig.Site_map_and_set.empty_set
                | Some s -> s
            in
            (*get a list of sites in the rule rhs*)
            let site_rhs_list =
              site_rhs_list parameter error agent_type store_sites_rhs
            in
            (*compute internal_flow: site -> modified site*)
            let internal_flow =
              let rec aux acc =
                match acc with
                  | [] | [_]  -> []
                  | x :: tl ->
                    let rec aux' acc' =
                      match acc' with
                        | [] -> []
                        | y :: tl' ->
                          if Cckappa_sig.Site_map_and_set.mem_set
                            x modified_set
                          then
                            (agent_type,y, x) :: aux' tl'
                          else aux' tl'
                    in aux' tl
              in
              aux (List.rev site_rhs_list)
            in
            (*store*)
            let error, get_old =
              Int_storage.Nearly_inf_Imperatif.unsafe_get
                parameter
                error
                agent_type
                store_internal_flow1
            in
            let old_list =
              match get_old with
                | None -> []
                | Some s -> s
            in
            let new_list = List.concat [internal_flow; old_list] in
            let error, store_internal_flow =
              Int_storage.Nearly_inf_Imperatif.set
                parameter
                error
                agent_type
                new_list
                store_internal_flow1              
            in
            error, store_internal_flow
      )
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
      ode_class.store_internal_flow1
  in    
 (*------------------------------------------------------------------------------*)
 (*compute internal_flow: site -> anchor site*)
  let error, store_internal_flow2 =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent store_internal_flow2 ->
        match agent with
          | Cckappa_sig.Ghost -> error, store_internal_flow2
          | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            let anchor_set =
              anchor_set parameter error agent_type store_sites_anchor_set1
                store_sites_anchor_set2
            in
            (*get a list of sites in the rule rhs*)
            let site_rhs_list =
              site_rhs_list parameter error agent_type store_sites_rhs
            in
            (*compute internal_flow: site -> anchor site*)
            let internal_flow =
              let rec aux acc =
                match acc with
                  | [] | [_]  -> []
                  | x :: tl ->
                    let rec aux' acc' =
                      match acc' with
                        | [] -> []
                        | y :: tl' ->
                          if Cckappa_sig.Site_map_and_set.mem_set
                            x anchor_set
                          then
                            (agent_type,y, x) :: aux' tl'
                          else aux' tl'
                    in aux' tl
              in
              aux (List.rev site_rhs_list)
            in
            (*store*)
            let error, get_old =
              Int_storage.Nearly_inf_Imperatif.unsafe_get
                parameter
                error
                agent_type
                store_internal_flow2
            in
            let old_list =
              match get_old with
                | None -> []
                | Some s -> s
            in
            let new_list = List.concat [internal_flow; old_list] in
            let error, store_internal_flow =
              Int_storage.Nearly_inf_Imperatif.set
                parameter
                error
                agent_type
                new_list
                store_internal_flow2              
            in
            error, store_internal_flow
      )
      rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
      ode_class.store_internal_flow2
  in
  (*------------------------------------------------------------------------------*)
  (*g)external flow: a -> b, if 'a': anchor site or 'b':modified site*)
  let error, store_external_flow =
    (*binding*)
    List.fold_left (fun (error, store_external_flow)
      (site_address, site_address') ->
        let agent_type = site_address.Cckappa_sig.agent_type in
        let agent_type' = site_address'.Cckappa_sig.agent_type in
        (*collect site that are bond in the rhs; fst element in a pair*)
        let error, get_bond_set =
          Int_storage.Nearly_inf_Imperatif.unsafe_get
            parameter
            error
            agent_type
            (fst store_sites_bond_pair_set)
        in
        let bond_set =
          match get_bond_set with
            | None -> Cckappa_sig.Site_map_and_set.empty_set
            | Some s -> s
        in
        (*get a list of anchor sites*)
        let error, get_anchor_set1 =
          Int_storage.Nearly_inf_Imperatif.fold
            parameter
            error
            (fun parameter error agent_type site old_set ->
              let error, set =
                Cckappa_sig.Site_map_and_set.union
                  parameter
                  error
                  site
                  old_set                 
              in
              error, set
            ) store_sites_anchor_set1 Cckappa_sig.Site_map_and_set.empty_set
        in
        let error, get_anchor_set2 =
          Int_storage.Nearly_inf_Imperatif.fold
            parameter
            error
            (fun parameter error agent_type site old_set ->
              let error, set =
                Cckappa_sig.Site_map_and_set.union
                  parameter
                  error
                  site
                  old_set                 
              in
              error, set
            ) store_sites_anchor_set2 Cckappa_sig.Site_map_and_set.empty_set
        in
        let error, anchor_set =
          Cckappa_sig.Site_map_and_set.union
            parameter
            error
            get_anchor_set1
            get_anchor_set2
        in
        (*get a list of rhs sites*)
        let site_rhs_list =
          get_sites_list_common parameter error store_sites_rhs
        in
        (*compute external_flow*)
        let external_flow =
          (*check in a list of site in the rhs*)
          let rec aux acc =
            match acc with
              | [] -> error, store_external_flow
              | x :: tl ->
                let rec aux' acc' =
                  match acc' with
                    | [] -> []
                    | y :: tl' ->
                      (*check if x is a member of anchor*)
                      if Cckappa_sig.Site_map_and_set.mem_set
                        x anchor_set &&
                        Cckappa_sig.Site_map_and_set.mem_set
                        y bond_set
                      then
                        let external_flow_list =
                          (agent_type', x, agent_type, y)::[] in
                        (*store: combine the result with old result in the list*)
                        List.concat [external_flow_list; store_external_flow]
                      else aux' tl'
                in error, aux' tl
          in aux (List.rev site_rhs_list)
        in external_flow
    )
      (error, ode_class.store_external_flow)
      bind
  in
  (*------------------------------------------------------------------------------*)
  (*return value of ode_class*)
  error,
  {
    store_sites_modified_set   = store_sites_modified_set;
    store_sites_bond_pair_set  = store_sites_bond_pair_set;
    store_sites_rhs            = store_sites_rhs;
    store_sites_anchor_set1    = store_sites_anchor_set1;
    store_internal_flow1       = store_internal_flow1;
    store_sites_anchor_set2    = store_sites_anchor_set2;
    store_internal_flow2       = store_internal_flow2;
    store_external_flow        = store_external_flow
  }
    
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler rules =
  let error, init =
    Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let init_pair = (init, init) in
  let error, init_rhs =
    Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let error, init_internal =
    Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let error, init_external =
    Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  (*init state of ode_class*)
  let init_ode =
    {
      store_sites_modified_set   = init;
      store_sites_bond_pair_set  = init_pair;
      store_sites_rhs            = init_rhs;
      store_sites_anchor_set1    = init;
      store_internal_flow1       = init_internal;
      store_sites_anchor_set2    = init;
      store_internal_flow2       = init_internal;
      store_external_flow        = []
    }
  in
  let error, ode_class =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule ode_class ->
        (*let _ = Printf.fprintf stdout "rule_id:%i\n" rule_id in*)
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
    (fun error parameter s ->
      let l = Cckappa_sig.Site_map_and_set.elements s in
      let _ =
        print_string "site_type_modified:";
        print_list l
      in
      error) parameter result

(*------------------------------------------------------------------------------*)    

let print_anchor parameter error result = 
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter s ->
      let l = Cckappa_sig.Site_map_and_set.elements s in
      let _ =
        print_string "site_type_anchor:";
        print_list l
      in
      error) parameter result

let print_anchor_pair parameter error (result, result') =
  let a = print_anchor parameter error result in
  let a2 = print_anchor parameter error result' in
  a, a2

(*------------------------------------------------------------------------------*)
let print_internal_flow (agent_name, site, site') =
  let i1 = Printf.fprintf stdout "- agent_type:%i:site:%i -> " agent_name site in
  let i2 = Printf.fprintf stdout "agent_type:%i:modified_type:%i\n" agent_name site' in
  i1, i2

let print_store_internal_flow1 parameter error result =
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter l ->
      let _ =
        let rec aux acc =
          match acc with
            | [] -> acc
            | x :: tl -> print_internal_flow x;
              aux tl
        in aux l
      in
      error)
    parameter
    result

let print_internal_flow2 (agent_name, site, site') =
  let i1 = Printf.fprintf stdout "- agent_type:%i:site:%i -> " agent_name site in
  let i2 = Printf.fprintf stdout "agent_type:%i:anchor_type:%i\n" agent_name site' in
  i1, i2

let print_store_internal_flow2 parameter error result =
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter l ->
      let _ = 
        let rec aux acc =
          match acc with
            | [] -> acc
            | x :: tl -> print_internal_flow2 x; aux tl
        in aux l
      in
      error)
    parameter result

(*------------------------------------------------------------------------------*)
let print_external_flow result =
  let rec aux acc =
    match acc with
      | [] -> acc
      | (agent_type,x,agent_type',y) :: tl ->
        Printf.fprintf stdout
          "anchor_type:%i:anchor:%i -> agent_type:%i:modified_type:%i\n"
          agent_type x agent_type' y;
        aux tl
  in aux result
  
(************************************************************************************)
(*MAIN PRINT*)
    
let print_ode parameter error
              { store_sites_modified_set;
                store_sites_bond_pair_set;
                store_sites_rhs;
                store_sites_anchor_set1;
                store_internal_flow1;
                store_sites_anchor_set2;
                store_internal_flow2;
                store_external_flow
              } =
  let _ = Printf.fprintf stdout "* Sites that are modified:\n" in
  let m = print_modified parameter error store_sites_modified_set in
  m;
  let _ = Printf.fprintf stdout "* Anchor sites:\n" in
  let a = print_anchor parameter error store_sites_anchor_set1 in
  a;
  let _ = Printf.fprintf stdout "* Anchor sites 2:\n" in
  let a2 = print_anchor parameter error store_sites_anchor_set2 in
  a2;
  let _ = Printf.fprintf stdout "* Internal flow:\n" in
  let i = print_store_internal_flow1 parameter error store_internal_flow1 in
  i;
  (*let _ = Printf.fprintf stdout "* Internal flow 2:\n" in*)
  let i2 = print_store_internal_flow2 parameter error store_internal_flow2 in
  i2;
  let _ = Printf.fprintf stdout "* External flow:\n" in
  let e = print_external_flow store_external_flow in
  e
    
(************************************************************************************)     
(*MAIN*)

let ode_fragmentation parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules in
  let _ = print_ode parameter error result in
  error, result
