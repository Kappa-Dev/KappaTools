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

type elt_set = Cckappa_sig.Site_map_and_set.elt

type pair_int_flow' =
    ((Cckappa_sig.agent_name * elt_set *  elt_set) list)AgentMap.t

type pair_ext_flow =
    ((Cckappa_sig.agent_name * int * Cckappa_sig.agent_name * int) list)

type ode_frag =
    {
      store_sites_modified_set   : set AgentMap.t;
      store_sites_bond_pair_set  : sites_ode;
      store_sites_rhs            : int list AgentMap.t;
      store_sites_rhs_set        : set AgentMap.t;
      store_sites_anchor_set1    : set AgentMap.t;
      (*store_internal_flow1       : pair_int_flow';*) 
      store_sites_anchor_set2    : set AgentMap.t;
      (*store_internal_flow2      : pair_int_flow'; *)
      store_external_flow        : pair_ext_flow
    }

(************************************************************************************)   
(*UTILITIES FUNCTIONS *)

(*------------------------------------------------------------------------------*)
(* A set of site -> a list of site*)

let get_site_common_list parameter error agent_type store_sites_common =
  let error, get_sites =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_common
  in
  let site_list =
    match get_sites with
      | None -> []
      | Some s -> s
  in
  site_list

(*------------------------------------------------------------------------------*)
(*A common function return a list by using folding in a set*)

let get_sites_fold_common parameter error store_sites_common =
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
(* A set of site*)

let get_set_common parameter error agent_type store_sites_common =
  let error, get_set =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      agent_type
      store_sites_common
  in
  let set =
    match get_set with
      | None -> Cckappa_sig.Site_map_and_set.empty_set
      | Some s -> s
  in
  set

(*------------------------------------------------------------------------------*)
(* A set of anchor site*)

let anchor_set parameter error agent_type store_sites_anchor1 store_sites_anchor2 =
  let anchor_set1 =
    get_set_common
      parameter
      error
      agent_type
      store_sites_anchor1
  in
  let anchor_set2 =
    get_set_common
      parameter
      error
      agent_type
      store_sites_anchor2
  in
  let error, anchor_set =
    Cckappa_sig.Site_map_and_set.union
      parameter
      error
      anchor_set1
      anchor_set2
  in
  anchor_set

(*------------------------------------------------------------------------------*)
(* A set of anchors site -> a list of anchors site: (combine two cases) fold*)

let get_anchor_common parameter error store_sites_anchor =
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
     ) store_sites_anchor Cckappa_sig.Site_map_and_set.empty_set

let fold_anchor_set parameter error store_sites_anchor_set1 store_sites_anchor_set2 =
  let error, anchor_set1 =
    get_anchor_common parameter error store_sites_anchor_set1
  in
  let error, anchor_set2 =
    get_anchor_common parameter error store_sites_anchor_set2
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
(*MODIFIED SITES*)

let add_site_modified parameter error agent_type site_set store_sites_modified_set =
  let old_set =
    get_set_common
      parameter
      error
      agent_type
      store_sites_modified_set
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
(*BINDING SITES - SET*)

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
  let old_set =
    get_set_common
      parameter
      error
      agent_type
      store_sites_bond_set
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
            let old_list =
              get_site_common_list
                parameter
                error
                agent_type
                store_sites_rhs
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
(*ANCHOR SITES*)

(*------------------------------------------------------------------------------*)
(*first case*)

let collect_sites_anchor_set1 parameter error get_rule 
    store_sites_modified_set
    store_sites_bond_pair_set
    store_sites_rhs
    store_sites_anchor_set1
    =
  Int_storage.Quick_Nearly_inf_Imperatif.fold
    parameter
    error
    (fun parameter error agent_id agent store_sites_anchor_set1 ->
      match agent with
        | Cckappa_sig.Ghost -> error, store_sites_anchor_set1
        | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          (*get a set of modified site in the rule rhs*)
          let modified_set =
            get_set_common
              parameter
              error
              agent_type
              store_sites_modified_set
          in
          (*- get a set of sites in the rule rhs that are bond (fst
            agent that has site that are bond*)
          let site_rhs_bond_fst_set =
            get_set_common
              parameter
              error
              agent_type
              (fst store_sites_bond_pair_set)
          in
          (*get a list of sites in the rule rhs*)
          let site_rhs_list =
            get_site_common_list parameter error agent_type store_sites_rhs
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
     ) get_rule.Cckappa_sig.rule_rhs.Cckappa_sig.views store_sites_anchor_set1

(*------------------------------------------------------------------------------*)
(*second case*)

let collect_sites_anchor_set2 parameter error get_rule
    store_sites_anchor_set1
    store_sites_bond_pair_set
    store_sites_rhs
    store_sites_anchor_set2
    =
  Int_storage.Quick_Nearly_inf_Imperatif.fold 
    parameter
    error
    (fun parameter error agent_id agent store_sites_anchor_set2 ->
      match agent with
        | Cckappa_sig.Ghost -> error, store_sites_anchor_set2
        | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          (*get a set of anchor sites from the first case and second case*)
          let anchor_set =
            anchor_set parameter error agent_type store_sites_anchor_set1
              store_sites_anchor_set2
          in
          (*get a set of site that are bond in the rhs (fst element)*)
          let site_rhs_bond_set =
            get_set_common
              parameter
              error
              agent_type
              (fst store_sites_bond_pair_set)
          in
          (*get a list of site from the rhs of rule*)
          let site_rhs_list =
            get_site_common_list parameter error agent_type store_sites_rhs
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
    get_rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
    store_sites_anchor_set2 (*FIXME*)

(************************************************************************************)
(*INTERNAL FLOW*)

(*------------------------------------------------------------------------------*)
(*TODO*)

(*Get a set of site rhs list*)
let collect_sites_rhs_set parameter error rule store_sites_rhs_set =
  Int_storage.Quick_Nearly_inf_Imperatif.fold
    parameter
    error
    (fun parameter error agent_id agent store_sites_rhs_set ->
      match agent with
        | Cckappa_sig.Ghost -> error, store_sites_rhs_set
        | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          (*get a set of site*)
          let site_set =
            Cckappa_sig.Site_map_and_set.fold_map
              (fun site _ current_set ->
                let error, set =
                  Cckappa_sig.Site_map_and_set.add_set
                    parameter
                    error
                    site
                    current_set
                in set)
              agent.Cckappa_sig.agent_interface Cckappa_sig.Site_map_and_set.empty_set
          in
          let error, get_old_set =
            Int_storage.Nearly_inf_Imperatif.unsafe_get
              parameter
              error
              agent_type
              store_sites_rhs_set
          in
          let old_set =
            match get_old_set with
              | None -> Cckappa_sig.Site_map_and_set.empty_set
              | Some s -> s
          in
          (*combine two of them*)
          let error, new_set =
            Cckappa_sig.Site_map_and_set.union
              parameter
              error
              site_set
              old_set
          in
          (*store*)
          let error, site_set =
            Int_storage.Nearly_inf_Imperatif.set
              parameter
              error
              agent_type
              new_set
              store_sites_rhs_set                
          in
          error, site_set         
    )
    rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
    store_sites_rhs_set

(*get a set of intersection between rhs and modified site*)

let cartesian_prod i a b =
  let rec loop a acc = match a with
    | [] -> List.rev acc
    | x :: xs ->
      loop xs (
        List.rev_append
          (List.rev (List.fold_left
                       (fun acc y ->
                         if x <> y then (i, x, y) :: acc
                         else acc
                       ) [] b))
          acc
      )
  in
  loop a [] 

let cartesian_prod' i a b =
  let b' = List.rev b in
  let rec loop a acc = match a with
    | [] -> List.rev acc
    | x :: xs ->
      loop xs (List.rev_append (List.rev_map (fun y -> (i,x, y)) b') acc)
  in
  loop a [] 

let internal_flow_rhs_modified parameter error agent_type store_sites_rhs_set
    store_sites_modified_set =
  (*get the result of intersection between rhs and modified*)
  let error, result_inter_set =
    Cckappa_sig.Site_map_and_set.inter
      parameter
      error
      store_sites_rhs_set
      store_sites_modified_set
  in
  (*TODO: keep type set and use casteran product for set *)
  let result_inter_list =
    Cckappa_sig.Site_map_and_set.elements result_inter_set in
  let rhs_list =
    Cckappa_sig.Site_map_and_set.elements store_sites_rhs_set in
  (*match rhs_list with
    | [] | [_] -> []
    | _ ->*)
      (*return the casterant product of two lists*)
      cartesian_prod' agent_type rhs_list result_inter_list 

(*------------------------------------------------------------------------------*)
(*compute internal_flow: site -> modified site*)

let collect_internal_flow1 parameter error get_rule
    store_sites_rhs_set
    store_sites_modified_set
    store_internal_flow1
    =
  Int_storage.Quick_Nearly_inf_Imperatif.fold
    parameter
    error
    (fun parameter error agent_id agent store_internal_flow ->
      match agent with
        | Cckappa_sig.Ghost -> error, store_internal_flow
        | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, get_rhs_set =
            Int_storage.Nearly_inf_Imperatif.unsafe_get
              parameter
              error
              agent_type
              store_sites_rhs_set
          in
          let rhs_set =
            match get_rhs_set with
              | None -> Cckappa_sig.Site_map_and_set.empty_set
              | Some s -> s
          in
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
          let get_internal_flow =
            internal_flow_rhs_modified
              parameter
              error
              agent_type
              rhs_set
              modified_set           
          in
          (*store*)
          let error, get_old_list =
            Int_storage.Nearly_inf_Imperatif.unsafe_get
              parameter
              error
              agent_type
              store_internal_flow
          in
          let old_list =
            match get_old_list with
              | None -> []
              | Some s -> s
          in
          let new_list = List.concat [get_internal_flow; old_list] in
          Int_storage.Nearly_inf_Imperatif.set
            parameter
            error
            agent_type
            new_list
            store_internal_flow
    )
    get_rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
    store_internal_flow1

(*------------------------------------------------------------------------------*)
(*compute internal_flow: site -> anchor site TODO*)

let internal_flow_rhs_anchor parameter error agent_type store_sites_rhs_set
    anchor_set
    =
  let error, result_inter_set =
    Cckappa_sig.Site_map_and_set.inter
      parameter
      error
      store_sites_rhs_set
      anchor_set
  in  
  (*TODO:keep set*)
  let result_inter_list =
    Cckappa_sig.Site_map_and_set.elements result_inter_set in
  let rhs_list = 
    Cckappa_sig.Site_map_and_set.elements store_sites_rhs_set in
  match rhs_list with
    | [] | [_] -> []
    | _ -> cartesian_prod agent_type rhs_list result_inter_list


(*------------------------------------------------------------------------------*)
(*compute internal_flow: site -> anchor site*)

let collect_internal_flow2 parameter error get_rule
    store_sites_rhs_set
    store_sites_anchor_set1
    store_sites_anchor_set2
    store_internal_flow2
    =
  Int_storage.Quick_Nearly_inf_Imperatif.fold
    parameter
    error
    (fun parameter error agent_id agent store_internal_flow2 ->
      match agent with
        | Cckappa_sig.Ghost -> error, store_internal_flow2
        | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          let error, get_rhs_set =
            Int_storage.Nearly_inf_Imperatif.unsafe_get
              parameter
              error
              agent_type
              store_sites_rhs_set
          in
          let rhs_set =
            match get_rhs_set with
              | None -> Cckappa_sig.Site_map_and_set.empty_set
              | Some s -> s
          in
          (*get a set of anchor sites*)
          let anchor_set =
            anchor_set parameter error agent_type store_sites_anchor_set1
              store_sites_anchor_set2
          in
          let get_internal_flow =
            internal_flow_rhs_anchor
              parameter
              error
              agent_type
              rhs_set
              anchor_set
          in
          (*store*)
          let error, get_old_list =
            Int_storage.Nearly_inf_Imperatif.unsafe_get
              parameter
              error
              agent_type
              store_internal_flow2
          in
          let old_list =
            match get_old_list with
              | None -> []
              | Some s -> s
          in
          let new_list = List.concat [get_internal_flow; old_list] in
          Int_storage.Nearly_inf_Imperatif.set
            parameter
            error
            agent_type
            new_list
            store_internal_flow2
    )
    get_rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
    store_internal_flow2


(************************************************************************************)   
(*EXTERNAL FLOW*)

let collect_external_flow parameter error bind
    store_sites_bond_pair_set
    store_sites_anchor_set1
    store_sites_anchor_set2
    store_sites_rhs
    store_external_flow
    =
  List.fold_left (fun (error, store_external_flow)
    (site_address, site_address') ->
      let agent_type = site_address.Cckappa_sig.agent_type in
      let agent_type' = site_address'.Cckappa_sig.agent_type in
      (*collect site that are bond in the rhs; fst element in a pair*)
      let bond_set =
        get_set_common
          parameter
          error
          agent_type
          (fst store_sites_bond_pair_set)
      in
      (*get a set of anchor sites*)
      let anchor_set =
        fold_anchor_set
          parameter
          error
          store_sites_anchor_set1 
          store_sites_anchor_set2 
      in
      (*get a list of rhs sites*)
      let site_rhs_list =
        get_sites_fold_common parameter error store_sites_rhs
      in
      (*compute external_flow*)
      let external_flow =
        (*check in a list of site in the rhs*)
        let rec aux acc = (*FIXME*)
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
                        (agent_type', x, agent_type, y) :: [] in
                        (*store: combine the result with old result in the list*)
                      List.concat [external_flow_list; store_external_flow]
                    else aux' tl'
              in error, aux' tl
        in aux (List.rev site_rhs_list)
      in external_flow
  )
    (error, store_external_flow)
    bind

(************************************************************************************)   
(*RULE*)

let scan_rule parameter error handler get_rule ode_class =
  let bind = get_rule.Cckappa_sig.actions.Cckappa_sig.bind in
  let bond_rhs = get_rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds in
  (*------------------------------------------------------------------------------*)
  (*a)collect modified sites*)
  let error, store_sites_modified_set =
    collect_sites_modified_set
      parameter
      error
      get_rule
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
      get_rule
      ode_class.store_sites_rhs
  in
  (*------------------------------------------------------------------------------*)
  (*d)Anchor sites(first case): A(x,y), B(x): 'x' of A is a modified site,
    'y' of A bind to 'y' of B => B(y) is an anchor site*)
  let error, store_sites_anchor_set1 =
    collect_sites_anchor_set1
      parameter
      error
      get_rule
      store_sites_modified_set
      store_sites_bond_pair_set
      store_sites_rhs
      ode_class.store_sites_anchor_set1
  in
  (*------------------------------------------------------------------------------*)
  (*e) collect anchor sites (second case): a site connected to a site in an
    agent with an anchor, the second agent should contain at least an anchor on
    another site. For example: A(x,y), B(x): Agent A where site x is an
    anchor, y bind to x in agent B. Site x of B is an anchor.*)
  let error, store_sites_anchor_set2 =
    collect_sites_anchor_set2
      parameter
      error
      get_rule
      store_sites_anchor_set1
      store_sites_bond_pair_set
      store_sites_rhs
      ode_class.store_sites_anchor_set2
  in
  (*------------------------------------------------------------------------------*)
  let error, store_sites_rhs_set =
    collect_sites_rhs_set
      parameter
      error
      get_rule
      ode_class.store_sites_rhs_set
  in
  (*------------------------------------------------------------------------------*)
  (*f)compute internal_flow: site -> modified site*)
 (* let error, store_internal_flow1 =
    collect_internal_flow1
      parameter
      error
      get_rule
      store_sites_rhs_set
      store_sites_modified_set
      ode_class.store_internal_flow1
  in
  (*------------------------------------------------------------------------------*)
  let error, store_internal_flow2 =
   collect_internal_flow2
     parameter
     error
     get_rule
     store_sites_rhs_set
     store_sites_anchor_set1
     store_sites_anchor_set2
     ode_class.store_internal_flow2
  in*)
  (*------------------------------------------------------------------------------*)
  (*g)external flow: a -> b, if 'a': anchor site or 'b':modified site*)
  let error, store_external_flow =
    collect_external_flow
      parameter
      error
      bind
      store_sites_bond_pair_set
      store_sites_anchor_set1
      store_sites_anchor_set2
      store_sites_rhs
      ode_class.store_external_flow
  in
  (*------------------------------------------------------------------------------*)
  (*return value of ode_class*)
  error,
  {
    store_sites_modified_set   = store_sites_modified_set;
    store_sites_bond_pair_set  = store_sites_bond_pair_set;
    store_sites_rhs            = store_sites_rhs;
    store_sites_rhs_set        = store_sites_rhs_set;
    store_sites_anchor_set1    = store_sites_anchor_set1;
    (*store_internal_flow1       = store_internal_flow1;*)
    store_sites_anchor_set2    = store_sites_anchor_set2;
    (*store_internal_flow2       = store_internal_flow2;*)
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
      store_sites_rhs_set        = init;
      store_sites_anchor_set1    = init;
      (*store_internal_flow1       = init_internal;*)
      store_sites_anchor_set2    = init;
      (*store_internal_flow2       = init_internal;*)
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

let print_internal_flow1 parameter error result =
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter result ->
      let _ = 
        let rec aux acc =
          match acc with
            | [] -> []
            | (agent_type, x, y) :: tl ->
              Printf.fprintf stdout
                "- agent_type:%i:site_type:%i -> agent_type:%i:modified_type:%i\n"
                agent_type x agent_type y;
              aux tl
        in aux result
      in error
    ) parameter result

let print_internal_flow2 (agent_name, site, site') =
  let i1 = Printf.fprintf stdout "- agent_type:%i:site:%i -> " agent_name site in
  let i2 = Printf.fprintf stdout "agent_type:%i:anchor_type:%i\n" agent_name site' in
  i1, i2

let print_internal_flow2 parameter error result =
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter l ->
      let _ = 
        let rec aux acc =
          match acc with
            | [] -> acc
            | x :: tl ->
              print_internal_flow2 x; aux tl
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
                store_sites_rhs_set;
                store_sites_anchor_set1;
                (*store_internal_flow1;*)
                store_sites_anchor_set2;
                (*store_internal_flow2;*)
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
  (*let i = print_internal_flow1 parameter error store_internal_flow1 in
  i;
  let i2 = print_internal_flow2 parameter error store_internal_flow2 in
  i2;*)
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
