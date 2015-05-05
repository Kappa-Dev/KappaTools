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

type elt_set = Cckappa_sig.Site_map_and_set.elt

type pair_int_flow =
    ((Cckappa_sig.agent_name * elt_set * elt_set) list) AgentMap.t

type pair_ext_flow =
    ((Cckappa_sig.agent_name * int * Cckappa_sig.agent_name * int) list)

type ode_frag =
    {
      store_sites_modified_set            : set AgentMap.t;
      store_sites_bond_pair_set           : sites_ode;
      store_sites_bond_pair_set_external  : sites_ode;
      store_sites_lhs                     : int list AgentMap.t;
      store_sites_anchor_set              : (set AgentMap.t * set AgentMap.t);
      store_internal_flow                 : (pair_int_flow * pair_int_flow);
      store_external_flow                 : pair_ext_flow
    }

(************************************************************************************)   
(*UTILITIES FUNCTIONS*)

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
  in site_list

(*------------------------------------------------------------------------------*)
(* A set of site*)

let get_site_common_set parameter error agent_type store_sites_common =
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
  in set

(*------------------------------------------------------------------------------*)
(* A set of anchor site*)

let anchor_set parameter error agent_type store_sites_anchor1 store_sites_anchor2 =
  let anchor_set1 =
    get_site_common_set
      parameter
      error
      agent_type
      store_sites_anchor1
  in
  let anchor_set2 =
    get_site_common_set
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
  in anchor_set

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
       in error, set)
     store_sites_anchor
     Cckappa_sig.Site_map_and_set.empty_set

let fold_anchor_set parameter error store_sites_anchor_set1 store_sites_anchor_set2 =
  let error, anchor_set1 = get_anchor_common parameter error store_sites_anchor_set1 in
  let error, anchor_set2 = get_anchor_common parameter error store_sites_anchor_set2 in
  let error, anchor_set =
    Cckappa_sig.Site_map_and_set.union
      parameter
      error
      anchor_set1
      anchor_set2
  in anchor_set
                
(************************************************************************************)
(*MODIFIED SITES*)

let add_site_modified parameter error agent_type site_set store_sites_modified_set =
  let old_set =
    get_site_common_set
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
                in set)
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
      rule.Cckappa_sig.diff_reverse
      store_sites_modified_set
  in error, store_sites_modified_set

(************************************************************************************)
(*BINDING SITES - SET*)

(*------------------------------------------------------------------------------*)
(*++ element in a pair of site that are bond*)

(*without take the old result*)
let collect_store_bond_set' parameter error bond_rhs site_address store_sites_bond_set =
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
        in set)
      site_address
      Cckappa_sig.Site_map_and_set.empty_set
  in
  (*store*)
  let error, store_sites_bond_set =
    Int_storage.Nearly_inf_Imperatif.set
      parameter
      error
      agent_type
      sites_bond_set
      store_sites_bond_set
  in error, store_sites_bond_set

(*with the old_result*)
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
        in set)
      site_address
      Cckappa_sig.Site_map_and_set.empty_set
  in
  (*get old*)
  let old_set =
    get_site_common_set
      parameter
      error
      agent_type
      store_sites_bond_set
  in
  let error, result_set =
    Cckappa_sig.Site_map_and_set.union
      parameter
      error
      sites_bond_set
      old_set
  in
  (*store*)
  let error, store_sites_bond_set =
    Int_storage.Nearly_inf_Imperatif.set
      parameter
      error
      agent_type
      result_set
      store_sites_bond_set
  in error, store_sites_bond_set

(*------------------------------------------------------------------------------*)
(*-- collect binding sites in the rhs with: site -> site*)

(*Use for collecting anchor site*)
let collect_sites_bond_pair_set parameter error bond_rhs
    site_address_1
    site_address_2
    store_sites_bond_set_1
    store_sites_bond_set_2
    store_sites_bond_pair_set =
  (*first binding agent, check at each rule, and not combine witht the old result*)
  let error, store_sites_bond_set_1 =
    collect_store_bond_set'
      parameter
      error
      bond_rhs
      site_address_1
      store_sites_bond_set_1
  in
  (*the second binding agent, it is a result of anchor, combine with the old result*)
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

let result_sites_bond_pair_set parameter error bond_rhs bind 
    store_sites_bond_pair_set =
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
      error, store_sites_bond_pair_set)
    (error, store_sites_bond_pair_set)
    bind

(*------------------------------------------------------------------------------*)
(*-- collect binding sites in the rhs with: site -> site*)

(*use for external flow; collect binding site at each rule and not combine them*)

let collect_sites_bond_pair_set_external parameter error bond_rhs
    site_address_1
    site_address_2
    store_sites_bond_set_1
    store_sites_bond_set_2
    store_sites_bond_pair_set =
  let error, store_sites_bond_set_1 =
    collect_store_bond_set'
      parameter
      error
      bond_rhs
      site_address_1
      store_sites_bond_set_1
  in
  let error, store_sites_bond_set_2 =
    collect_store_bond_set'
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

let result_sites_bond_pair_set_external parameter error bond_rhs bind 
    store_sites_bond_pair_set =
  List.fold_left (fun (error, store_sites_bond_pair_set)
    (site_address_1, site_address_2) ->
      let error, store_sites_bond_pair_set =
        error, collect_sites_bond_pair_set_external
          parameter
          error
          bond_rhs
          site_address_1
          site_address_2
          (fst store_sites_bond_pair_set)
          (snd store_sites_bond_pair_set)
          store_sites_bond_pair_set
      in
      error, store_sites_bond_pair_set)
    (error, store_sites_bond_pair_set)
    bind

(************************************************************************************)
(*SITES LHS: get site of each agent in each rule*)

let store_sites_lhs parameter error rule store_sites_lhs =
  let error, store_sites_lhs =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter
      error
      (fun parameter error agent_id agent store_sites_lhs ->
        match agent with
       | Cckappa_sig.Ghost -> error, store_sites_lhs
       | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          let site_list =
            Cckappa_sig.Site_map_and_set.fold_map
              (fun site _ current_list ->
                site :: current_list)
              agent.Cckappa_sig.agent_interface []
          in
          let error, sites_list =
            Int_storage.Nearly_inf_Imperatif.set
              parameter
              error
              agent_type
              site_list
              store_sites_lhs
          in
          error, sites_list
      )
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      store_sites_lhs
  in error, store_sites_lhs

(************************************************************************************)
(*ANCHOR SITES*)

let collect_sites_anchor_set parameter error get_rule 
    store_sites_modified_set
    store_sites_bond_pair_set
    store_sites_lhs
    store_sites_anchor_set =
  Int_storage.Quick_Nearly_inf_Imperatif.fold
    parameter
    error
    (fun parameter error agent_id agent store_sites_anchor_set ->
      match agent with
        | Cckappa_sig.Ghost -> error, store_sites_anchor_set
        | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          (*get a set of modified site in the rule rhs*)
          let modified_set =
            get_site_common_set
              parameter
              error
              agent_type
              store_sites_modified_set
          in
          (*get a set of sites in the rule rhs that are bond (fst
            agent that has site that are bond*)
          let site_rhs_bond_fst_set =
            get_site_common_set
              parameter
              error
              agent_type
              (fst store_sites_bond_pair_set)
          in
          (*get a list of sites in the rule lhs*)
          let site_lhs_list =
            get_site_common_list
              parameter
              error
              agent_type
              store_sites_lhs
          in
          (*get a set of anchor sites from the first case and second case*)
          let anchor_set =
            anchor_set
              parameter
              error
              agent_type
              (fst store_sites_anchor_set)
              (snd store_sites_anchor_set)
          in
          (*first case: a site connected to a modified site*)
          let error, anchor_list1 =
            let rec aux acc =
              match acc with
                | [] -> error, (fst store_sites_anchor_set)
                | x :: tl ->
                  begin
                    if Cckappa_sig.Site_map_and_set.mem_set
                      x modified_set &&
                      Cckappa_sig.Site_map_and_set.mem_set
                      x site_rhs_bond_fst_set
                    then
                      let a = snd store_sites_bond_pair_set in
                      error, a
                    else aux tl
                  end
            in aux (List.rev site_lhs_list)
          in
          (* second case: at least two sites, one of them belong to an anchor site*)
          (*get a set of sites in the rule rhs that are bond (fst
            agent that has site that are bond*)
          let error, anchor_list2 =
            match (List.rev site_lhs_list) with
              | [] | [_] -> error, (snd store_sites_anchor_set)
              | x :: tl ->
                let rec aux to_visit =
                  match to_visit with
                    | [] -> error, (snd store_sites_anchor_set)
                    | y :: tl' ->
                      begin
                        if Cckappa_sig.Site_map_and_set.mem_set
                          x anchor_set ||
                          Cckappa_sig.Site_map_and_set.mem_set
                          x modified_set &&
                          Cckappa_sig.Site_map_and_set.mem_set
                          y site_rhs_bond_fst_set
                        then
                          let a = snd store_sites_bond_pair_set in
                          error, a
                        else aux tl'
                      end
                in aux tl
          in error, (anchor_list1, anchor_list2)
     ) get_rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
       store_sites_anchor_set

(************************************************************************************)
(*INTERNAL FLOW*)

(*cartesian product*)

let cartesian_prod_eq i a b =
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

(*------------------------------------------------------------------------------*)
(*compute internal_flow: site -> modified site*)

let internal_flow_rhs_modified parameter error agent_type
    store_sites_lhs
    store_sites_modified_set =
  let result_modified_list =
    Cckappa_sig.Site_map_and_set.elements store_sites_modified_set in
  let site_lhs =
    get_site_common_list
      parameter
      error
      agent_type
      store_sites_lhs
  in
  match site_lhs with
    | [] | [_] -> []
    | _ -> cartesian_prod_eq
      agent_type
      site_lhs
      result_modified_list

(*------------------------------------------------------------------------------*)
(*compute internal_flow: site -> anchor site*)

let internal_flow_lhs_anchor parameter error agent_type
    store_sites_lhs
    anchor_set =
  let site_lhs =
    get_site_common_list
      parameter
      error
      agent_type
      store_sites_lhs
  in
  let anchor_list = Cckappa_sig.Site_map_and_set.elements anchor_set in
  match site_lhs with
    | [] | [_] -> []
    | _ ->
      cartesian_prod_eq
        agent_type
        site_lhs
        anchor_list

(*------------------------------------------------------------------------------*)
(*INTERNAL FLOW*)

let collect_internal_flow parameter error get_rule
    store_sites_lhs
    store_sites_modified_set
    store_sites_anchor_set
    store_internal_flow =
  Int_storage.Quick_Nearly_inf_Imperatif.fold
    parameter
    error
    (fun parameter error agent_id agent store_internal_flow ->
      match agent with
        | Cckappa_sig.Ghost -> error, store_internal_flow
        | Cckappa_sig.Agent agent ->
          let agent_type = agent.Cckappa_sig.agent_name in
          let modified_set =
            get_site_common_set
              parameter
              error
              agent_type
              store_sites_modified_set
          in
          let anchor_set =
            anchor_set
              parameter
              error
              agent_type
              (fst store_sites_anchor_set)
              (snd store_sites_anchor_set)
          in
          (*1st: site -> modified site*)
          let get_internal_flow1 =
            internal_flow_rhs_modified
              parameter
              error
              agent_type
              store_sites_lhs
              modified_set           
          in
          let error, internal_flow1 =
            Int_storage.Nearly_inf_Imperatif.set
              parameter
              error
              agent_type
              get_internal_flow1
              (fst store_internal_flow)
          in
          (*2nd: site -> anchor site*)
          let get_internal_flow2 =
            internal_flow_lhs_anchor
              parameter
              error
              agent_type
              store_sites_lhs
              anchor_set
          in
          let error, internal_flow2 =
            Int_storage.Nearly_inf_Imperatif.set
              parameter
              error
              agent_type
              get_internal_flow2
              (snd store_internal_flow)
          in
          (*result*)
          error, (internal_flow1, internal_flow2)
    ) get_rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      store_internal_flow

(************************************************************************************)   
(*EXTERNAL FLOW*)

let cartesian_prod_external i anchor_set i' bond_fst_list bond_snd_set =
  let anchor_list = Cckappa_sig.Site_map_and_set.elements anchor_set in
  let rec loop anchor_list acc =
    match anchor_list with
      | [] -> List.rev acc
      | x :: xs ->
        loop xs
          (List.rev_append
             (List.rev (List.fold_left
                          (fun acc y ->
                            if Cckappa_sig.Site_map_and_set.mem_set x anchor_set &&
                              Cckappa_sig.Site_map_and_set.mem_set x bond_snd_set
                            then (i, x, i', y) :: acc
                            else acc
                          ) [] bond_fst_list))
             acc
          )
  in
  loop anchor_list [] 
    
let collect_external_flow parameter error bind
    store_sites_bond_pair_set_external
    store_sites_anchor_set1
    store_sites_anchor_set2
    store_external_flow =
  List.fold_left (fun (error, store_external_flow)
    (site_address_1, site_address_2) ->
      let agent_type_1 = site_address_1.Cckappa_sig.agent_type in
      let agent_type_2 = site_address_2.Cckappa_sig.agent_type in
      (*collect site that are bond in the rhs; fst element in a pair*)
      let bond_fst_set =
        get_site_common_set
          parameter
          error
          agent_type_1
          (fst store_sites_bond_pair_set_external)
      in
      let bond_snd_set =
        get_site_common_set
          parameter
          error
          agent_type_2
          (snd store_sites_bond_pair_set_external)
      in
      (*get a set of anchor sites*)
      let anchor_set =
        fold_anchor_set
          parameter
          error
          store_sites_anchor_set1 
          store_sites_anchor_set2 
      in
      let bond_fst_list = Cckappa_sig.Site_map_and_set.elements bond_fst_set in
      let collect_external_flow =
        cartesian_prod_external
          agent_type_2
          anchor_set
          agent_type_1
	  bond_fst_list
          bond_snd_set
      in
     (*store by getting the old result and combine with the new result*)
      let external_flow = List.concat [collect_external_flow; store_external_flow] in
      error, external_flow)
    (error, store_external_flow)
    bind

(************************************************************************************)   
(*RULE*)

let scan_rule parameter error handler get_rule ode_class =
  let bind = get_rule.Cckappa_sig.actions.Cckappa_sig.bind in
  let bond_rhs = get_rule.Cckappa_sig.rule_rhs.Cckappa_sig.bonds in
  (*------------------------------------------------------------------------------*)
  (*a) collect modified sites*)
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
    result_sites_bond_pair_set
      parameter
      error
      bond_rhs
      bind
      ode_class.store_sites_bond_pair_set
  in
  let error, store_sites_bond_pair_set_external =
    result_sites_bond_pair_set_external
      parameter
      error
      bond_rhs
      bind
      ode_class.store_sites_bond_pair_set_external
  in
  (*------------------------------------------------------------------------------*)
  (*c) collect sites from the lhs rule without combining their site at each rule*)
  let error, store_sites_lhs =
    store_sites_lhs
      parameter
      error
      get_rule
      ode_class.store_sites_lhs
  in
  (*------------------------------------------------------------------------------*)
  (*d) 1st: anchor sites (first case): A(x,y), B(x): 'x' of A is a modified site,
    'y' of A bind to 'y' of B => B(y) is an anchor site;
    - 2nd: collect anchor sites (second case): a site connected to a site in an
    agent with an anchor, the second agent should contain at least an anchor on
    another site. For example: A(x,y), B(x): Agent A where site x is an
    anchor, y bind to x in agent B. Site x of B is an anchor.
  *)
  let error, store_sites_anchor_set =
    collect_sites_anchor_set
      parameter
      error
      get_rule
      store_sites_modified_set
      store_sites_bond_pair_set
      store_sites_lhs
      ode_class.store_sites_anchor_set
  in
  (*------------------------------------------------------------------------------*)
  (*e) compute internal_flow: site -> modified/anchor site*)
  let error, store_internal_flow =
    collect_internal_flow
      parameter
      error
      get_rule
      store_sites_lhs
      store_sites_modified_set
      store_sites_anchor_set
      ode_class.store_internal_flow
  in
  (*------------------------------------------------------------------------------*)
  (*f) external flow: a -> b, if 'a': anchor site or 'b':modified site*)
  let error, store_external_flow =
    collect_external_flow
      parameter
      error
      bind
      store_sites_bond_pair_set_external
      (fst store_sites_anchor_set)
      (snd store_sites_anchor_set)
      ode_class.store_external_flow
  in
  (*------------------------------------------------------------------------------*)
  (*return value of ode_class*)
  error,
  {
    store_sites_modified_set            = store_sites_modified_set;
    store_sites_bond_pair_set           = store_sites_bond_pair_set;
    store_sites_bond_pair_set_external  = store_sites_bond_pair_set_external;
    store_sites_lhs                     = store_sites_lhs;
    store_sites_anchor_set              = store_sites_anchor_set;
    store_internal_flow                 = store_internal_flow;
    store_external_flow                 = store_external_flow
  }
    
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler rules =
  let error, init = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let init_pair = (init, init) in
  let error, init_lhs = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let error, init_internal = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let error, init_external = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  (*init state of ode_class*)
  let init_ode =
    {
      store_sites_modified_set            = init;
      store_sites_bond_pair_set           = init_pair;
      store_sites_bond_pair_set_external  = init_pair;
      store_sites_lhs                     = init_lhs;
      store_sites_anchor_set              = (init, init);
      store_internal_flow                 = (init_internal, init_internal);
      store_external_flow                 = []
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
    (fun error parameter s ->
      let l = Cckappa_sig.Site_map_and_set.elements s in
      let _ = print_string "site_type_modified:"; print_list l in
      error) parameter result

(*------------------------------------------------------------------------------*)    

let print_anchor parameter error result = 
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter s ->
      let l = Cckappa_sig.Site_map_and_set.elements s in
      let _ = print_string "site_type_anchor:"; print_list l in
      error) parameter result

(*------------------------------------------------------------------------------*)

let print_internal_flow1 parameter error result =
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter l ->
      let _ = 
        let rec aux acc =
          match acc with
            | [] -> []
            | (agent_type, x, y) :: tl ->
              Printf.fprintf stdout
                "- agent_type:%i:site_type:%i -> agent_type:%i:modified_type:%i\n"
                agent_type x agent_type y;
              aux tl
        in aux l
      in error) parameter result

let print_internal_flow2 parameter error result =
  Int_storage.Nearly_inf_Imperatif.print
    error
    (fun error parameter l ->
      let _ = 
        let rec aux acc =
          match acc with
            | [] -> acc
            | (agent_type, x, y) :: tl ->
              Printf.fprintf stdout
                "- agent_type:%i:site_type:%i -> agent_type:%i:anchor_type:%i\n" 
                agent_type x agent_type y;
              aux tl
        in aux l
      in error) parameter result

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
                store_sites_bond_pair_set_external;
                store_sites_lhs;
                store_sites_anchor_set;
                store_internal_flow;
                store_external_flow
              } =
  let _ = Printf.fprintf stdout "* Sites that are modified:\n" in
  let _ = print_modified parameter error store_sites_modified_set in
  let _ = Printf.fprintf stdout "* Anchor sites:\n" in
  let _ = print_anchor parameter error (fst store_sites_anchor_set) in
  let _ = Printf.fprintf stdout "* Anchor sites 2:\n" in
  let _ = print_anchor parameter error (snd store_sites_anchor_set) in
  let _ = Printf.fprintf stdout "* Internal flow:\n" in
  let _ = print_internal_flow1 parameter error (fst store_internal_flow) in
  let _ = print_internal_flow2 parameter error (snd store_internal_flow) in
  let _ = Printf.fprintf stdout "* External flow:\n" in
  let _ = print_external_flow store_external_flow in
  ()
    
(************************************************************************************)     
(*MAIN*)

let ode_fragmentation parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules in
  let _ = print_ode parameter error result in
  error, result
