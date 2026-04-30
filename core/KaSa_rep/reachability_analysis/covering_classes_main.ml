(**
  * covering_classes.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2015, the 23th of Feburary
  * Last modification: Time-stamp: <Aug 21 2018>
  *
  * Compute the relations between the left hand site of a rule and its sites.
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let trace = false

let compute_cv_max ?start_cv parameters error agent_type =
  match start_cv with
  | None -> error, -1
  | Some a ->
    (match
       Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get parameters
         error agent_type a
     with
    | error, None -> error, -1
    | error, Some i -> error, Covering_classes_type.int_of_cv_id i)

let ignore_cv ~cv_max cv_id =
  compare (Covering_classes_type.int_of_cv_id cv_id) cv_max < 0

let is_new_rule ~start k =
  match start with
  | None -> true
  | Some max_rule -> compare k max_rule > 0

let see_agent ~modified_agents parameters error agent_type =
  let a, _ = modified_agents in
  let error, a =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set parameters
      error agent_type true a
  in
  error, (a, true)

let is_there_new_cv ~modified_agents = snd modified_agents

let is_there_new_cv_in_agent ~modified_agents parameters error agent =
  match
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
      parameters error agent (fst modified_agents)
  with
  | error, None -> error, false
  | error, Some a -> error, a

(*******************************************************************************)
let compare_unit_covering_class_id _ _ = Covering_classes_type.dummy_cv_id

let collect_modified_map parameters error kappa_handler diff_reverse
    store_modified_map =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error _agent_id site_modif store_modified_map ->
      let agent_type = site_modif.Cckappa_sig.agent_name in
      let error, old_map =
        match
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
          .unsafe_get parameters error agent_type store_modified_map
        with
        | error, None -> error, Ckappa_sig.Site_map_and_set.Map.empty
        | error, Some m -> error, m
      in
      let error', new_map =
        Ckappa_sig.Site_map_and_set.Map.fold
          (fun site _port (error, current_map) ->
            (*store site map*)
            let error, b =
              Handler.is_counter parameters error kappa_handler agent_type site
            in
            if b then
              error, current_map
            else (
              let error, site_map =
                Ckappa_sig.Site_map_and_set.Map.add_or_overwrite parameters
                  error site site current_map
              in
              error, site_map
            ))
          site_modif.Cckappa_sig.agent_interface (error, old_map)
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      (*compute site_map*)
      (*store*)
      let error', store_modified_map =
        if Ckappa_sig.Site_map_and_set.Map.is_empty new_map then
          error, store_modified_map
        else
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
            parameters error agent_type new_map store_modified_map
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      error, store_modified_map)
    diff_reverse store_modified_map

(*-------------------------------------------------------------------------*)
(*compute covering classes, site test and bdu*)

let collect_covering_classes_regular parameters error kappa_handler views
    diff_reverse store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold2_common
      parameters error
      (fun parameters error _agent_id agent site_modif store_result ->
        (* if in the interface there is no site modified then do nothing *)
        if
          Ckappa_sig.Site_map_and_set.Map.is_empty
            site_modif.Cckappa_sig.agent_interface
        then
          error, store_result
        else (
          match agent with
          | Cckappa_sig.Ghost | Cckappa_sig.Unknown_agent _ ->
            error, store_result
          | Cckappa_sig.Dead_agent (agent, _, _, _) | Cckappa_sig.Agent agent ->
            let agent_type = agent.Cckappa_sig.agent_name in
            (*get a list of sites from an interface at each rule*)
            let error, site_list =
              Ckappa_sig.Site_map_and_set.Map.fold
                (fun site _ (error, current_list) ->
                  let error, b =
                    Handler.is_counter parameters error kappa_handler agent_type
                      site
                  in
                  if b then
                    error, current_list
                  else
                    error, site :: current_list)
                agent.Cckappa_sig.agent_interface (error, [])
            in
            (*compute covering_class*)
            (match site_list with
            | [] -> error, store_result
            | _ ->
              let error, old_list =
                match
                  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
                  .unsafe_get parameters error agent_type store_result
                with
                | error, None -> error, []
                | error, Some l -> error, l
              in
              let new_pair_list = List.rev site_list :: old_list in
              let error, store_result =
                Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
                  parameters error agent_type new_pair_list store_result
              in
              error, store_result)
        ))
      views diff_reverse store_result
  in
  error, store_result

let collect_covering_classes_side_effects parameters error _kappa_handler remove
    store_result =
  List.fold_left
    (fun (error, store_result) (_, agent, list) ->
      let declared =
        Ckappa_sig.Site_map_and_set.Map.fold
          (fun site _ list -> site :: list)
          agent.Cckappa_sig.agent_interface []
      in
      let declared = List.rev declared in
      let error, old_list =
        match
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
          .unsafe_get parameters error agent.Cckappa_sig.agent_name store_result
        with
        | error, None -> error, []
        | error, Some l -> error, l
      in
      let new_list =
        List.fold_left
          (fun new_list site ->
            List.merge Ckappa_sig.compare_site_name declared [ site ]
            :: new_list)
          old_list list
      in
      Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
        parameters error agent.Cckappa_sig.agent_name new_list store_result)
    (error, store_result) remove

(************************************************************************************)
(*compute covering class: it is a covering class whenever there is a
  modified site in that agent. (CHECK on their left-hand side)

  For example: A(x~u,y~u) -> A(x~u,y~p) (where y is a modified site), then
  there is one covering class for agent A: CV_1: (x,y)

  - If the rule is: A(x~u), A(y~u) -> A(x~p), A(y~p), (x,y are modified
  sites), then agent A has two covering classes: CV_1: x; CV_2: y

  - If the rule is: A(x~u), A(y~u) -> A(x~u), A(y~p), (y is a modified
  site), then agent A has only one covering class: CV_1: y
*)

let scan_rule_covering_classes parameters error kappa_handler rule classes =
  (*----------------------------------------------------------------------*)
  (*compute modified map*)
  let error, store_modified_map =
    collect_modified_map parameters error kappa_handler
      rule.Cckappa_sig.diff_reverse
      classes.Covering_classes_type.store_modified_map
  in
  (*----------------------------------------------------------------------*)
  (*compute covering_class*)
  let error, store_covering_classes =
    collect_covering_classes_regular parameters error kappa_handler
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views rule.Cckappa_sig.diff_reverse
      classes.Covering_classes_type.store_covering_classes
  in
  let error, store_covering_classes =
    collect_covering_classes_side_effects parameters error kappa_handler
      rule.Cckappa_sig.actions.Cckappa_sig.remove store_covering_classes
  in
  (*----------------------------------------------------------------------*)
  (*result*)
  ( error,
    {
      Covering_classes_type.store_modified_map;
      Covering_classes_type.store_covering_classes;
    } )

(***************************************************************************)
(*RULES*)

let scan_rule_set_covering_classes ?start parameters error kappa_handler rules =
  let n_agents = kappa_handler.Cckappa_sig.nagents in
  let error, init_modif_map =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
    .create_biggest_key parameters error n_agents
  in
  let error, init_class =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
    .create_biggest_key parameters error n_agents
  in
  let last_site parameters error ag =
    match start with
    | None -> error, Ckappa_sig.site_name_of_int (-1)
    | Some new_index ->
      (match new_index.Diff.next_site_per_agent with
      | None -> error, Ckappa_sig.site_name_of_int (-1)
      | Some a ->
        (match
           Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.unsafe_get
             parameters error ag a
         with
        | error, None ->
          if compare ag new_index.Diff.next_agent >= 0 then
            error, Ckappa_sig.site_name_of_int (-1)
          else
            Exception.warn parameters error __POS__
              ~message:"This agent should be in the data-structure" Exit
              (Ckappa_sig.site_name_of_int (-1))
        | error, Some a -> error, a))
  in
  (*----------------------------------------------------------------------*)
  (* add each singleton as a covering class *)
  let error, init_class =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.fold parameters error
      (fun parameters error agent_type b init_class ->
        let error, last_site = last_site parameters error agent_type in
        Ckappa_sig.Dictionary_of_sites.fold
          (fun _ _ b (error, init_class) ->
            if Ckappa_sig.compare_site_name b last_site <= 0 then
              error, init_class
            else (
              let error, bool =
                Handler.is_counter parameters error kappa_handler agent_type b
              in

              if bool then
                error, init_class
              else (
                (* we could avoid this step, if we know that the site was already present *)
                let error, l' =
                  match
                    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
                    .unsafe_get parameters error agent_type init_class
                  with
                  | error, None -> error, [ [ b ] ]
                  | error, Some l -> error, [ b ] :: l
                in
                Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
                  parameters error agent_type l' init_class
              )
            ))
          b (error, init_class))
      kappa_handler.Cckappa_sig.sites init_class
  in
  (*-----------------------------------------------------------------------*)
  (*init state of covering class*)
  let init_class =
    {
      Covering_classes_type.store_modified_map = init_modif_map;
      Covering_classes_type.store_covering_classes = init_class;
    }
  in
  (*---------------------------------------------------------------------*)
  (*map each agent to a list of covering classes*)
  let start =
    match start with
    | None -> None
    | Some a -> Some a.Diff.next_rule
  in
  let error, store_covering_classes =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold ?start parameters
      error
      (fun parameters error _rule_id rule classes ->
        let error, result =
          scan_rule_covering_classes parameters error kappa_handler
            rule.Cckappa_sig.e_rule_c_rule classes
        in
        error, result)
      rules init_class
  in
  error, store_covering_classes

(***************************************************************************)
(*clean covering classes*)

let length_sorted (l : Ckappa_sig.c_site_name list list) :
    Ckappa_sig.c_site_name list list =
  let list_length = List.rev_map (fun list -> list, List.length list) l in
  let lists = List.sort (fun a b -> compare (snd a) (snd b)) list_length in
  List.rev_map fst lists

(******************************************************************************)
(*CLEANING*)
let store_remanent parameters error covering_class _modified_map remanent
    _nr_guard_parameters =
  (*add each variable that occurs in a guard to each covering class*)
  (*let guard_p_list =
      Ckappa_sig.get_list_of_guard_parameters nr_guard_parameters
    in*)
  let covering_class_with_guard_p =
    List.rev_map (fun x -> Ckappa_sig.Site x) (List.rev covering_class)
    (*@ List.map (fun x -> Ckappa_sig.Guard_p x) guard_p_list*)
  in
  (*-------------------------------------------------------------------------*)
  (* current state of remanent*)
  let pointer_backward =
    remanent.Covering_classes_type.store_pointer_backward
  in
  let good_covering_class = remanent.Covering_classes_type.store_dic in
  (*-------------------------------------------------------------------------*)
  (*covering class dictionary*)
  let error, output =
    Covering_classes_type.Dictionary_of_List_sites_or_guard.allocate parameters
      error compare_unit_covering_class_id
      covering_class_with_guard_p (*value: c_site_or_guard_p list*)
      () Misc_sa.const_unit good_covering_class
  in
  let error, (cv_id, store_dic) =
    match output with
    | Some (id, _, _, dic) -> error, (id, dic)
    | None ->
      Exception.warn parameters error __POS__ Exit
        (Covering_classes_type.dummy_cv_id, good_covering_class)
  in
  (*-----------------------------------------------------------------------*)
  (*store pointer backward*)
  let error, pointer_backward =
    List.fold_left
      (fun (error, pointer_backward) old_cv_id ->
        let error, old_cv_set =
          match
            Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.unsafe_get
              parameters error old_cv_id pointer_backward
          with
          | error, None -> error, Covering_classes_type.CV_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error', new_cv_set =
          Covering_classes_type.CV_map_and_set.Set.add parameters error cv_id
            old_cv_set
        in
        let error =
          Exception.check_point Exception.warn parameters error error' __POS__
            Exit
        in
        let error, pointer_backward =
          Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.set parameters
            error old_cv_id (*int*)
            new_cv_set (*set of int*)
            pointer_backward
        in
        error, pointer_backward)
      (error, pointer_backward) covering_class
  in
  (*--------------------------------------------------------------------*)
  (*result*)
  ( error,
    {
      Covering_classes_type.store_pointer_backward = pointer_backward;
      Covering_classes_type.store_dic;
    },
    (cv_id, covering_class_with_guard_p) )

(*--------------------------------------------------------------------------*)
(*CLEAN: In a covering class, it will store the old result of the previous
  covering class of an agent.

  For example:
  - rule 0: agent A has a covering class: (0)
  - rule 1: agent A has a covering class: (0,1)
  => Then do the intersection of two covering classes of agent A:
  (0) inter (0,1) -> 0
*)

let clean_classes ?patch parameters error covering_classes modified_map
    nr_guard_parameters =
  let error, init_remanent =
    match patch with
    | None ->
      let error, init_pointer =
        Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.create parameters
          error 0
      in
      let init_store_dic =
        Covering_classes_type.Dictionary_of_List_sites_or_guard.init ()
      in
      (*------------------------------------------------------------------------*)
      (*init state of dictionary*)
      ( error,
        {
          Covering_classes_type.store_pointer_backward = init_pointer;
          Covering_classes_type.store_dic = init_store_dic;
        } )
    | Some a -> error, a
  in
  (*------------------------------------------------------------------------*)
  (*cleaning*)
  let current_covering_classes = length_sorted covering_classes in
  List.fold_left
    (fun (error, bool, remanent, contrib) covering_class ->
      match covering_class with
      | [] -> error, bool, remanent, contrib
      | t :: tl ->
        let pointer_backward =
          remanent.Covering_classes_type.store_pointer_backward
        in
        (* return the set of list(id) containing t.
           For example: current_covering_classes: [[0;1];[0]]
           t = 0 => (id:1;id:2) of type set;
           remanent_type: [(id:1,[0;1]);(id:2,[0])];
           (id:pointer_backward, dic: int list)
        *)
        let error, potential_supersets =
          match
            Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.unsafe_get
              parameters error t pointer_backward
          with
          | error, None -> error, Covering_classes_type.CV_map_and_set.Set.empty
          | error, Some set -> error, set
        in
        let rec aux to_visit potential_supersets =
          match to_visit with
          | [] -> error, bool, remanent, contrib
          | t' :: tl' ->
            (* get the set of list(id) containing t' *)
            let error, potential_supersets' =
              match
                Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.unsafe_get
                  parameters error t' pointer_backward
              with
              | error, None ->
                error, Covering_classes_type.CV_map_and_set.Set.empty
              | error, Some set -> error, set
            in
            (*------------------------------------------------------------*)
            (* intersection of two sets *)
            let error', potential_superset =
              Covering_classes_type.CV_map_and_set.Set.inter parameters error
                potential_supersets potential_supersets'
            in
            let error =
              Exception.check_point Exception.warn parameters error error'
                __POS__ Exit
            in
            if
              Covering_classes_type.CV_map_and_set.Set.is_empty
                potential_superset
            then (
              let error, result_covering_dic, covering_class =
                store_remanent parameters error covering_class modified_map
                  remanent nr_guard_parameters
              in
              error, true, result_covering_dic, covering_class :: contrib
            ) else
              aux tl' potential_superset
        in
        (*-------------------------------------------------------------------*)
        (*check the beginning state of a superset*)
        if Covering_classes_type.CV_map_and_set.Set.is_empty potential_supersets
        then (
          (*if it is empty then store it to remanent*)
          let error, result_covering_dic, contrib_elt =
            store_remanent parameters error covering_class modified_map remanent
              nr_guard_parameters
          in
          error, true, result_covering_dic, contrib_elt :: contrib
        ) else
          aux tl potential_supersets)
    (error, false, init_remanent, [])
    current_covering_classes

(*-------------------------------------------------------------------------*)
(*compute covering classes in the set of rules*)

let scan_rule_set_remanent ?patch ~modified_agents parameters error
    kappa_handler rules =
  (*create a new initial state to store after cleaning the covering classes*)
  let error, init_result, start, next_agent =
    match patch with
    | None ->
      let error, init =
        Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create
          parameters error 0
      in
      error, init, None, Ckappa_sig.dummy_agent_name
    | Some (init_result, nr) ->
      ( error,
        init_result.Covering_classes_type.store_covering_classes_predicate,
        Some nr,
        nr.Diff.next_agent )
  in
  let error, patch_remanent =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create
      parameters error 0
  in
  let error, start_cv =
    if Ckappa_sig.int_of_agent_name next_agent = 0 then
      error, None
    else (
      let error, a =
        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.init parameters
          error
          (Ckappa_sig.int_of_agent_name next_agent - 1)
          (fun parameters error id ->
            match
              Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
              .unsafe_get parameters error id init_result
            with
            | error, None -> error, Covering_classes_type.dummy_cv_id
            | error, Some a ->
              let error, a =
                Covering_classes_type.Dictionary_of_List_sites_or_guard
                .last_entry parameters error a.Covering_classes_type.store_dic
              in
              let a = Covering_classes_type.next_cv_id a in
              error, a)
      in
      error, Some a
    )
  in
  let error, store_covering_classes =
    scan_rule_set_covering_classes ?start parameters error kappa_handler rules
  in
  let result_covering_classes =
    store_covering_classes.Covering_classes_type.store_covering_classes
  in
  let error, (start_cv, modified_agents, remanent_dictionary, patch_remanent) =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
      error
      (fun parameters error agent_type covering_class
           (start_cv, modified_agents, init_remanent, patch_remanent) ->
        (*----------------------------------------------------------------*)
        (*get modified site*)
        let error, modified_map =
          match
            Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
            .unsafe_get parameters error agent_type
              store_covering_classes.Covering_classes_type.store_modified_map
          with
          | error, None -> error, Ckappa_sig.Site_map_and_set.Map.empty
          | error, Some m -> error, m
        in
        let error, patch =
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
          .unsafe_get parameters error agent_type init_remanent
        in
        (*-----------------------------------------------------------------*)
        (*clean the covering classes, removed duplicate of covering classes*)
        let error, bool, store_remanent_dic, new_cv_list =
          clean_classes ?patch parameters error covering_class modified_map
            (Handler.get_nr_guard_parameters kappa_handler)
        in
        let error, modified_agents =
          if bool then
            see_agent ~modified_agents parameters error agent_type
          else
            error, modified_agents
        in
        (*compute the number of covering classes*)
        let error, get_number_cv =
          Covering_classes_type.Dictionary_of_List_sites_or_guard.last_entry
            parameters error store_remanent_dic.Covering_classes_type.store_dic
        in
        let number_cv = Covering_classes_type.int_of_cv_id get_number_cv + 1 in
        (*----------------------------------------------------------------*)
        (*print covering classes*)
        let _ =
          if Remanent_parameters.get_dump_site_dependencies parameters then (
            let parameters = Remanent_parameters.update_prefix parameters "" in
            let error, agent_string =
              Handler.string_of_agent parameters error kappa_handler agent_type
            in
            let _ =
              Covering_classes_type.Dictionary_of_List_sites_or_guard.iter
                parameters error
                (fun parameters error elt_id (*key*) site_type_list (*value*) _
                     _ ->
                  let _ =
                    Printf.fprintf stdout
                      "Potential dependencies between sites:Number of covering \
                       classes:%i\n"
                      number_cv
                  in
                  let _ =
                    (*print covering_class_id*)
                    Printf.fprintf stdout
                      "Potential dependencies between sites:\n\
                       agent_type:%s:%s:covering_class_id:%i\n"
                      (Ckappa_sig.string_of_agent_name agent_type)
                      agent_string
                      (Covering_classes_type.int_of_cv_id elt_id)
                  in
                  let error =
                    List.fold_left
                      (fun error site_type ->
                        let error, site_string =
                          Handler.string_of_site_or_guard parameters error
                            kappa_handler agent_type site_type
                        in
                        let () =
                          match site_type with
                          | Ckappa_sig.Site s ->
                            Printf.fprintf stdout "site_type:%i:%s\n"
                              (Ckappa_sig.int_of_site_name s)
                              site_string
                          | Ckappa_sig.Guard_p g ->
                            Printf.fprintf stdout "guard_parameter:%i:%s\n"
                              (Ckappa_sig.int_of_guard_parameter g)
                              site_string
                        in
                        error)
                      error site_type_list
                  in
                  error)
                store_remanent_dic.Covering_classes_type.store_dic
            in
            ()
          )
        in
        (*---------------------------------------------------------------*)
        (*store the covering classes after cleaning theirs duplicate classes*)
        let error, store_remanent =
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
            parameters error agent_type store_remanent_dic init_remanent
        in
        let error, patch_remanent =
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
            parameters error agent_type new_cv_list patch_remanent
        in
        (*----------------------------------------------------------------*)
        (*result*)
        error, (start_cv, modified_agents, store_remanent, patch_remanent))
      result_covering_classes
      (start_cv, modified_agents, init_result, patch_remanent)
  in
  error, remanent_dictionary, modified_agents, start_cv, patch_remanent

(**************************************************************************)
(*MAIN*)

let covering_classes ?patch parameters error kappa_handler cc_compil =
  let parameters = Remanent_parameters.update_prefix parameters "agent_type:" in
  scan_rule_set_remanent ?patch parameters error kappa_handler
    cc_compil.Cckappa_sig.rules

let init_predicate_covering_classes parameters error =
  let error, init_covering_classes =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create
      parameters error 0
  in
  let error, init_remanent_triple =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create
      parameters error 0
  in
  let error, init_site_correspondence =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create
      parameters error 0
  in
  ( error,
    {
      Covering_classes_type.store_covering_classes_predicate =
        init_covering_classes;
      Covering_classes_type.store_list_of_site_type_in_covering_classes =
        Covering_classes_type.AgentCV_map_and_set.Map.empty;
      Covering_classes_type.store_covering_classes_id =
        Common_static.empty_agentsiteorguard;
      Covering_classes_type.store_remanent_triple = init_remanent_triple;
      Covering_classes_type.site_correspondence = init_site_correspondence;
    } )

let site_covering_classes ?patch ~patch_remanent  parameters error
    (*covering_classes*) =
  let store_result =
    match patch with
    | None -> Ckappa_sig.AgentSiteOrGuard_map_and_set.Map.empty
    | Some (a, _) -> a.Covering_classes_type.store_covering_classes_id
  in
  let error, store_result =
    (*From sites return a list of covering_class_id*)
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
      error
      (fun parameters error agent_type_cv remanent store_result ->
          (*get a list of covering_class_id from remanent*)
          let error, store_result =
          List.fold_left 
              (fun (error, store_result) (cv_id,list_of_site_type)  -> 
                (*get site_cv in value*)
                  List.fold_left
                    (fun (error, store_result) site_type_cv ->
                      let error, store_result =
                        Common_map.add_dependency_pair_sites_cv parameters error
                          (agent_type_cv, site_type_cv)
                          cv_id store_result
                      in
                      error, store_result)
                    (error, store_result) list_of_site_type)
               (error, store_result) remanent 
          in
          error, store_result
        )
        (*REMARK: when it is folding inside a list, start with empty result,
          because the add_link function has already called the old result.*)
      patch_remanent store_result
  in
  error, store_result

let list_of_site_type_in_covering_class ?patch ~patch_remanent parameters error
    _covering_classes =
  let store_result =
    match patch with
    | None -> Covering_classes_type.AgentCV_map_and_set.Map.empty
    | Some (a, _) ->
      a.Covering_classes_type.store_list_of_site_type_in_covering_classes
  in
  let error, store_result =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
      error
      (fun parameters error agent_type_cv l store_result ->
        List.fold_left
          (fun (error, store_result) (cv_id, list_of_site_type) ->
            let error, old =
              Common_map.get_pair_agent_cv parameters error
                (agent_type_cv, cv_id) store_result
            in
            let new_list = List.append list_of_site_type old in
            let error, store_result =
              Covering_classes_type.AgentCV_map_and_set.Map.add_or_overwrite
                parameters error (agent_type_cv, cv_id) new_list store_result
            in
            error, store_result)
          (error, store_result) l)
      patch_remanent store_result
  in
  error, store_result

let collect_remanent_triple ?patch ~patch_remanent  parameters error =
  let error, store_result =
    match patch with
    | None ->
      Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create
        parameters error 0
    | Some (a, _) -> error, a.Covering_classes_type.store_remanent_triple
  in
  let error, patch_store_result = 
      Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create
        parameters error 0
  in 
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error agent_type remanent (store_result, patch_store_result) ->
        let error, old =
          match patch with
          | None -> error, []
          | Some _ ->
            (match
               Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
               .unsafe_get parameters error agent_type store_result
             with
            | error, None -> error, []
            | error, Some a -> error, a)
        in

        (*-----------------------------------------------------------------*)
        let error, triple_list, patch_triple_list =
        List.fold_left 
          (fun (error, current_list, patch_triple_list) (cv_id,list) -> 
(*              if ignore_cv ~cv_max cv_id then
                error, current_list
              else ( *)
                let error, set = Common_map.list2set parameters error list in
                
                error, 
                (cv_id, list, set)::current_list, (cv_id, list, set)::patch_triple_list 

              ) (error, old, []) 
            remanent 
        in
        (*--------------------------------------------------------*)
        let error, store_result =
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
            parameters error agent_type triple_list store_result
        in
         let error, patch_store_result =
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
            parameters error agent_type patch_triple_list patch_store_result
        in
        error, (store_result, patch_store_result)
      )
    patch_remanent (store_result, patch_store_result) 

let scan_predicate_covering_classes ?patch ~modified_agents parameters error
    handler_kappa compil =
  let error, store_covering_classes, modified_agents, start_cv, patch_remanent =
    covering_classes ?patch ~modified_agents parameters error handler_kappa
      compil
  in
  (*-----------------------------------------------------------------------*)
  let error, store_list_of_site_type_in_covering_classes =
    list_of_site_type_in_covering_class ?patch ~patch_remanent parameters error
      store_covering_classes
  in
  (*-----------------------------------------------------------------------*)
  (*static information of covering classes: from sites -> covering_class id
    list*)
  let error, store_covering_classes_id =
    site_covering_classes 
      ?patch ~patch_remanent parameters error
      
  in
  (*------------------------------------------------------------------------*)
  let error, (store_remanent_triple, patch_store_remanent_triple) =
    collect_remanent_triple 
      ?patch ~patch_remanent  parameters error
  in
  let error, init_array =
    match patch with
    | None ->
      Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create
        parameters error 0
    | Some (a, _) -> error, a.Covering_classes_type.site_correspondence
  in
  let error, site_correspondence =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
      error
      (fun parameters error ag list map ->
        (
          let error, last_site =
            Handler.last_site_of_agent parameters error handler_kappa ag
          in
          let size_map1 = 1 + Ckappa_sig.int_of_site_name last_site in
          let size_map2 = 1 + List.length list in
          let array =
            match patch with
            | None ->
              Covering_classes_type.Cv_id_nearly_Inf_Int_storage_Imperatif
              .create parameters error 0
            | Some _ ->
              (match
                 Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif
                 .unsafe_get parameters error ag map
               with
              | error, None ->
                Covering_classes_type.Cv_id_nearly_Inf_Int_storage_Imperatif
                .create parameters error 0
              | error, Some a -> error, a)
          in
          let error, array =
            List.fold_left
              (fun (error, array) (cv_id, list, _) ->
                  let rec aux acc k map1 map2 error =
                    match acc with
                    | [] -> error, (map1, map2)
                    | Ckappa_sig.Guard_p _ :: _ ->
                      (*only sites are converted to a new index*)
                      error, (map1, map2)
                    | Ckappa_sig.Site h :: tl ->
                      let error, map1 =
                        Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif
                        .set parameters error h k map1
                      in
                      let error, map2 =
                        Ckappa_sig.Mvbdu_var_nearly_Inf_Int_storage_Imperatif
                        .set parameters error k h map2
                      in
                      aux tl
                        (Ckappa_sig.mvbdu_var_of_int
                           (Ckappa_sig.int_of_mvbdu_var k + 1))
                        map1 map2 error
                  in
                  let error, map1 =
                    Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.create
                      parameters error size_map1
                  in
                  let error, map2 =
                    Ckappa_sig.Mvbdu_var_nearly_Inf_Int_storage_Imperatif.create
                      parameters error size_map2
                  in
                  let error, (map1, map2) =
                    aux list Ckappa_sig.dummy_mvbdu_var_1 map1 map2 error
                  in
                  Covering_classes_type.Cv_id_nearly_Inf_Int_storage_Imperatif
                  .set parameters error cv_id (map1, map2) array
                )
              array list
          in
          Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
            parameters error ag array map
        ))
      patch_store_remanent_triple init_array
  in
  ( error,
    {
      Covering_classes_type.store_covering_classes_predicate =
        store_covering_classes;
      Covering_classes_type.store_list_of_site_type_in_covering_classes;
      Covering_classes_type.store_covering_classes_id;
      Covering_classes_type.store_remanent_triple;
      Covering_classes_type.site_correspondence;
    },
    modified_agents,
    start_cv, 
    patch_store_remanent_triple )
