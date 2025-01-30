(**
  * agent_trace.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation:                      <2016-03-21 10:00:00 feret>
  * Last modification: Time-stamp: <Jan 08 2020>
  * *
  * Compute the projection of the traces for each insighful
   * subset of site in each agent

  *
  * Copyright 2016 Institut National
  * de Recherche en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type fst_label = Rule of Ckappa_sig.c_rule_id | Init of int
type label = fst_label * Ckappa_sig.c_agent_id * int

let int_of_fst_label i =
  match i with
  | Rule r -> Ckappa_sig.int_of_rule_id r
  | Init i -> -(i + 1)

let int_pair_of_label (i, j, _) =
  int_of_fst_label i, Ckappa_sig.int_of_agent_id j

module Label = Map_wrapper.Make (SetMap.Make (struct
  type t = label

  let compare = compare
  let print _ _ = ()
end))

module LabelMap = Label.Map
module LabelSet = Label.Set

module Site = Map_wrapper.Make (SetMap.Make (struct
  type t = Ckappa_sig.c_site_name

  let compare = compare
  let print _ _ = ()
end))

module SiteMap = Site.Map
module SiteSet = Site.Set

module SitePair = Map_wrapper.Make (SetMap.Make (struct
  type t = Ckappa_sig.c_site_name * Ckappa_sig.c_site_name

  let compare = compare
  let print _ _ = ()
end))

module SitePairSet = SitePair.Set

type extensional_representation = {
  title: string;
  agent_type: Ckappa_sig.c_agent_name;
  agent_string: string;
  nodes: Ckappa_sig.Views_intbdu.mvbdu list;
  edges:
    (Ckappa_sig.Views_intbdu.mvbdu * label * Ckappa_sig.Views_intbdu.mvbdu) list;
  nodes_creation: (Ckappa_sig.Views_intbdu.mvbdu * label) list;
  nodes_degradation: (Ckappa_sig.Views_intbdu.mvbdu * label) list;
  subframe: Mods.IntSet.t Mods.IntMap.t;
  nodes_in_bdu: Mods.IntSet.t;
}

let mvbdu_of_association_list_gen gen asso_list =
  let hconsed_list = gen asso_list in
  let mvbdu_true = Ckappa_sig.Views_intbdu.mvbdu_true () in
  Ckappa_sig.Views_intbdu.mvbdu_redefine mvbdu_true hconsed_list

let mvbdu_of_association_list asso =
  mvbdu_of_association_list_gen Ckappa_sig.Views_intbdu.build_association_list
    asso

let mvbdu_of_reverse_order_association_list asso =
  mvbdu_of_association_list_gen
    Ckappa_sig.Views_intbdu.build_reverse_sorted_association_list asso

let empty_transition_system title agent_string agent_name =
  {
    title;
    agent_type = agent_name;
    agent_string;
    edges = [];
    nodes_creation = [];
    nodes_degradation = [];
    nodes = [];
    subframe = Mods.IntMap.empty;
    nodes_in_bdu = Mods.IntSet.empty;
  }

let hash_of_association_list list =
  let hconsed_list = Ckappa_sig.Views_intbdu.build_association_list list in
  let hash = Ckappa_sig.Views_intbdu.hash_of_association_list hconsed_list in
  hash

let build_asso_of_mvbdu parameters error mvbdu =
  let list = Ckappa_sig.Views_intbdu.extensional_of_mvbdu mvbdu in
  match list with
  | [ list ] -> error, list
  | _ ->
    let error, list = Exception.warn parameters error __POS__ Exit [] in
    error, list

let hash_of_mvbdu parameters error mvbdu =
  let error, asso = build_asso_of_mvbdu parameters error mvbdu in
  error, hash_of_association_list asso

let add_node parameters error q transition_system =
  let bdu_set = transition_system.nodes_in_bdu in
  let error, hash = hash_of_mvbdu parameters error q in
  if Mods.IntSet.mem hash bdu_set then
    error, transition_system
  else
    ( error,
      {
        transition_system with
        nodes = q :: transition_system.nodes;
        nodes_in_bdu = Mods.IntSet.add hash bdu_set;
      } )

let add_edge q q' label transition_system =
  { transition_system with edges = (q, label, q') :: transition_system.edges }

let convert_label (r, a) =
  ( Ckappa_sig.state_index_of_int (int_of_fst_label r),
    Ckappa_sig.state_index_of_int (Ckappa_sig.int_of_agent_id a) )

let add_creation parameters error r_id ag_id mvbdu transition_system =
  let error, transition_system =
    add_node parameters error mvbdu transition_system
  in
  ( error,
    {
      transition_system with
      nodes_creation =
        (mvbdu, (r_id, ag_id, 0)) :: transition_system.nodes_creation;
    } )

let dump_edge logger parameters error compil key key' label =
  let error, rule_name =
    if Remanent_parameters.get_show_rule_names_in_local_traces parameters then
      Handler.string_of_rule ~with_loc:false ~with_rule:false ~with_ast:false
        parameters error compil (fst label)
    else
      error, ""
  in
  let () =
    Graph_loggers.print_edge logger ("Node_" ^ key) ("Node_" ^ key')
      ~directives:[ Graph_loggers_sig.Label rule_name ]
  in
  error

let string_key_of_asso list =
  let hash = hash_of_association_list list in
  Printf.sprintf "Node_%i" hash

let string_label_of_asso parameters error handler_kappa transition_system list =
  let string = transition_system.agent_string ^ "(" in
  let error, string, _ =
    List.fold_left
      (fun (error, string, bool) (site_type, state) ->
        let string =
          if bool then
            string ^ ","
          else
            string
        in
        let error', site_string =
          Handler.string_of_site parameters error handler_kappa ~state
            transition_system.agent_type site_type
        in
        let error =
          Exception.check_point Exception.warn parameters error error' __POS__
            Exit
        in
        error, string ^ site_string, true)
      (error, string, false) list
  in
  error, string ^ ")"

let dump_mvbdu logger parameters error handler_kappa transition_system mvbdu =
  let error, list = build_asso_of_mvbdu parameters error mvbdu in
  let key = string_key_of_asso list in
  let error, label =
    string_label_of_asso parameters error handler_kappa transition_system list
  in
  let () =
    Graph_loggers.print_node logger key
      ~directives:[ Graph_loggers_sig.Label label ]
  in
  error

let add_node_from_mvbdu _parameters _handler_kappa error _agent_type
    _agent_string mvbdu transition_system =
  error, { transition_system with nodes = mvbdu :: transition_system.nodes }

let bdu_of_view parameters error test =
  let mvbdu_false = Ckappa_sig.Views_intbdu.mvbdu_false () in
  let mvbdu_true = Ckappa_sig.Views_intbdu.mvbdu_true () in
  List.fold_left
    (fun (error, mvbdu) (site, state) ->
      let error, lub =
        match state.Cckappa_sig.site_state.Cckappa_sig.min with
        | Some a -> error, Ckappa_sig.int_of_state_index a
        | None -> Exception.warn parameters error __POS__ Exit 0
      in
      let error, glb =
        match state.Cckappa_sig.site_state.Cckappa_sig.max with
        | Some a -> error, Ckappa_sig.int_of_state_index a
        | None -> Exception.warn parameters error __POS__ Exit 0
      in
      let rec aux k mvbdu =
        if k > glb then
          mvbdu
        else (
          let mvbdu' =
            mvbdu_of_association_list [ site, Ckappa_sig.state_index_of_int k ]
          in
          aux (k + 1) (Ckappa_sig.Views_intbdu.mvbdu_or mvbdu mvbdu')
        )
      in
      let mvbdu' = aux lub mvbdu_false in
      error, Ckappa_sig.Views_intbdu.mvbdu_and mvbdu mvbdu')
    (error, mvbdu_true) test

let asso_of_view_in_list parameters error view =
  let error, list =
    List.fold_left
      (fun (error, list) (site, state) ->
        let error, lub =
          match state.Cckappa_sig.site_state.Cckappa_sig.min with
          | Some a -> error, a
          | None ->
            Exception.warn parameters error __POS__ Exit
              (Ckappa_sig.state_index_of_int 0)
        in
        let error, glb =
          match state.Cckappa_sig.site_state.Cckappa_sig.max with
          | Some a -> error, a
          | None ->
            Exception.warn parameters error __POS__ Exit
              (Ckappa_sig.state_index_of_int 0)
        in
        if lub = glb then
          error, (site, lub) :: list
        else
          Exception.warn parameters error __POS__ Exit list)
      (error, []) view
  in
  error, Ckappa_sig.Views_intbdu.build_association_list list

let asso_of_view_in_map parameters error map =
  asso_of_view_in_list parameters error
    (Ckappa_sig.Site_map_and_set.Map.fold
       (fun site state list -> (site, state) :: list)
       map [])

let restrict_asso asso set =
  let asso = Ckappa_sig.Views_intbdu.extensional_of_association_list asso in
  let asso = List.filter (fun (a, _) -> SiteSet.mem a set) asso in
  Ckappa_sig.Views_intbdu.build_association_list asso

type compute_full_support_output =
  | Mod of
      (Ckappa_sig.c_agent_name
      * SiteSet.t
      * Ckappa_sig.Views_bdu.mvbdu
      * SiteSet.t
      * Ckappa_sig.Views_intbdu.hconsed_association_list)
  | Creation of
      (Ckappa_sig.c_agent_name
      * SiteSet.t
      * Ckappa_sig.Views_intbdu.hconsed_association_list)
  | Degradation of
      (Ckappa_sig.c_agent_name * SiteSet.t * Ckappa_sig.Views_bdu.mvbdu)
  | Nil

let compute_full_support parameters error handler ag_id rule =
  let test = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs in
  let diff = rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.diff_direct in
  let error', agent =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameters
      error ag_id test.Cckappa_sig.views
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  let view =
    match agent with
    | None
    | Some Cckappa_sig.Ghost
    | Some (Cckappa_sig.Dead_agent _)
    | Some (Cckappa_sig.Unknown_agent _) ->
      None
    | Some (Cckappa_sig.Agent ag) -> Some ag
  in
  let parse error v =
    match v with
    | Some v ->
      let error, list, list' =
        Ckappa_sig.Site_map_and_set.Map.fold
          (fun site state (error, list, list') ->
            let error, b_counter =
              Handler.is_counter parameters error handler
                v.Cckappa_sig.agent_name site
            in
            if not b_counter then (
              let error', list = SiteSet.add parameters error site list in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              error, list, (site, state) :: list'
            ) else
              error, list, list')
          v.Cckappa_sig.agent_interface (error, SiteSet.empty, [])
      in
      error, Some v.Cckappa_sig.agent_name, Some (list, list')
    | None -> error, None, None
  in
  let error, agent =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
      parameters error ag_id diff
  in
  let error =
    Exception.check_point Exception.warn parameters error error'
      ~message:(string_of_int (Ckappa_sig.int_of_agent_id ag_id))
      __POS__ Exit
  in
  let error, name', test_opt = parse error view in
  let error, name'', diff = parse error agent in
  let error, name =
    match name', name'' with
    | Some a, Some a' when a = a' -> error, a
    | Some a, None | None, Some a -> error, a
    | Some _, Some _ | None, None ->
      Exception.warn parameters error __POS__ Exit Ckappa_sig.dummy_agent_name
  in
  match diff with
  | None ->
    (match test_opt with
    | None -> Exception.warn parameters error __POS__ Exit Nil
    | Some (list, list') ->
      let error, list' = bdu_of_view parameters error list' in
      error, Degradation (name, list, list'))
  | Some (list'', list''') ->
    let error, list''' = asso_of_view_in_list parameters error list''' in
    (match test_opt with
    | None -> error, Creation (name, list'', list''')
    | Some (list, list') ->
      let error, list' = bdu_of_view parameters error list' in
      error, Mod (name, list, list', list'', list'''))

let empty_hs parameters error hand_side =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun _parameters error _ agent b ->
      match agent with
      | Cckappa_sig.Ghost -> error, b
      | Cckappa_sig.Dead_agent _ | Cckappa_sig.Unknown_agent _
      | Cckappa_sig.Agent _ ->
        error, false)
    hand_side.Cckappa_sig.views true

let empty_rule parameters error _r_id rule =
  let error, b =
    empty_hs parameters error
      rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs
  in
  if b then
    empty_hs parameters error
      rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_rhs
  else
    error, b

let build_support parameters error handler rules dead_rules =
  Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameters error
    (fun parameters error r_id rule (map, creation, degradation) ->
      let error, b = dead_rules parameters error r_id in
      let error, b' = empty_rule parameters error r_id rule in
      if b || b' then
        error, (map, creation, degradation)
      else
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
          parameters error
          (fun parameters error ag_id _ (map, creation, degradation) ->
            match compute_full_support parameters error handler ag_id rule with
            | error, Nil -> error, (map, creation, degradation)
            | error, Creation (agent_name, _, asso) ->
              let error', old_list =
                Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs
                  parameters error [] agent_name creation
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let error, creation =
                Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite parameters
                  error agent_name
                  (((Rule r_id, ag_id), asso) :: old_list)
                  creation
              in
              error, (map, creation, degradation)
            | error, Degradation (agent_name, _, mvbdu) ->
              let error', old_map =
                Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs
                  parameters error LabelMap.empty agent_name degradation
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let error', new_map =
                LabelMap.add parameters error (Rule r_id, ag_id, 0) mvbdu
                  old_map
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let error, degradation =
                Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite parameters
                  error agent_name new_map degradation
              in
              error, (map, creation, degradation)
            | error, Mod (agent_name, set_test, asso_test, set_mod, asso_mod) ->
              let error', old_map =
                Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs
                  parameters error LabelMap.empty agent_name map
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let error', new_map =
                LabelMap.add parameters error (Rule r_id, ag_id, 0)
                  (set_test, asso_test, set_mod, asso_mod)
                  old_map
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let error, map =
                Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite parameters
                  error agent_name new_map map
              in
              error, (map, creation, degradation))
          rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
          (map, creation, degradation))
    rules
    ( Ckappa_sig.Agent_map_and_set.Map.empty,
      Ckappa_sig.Agent_map_and_set.Map.empty,
      Ckappa_sig.Agent_map_and_set.Map.empty )

let pw parameters error list =
  let error, map =
    List.fold_left
      (fun (error, acc) (_, (a, b)) ->
        let error, old =
          SiteMap.find_default_without_logs parameters error [] a acc
        in
        SiteMap.add_or_overwrite parameters error a (b :: old) acc)
      (error, SiteMap.empty) list
  in
  ( error,
    SiteMap.fold
      (fun a l acc ->
        List.fold_left
          (fun acc list ->
            List.fold_left (fun acc b -> ((a, b) :: list) :: acc) acc l)
          acc acc)
      map [ [] ] )

let smash_side_effect parameters error static dead_rules =
  let error, init =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.create parameters
      error 0
  in
  Ckappa_sig.AgentRule_map_and_set.Map.fold
    (fun (agent_name, rule_id) _ (error, map) ->
      let error, b = dead_rules parameters error rule_id in
      if b then
        error, map
      else (
        let error, old_asso =
          match
            Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.unsafe_get
              parameters error agent_name map
          with
          | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
          | error, Some set -> error, set
        in
        let error, new_asso =
          Ckappa_sig.Rule_map_and_set.Set.add parameters error rule_id old_asso
        in
        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.set parameters
          error agent_name new_asso map
      ))
    (Analyzer_headers.get_potential_side_effects static)
    (error, init)

let build_side_effect parameters error static agent_name site_set smashed_map =
  let error, init =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.create parameters error 0
  in
  let error, ruleset =
    match
      Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.unsafe_get
        parameters error agent_name smashed_map
    with
    | error, None -> error, Ckappa_sig.Rule_map_and_set.Set.empty
    | error, Some set -> error, set
  in
  Ckappa_sig.Rule_map_and_set.Set.fold
    (fun r_id (error, map) ->
      let error', list =
        Ckappa_sig.AgentRule_map_and_set.Map.find_default_without_logs
          parameters error [] (agent_name, r_id)
          (Analyzer_headers.get_potential_side_effects static)
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      let list = List.filter (fun (_, (a, _)) -> SiteSet.mem a site_set) list in
      (* TO DO Better *)
      let error, list = pw parameters error list in
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.set parameters error r_id
        list map)
    ruleset (error, init)

let commute parameters error label1 label2 support =
  let error, opt1 = LabelMap.find_option parameters error label1 support in
  let error, opt2 = LabelMap.find_option parameters error label2 support in
  match opt1, opt2 with
  | None, _ | _, None -> error, true
  | Some (test1, _, act1, _), Some (test2, _, act2, _) ->
    let error, inter1 = SiteSet.inter parameters error test1 act2 in
    if SiteSet.is_empty inter1 then (
      let error, inter2 = SiteSet.inter parameters error test2 act1 in
      if SiteSet.is_empty inter2 then
        error, true
      else
        error, false
    ) else
      error, false

let cannot_be_concurrent mvbdu mvbdu1 mvbdu2 =
  let mvbdu_and = Ckappa_sig.Views_intbdu.mvbdu_and mvbdu1 mvbdu2 in
  let mvbdu = Ckappa_sig.Views_intbdu.mvbdu_and mvbdu_and mvbdu in
  let mvbdu_false = Ckappa_sig.Views_intbdu.mvbdu_false () in
  Ckappa_sig.Views_intbdu.equal mvbdu_false mvbdu

let concurrent_sites parameters error mvbdu support =
  LabelMap.fold
    (fun label (_test, asso, _act, _asso_modif) (error, map') ->
      let error, sites_in_conflict =
        LabelMap.fold
          (fun label' (_test, asso', act, _asso_modif') (error, accu) ->
            let error, is_commute =
              commute parameters error label label' support
            in
            if is_commute then
              if cannot_be_concurrent mvbdu asso asso' then
                error, accu
              else (
                let error, accu = SiteSet.union parameters error accu act in
                error, accu
              )
            else
              error, accu)
          support (error, SiteSet.empty)
      in
      let ext_list = SiteSet.elements sites_in_conflict in
      let hconsed = Ckappa_sig.Views_intbdu.build_variables_list ext_list in
      let error', map' =
        LabelMap.add parameters error label (sites_in_conflict, hconsed) map'
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      error, map')
    support (error, LabelMap.empty)

let trivial_concurrent_sites _ error _ support =
  let hconsed = Ckappa_sig.Views_intbdu.build_variables_list [] in
  error, LabelMap.map (fun _ -> SiteSet.empty, hconsed) support

let compute_var_set transition_system =
  List.fold_left
    (fun (map_interface, map_domain) mvbdu ->
      let varlist = Ckappa_sig.Views_intbdu.variables_list_of_mvbdu mvbdu in
      let hash = Ckappa_sig.Views_intbdu.hash_of_variables_list varlist in
      let map_interface = Mods.IntMap.add hash varlist map_interface in
      let old_list = Mods.IntMap.find_default [] hash map_domain in
      let map_domain = Mods.IntMap.add hash (mvbdu :: old_list) map_domain in
      map_interface, map_domain)
    (Mods.IntMap.empty, Mods.IntMap.empty)
    transition_system.nodes

let distrib_list list1 list2 =
  let rec aux list1 list2 list12 list1wo2 list2wo1 =
    match list1, list2 with
    | [], _ -> List.rev list12, List.rev list1wo2, List.rev list2wo1 @ list2
    | _, [] -> List.rev list12, List.rev list1wo2 @ list1, List.rev list2wo1
    | h :: q, h' :: q' ->
      if h = h' then
        aux q q' (h :: list12) list1wo2 list2wo1
      else if h < h' then
        aux q list2 list12 (h :: list1wo2) list2wo1
      else
        aux list1 q' list12 list1wo2 (h' :: list2wo1)
  in
  aux list1 list2 [] [] []

let distrib_list list1 list2 =
  let lista, listb, listc = distrib_list list1 list2 in
  let f list =
    let () =
      List.iter
        (fun s -> Printf.fprintf stdout "%i;" (Ckappa_sig.int_of_site_name s))
        list
    in
    let () = Printf.fprintf stdout "\n" in
    ()
  in
  let () = Printf.fprintf stdout "DISTRIB\n" in
  let () = f list1 in
  let () = f list2 in
  let () = f lista in
  let () = f listb in
  let () = f listc in
  let () = Printf.fprintf stdout "\n" in
  lista, listb, listc

let powerset map_interface =
  let list =
    Mods.IntMap.fold
      (fun hash hconsed list -> (hash, hconsed) :: list)
      map_interface []
  in
  let list =
    List.sort
      (fun (_, b) (_, d) ->
        compare
          (Ckappa_sig.Views_intbdu.nbr_variables d)
          (Ckappa_sig.Views_intbdu.nbr_variables b))
      list
  in
  let rec aux to_use already_built =
    match to_use with
    | [] -> already_built
    | (hash, hconsed) :: tail ->
      let already_built =
        List.fold_left
          (fun list (hash', hconsed', proof) ->
            let hconsed'' =
              Ckappa_sig.Views_intbdu.merge_variables_lists hconsed hconsed'
            in
            let hash'' =
              Ckappa_sig.Views_intbdu.hash_of_variables_list hconsed''
            in
            if hash' <> hash'' then
              (hash'', hconsed'', (hash, hconsed) :: proof)
              :: (hash', hconsed', proof) :: list
            else
              (hash', hconsed', proof) :: list)
          [] already_built
      in
      aux tail already_built
  in
  let hconsed = Ckappa_sig.Views_intbdu.build_variables_list [] in
  let hash = Ckappa_sig.Views_intbdu.hash_of_variables_list hconsed in
  let list = aux list [ hash, hconsed, [] ] in
  let extended_map =
    List.fold_left
      (fun extended_map (hash, _hconsed, proof) ->
        let old_list = Mods.IntMap.find_default [] hash extended_map in
        let new_list =
          match proof with
          | [] | [ _ ] -> old_list
          | _ -> proof :: old_list
        in
        Mods.IntMap.add hash new_list extended_map)
      Mods.IntMap.empty list
  in
  Mods.IntMap.fold
    (fun hash list map ->
      let list =
        List.filter (fun l -> not (List.exists (fun (a, _) -> a = hash) l)) list
      in
      match list with
      | [] | [ _ ] -> map
      | _ -> Mods.IntMap.add hash list map)
    extended_map Mods.IntMap.empty

let add_singular parameters error transition_system =
  let map1, map2 = compute_var_set transition_system in
  let ambiguous_node = powerset map1 in
  Mods.IntMap.fold
    (fun _ proof_list (error, transition_system) ->
      let rec aux map2 proof_list (error, transition_system) =
        match proof_list with
        | [] -> error, transition_system
        | proof :: tail ->
          let error, transition_system =
            List.fold_left
              (fun (error, transition_system) proof' ->
                let gen map2 proof =
                  List.fold_left
                    (fun mvbdu (hashvarlist, _) ->
                      let mvbdu' =
                        List.fold_left
                          (fun mvbdu'' mvbdu''' ->
                            Ckappa_sig.Views_intbdu.mvbdu_or mvbdu'' mvbdu''')
                          (Ckappa_sig.Views_intbdu.mvbdu_false ())
                          (Mods.IntMap.find_default [] hashvarlist map2)
                      in
                      Ckappa_sig.Views_intbdu.mvbdu_and mvbdu' mvbdu)
                    (Ckappa_sig.Views_intbdu.mvbdu_true ())
                    proof
                in
                let mvbdu =
                  Ckappa_sig.Views_intbdu.mvbdu_and (gen map2 proof)
                    (gen map2 proof')
                in
                let error, transition_system =
                  if
                    Remanent_parameters.get_add_singular_microstates parameters
                    || Remanent_parameters.get_compute_separating_transitions
                         parameters
                  then (
                    let mvbdu_list =
                      Ckappa_sig.Views_intbdu.extensional_of_mvbdu mvbdu
                    in
                    List.fold_left
                      (fun (error, transition_system) asso ->
                        add_node parameters error
                          (Ckappa_sig.Views_intbdu.mvbdu_of_association_list
                             asso)
                          transition_system)
                      (error, transition_system) mvbdu_list
                  ) else
                    error, transition_system
                in
                let error, transition_system =
                  if
                    Remanent_parameters.get_add_singular_macrostates parameters
                    || Remanent_parameters.get_compute_separating_transitions
                         parameters
                  then
                    List.fold_left
                      (fun (error, transition_system) (_, hconsed) ->
                        let length =
                          Ckappa_sig.Views_intbdu.nbr_variables hconsed
                        in
                        let list_var =
                          Ckappa_sig.Views_intbdu.extensional_of_variables_list
                            hconsed
                        in
                        List.fold_left
                          (fun (error, transition_system) (_, hconsed') ->
                            let length' =
                              Ckappa_sig.Views_intbdu.nbr_variables hconsed'
                            in
                            let hconsed'' =
                              Ckappa_sig.Views_intbdu.merge_variables_lists
                                hconsed hconsed'
                            in
                            let length'' =
                              Ckappa_sig.Views_intbdu.nbr_variables hconsed''
                            in
                            if length' = length'' || length = length'' then
                              error, transition_system
                            else (
                              let list_var' =
                                Ckappa_sig.Views_intbdu
                                .extensional_of_variables_list hconsed'
                              in
                              let lista, listb, listc =
                                distrib_list list_var list_var'
                              in
                              List.fold_left
                                (fun (error, transition_system) list_var ->
                                  let hconsed =
                                    Ckappa_sig.Views_intbdu.build_variables_list
                                      list_var
                                  in
                                  let restricted_mvbdu =
                                    Ckappa_sig.Views_intbdu
                                    .mvbdu_project_keep_only mvbdu hconsed
                                  in
                                  let mvbdu_list =
                                    Ckappa_sig.Views_intbdu.extensional_of_mvbdu
                                      restricted_mvbdu
                                  in
                                  List.fold_left
                                    (fun (error, transition_system) asso ->
                                      add_node parameters error
                                        (Ckappa_sig.Views_intbdu
                                         .mvbdu_of_association_list asso)
                                        transition_system)
                                    (error, transition_system) mvbdu_list)
                                (error, transition_system)
                                [ lista; listb; listc ]
                            ))
                          (error, transition_system) proof)
                      (error, transition_system) proof'
                  else
                    error, transition_system
                in
                error, transition_system)
              (error, transition_system) tail
          in
          aux map2 tail (error, transition_system)
      in
      aux map2 proof_list (error, transition_system))
    ambiguous_node (error, transition_system)

let merge_neighbour parameters error concurrent_sites =
  LabelMap.fold
    (fun label (sites_in_conflict, _hconsed) (error, map) ->
      let error, set, _list =
        LabelMap.fold
          (fun label' (sites_in_conflict', _hconsed') (error, set, list) ->
            let error, diff1 =
              SiteSet.minus parameters error sites_in_conflict
                sites_in_conflict'
            in
            let error, diff2 =
              SiteSet.minus parameters error sites_in_conflict'
                sites_in_conflict
            in
            if SiteSet.is_singleton diff1 && SiteSet.is_singleton diff2 then (
              let a1 = SiteSet.min_elt diff1 in
              let a2 = SiteSet.min_elt diff2 in
              match a1, a2 with
              | Some a1, Some a2 ->
                let error, set =
                  SitePairSet.add_when_not_in parameters error (a1, a2) set
                in
                error, set, label' :: list
              | None, _ | _, None ->
                let error, set =
                  Exception.warn parameters error __POS__ Exit set
                in
                error, set, list
            ) else
              error, set, list)
          concurrent_sites
          (error, SitePairSet.empty, [])
      in
      if SitePairSet.is_singleton set then (
        match SitePairSet.min_elt set with
        | None -> Exception.warn parameters error __POS__ Exit map
        | Some (a1, _a2) ->
          let error, sites_in_conflict =
            SiteSet.remove parameters error a1 sites_in_conflict
          in
          let ext_list = SiteSet.elements sites_in_conflict in
          let hconsed = Ckappa_sig.Views_intbdu.build_variables_list ext_list in
          LabelMap.overwrite parameters error label
            (sites_in_conflict, hconsed)
            map
      ) else
        error, map)
    concurrent_sites (error, concurrent_sites)

let concurrent_sites parameters error mvbdu support =
  if Remanent_parameters.get_use_macrotransitions_in_local_traces parameters
  then (
    let error, concurrent = concurrent_sites parameters error mvbdu support in
    if Remanent_parameters.get_ignore_local_losanges parameters then
      merge_neighbour parameters error concurrent
    else
      error, concurrent
  ) else
    trivial_concurrent_sites parameters error mvbdu support

let add key im map =
  let old_set = Mods.IntMap.find_default Mods.IntSet.empty key map in
  Mods.IntMap.add key (Mods.IntSet.add im old_set) map

let addsub small big transition_system =
  { transition_system with subframe = add small big transition_system.subframe }

let add_whether_subframe parameters error mvbdu1 mvbdu2 transition_system =
  if Ckappa_sig.Views_intbdu.equal mvbdu1 mvbdu2 then
    error, transition_system
  else (
    let mvbdu_lub = Ckappa_sig.Views_intbdu.mvbdu_or mvbdu1 mvbdu2 in
    if Ckappa_sig.Views_intbdu.equal mvbdu_lub mvbdu2 then (
      let error, hash1 = hash_of_mvbdu parameters error mvbdu1 in
      let error, hash2 = hash_of_mvbdu parameters error mvbdu2 in
      error, addsub hash1 hash2 transition_system
    ) else
      error, transition_system
  )

let check_all_subframes parameters error transition_system =
  List.fold_left
    (fun (error, transition_system) mvbdu1 ->
      List.fold_left
        (fun (error, transition_system) mvbdu2 ->
          add_whether_subframe parameters error mvbdu1 mvbdu2 transition_system)
        (error, transition_system) transition_system.nodes)
    (error, transition_system) transition_system.nodes

let reduce_subframes transition_system =
  {
    transition_system with
    subframe =
      Mods.IntMap.map
        (fun set ->
          Mods.IntSet.filter
            (fun i ->
              Mods.IntSet.for_all
                (fun j ->
                  not
                    (Mods.IntSet.mem i
                       (Mods.IntMap.find_default Mods.IntSet.empty j
                          transition_system.subframe)))
                set)
            set)
        transition_system.subframe;
  }

let print logger parameters compil handler_kappa handler error transition_system
    =
  let () = Graph_loggers.print_graph_preamble logger transition_system.title in
  (* nodes -> Initial *)
  let error, handler =
    List.fold_left
      (fun (error, handler) (mvbdu, _label) ->
        let error, key = hash_of_mvbdu parameters error mvbdu in
        let () =
          Graph_loggers.print_node logger
            ("Init_" ^ string_of_int key)
            ~directives:
              [
                Graph_loggers_sig.Width 0;
                Graph_loggers_sig.Height 0;
                Graph_loggers_sig.Shape Graph_loggers_sig.Invisible;
                Graph_loggers_sig.Label "";
              ]
        in
        error, handler)
      (error, handler) transition_system.nodes_creation
  in
  (* nodes -> final *)
  let error, handler =
    List.fold_left
      (fun (error, handler) (mvbdu, _label) ->
        let error, key = hash_of_mvbdu parameters error mvbdu in
        let () =
          Graph_loggers.print_node logger
            ("Final_" ^ string_of_int key)
            ~directives:
              [
                Graph_loggers_sig.Width 0;
                Graph_loggers_sig.Height 0;
                Graph_loggers_sig.Shape Graph_loggers_sig.Invisible;
                Graph_loggers_sig.Label "";
              ]
        in
        error, handler)
      (error, handler) transition_system.nodes_degradation
  in
  (* nodes -> regular *)
  let error =
    List.fold_left
      (fun error ->
        dump_mvbdu logger parameters error handler_kappa transition_system)
      error transition_system.nodes
  in
  (* edges  *)
  let error, handler =
    List.fold_left
      (fun (error, handler) (q, label, q') ->
        let error, key = hash_of_mvbdu parameters error q in
        let error, key' = hash_of_mvbdu parameters error q' in
        let error, rule_name =
          if Remanent_parameters.get_show_rule_names_in_local_traces parameters
          then (
            match label with
            | Rule r, _, _ ->
              Handler.string_of_rule ~with_loc:false ~with_rule:false
                ~with_ast:false parameters error compil r
            | Init _, _, _ -> error, ""
          ) else
            error, ""
        in
        let () =
          Graph_loggers.print_edge logger
            ("Node_" ^ string_of_int key)
            ("Node_" ^ string_of_int key')
            ~directives:[ Graph_loggers_sig.Label rule_name ]
        in
        error, handler)
      (error, handler) transition_system.edges
  in
  let error, _ =
    List.fold_left
      (fun (error, handler) (q, label) ->
        let error, key = hash_of_mvbdu parameters error q in
        let error, rule_name =
          if Remanent_parameters.get_show_rule_names_in_local_traces parameters
          then (
            match label with
            | Rule r, _, _ ->
              Handler.string_of_rule ~with_loc:false ~with_rule:false
                ~with_ast:false parameters error compil r
            | Init _, _, _ -> error, ""
          ) else
            error, ""
        in
        let () =
          Graph_loggers.print_edge logger
            ("Init_" ^ string_of_int key)
            ("Node_" ^ string_of_int key)
            ~directives:[ Graph_loggers_sig.Label rule_name ]
        in
        error, handler)
      (error, handler) transition_system.nodes_creation
  in
  let error, _ =
    List.fold_left
      (fun (error, handler) (q, label) ->
        let error, key = hash_of_mvbdu parameters error q in
        let error, rule_name =
          if Remanent_parameters.get_show_rule_names_in_local_traces parameters
          then (
            match label with
            | Rule r, _, _ ->
              Handler.string_of_rule ~with_loc:false ~with_rule:false
                ~with_ast:false parameters error compil r
            | Init _, _, _ -> error, ""
          ) else
            error, ""
        in
        let () =
          Graph_loggers.print_edge logger
            ("Node_" ^ string_of_int key)
            ("Final_" ^ string_of_int key)
            ~directives:[ Graph_loggers_sig.Label rule_name ]
        in
        error, handler)
      (error, handler) transition_system.nodes_degradation
  in
  let () =
    Mods.IntMap.iter
      (fun key l ->
        if Mods.IntSet.is_empty l then
          ()
        else (
          let k = "Node_" ^ string_of_int key in
          let l =
            List.rev
              (Mods.IntSet.fold
                 (fun i list -> ("Node_" ^ string_of_int i) :: list)
                 l [])
          in
          Graph_loggers.print_one_to_n_relation logger
            ~style_one:Graph_loggers_sig.Dotted
            ~style_n:Graph_loggers_sig.Dashed k l
        ))
      transition_system.subframe
  in
  let () = Graph_loggers.print_graph_foot logger in
  error

let empty_bridge_set = Mods.IntMap.empty

let add_bridge (a, b, c) set =
  let b = Ckappa_sig.int_of_rule_id b in
  let old = Mods.IntMap.find_default [] b set in
  Mods.IntMap.add b ((a, c) :: old) set

let agent_trace parameters log_info error dead_rules handler static
    handler_kappa compil output =
  let transition_system_length = [] in
  let bridges = empty_bridge_set in
  let error, low = Graphs.Nodearray.create parameters error 1 in
  let error, pre = Graphs.Nodearray.create parameters error 1 in
  let error, on_stack = Graphs.Nodearray.create parameters error 1 in
  let error, scc = Graphs.Nodearray.create parameters error 1 in
  let () = Ckappa_sig.Views_intbdu.import_handler handler in
  let rules = compil.Cckappa_sig.rules in
  let init = compil.Cckappa_sig.init in
  let error, side_effects =
    smash_side_effect parameters error static dead_rules
  in
  let error, (support, creation, degradation) =
    build_support parameters error handler_kappa rules dead_rules
  in
  let error, init =
    Int_storage.Nearly_inf_Imperatif.fold parameters error
      (fun parameters error i_id init init_map ->
        let mixture = init.Cckappa_sig.e_init_c_mixture in
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
          parameters error
          (fun parameters error ag_id view init_map ->
            match view with
            | Cckappa_sig.Agent agent ->
              let error', old_list =
                Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs
                  parameters error [] agent.Cckappa_sig.agent_name init_map
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let error, asso =
                asso_of_view_in_map parameters error
                  agent.Cckappa_sig.agent_interface
              in
              let error, new_map =
                Ckappa_sig.Agent_map_and_set.Map.add_or_overwrite parameters
                  error agent.Cckappa_sig.agent_name
                  (((Init i_id, ag_id), asso) :: old_list)
                  init_map
              in
              error, new_map
            | Cckappa_sig.Ghost | Cckappa_sig.Dead_agent _
            | Cckappa_sig.Unknown_agent _ ->
              Exception.warn parameters error __POS__ Exit init_map)
          mixture.Cckappa_sig.views init_map)
      init creation
  in
  let empty = Ckappa_sig.Views_intbdu.build_variables_list [] in
  let error, (_, _, _, _, bridges, transition_system_length, log_info) =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
      error
      (fun parameters error agent_type map
           (pre, low, on_stack, scc, bridges, transition_system_length, log_info) ->
        let error, support =
          Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs parameters
            error LabelMap.empty agent_type support
        in
        let error, degradation =
          Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs parameters
            error LabelMap.empty agent_type degradation
        in
        let error', agent_string =
          try Handler.string_of_agent parameters error handler_kappa agent_type
          with _ ->
            Exception.warn parameters error __POS__ Exit
              (Ckappa_sig.string_of_agent_name agent_type)
        in
        let error =
          Exception.check_point Exception.warn parameters error error' __POS__
            Exit
        in
        Wrapped_modules.LoggedIntMap.fold
          (fun _ mvbdu
               ( error,
                 ( pre,
                   low,
                   on_stack,
                   scc,
                   bridges,
                   transition_system_length,
                   log_info ) ) ->
            try
              let sites =
                Ckappa_sig.Views_intbdu.variables_list_of_mvbdu mvbdu
              in
              let ext_list =
                Ckappa_sig.Views_intbdu.extensional_of_variables_list sites
              in
              let def_list =
                List.rev_map
                  (fun i -> i, Ckappa_sig.state_index_of_int 0)
                  ext_list
              in
              let mvbdu_default_value =
                mvbdu_of_reverse_order_association_list def_list
              in
              let error', site_set =
                List.fold_left
                  (fun (error, set) site ->
                    SiteSet.add parameters error site set)
                  (error, SiteSet.empty) ext_list
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let error, potential_side_effect =
                build_side_effect parameters error static agent_type site_set
                  side_effects
              in
              let error, support =
                LabelMap.fold
                  (fun label (test, asso_test, modif, asso_modif) (error, map) ->
                    let error, test =
                      SiteSet.inter parameters error test site_set
                    in
                    let asso_test =
                      Ckappa_sig.Views_intbdu.mvbdu_project_keep_only asso_test
                        sites
                    in
                    let error, modif =
                      SiteSet.inter parameters error modif site_set
                    in
                    if SiteSet.is_empty modif then
                      error, map
                    else (
                      let asso_modif =
                        Ckappa_sig.Views_intbdu.extensional_of_association_list
                          asso_modif
                      in
                      let asso_modif =
                        List.filter
                          (fun (a, _) -> SiteSet.mem a site_set)
                          asso_modif
                      in
                      let asso_modif =
                        Ckappa_sig.Views_intbdu.build_association_list
                          asso_modif
                      in
                      let error', map =
                        LabelMap.add parameters error label
                          (test, asso_test, modif, asso_modif)
                          map
                      in
                      let error =
                        Exception.check_point Exception.warn parameters error
                          error' __POS__ Exit
                      in
                      error, map
                    ))
                  support (error, LabelMap.empty)
              in
              let error, support =
                Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold parameters
                  error
                  (fun parameters error r_id list support ->
                    let error, support, _ =
                      List.fold_left
                        (fun (error, support, id) list ->
                          let error', test =
                            List.fold_left
                              (fun (error, set) (a, _) ->
                                SiteSet.add parameters error a set)
                              (error, SiteSet.empty) list
                          in
                          let error =
                            Exception.check_point Exception.warn parameters
                              error error' __POS__ Exit
                          in
                          let modif = test in
                          let asso_test =
                            Ckappa_sig.Views_intbdu.build_association_list list
                          in
                          let asso_test =
                            Ckappa_sig.Views_intbdu.mvbdu_redefine
                              (Ckappa_sig.Views_intbdu.mvbdu_true ())
                              asso_test
                          in
                          let asso_modif =
                            List.rev_map
                              (fun (a, _) -> a, Ckappa_sig.dummy_state_index)
                              (List.rev list)
                          in
                          let asso_modif =
                            Ckappa_sig.Views_intbdu.build_association_list
                              asso_modif
                          in
                          let label =
                            Rule r_id, Ckappa_sig.agent_id_of_int (-1), id
                          in
                          let error', support =
                            LabelMap.add parameters error label
                              (test, asso_test, modif, asso_modif)
                              support
                          in
                          let error =
                            Exception.check_point Exception.warn parameters
                              error error' __POS__ Exit
                          in
                          error, support, id + 1)
                        (error, support, 0) list
                    in
                    error, support)
                  potential_side_effect support
              in
              let error, concurrent_sites =
                concurrent_sites parameters error mvbdu support
              in
              let error, concurrent_sites =
                merge_neighbour parameters error concurrent_sites
              in
              let error, file_name =
                List.fold_left
                  (fun (error, string) site ->
                    let error, site_string =
                      Handler.string_of_site_in_file_name parameters error
                        handler_kappa agent_type site
                    in
                    error, string ^ "." ^ site_string)
                  ( error,
                    Remanent_parameters.get_local_trace_directory parameters
                    ^ Remanent_parameters.get_local_trace_prefix parameters
                    ^ agent_string )
                  ext_list
              in
              let transition_system =
                empty_transition_system file_name agent_string agent_type
              in
              let fic =
                if Remanent_parameters.get_compute_local_traces parameters then
                  Remanent_parameters.open_out file_name
                    (Remanent_parameters.ext_format
                       (Remanent_parameters.get_local_trace_format parameters))
                else (
                  match
                    Loggers.channel_of_logger
                      (Remanent_parameters.get_logger parameters)
                  with
                  | Some channel -> channel
                  | None -> stdout
                )
              in
              let error', init_list =
                Ckappa_sig.Agent_map_and_set.Map.find_default_without_logs
                  parameters error [] agent_type init
              in
              let error =
                Exception.check_point Exception.warn parameters error error'
                  __POS__ Exit
              in
              let error, transition_system =
                (* initial/creation states *)
                List.fold_left
                  (fun (error, transition_system) ((i_id, ag_id), asso) ->
                    let update = restrict_asso asso site_set in
                    let mvbdu =
                      Ckappa_sig.Views_intbdu.mvbdu_redefine mvbdu_default_value
                        update
                    in
                    let error, transition_system =
                      add_creation parameters error i_id ag_id mvbdu
                        transition_system
                    in
                    error, transition_system)
                  (error, transition_system) init_list
              in
              let error, transition_system =
                (* regular transitions *)
                LabelMap.fold
                  (fun label (_test, asso, modif, asso_modif)
                       (error, transition_system) ->
                    if SiteSet.is_empty modif then
                      error, transition_system
                    else (
                      let error', concurrent_site =
                        LabelMap.find_default parameters error
                          (SiteSet.empty, empty) label concurrent_sites
                      in
                      let error =
                        Exception.check_point Exception.warn parameters error
                          error' __POS__ Exit
                      in
                      let mvbdu =
                        Ckappa_sig.Views_intbdu.mvbdu_project_abstract_away
                          mvbdu (snd concurrent_site)
                      in
                      let mvbdu =
                        Ckappa_sig.Views_intbdu.mvbdu_and mvbdu asso
                      in
                      let pre =
                        Ckappa_sig.Views_intbdu.extensional_of_mvbdu mvbdu
                      in
                      let error, transition_system =
                        List.fold_left
                          (fun (error, transition_system) list ->
                            let pre = mvbdu_of_association_list list in
                            let post =
                              Ckappa_sig.Views_intbdu.mvbdu_redefine pre
                                asso_modif
                            in
                            let error, transition_system =
                              add_node parameters error pre transition_system
                            in
                            let error, transition_system =
                              add_node parameters error post transition_system
                            in

                            let transition_system =
                              add_edge pre post label transition_system
                            in
                            error, transition_system)
                          (error, transition_system) pre
                      in
                      error, transition_system
                    ))
                  support (error, transition_system)
              in
              (* degradation transitions *)
              let transition_system =
                LabelMap.fold
                  (fun label asso transition_system ->
                    List.fold_left
                      (fun transition_system mvbdu ->
                        if
                          Ckappa_sig.Views_intbdu.equal
                            (Ckappa_sig.Views_intbdu.mvbdu_false ())
                            (Ckappa_sig.Views_intbdu.mvbdu_and mvbdu asso)
                        then
                          transition_system
                        else
                          {
                            transition_system with
                            nodes_degradation =
                              (mvbdu, label)
                              :: transition_system.nodes_degradation;
                          })
                      transition_system transition_system.nodes)
                  degradation transition_system
              in
              let transition_system_length =
                List.length transition_system.edges :: transition_system_length
              in
              let error, transition_system =
                if
                  Remanent_parameters.get_add_singular_macrostates parameters
                  || Remanent_parameters.get_add_singular_microstates parameters
                then
                  add_singular parameters error transition_system
                else
                  error, transition_system
              in
              let error, transition_system =
                check_all_subframes parameters error transition_system
              in
              let transition_system = reduce_subframes transition_system in
              let error, log_info =
                if Remanent_parameters.get_compute_local_traces parameters then (
                  let format =
                    match
                      Remanent_parameters.get_local_trace_format parameters
                    with
                    | Remanent_parameters_sig.GEPHI -> Loggers.GEPHI
                    | Remanent_parameters_sig.DIM -> Loggers.Matrix
                    | Remanent_parameters_sig.DOT -> Loggers.DOT
                    | Remanent_parameters_sig.HTML -> Loggers.HTML_Graph
                  in
                  let logger =
                    Loggers.open_logger_from_channel fic ~mode:format
                  in
                  let logger = Graph_loggers_sig.extend_logger logger in
                  let error =
                    print logger parameters compil handler_kappa () error
                      transition_system
                  in
                  let _ = close_out fic in
                  error, log_info
                ) else
                  error, log_info
              in
              let error, nodes, node_labels =
                List.fold_left
                  (fun (error, nodes, node_labels) mvbdu ->
                    let error, node = hash_of_mvbdu parameters error mvbdu in
                    let error, list =
                      build_asso_of_mvbdu parameters error mvbdu
                    in
                    let error, label =
                      string_label_of_asso parameters error handler_kappa
                        transition_system list
                    in
                    let node = Graphs.node_of_int node in
                    ( error,
                      node :: nodes,
                      Graphs.NodeMap.add node label node_labels ))
                  (error, [], Graphs.NodeMap.empty)
                  transition_system.nodes
              in
              let node_label node =
                Graphs.NodeMap.find_default "" node node_labels
              in
              let error, edges =
                List.fold_left
                  (fun (error, edges) (mvbdu1, label, mvbdu2) ->
                    let error, node1 = hash_of_mvbdu parameters error mvbdu1 in
                    let error, node2 = hash_of_mvbdu parameters error mvbdu2 in
                    match label with
                    | Init _, _, _ -> error, edges
                    | Rule id, _, _ ->
                      ( error,
                        (Graphs.node_of_int node1, id, Graphs.node_of_int node2)
                        :: edges ))
                  (error, []) transition_system.edges
              in
              let edges =
                Mods.IntMap.fold
                  (fun _ l edges ->
                    if Mods.IntSet.is_empty l then
                      edges
                    else (
                      let first_last, edges =
                        Mods.IntSet.fold
                          (fun next (first_last, edges) ->
                            let next = Graphs.node_of_int next in
                            match first_last with
                            | None -> Some (next, next), edges
                            | Some (first, last) ->
                              ( Some (first, next),
                                (last, Ckappa_sig.dummy_rule_id, next) :: edges
                              ))
                          l (None, edges)
                      in
                      match first_last with
                      | None -> edges
                      | Some (x, y) when x = y -> edges
                      | Some (first, last) ->
                        (last, Ckappa_sig.dummy_rule_id, first) :: edges
                    ))
                  transition_system.subframe edges
              in
              let error, graph =
                Graphs.create parameters error node_label nodes edges
              in
              let error, pre, low, on_stack, scc, bridges =
                Graphs.add_bridges ~low ~pre ~on_stack ~scc add_bridge
                  parameters error
                  (fun n -> n)
                  (fun e -> Ckappa_sig.string_of_rule_id e)
                  graph bridges
              in
              ( error,
                ( pre,
                  low,
                  on_stack,
                  scc,
                  bridges,
                  transition_system_length,
                  log_info ) )
            with Sys.Break ->
              ( error,
                ( pre,
                  low,
                  on_stack,
                  scc,
                  bridges,
                  transition_system_length,
                  log_info ) ))
          map
          ( error,
            ( pre,
              low,
              on_stack,
              scc,
              bridges,
              transition_system_length,
              log_info ) ))
      output
      (pre, low, on_stack, scc, bridges, transition_system_length, log_info)
  in
  let bridges =
    if Remanent_parameters.get_compute_separating_transitions parameters then
      Some bridges
    else
      None
  in
  let transition_system_length = Some transition_system_length in
  match Ckappa_sig.Views_intbdu.export_handler error with
  | error, Some h -> error, log_info, h, bridges, transition_system_length
  | error, None ->
    let error, h = Exception.warn parameters error __POS__ Exit handler in
    error, log_info, h, bridges, transition_system_length
