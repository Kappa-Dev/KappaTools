(**
   * communication.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 22th of February
   * Last modification: Time-stamp: <Aug 17 2018>
   *
   * Abstract domain to record live rules
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

type path_defined_in =
  | LHS of (Ckappa_sig.c_rule_id * Cckappa_sig.enriched_rule)
  | RHS of (Ckappa_sig.c_rule_id * Cckappa_sig.enriched_rule)
  | Pattern

type event =
  | Dummy (* to avoid compilation warning *)
  | Check_rule of Ckappa_sig.c_rule_id
  | See_a_new_bond of
      (*in contact map*)
      ((Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
      * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state))
  (*in the domain of parallel and site accross*)
  | Modified_sites of (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name)

(*For example:
  E_1(x!1),R_2(x!1,y!2),R_3(y!2,x) -> E_1(x!1),R_2(x!1,y),R_3(y,x)

  - Agent_id: 1 (E)
  - relative_addres:
     [{
       site_out : x
       agent_type_in: R2
       site_in : x
      };
      {
        site_out: y
        agent_type_in: R3
        site_in : y
      }
      ]
  - site : y (denoted the site y of R3)

  Go to agent_id 1:
  Exit current agent E by site x, then enter an agent of type R2 via a site x.
  Exit current agent R2 by site y, then enter an agent of type R3 via a site y.
*)

type step = {
  site_out: Ckappa_sig.c_site_name;
  site_in: Ckappa_sig.c_site_name;
  agent_type_in: Ckappa_sig.c_agent_name;
}

type path = {
  agent_id: Ckappa_sig.c_agent_id;
  relative_address: step list;
  site: Ckappa_sig.c_site_name;
}

type path_in_pattern = { defined_in: path_defined_in; path: path }

let get_defined_in p = p.defined_in
let get_agent_id p = p.path.agent_id
let get_site p = p.path.site
let get_relative_address p = p.path.relative_address

module type PathMap = sig
  type 'a t

  val empty : 'a -> 'a t
  val add : path -> 'a -> 'a t -> 'a t
  val find : path -> 'a t -> 'a option
end

module PathSetMap = SetMap.Make (struct
  type t = path

  let compare = compare
  let print _ _ = ()
end)

module PathMap : PathMap = struct
  include PathSetMap.Map

  let empty _ = empty
  let find = find_option
end

type 'a fold =
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Ckappa_sig.c_agent_name ->
  Ckappa_sig.c_site_name ->
  Exception.method_handler
  * ((Remanent_parameters_sig.parameters ->
     Ckappa_sig.c_state ->
     Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state ->
     Exception.method_handler * 'a ->
     Exception.method_handler * 'a) ->
    Exception.method_handler ->
    'a ->
    Exception.method_handler * 'a)
    Usual_domains.flat_lattice

type prefold = { fold: 'a. 'a fold }

(*precondition is used to get the information between domains*)
type precondition = {
  precondition_dummy: unit; (* to avoid compilation warning *)
  the_rule_is_applied_for_the_first_time: Usual_domains.maybe_bool;
  state_of_sites_in_precondition:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Analyzer_headers.global_dynamic_information ->
    path ->
    Exception.method_handler
    * Analyzer_headers.global_dynamic_information
    * Ckappa_sig.c_state list Usual_domains.flat_lattice;
  cache_state_of_sites:
    Ckappa_sig.c_state list Usual_domains.flat_lattice PathMap.t;
  partner_map:
    Exception.method_handler ->
    Ckappa_sig.c_agent_name ->
    Ckappa_sig.c_site_name ->
    Ckappa_sig.c_state ->
    Exception.method_handler
    * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
      Usual_domains.flat_lattice;
  partner_fold: 'a. 'a fold;
}

let is_the_rule_applied_for_the_first_time precondition =
  precondition.the_rule_is_applied_for_the_first_time

let the_rule_is_or_not_applied_for_the_first_time bool parameter error
    precondition =
  match precondition.the_rule_is_applied_for_the_first_time with
  | Usual_domains.Maybe ->
    ( error,
      {
        precondition with
        the_rule_is_applied_for_the_first_time = Usual_domains.Sure_value bool;
      } )
  | Usual_domains.Sure_value b when b = bool -> error, precondition
  | Usual_domains.Sure_value _ ->
    Exception.warn parameter error __POS__
      ~message:"inconsistent computation in three-value logic" Exit precondition

let the_rule_is_applied_for_the_first_time p e wp =
  the_rule_is_or_not_applied_for_the_first_time true p e wp

let the_rule_is_not_applied_for_the_first_time p e wp =
  the_rule_is_or_not_applied_for_the_first_time false p e wp

let dummy_precondition =
  {
    precondition_dummy = ();
    the_rule_is_applied_for_the_first_time = Usual_domains.Maybe;
    state_of_sites_in_precondition =
      (fun _ error dynamic _ -> error, dynamic, Usual_domains.Any);
    cache_state_of_sites = PathMap.empty Usual_domains.Any;
    partner_map = (fun error _ _ _ -> error, Usual_domains.Any);
    partner_fold = (fun _ error _ _ -> error, Usual_domains.Any);
  }

let get_potential_partner precondition error agent_type site state =
  let error, rep = precondition.partner_map error agent_type site state in
  error, precondition, rep

let fold_over_potential_partners parameters error precondition agent_type site f
    init =
  match precondition.partner_fold parameters error agent_type site with
  | error, Usual_domains.Any -> error, precondition, Usual_domains.Top
  | error, Usual_domains.Undefined ->
    (* In theory, this could happen, but it would be worth being warned
       about it *)
    let error, () =
      Exception.warn parameters error __POS__ ~message:"bottom propagation" Exit
        ()
    in
    error, precondition, Usual_domains.Not_top init
  | error, Usual_domains.Val v ->
    let error, output = v f error init in
    error, precondition, Usual_domains.Not_top output

let overwrite_potential_partners_map
    (_parameters : Remanent_parameters_sig.parameters)
    (error : Exception.method_handler) precondition f (fold : prefold) =
  ( error,
    {
      precondition with
      partner_map = f;
      partner_fold =
        (fun parameters error agent_type site_type ->
          fold.fold parameters error agent_type site_type);
    } )

type output =
  | Cannot_exist
  | May_exist of path
  | Located of Ckappa_sig.c_agent_id

let last_agent_type parameters error rule path =
  match List.rev path.relative_address with
  | [] ->
    let agent =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
        parameters error path.agent_id (*A*)
        rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
    in
    (match agent with
    | error, None ->
      Exception.warn parameters error __POS__ Exit Ckappa_sig.dummy_agent_name
    | error, Some agent ->
      (match agent with
      | Cckappa_sig.Ghost | Cckappa_sig.Dead_agent _
      | Cckappa_sig.Unknown_agent _ ->
        Exception.warn parameters error __POS__ Exit Ckappa_sig.dummy_agent_name
      | Cckappa_sig.Agent proper_agent ->
        error, proper_agent.Cckappa_sig.agent_name))
  | head :: _ -> error, head.agent_type_in

let may_be_modified parameters error rule path =
  let error, agent_name = last_agent_type parameters error rule path in
  let site_name = path.site in
  let modif = rule.Cckappa_sig.diff_direct in
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error _ agent list ->
      if agent.Cckappa_sig.agent_name = agent_name then (
        match
          Ckappa_sig.Site_map_and_set.Map.find_option_without_logs parameters
            error site_name agent.Cckappa_sig.agent_interface
        with
        | error, None -> error, list
        | error, Some interval ->
          let range = interval.Cckappa_sig.site_state in
          if range.Cckappa_sig.min = range.Cckappa_sig.max then (
            match range.Cckappa_sig.min with
            | None -> Exception.warn parameters error __POS__ Exit list
            | Some a -> error, a :: list
          ) else
            Exception.warn parameters error __POS__ Exit list
      ) else
        error, list)
    modif []

let may_get_free_by_side_effect parameters error static _precondition rule_id
    rule path =
  let error, agent_name = last_agent_type parameters error rule path in
  let error, list =
    Ckappa_sig.AgentRule_map_and_set.Map.find_default_without_logs parameters
      error [] (agent_name, rule_id)
      (Analyzer_headers.get_potential_side_effects static)
  in
  let site_name = path.site in
  (* TO DO BETTER *)
  error, List.exists (fun (_, (a, _)) -> a = site_name) list

let min_not_free interv =
  match interv.Cckappa_sig.min with
  | None -> Ckappa_sig.dummy_state_index_1
  | Some min ->
    if min = Ckappa_sig.dummy_state_index then
      Ckappa_sig.dummy_state_index_1
    else
      min

let check_state_compatibility parameters error kappa_handler cc agent_id_source
    site_source agent_target site_target =
  match
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
      parameters error agent_id_source (*A*) cc.Cckappa_sig.views
  with
  | error, Some Cckappa_sig.Ghost -> error, true
  | error, None
  | error, Some (Cckappa_sig.Dead_agent _ | Cckappa_sig.Unknown_agent _) ->
    Exception.warn parameters error __POS__ ~message:"null pointer" Exit false
  | error, Some (Cckappa_sig.Agent ag) ->
    let agent_source = ag.Cckappa_sig.agent_name in
    let error, interval =
      Ckappa_sig.Site_map_and_set.Map.find_option_without_logs parameters error
        site_source ag.Cckappa_sig.agent_interface
    in
    (match interval with
    | None -> error, true
    | Some i
      when i.Cckappa_sig.site_state.Cckappa_sig.min = None
           || i.Cckappa_sig.site_state.Cckappa_sig.max = None ->
      Exception.warn parameters error __POS__ Exit true
    | Some i ->
      let interv = i.Cckappa_sig.site_state in
      (match interv.Cckappa_sig.min, interv.Cckappa_sig.max with
      | None, _ | _, None -> Exception.warn parameters error __POS__ Exit true
      | Some _, Some max ->
        let rec aux error k =
          if Ckappa_sig.compare_state_index k max > 0 then
            error, false
          else (
            let error, opt =
              Handler.dual parameters error kappa_handler agent_source
                site_source k
            in
            match opt with
            | Some (agent_target', site_target', _)
              when agent_target' = agent_target && site_target' = site_target ->
              error, true
            | Some _ -> aux error (Ckappa_sig.next_state_index k)
            | None ->
              let error, () = Exception.warn parameters error __POS__ Exit () in
              aux error (Ckappa_sig.next_state_index k)
          )
        in
        aux error (min_not_free interv)))

let rec follow_path_inside_cc parameters error kappa_handler cc path =
  match path.relative_address with
  | [] -> error, Located path.agent_id
  | head :: tail ->
    (match
       Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
         parameters error path.agent_id (*A*) cc.Cckappa_sig.bonds
     with
    | error, None ->
      let error, bool =
        check_state_compatibility parameters error kappa_handler cc
          path.agent_id head.site_out head.agent_type_in head.site_in
      in
      if bool then
        error, May_exist path
      else
        error, Cannot_exist
    | error, Some map ->
      (match
         Ckappa_sig.Site_map_and_set.Map.find_option_without_logs parameters
           error head.site_out (*A.x*) map
       with
      | error, None ->
        let error, bool =
          check_state_compatibility parameters error kappa_handler cc
            path.agent_id head.site_out head.agent_type_in head.site_in
        in
        if bool then
          error, May_exist path
        else
          error, Cannot_exist
      | error, Some site_add ->
        (*-----------------------------------------------*)
        (*A.x is bound to something*)
        let agent_type' = site_add.Cckappa_sig.agent_type in
        let site_type' = site_add.Cckappa_sig.site in
        (*check that A.x is bound to B.y*)
        if agent_type' = head.agent_type_in && site_type' = head.site_in then
          (*recursively apply to #i tail*)
          follow_path_inside_cc parameters error kappa_handler cc
            {
              path with
              agent_id = site_add.Cckappa_sig.agent_index;
              relative_address = tail;
            }
        else
          error, Cannot_exist))

let rec post_condition error rule_id r precondition static dynamic path =
  let parameters = Analyzer_headers.get_parameter static in
  let kappa_handler = Analyzer_headers.get_kappa_handler static in
  let rule = r.Cckappa_sig.e_rule_c_rule in
  let cc = rule.Cckappa_sig.rule_rhs in
  (*---------------------------------------------------------*)
  (*inside the pattern, check the binding information in the lhs of the current agent*)
  let error, (potential_values, continuation_opt) =
    match follow_path_inside_cc parameters error kappa_handler cc path with
    | error, Cannot_exist -> error, (Usual_domains.Undefined, None)
    | error, May_exist continuation ->
      error, (Usual_domains.Any, Some continuation)
    | error, Located agent_id ->
      let agent =
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
          parameters error agent_id (*A*)
          cc.Cckappa_sig.views
      in
      (match agent with
      | error, None ->
        Exception.warn parameters error __POS__ Exit
          (Usual_domains.Undefined, None)
      | error, Some agent ->
        (match agent with
        | Cckappa_sig.Ghost | Cckappa_sig.Dead_agent _
        | Cckappa_sig.Unknown_agent _ ->
          Exception.warn parameters error __POS__ Exit
            ~message:(string_of_int (Ckappa_sig.int_of_agent_id agent_id))
            (Usual_domains.Undefined, None)
        | Cckappa_sig.Agent proper_agent ->
          let error, state_opt =
            Ckappa_sig.Site_map_and_set.Map.find_option_without_logs parameters
              error path.site proper_agent.Cckappa_sig.agent_interface
          in
          (match state_opt with
          | None ->
            if
              List.exists
                (fun (a, _) -> a = agent_id)
                rule.Cckappa_sig.actions.Cckappa_sig.creation
            then
              (* the agent has been created and the site not specified *)
              (* we know that its state is 0 *)
              error, (Usual_domains.Val [ Ckappa_sig.dummy_state_index ], None)
            else
              ( error,
                ( Usual_domains.Any,
                  Some { path with agent_id; relative_address = [] } ) )
          | Some interval ->
            let interval = interval.Cckappa_sig.site_state in
            let error, min =
              match interval.Cckappa_sig.min with
              | Some a -> error, a
              | None ->
                Exception.warn parameters error __POS__ Exit
                  Ckappa_sig.dummy_state_index
            in
            let error, max =
              match interval.Cckappa_sig.max with
              | Some a -> error, a
              | None ->
                Exception.warn parameters error __POS__ Exit
                  Ckappa_sig.dummy_state_index
            in
            let list =
              let rec aux k output =
                if Ckappa_sig.compare_state_index k min < 0 then
                  output
                else
                  aux (Ckappa_sig.pred_state_index k) (k :: output)
              in
              aux max []
            in
            error, (Usual_domains.Val list, None))))
  in
  match potential_values with
  | Usual_domains.Undefined | Usual_domains.Val _ ->
    error, dynamic, potential_values
  | Usual_domains.Any ->
    let error, path =
      match continuation_opt with
      | None -> Exception.warn parameters error __POS__ Exit path
      | Some path -> error, path
    in
    if
      List.exists
        (fun (a, _) -> a = path.agent_id)
        rule.Cckappa_sig.actions.Cckappa_sig.creation
    then
      (* try to exit from a site that is unspecified in a created agent *)
      (* this site is free, thus the path is not realisable *)
      error, dynamic, Usual_domains.Undefined
    else (
      let path = { defined_in = LHS (rule_id, r); path } in
      let error, dynamic, precondition, values =
        get_state_of_site error precondition static dynamic path
      in
      let error, bool =
        may_get_free_by_side_effect parameters error static precondition rule_id
          rule path.path
      in
      let error, list = may_be_modified parameters error rule path.path in

      match values with
      | Usual_domains.Val l ->
        if bool || list <> [] then (
          let l_side =
            if bool then
              Ckappa_sig.state_index_of_int 0 :: l
            else
              l
          in
          let l_all =
            List_util.remove_consecutive_double
              (List.sort Ckappa_sig.compare_state_index
                 (List.rev_append list l_side))
          in
          error, dynamic, Usual_domains.Val l_all
        ) else
          error, dynamic, values
      | Usual_domains.Undefined | Usual_domains.Any -> error, dynamic, values
    )

and get_state_of_site error precondition static dynamic path =
  let parameters = Analyzer_headers.get_parameter static in
  match path.defined_in with
  | LHS _ | Pattern ->
    let error, dynamic, range =
      precondition.state_of_sites_in_precondition parameters error dynamic
        path.path
    in
    error, dynamic, precondition, range
  | RHS (rule_id, rule) ->
    let error, dynamic, range =
      post_condition error rule_id rule precondition static dynamic path.path
    in
    error, dynamic, precondition, range

let refine_information_about_state_of_sites_in_precondition precondition f =
  let new_f parameter error dynamic path =
    let error, dynamic, old_output =
      precondition.state_of_sites_in_precondition parameter error dynamic path
    in
    f parameter error dynamic path old_output
  in
  {
    precondition with
    cache_state_of_sites = PathMap.empty Usual_domains.Any;
    state_of_sites_in_precondition = new_f;
  }

let get_state_of_site_in_pre_post_condition get_global_static_information
    get_global_dynamic_information set_global_dynamic_information error static
    dynamic agent_id site_type defined_in precondition =
  let static = get_global_static_information static in
  let parameter = Analyzer_headers.get_parameter static in
  let path_in_pattern = { agent_id; relative_address = []; site = site_type } in
  let path = { defined_in; path = path_in_pattern } in
  (*get a list of site_type2 state in the precondition*)
  let error, global_dynamic, precondition, state_list_lattice =
    get_state_of_site error precondition static
      (get_global_dynamic_information dynamic)
      path
  in
  let error, state_list =
    match state_list_lattice with
    | Usual_domains.Val l -> error, l
    | Usual_domains.Undefined -> Exception.warn parameter error __POS__ Exit []
    | Usual_domains.Any ->
      let error, () =
        if Remanent_parameters.get_view_analysis parameter then
          Exception.warn parameter error __POS__ Exit ()
        else
          error, ()
      in
      let error, l =
        match defined_in with
        | RHS (_, r) | LHS (_, r) ->
          let rule = r.Cckappa_sig.e_rule_c_rule in
          let error, agent_type =
            last_agent_type parameter error rule path_in_pattern
          in
          let error, l =
            Handler.state_list parameter
              (Analyzer_headers.get_kappa_handler static)
              error agent_type site_type
          in
          if l = [] then
            Exception.warn parameter error __POS__ Exit []
          else
            error, l
        | Pattern -> Exception.warn parameter error __POS__ Exit []
      in
      error, l
  in
  let dynamic = set_global_dynamic_information global_dynamic dynamic in
  error, dynamic, precondition, state_list

(*use this function before apply a rule, like in is_enabled*)

let get_state_of_site_in_precondition get_global_dynamic_information
    set_global_dynamic_information parameter error kappa_handler dynamic rule
    agent_id site_type precondition =
  let defined_in = LHS rule in
  get_state_of_site_in_pre_post_condition get_global_dynamic_information
    set_global_dynamic_information parameter error kappa_handler dynamic
    agent_id site_type defined_in precondition

let get_state_of_site_in_postcondition get_global_dynamic_information
    set_global_dynamic_information parameter error kappa_handler dynamic rule
    agent_id site_type precondition =
  let defined_in = RHS rule in
  get_state_of_site_in_pre_post_condition get_global_dynamic_information
    set_global_dynamic_information parameter error kappa_handler dynamic
    agent_id site_type defined_in precondition

let add_rule ?(local_trace = false) parameters compiled _kappa_handler error
    rule_id event_list =
  let error =
    if
      local_trace
      || Remanent_parameters.get_dump_reachability_analysis_wl parameters
    then (
      let error, rule_id_string =
        try Handler.string_of_rule parameters error compiled rule_id
        with _ ->
          Exception.warn parameters error __POS__ Exit
            (Ckappa_sig.string_of_rule_id rule_id)
      in
      let tab = "\t\t\t" in
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "%s%s(%s) should be investigated "
          (Remanent_parameters.get_prefix parameters)
          tab rule_id_string
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      error
    ) else
      error
  in
  error, Check_rule rule_id :: event_list

type site_working_list =
  unit
  Ckappa_sig
  .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
  .t

let init_sites_working_list parameters error =
  Ckappa_sig
  .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
  .create parameters error (0, 0)

let clear_sites_working_list parameters error sites_wl =
  Ckappa_sig
  .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
  .free_all parameters error sites_wl

let add_site parameters error agent site sites_wl =
  match
    Ckappa_sig
    .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
    .unsafe_get parameters error (agent, site) sites_wl
  with
  | error, None ->
    Ckappa_sig
    .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
    .set parameters error (agent, site) () sites_wl
  | error, Some _ -> error, sites_wl

let fold_sites p e f (acc : site_working_list) =
  Ckappa_sig
  .Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
  .fold p e f acc
