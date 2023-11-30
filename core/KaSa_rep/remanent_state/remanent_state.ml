(**
  * remanent_state.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: June, the 25th of 2016
  * Last modification: Time-stamp: <Mar 18 2020>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

(**********************)
(* compilation result *)
(**********************)

type compilation = Ast.parsing_compil
type init = Compil of compilation | Files of string list
type initial_state = (Primitives.alg_expr * Primitives.elementary_rule) list

type refined_compilation =
  ( Ckappa_sig.agent,
    Ckappa_sig.mixture,
    Ckappa_sig.mixture,
    string,
    Ckappa_sig.mixture Ckappa_sig.rule )
  Ast.compil

type quark_map = Quark_type.quarks
type rule_id = int
type var_id = int

(**************)
(* JSon labels*)
(**************)

let accuracy_string = "accuracy"
let site = "site name"
let stateslist = "states list"
let interface = "interface"
let contactmap = "contact map"
let dead_rules = "dead rules"
let contactmaps = "contact maps"
let influencemaps = "influence maps"
let separating_transitions = "separating transitions"
let errors = "errors"
let scc_contact_maps = "scc contact maps"
let scc_contact_map = "scc contact map"
let accuracy_scc = "accuracy scc"
let scc = "scc"

type dead_rules = Public_data.dead_rules

let info_to_rule (s1, loc, direction, s2, id) =
  {
    Public_data.rule_id = Ckappa_sig.int_of_rule_id id;
    Public_data.rule_position = loc;
    Public_data.rule_label = s1;
    Public_data.rule_ast = s2;
    Public_data.rule_direction = direction;
    Public_data.rule_hidden = false;
  }

type dead_agents = Public_data.dead_agents
type separating_transitions = Public_data.separating_transitions

(******************************************************************************)
(*********************)
(* refinement lemmas *)
(*********************)

type interface =
  (string option (* internal state *)
  * Site_graphs.KaSa_site_graph.binding_state option (*binding state*)
  * (int option * int option) option (*counter state*))
  Wrapped_modules.LoggedStringMap.t

let interface_to_json =
  Wrapped_modules.LoggedStringMap.to_json ~lab_key:site ~lab_value:stateslist
    JsonUtil.of_string (fun (internal_opt, binding_opt, counter_opt) ->
      JsonUtil.of_triple ~lab1:Public_data.prop ~lab2:Public_data.bind
        ~lab3:Public_data.counter
        (fun internal_opt ->
          JsonUtil.of_option
            (fun internal_state -> JsonUtil.of_string internal_state)
            internal_opt)
        (JsonUtil.of_option Site_graphs.KaSa_site_graph.binding_state_to_json)
        (fun counter_opt ->
          JsonUtil.of_option
            (JsonUtil.of_pair ~lab1:Public_data.inf ~lab2:Public_data.sup
               (JsonUtil.of_option JsonUtil.of_int)
               (JsonUtil.of_option JsonUtil.of_int))
            counter_opt)
        (internal_opt, binding_opt, counter_opt))

let interface_of_json =
  Wrapped_modules.LoggedStringMap.of_json ~lab_key:site ~lab_value:stateslist
    ~error_msg:interface (*json -> elt*)
    (fun json -> JsonUtil.to_string ~error_msg:site json) (*json -> 'value*)
    (JsonUtil.to_triple ~lab1:Public_data.prop ~lab2:Public_data.bind
       ~lab3:Public_data.counter ~error_msg:"wrong binding state"
       (JsonUtil.to_option (JsonUtil.to_string ~error_msg:Public_data.prop))
       (JsonUtil.to_option Site_graphs.KaSa_site_graph.binding_state_of_json)
       (JsonUtil.to_option
          (JsonUtil.to_pair ~lab1:Public_data.inf ~lab2:Public_data.sup
             ~error_msg:"wrong counter state"
             (JsonUtil.to_option
                (JsonUtil.to_int ~error_msg:"wrong counter bound"))
             (JsonUtil.to_option
                (JsonUtil.to_int ~error_msg:"wrong counter bound")))))

type agent = string * (* agent name *)
                      interface

(***************************************************************************)

type constraints_list = agent list Public_data.poly_constraints_list

let lemmas_list_of_json json =
  Public_data.lemmas_list_of_json_gen interface_of_json json

let lemmas_list_to_json l =
  Public_data.lemmas_list_to_json_gen interface_to_json l

(******************************************************************************)
(******************************************************************************)

(****************************)
(* Internal representations *)
(****************************)

type internal_influence_map =
  Ckappa_sig.c_rule_id list
  * Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t
  * Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t

type internal_contact_map =
  (Ckappa_sig.c_state list
  * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name) list)
  Ckappa_sig.Site_map_and_set.Map.t
  Ckappa_sig.Agent_map_and_set.Map.t

type internal_scc_decomposition =
  ((Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name)
  * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name))
  list
  list

type ('static, 'dynamic) reachability_result = 'static * 'dynamic
type subviews_info = unit

type flow =
  Ckappa_sig.Site_union_find.t
  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t

type internal_constraints_list =
  Site_graphs.KaSa_site_graph.t Public_data.poly_constraints_list

(*******************************************************************)
type symmetric_sites = Symmetries.symmetries option
(*******************************************************************)

type influence_edge = Quark_type.Labels.label_set_couple

type bidirectional_influence_map = {
  positive_influence_fwd: (Ckappa_sig.c_rule_id * influence_edge) list array;
  positive_influence_bwd: (Ckappa_sig.c_rule_id * influence_edge) list array;
  negative_influence_fwd: (Ckappa_sig.c_rule_id * influence_edge) list array;
  negative_influence_bwd: (Ckappa_sig.c_rule_id * influence_edge) list array;
}

type distance = { fwd: int; bwd: int; total: int }

type local_influence_map_blackboard = {
  blackboard_distance: distance option array;
  blackboard_is_done: bool array;
  blackboard_to_be_explored: bool array;
}

type ('static, 'dynamic) state = {
  parameters: Remanent_parameters_sig.parameters;
  log_info: StoryProfiling.StoryStats.log_info;
  handler: Cckappa_sig.kappa_handler option;
  init: init;
  env: Model.t option option;
  contact_map_int: Contact_map.t option option;
  init_state: initial_state option;
  compilation: compilation option;
  refined_compilation: refined_compilation option;
  c_compil: Cckappa_sig.compil option;
  quark_map: quark_map option;
  pos_of_rules_and_vars: Public_data.pos_of_rules_and_vars option;
  internal_influence_map: internal_influence_map Public_data.AccuracyMap.t;
  influence_map: Public_data.influence_map Public_data.AccuracyMap.t;
  bidirectional_influence_map:
    bidirectional_influence_map Public_data.AccuracyMap.t;
  local_influence_map_blackboard: local_influence_map_blackboard option;
  internal_contact_map: internal_contact_map Public_data.AccuracyMap.t;
  contact_map: Public_data.contact_map Public_data.AccuracyMap.t;
  internal_scc_decomposition:
    internal_scc_decomposition Public_data.AccuracyMap.t
    Public_data.AccuracyMap.t;
  scc_decomposition:
    Public_data.scc Public_data.AccuracyMap.t Public_data.AccuracyMap.t;
  signature: Signature.s option;
  bdu_handler: Mvbdu_wrapper.Mvbdu.handler;
  reachability_state: ('static, 'dynamic) reachability_result option;
  subviews_info: subviews_info option;
  dead_rules: dead_rules option;
  dead_agents: dead_agents option;
  ode_flow: Ode_fragmentation_type.ode_frag option;
  ctmc_flow: flow option;
  errors: Exception.method_handler;
  internal_constraints_list: internal_constraints_list option;
  constraints_list: constraints_list option;
  symmetric_sites: symmetric_sites Public_data.AccuracyMap.t;
  separating_transitions: separating_transitions option;
  transition_system_length: int list option;
}

let get_data state =
  ( state.handler,
    state.dead_rules,
    state.separating_transitions,
    state.transition_system_length )

let create_state ?errors ?env ?init_state ?reset parameters init =
  let error =
    match errors with
    | None -> Exception.empty_error_handler
    | Some error -> error
  in
  let error, handler_bdu =
    if Mvbdu_wrapper.Mvbdu.is_init () then (
      match reset with
      | Some true -> Mvbdu_wrapper.Mvbdu.reset parameters error
      | None | Some false -> Mvbdu_wrapper.Mvbdu.get_handler parameters error
    ) else
      Mvbdu_wrapper.Mvbdu.init parameters error
  in
  {
    parameters;
    log_info = StoryProfiling.StoryStats.init_log_info ();
    handler = None;
    init;
    env;
    contact_map_int = None;
    init_state;
    compilation = None;
    refined_compilation = None;
    c_compil = None;
    quark_map = None;
    internal_influence_map = Public_data.AccuracyMap.empty;
    influence_map = Public_data.AccuracyMap.empty;
    bidirectional_influence_map = Public_data.AccuracyMap.empty;
    pos_of_rules_and_vars = None;
    local_influence_map_blackboard = None;
    internal_contact_map = Public_data.AccuracyMap.empty;
    internal_scc_decomposition = Public_data.AccuracyMap.empty;
    scc_decomposition = Public_data.AccuracyMap.empty;
    contact_map = Public_data.AccuracyMap.empty;
    signature = None;
    bdu_handler = handler_bdu;
    ode_flow = None;
    ctmc_flow = None;
    reachability_state = None;
    subviews_info = None;
    dead_rules = None;
    dead_agents = None;
    errors = error;
    internal_constraints_list = None;
    constraints_list = None;
    symmetric_sites = Public_data.AccuracyMap.empty;
    separating_transitions = None;
    transition_system_length = None;
  }

(**************)
(* JSON: main *)
(**************)

let annotate map =
  Public_data.AccuracyMap.fold (fun x y l -> (x, (x, y)) :: l) map []

let add_map get title label to_json state l =
  let map = get state in
  if Public_data.AccuracyMap.is_empty map then
    l
  else (
    let y = annotate (get state) in
    ( title,
      JsonUtil.of_list
        (JsonUtil.of_pair ~lab1:accuracy_string ~lab2:label
           Public_data.accuracy_to_json (fun x ->
             match to_json x with
             | `Assoc [ (s, m) ] when s = label -> m
             | x ->
               raise
                 (Yojson.Basic.Util.Type_error (JsonUtil.build_msg title, x))))
        (List.rev y) )
    :: l
  )

let get_map empty add of_json label json =
  let l =
    JsonUtil.to_list
      (JsonUtil.to_pair ~lab1:accuracy_string ~lab2:label ~error_msg:"pair11"
         Public_data.accuracy_of_json (fun json ->
           of_json (`Assoc [ label, json ])))
      json
  in
  List.fold_left (fun map (x, y) -> add x (snd y) map) empty l

let get_contact_map_map state = state.contact_map
let get_pos_of_rules_and_vars state = state.pos_of_rules_and_vars
let get_influence_map_map state = state.influence_map
let get_constraints_list state = state.constraints_list

let set_pos_of_rules_and_vars l state =
  { state with pos_of_rules_and_vars = Some l }

let add_errors state l =
  (errors, Exception_without_parameter.to_json state.errors) :: l

let add_contact_map_to_json state l =
  add_map get_contact_map_map contactmaps contactmap
    Public_data.contact_map_to_json state l

(*************************************************)
(*strongly connected component*)

let get_scc_decomposition state = state.scc_decomposition

let add_triple title label to_json =
  JsonUtil.of_triple ~lab1:accuracy_string ~lab2:accuracy_scc ~lab3:scc
    Public_data.accuracy_to_json Public_data.accuracy_to_json
    (fun
      (l :
        Public_data.accuracy_level
        * Public_data.accuracy_level
        * Public_data.scc)
    ->
      match to_json l with
      | `Assoc [ (s, m) ] when s = label -> m
      | x -> raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg title, x)))

let add_list_triple title lable to_json =
  JsonUtil.of_list (add_triple title lable to_json)

let add_list_of_list_of_triple title lable to_json =
  JsonUtil.of_list (add_list_triple title lable to_json)

let annotate_triple (map : Public_data.scc Public_data.AccuracyMap.t) :
    (Public_data.accuracy_level
    * Public_data.accuracy_level
    * (Public_data.accuracy_level
      * Public_data.accuracy_level
      * Public_data.scc))
    list =
  Public_data.AccuracyMap.fold (fun x scc l -> (x, x, (x, x, scc)) :: l) map []

let annotate_map_triple
    (map : Public_data.scc Public_data.AccuracyMap.t Public_data.AccuracyMap.t)
    =
  Public_data.AccuracyMap.fold
    (fun _ map l ->
      let map = annotate_triple map in
      map :: l)
    map [ [] ]

let add_scc_map get title label to_json state
    (l : (string * Yojson.Basic.t) list) =
  let map = get state in
  if Public_data.AccuracyMap.is_empty map then
    l
  else (
    let y = annotate_map_triple (get state) in
    (title, (add_list_of_list_of_triple title label to_json) (List.rev y)) :: l
  )

let add_scc_map_to_json state (l : (string * Yojson.Basic.t) list) :
    (string * Yojson.Basic.t) list =
  add_scc_map get_scc_decomposition scc_contact_map scc_contact_maps
    Public_data.scc_to_json state l

let add_influence_map_to_json state l =
  add_map get_influence_map_map influencemaps Public_data.influencemap
    Public_data.influence_map_to_json state l

let add_dead_rules_to_json state l =
  match state.dead_rules with
  | None -> l
  | Some rules -> (dead_rules, Public_data.dead_rules_to_json rules) :: l

let add_refinements_lemmas_to_json state l =
  match get_constraints_list state with
  | None -> l
  | Some constraints ->
    (Public_data.refinement_lemmas, lemmas_list_to_json constraints) :: l

let get_separating_transitions state = state.separating_transitions

let set_separating_transitions l state =
  { state with separating_transitions = Some l }

let add_separating_transitions state l =
  match get_separating_transitions state with
  | None -> l
  | Some list ->
    (separating_transitions, Public_data.separating_transitions_to_json list)
    :: l

let get_transition_system_length state = state.transition_system_length

let set_transition_system_length l state =
  { state with transition_system_length = Some l }

let to_json state =
  let l = [] in
  let l = add_errors state l in
  let l = add_refinements_lemmas_to_json state l in
  let l = add_dead_rules_to_json state l in
  let l = add_influence_map_to_json state l in
  let l = add_contact_map_to_json state l in
  let l = add_scc_map_to_json state l in
  let l = add_separating_transitions state l in
  (`Assoc l : Yojson.Basic.t)

let of_json = function
  | `Assoc l as json ->
    let errors =
      try Exception_without_parameter.of_json (List.assoc errors l)
      with Not_found ->
        raise
          (Yojson.Basic.Util.Type_error
             (JsonUtil.build_msg "no error handler", json))
    in
    let contact_maps =
      try
        get_map Public_data.AccuracyMap.empty Public_data.AccuracyMap.add
          Public_data.contact_map_of_json contactmap (List.assoc contactmaps l)
      with Not_found -> Public_data.AccuracyMap.empty
    in
    let influence_maps =
      try
        get_map Public_data.AccuracyMap.empty Public_data.AccuracyMap.add
          Public_data.influence_map_of_json Public_data.influencemap
          (List.assoc influencemaps l)
      with Not_found -> Public_data.AccuracyMap.empty
    in
    let dead_rules =
      try Some (Public_data.dead_rules_of_json (List.assoc dead_rules l))
      with Not_found -> None
    in
    let constraints =
      try
        Some (lemmas_list_of_json (List.assoc Public_data.refinement_lemmas l))
      with Not_found -> None
    in
    let separating_transitions =
      try
        Some
          (Public_data.separating_transitions_of_json
             (List.assoc separating_transitions l))
      with Not_found -> None
    in
    ( errors,
      contact_maps,
      influence_maps,
      dead_rules,
      constraints,
      separating_transitions )
  | x ->
    raise
      (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "remanent state", x))

let do_event_gen f phase n state =
  let error, log_info =
    f state.parameters state.errors phase n state.log_info
  in
  { state with errors = error; log_info }

let add_event x y = do_event_gen StoryProfiling.StoryStats.add_event x y
let close_event x y = do_event_gen StoryProfiling.StoryStats.close_event x y
let set_parameters parameters state = { state with parameters }
let get_parameters state = state.parameters
let get_init state = state.init
let set_init_state init state = { state with init_state = Some init }
let get_init_state state = state.init_state
let set_env model state = { state with env = Some model }
let get_env state = state.env

(*contact map from kappa*)
let set_contact_map_int cm state = { state with contact_map_int = Some cm }
let get_contact_map_int state = state.contact_map_int

let set_compilation compilation state =
  { state with compilation = Some compilation }

let get_compilation state = state.compilation
let set_handler handler state = { state with handler = Some handler }
let get_handler state = state.handler
let set_c_compil c_compil state = { state with c_compil = Some c_compil }
let get_c_compil state = state.c_compil

let set_refined_compil refined_compil state =
  { state with refined_compilation = Some refined_compil }

let get_refined_compil state = state.refined_compilation
let set_errors errors state = { state with errors }
let get_errors state = state.errors
let set_quark_map quark_map state = { state with quark_map = Some quark_map }
let get_quark_map state = state.quark_map

let set_contact_map accuracy map state =
  {
    state with
    contact_map = Public_data.AccuracyMap.add accuracy map state.contact_map;
  }

let get_contact_map accuracy state =
  Public_data.AccuracyMap.find_option accuracy state.contact_map

let set_signature signature state = { state with signature = Some signature }
let get_signature state = state.signature

let set_influence_map accuracy map state =
  {
    state with
    influence_map = Public_data.AccuracyMap.add accuracy map state.influence_map;
  }

let get_influence_map accuracy state =
  Public_data.AccuracyMap.find_option accuracy state.influence_map

let set_bidirectional_influence_map accuracy map state =
  {
    state with
    bidirectional_influence_map =
      Public_data.AccuracyMap.add accuracy map state.bidirectional_influence_map;
  }

let get_bidirectional_influence_map accuracy state =
  Public_data.AccuracyMap.find_option accuracy state.bidirectional_influence_map

let set_local_influence_map_blackboard blackboard state =
  { state with local_influence_map_blackboard = Some blackboard }

let get_local_influence_map_blackboard state =
  state.local_influence_map_blackboard

let set_internal_influence_map accuracy map state =
  {
    state with
    internal_influence_map =
      Public_data.AccuracyMap.add accuracy map state.internal_influence_map;
  }

let get_internal_influence_map accuracy state =
  Public_data.AccuracyMap.find_option accuracy state.internal_influence_map

let set_internal_contact_map accuracy int_contact_map state =
  {
    state with
    internal_contact_map =
      Public_data.AccuracyMap.add accuracy int_contact_map
        state.internal_contact_map;
  }

let get_internal_contact_map accuracy state =
  Public_data.AccuracyMap.find_option accuracy state.internal_contact_map

let set_internal_scc_decomposition accuracy accuracy' dec state =
  let old =
    Public_data.AccuracyMap.find_default Public_data.AccuracyMap.empty accuracy
      state.internal_scc_decomposition
  in
  {
    state with
    internal_scc_decomposition =
      Public_data.AccuracyMap.add accuracy
        (Public_data.AccuracyMap.add accuracy' dec old)
        state.internal_scc_decomposition;
  }

let get_internal_scc_decomposition_map state = state.internal_scc_decomposition

let get_internal_scc_decomposition accuracy accuracy' state =
  match
    Public_data.AccuracyMap.find_option accuracy
      state.internal_scc_decomposition
  with
  | None -> None
  | Some a -> Public_data.AccuracyMap.find_option accuracy' a

let set_scc_decomposition accuracy accuracy' dec state =
  let old =
    Public_data.AccuracyMap.find_default Public_data.AccuracyMap.empty accuracy
      state.scc_decomposition
  in
  {
    state with
    scc_decomposition =
      Public_data.AccuracyMap.add accuracy
        (Public_data.AccuracyMap.add accuracy' dec old)
        state.scc_decomposition;
  }

let get_scc_decomposition accuracy accuracy' state =
  match
    Public_data.AccuracyMap.find_option accuracy state.scc_decomposition
  with
  | None -> None
  | Some a -> Public_data.AccuracyMap.find_option accuracy' a

let get_reachability_result state = state.reachability_state

let set_reachability_result reachability_state state =
  { state with reachability_state = Some reachability_state }

let get_dead_rules state = state.dead_rules

let set_dead_rules dead_rules state =
  { state with dead_rules = Some dead_rules }

let get_dead_agents state = state.dead_agents

let set_dead_agents dead_agents state =
  { state with dead_agents = Some dead_agents }

let get_subviews_info state = state.subviews_info

let set_subviews_info subviews state =
  { state with subviews_info = Some subviews }

let set_bdu_handler bdu_handler state = { state with bdu_handler }
let get_bdu_handler state = state.bdu_handler
let set_ode_flow flow state = { state with ode_flow = Some flow }
let get_ode_flow state = state.ode_flow
let set_ctmc_flow flow state = { state with ctmc_flow = Some flow }
let get_ctmc_flow state = state.ctmc_flow
let get_influence_map_map state = state.influence_map
let get_internal_contact_map_map state = state.internal_contact_map
let get_internal_influence_map_map state = state.internal_influence_map
let get_log_info state = state.log_info
let set_log_info log state = { state with log_info = log }
let get_internal_constraints_list state = state.internal_constraints_list

let set_internal_constraints_list list state =
  { state with internal_constraints_list = Some list }

let get_constraints_list state = state.constraints_list

let set_constraints_list list state =
  { state with constraints_list = Some list }

let get_symmetries accuracy state =
  Public_data.AccuracyMap.find_option accuracy state.symmetric_sites

let set_symmetries accuracy partition state =
  {
    state with
    symmetric_sites =
      Public_data.AccuracyMap.add accuracy partition state.symmetric_sites;
  }

let info_to_agent (agent_name, pos, agent_id) =
  {
    Public_data.agent_id = Ckappa_sig.int_of_agent_name agent_id;
    Public_data.agent_ast = agent_name;
    Public_data.agent_position = pos;
  }
