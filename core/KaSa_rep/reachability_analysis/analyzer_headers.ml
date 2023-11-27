(**
  * analyzer_headers.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2016, the 30th of January
  * Last modification: Time-stamp: <Aug 22 2018>
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type pattern_matching_flag = Embeddings | Morphisms

type compilation_result = {
  cc_code: Cckappa_sig.compil;
  kappa_handler: Cckappa_sig.kappa_handler;
}

type global_static_information = {
  global_compilation_result: compilation_result;
  global_parameter: Remanent_parameters_sig.parameters;
  global_common_views: Common_static.common_views;
  global_wake_up_relation: Common_static.site_to_rules;
}

let add_wake_up_relation static wake =
  { static with global_wake_up_relation = wake }

type global_dynamic_information = {
  dynamic_dummy: unit;
  mvbdu_handler: Mvbdu_wrapper.Mvbdu.handler;
  log_info: StoryProfiling.StoryStats.log_info;
}

type ('static, 'dynamic) kasa_state = ('static, 'dynamic) Remanent_state.state
type initial_state = Cckappa_sig.enriched_init

let get_wake_up_relation static = static.global_wake_up_relation
let get_parameter static = static.global_parameter
let get_compilation_information static = static.global_compilation_result

let get_kappa_handler static =
  (get_compilation_information static).kappa_handler

let get_cc_code static = (get_compilation_information static).cc_code

(*****************************************************************************)

let get_common_views static = static.global_common_views

let set_common_views common static =
  { static with global_common_views = common }

(*****************************************************************************)

let get_agent_name static =
  (get_common_views static).Common_static.store_agent_name

let set_agent_name agent_name static =
  set_common_views
    {
      (get_common_views static) with
      Common_static.store_agent_name = agent_name;
    }
    static

let get_agent_name_from_pattern static =
  (get_common_views static).Common_static.store_agent_name_from_pattern

let set_agent_name_from_pattern agent_name static =
  set_common_views
    {
      (get_common_views static) with
      Common_static.store_agent_name_from_pattern = agent_name;
    }
    static

(*****************************************************************************)
(*SIDE EFFECTS*)
(*****************************************************************************)

let get_side_effects_views static =
  (get_common_views static).Common_static.store_side_effects_views

let set_side_effects_views eff static =
  set_common_views
    {
      (get_common_views static) with
      Common_static.store_side_effects_views = eff;
    }
    static

let get_side_effects static =
  (get_side_effects_views static).Common_static.store_side_effects

let set_side_effects eff static =
  set_side_effects_views
    {
      (get_side_effects_views static) with
      Common_static.store_side_effects = eff;
    }
    static

let get_potential_side_effects static =
  (get_side_effects_views static).Common_static.store_potential_side_effects

let set_potential_side_effects eff static =
  set_side_effects_views
    {
      (get_side_effects_views static) with
      Common_static.store_potential_side_effects = eff;
    }
    static

let get_potential_side_effects_per_rule static =
  (get_common_views static).Common_static.store_potential_side_effects_per_rule

let set_potential_side_effects_per_rule eff static =
  set_common_views
    {
      (get_common_views static) with
      Common_static.store_potential_side_effects_per_rule = eff;
    }
    static

(*****************************************************************************)
(*BINDING*)
(*****************************************************************************)

let get_binding_views static =
  (get_common_views static).Common_static.store_binding_views

let set_binding_views b static =
  set_common_views
    { (get_common_views static) with Common_static.store_binding_views = b }
    static

let get_bonds_rhs static =
  (get_binding_views static).Common_static.store_bonds_rhs

let set_bonds_rhs bonds static =
  set_binding_views
    { (get_binding_views static) with Common_static.store_bonds_rhs = bonds }
    static

let get_bonds_lhs static =
  (get_binding_views static).Common_static.store_bonds_lhs

let set_bonds_lhs bonds static =
  set_binding_views
    { (get_binding_views static) with Common_static.store_bonds_lhs = bonds }
    static

let get_action_binding static =
  (get_binding_views static).Common_static.store_action_binding

let set_action_binding bonds static =
  set_binding_views
    {
      (get_binding_views static) with
      Common_static.store_action_binding = bonds;
    }
    static

(*****************************************************************************)
(*VIEWS*)
(*****************************************************************************)

let get_test_views static = (get_common_views static).Common_static.store_test

let set_test_views sites static =
  set_common_views
    { (get_common_views static) with Common_static.store_test = sites }
    static

let get_views_rhs static = (get_test_views static).Common_static.store_views_rhs

let set_views_rhs sites static =
  set_test_views
    { (get_test_views static) with Common_static.store_views_rhs = sites }
    static

let get_views_lhs static = (get_test_views static).Common_static.store_views_lhs

let set_views_lhs sites static =
  set_test_views
    { (get_test_views static) with Common_static.store_views_lhs = sites }
    static

(*****************************************************************************)
(*MODIFICATION*)
(*****************************************************************************)

let get_modified_views static =
  (get_common_views static).Common_static.store_modification

let set_modified_views sites static =
  set_common_views
    { (get_common_views static) with Common_static.store_modification = sites }
    static

let get_modified_map static =
  (get_modified_views static).Common_static.store_modified_map

let set_modified_map sites static =
  set_modified_views
    {
      (get_modified_views static) with
      Common_static.store_modified_map = sites;
    }
    static

let get_project_modified_map static =
  (get_modified_views static).Common_static.store_project_modified_map

let set_project_modified_map sites static =
  set_modified_views
    {
      (get_modified_views static) with
      Common_static.store_project_modified_map = sites;
    }
    static

(*****************************************************************************)
(*VIEWS AND MODIFICATION*)
(*****************************************************************************)

let get_test_modif_map static =
  (get_common_views static).Common_static.store_test_modif_map

let set_test_modif_map sites static =
  set_common_views
    {
      (get_common_views static) with
      Common_static.store_test_modif_map = sites;
    }
    static

(*****************************************************************************)
(*INITIAL STATES*)

let compute_initial_state error static =
  let parameters = get_parameter static in
  let compil = get_cc_code static in
  let error, init =
    Int_storage.Nearly_inf_Imperatif.fold parameters error
      (fun _parameters error _ i l -> error, i :: l)
      compil.Cckappa_sig.init []
  in
  error, List.rev init

(*****************************************************************************)
(*RULE*)
(*****************************************************************************)

let get_mvbdu_handler dynamic = dynamic.mvbdu_handler
let set_mvbdu_handler handler dynamic = { dynamic with mvbdu_handler = handler }
let get_log_info dynamic = dynamic.log_info
let set_log_info log_info dynamic = { dynamic with log_info }

let scan_rule static error =
  let parameters = get_parameter static in
  let kappa_handler = get_kappa_handler static in
  let compilation = get_cc_code static in
  let error, store_result =
    Common_static.scan_rule_set parameters error kappa_handler compilation
  in
  let static = set_common_views store_result static in
  error, static

let initialize_global_information parameters log_info error mvbdu_handler
    compilation kappa_handler =
  let error, init_common = Common_static.init_common_views parameters error in
  let error, wake_up = Common_static.empty_site_to_rules parameters error in
  let init_global_static =
    {
      global_compilation_result = { cc_code = compilation; kappa_handler };
      global_parameter = parameters;
      global_common_views = init_common;
      global_wake_up_relation = wake_up;
    }
  in
  let init_dynamic = { dynamic_dummy = (); mvbdu_handler; log_info } in
  let error, static = scan_rule init_global_static error in
  error, static, init_dynamic

let dummy_dead_rules _ error _ = error, false
let dummy_side_effects _ error _ = error, None
