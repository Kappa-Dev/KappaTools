(**
  * analyzer_headers.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2016, the 30th of January
  * Last modification: Time-stamp: <Nov 28 2016>
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type compilation_result =
  {
    cc_code       : Cckappa_sig.compil;
    kappa_handler : Cckappa_sig.kappa_handler
  }

type global_static_information =
  {
    global_compilation_result : compilation_result;
    global_parameter : Remanent_parameters_sig.parameters;
    global_bdu_common_static : Common_static.bdu_common_static;
    global_wake_up_relation: Common_static.site_to_rules ;
  }

let add_wake_up_relation static wake =
  {static with global_wake_up_relation = wake }

type global_dynamic_information =
  {
    dynamic_dummy: unit;
    mvbdu_handler: Mvbdu_wrapper.Mvbdu.handler;
    log_info: StoryProfiling.StoryStats.log_info;
  }

type event =
  | Dummy
  | Check_rule of Ckappa_sig.c_rule_id
  | See_a_new_bond of
      (
        (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
        * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
           Ckappa_sig.c_state)
      )

type 'a bot_or_not =
  | Bot
  | Not_bot of 'a

type 'a top_or_not =
  | Top
  | Not_top of 'a

type maybe_bool =
  | Sure_value of bool
  | Maybe

type step =
  {
    site_out: Ckappa_sig.c_site_name;
    site_in: Ckappa_sig.c_site_name;
    agent_type_in: Ckappa_sig.c_agent_name
  }
type path =
  {
    agent_id: Ckappa_sig.c_agent_id;
    relative_address: step list;
    site: Ckappa_sig.c_site_name;
  }

module type PathMap =
sig
  type 'a t
  val empty: 'a -> 'a t
  val add: path -> 'a -> 'a t -> 'a t
  val find: path -> 'a t -> 'a option
end

module PathSetMap =
  SetMap.Make (struct type t = path let compare = compare let print _ _ = () end)

module PathMap =
  (struct

    type 'a t   = 'a PathSetMap.Map.t

    let empty _ = PathSetMap.Map.empty
    let add     = PathSetMap.Map.add
    let find    = PathSetMap.Map.find_option

  end:PathMap)

type ('static, 'dynamic) kasa_state = ('static, 'dynamic) Remanent_state.state

type initial_state = Cckappa_sig.mixture

let get_wake_up_relation static = static.global_wake_up_relation

let get_parameter static = static.global_parameter

let get_compilation_information static = static.global_compilation_result

let get_kappa_handler static = (get_compilation_information static).kappa_handler

let get_cc_code static = (get_compilation_information static).cc_code

let get_bdu_common_static static = static.global_bdu_common_static

let set_bdu_common_static common static =
  {
    static with
    global_bdu_common_static = common
  }

let get_agent_name static =
  (get_bdu_common_static static).Common_static.store_agent_name

let set_agent_name agent_name static =
  set_bdu_common_static
    {
      (get_bdu_common_static static) with
      Common_static.store_agent_name = agent_name
    }
    static

let get_side_effects static =
  (get_bdu_common_static static).Common_static.store_side_effects

let set_side_effects eff static =
  set_bdu_common_static
    {
      (get_bdu_common_static static) with
      Common_static.store_side_effects = eff
    }
    static

let get_potential_side_effects static =
  (get_bdu_common_static static).Common_static.store_potential_side_effects

let set_potential_side_effects eff static =
  set_bdu_common_static
    {
      (get_bdu_common_static static) with
      Common_static.store_potential_side_effects = eff
    }
    static

let get_potential_side_effects_per_rule static =
  (get_bdu_common_static static).Common_static.store_potential_side_effects_per_rule

let set_potential_side_effects_per_rule eff static =
  set_bdu_common_static
    {
      (get_bdu_common_static static) with
      Common_static.store_potential_side_effects_per_rule = eff
    }
    static

let get_bonds_rhs static =
  (get_bdu_common_static static).Common_static.store_bonds_rhs

let set_bonds_rhs bonds static =
  set_bdu_common_static
    {
      (get_bdu_common_static static) with
      Common_static.store_bonds_rhs = bonds
    }
    static

let get_bonds_lhs static =
  (get_bdu_common_static static).Common_static.store_bonds_lhs

let set_bonds_lhs bonds static =
  set_bdu_common_static
    {
      (get_bdu_common_static static) with
      Common_static.store_bonds_lhs = bonds
    }
    static

let get_action_binding static =
  (get_bdu_common_static static).Common_static.store_action_binding

let set_action_binding bonds static =
  set_bdu_common_static
    {
      (get_bdu_common_static static) with
      Common_static.store_action_binding = bonds
    }
    static

let get_views_rhs static =
  (get_bdu_common_static static).Common_static.store_views_rhs

let set_views_rhs sites static =
  set_bdu_common_static
    {
      (get_bdu_common_static static) with
      Common_static.store_views_rhs = sites
    }
    static

let get_views_lhs static =
  (get_bdu_common_static static).Common_static.store_views_lhs

let set_views_lhs sites static =
  set_bdu_common_static
    {
      (get_bdu_common_static static) with
      Common_static.store_views_lhs = sites
    }
    static

let get_created_bonds static =
  (get_bdu_common_static static).Common_static.store_created_bonds

let set_created_bonds sites static =
  set_bdu_common_static
    {
      (get_bdu_common_static static) with
      Common_static.store_created_bonds = sites
    }
    static

let get_modified_map static =
  (get_bdu_common_static static).Common_static.store_modified_map

let set_modified_map sites static =
  set_bdu_common_static
    {
      (get_bdu_common_static static) with
      Common_static.store_modified_map = sites
    }
    static

let get_project_modified_map static =
  (get_bdu_common_static static).Common_static.store_project_modified_map

let set_project_modified_map sites static =
  set_bdu_common_static
    {
      (get_bdu_common_static static) with
      Common_static.store_project_modified_map = sites
    }
    static

let compute_initial_state error static =
  let parameters = get_parameter static in
  let compil = get_cc_code static in
  let error, init =
    Int_storage.Nearly_inf_Imperatif.fold
       parameters
       error
       (fun _parameters error _ i l -> error, i.Cckappa_sig.e_init_c_mixture :: l)
       compil.Cckappa_sig.init
       []
  in
  let error, init =
    Int_storage.Nearly_inf_Imperatif.fold
       parameters
       error
       (fun _parameters error _ perturbation l ->
          let list =
            Ckappa_sig.introduceable_species_in_pertubation perturbation
          in
          error, List.fold_left
            (fun l elt -> elt::l)
            l list)
       compil.Cckappa_sig.perturbations
       init
  in
  error, List.rev init

let get_mvbdu_handler dynamic = dynamic.mvbdu_handler
let set_mvbdu_handler handler dynamic = {dynamic with mvbdu_handler = handler}
let get_log_info dynamic = dynamic.log_info
let set_log_info log_info dynamic = {dynamic with log_info = log_info}

let scan_rule static error =
  let parameters = get_parameter static in
  let kappa_handler = get_kappa_handler static in
  let compilation = get_cc_code static in
  let error, store_result =
    Common_static.scan_rule_set parameters error kappa_handler compilation
  in
  let static = set_bdu_common_static store_result static in
  error, static

let initialize_global_information parameters log_info error mvbdu_handler
    compilation kappa_handler =
  let init_common = Common_static.init_bdu_common_static in
  let error, wake_up  = Common_static.empty_site_to_rules parameters error in
  let init_global_static =
    {
      global_compilation_result =
        {
          cc_code = compilation;
          kappa_handler = kappa_handler;
        };
      global_parameter     = parameters;
      global_bdu_common_static = init_common;
      global_wake_up_relation = wake_up;
    }
  in
  let init_dynamic =
    {
      dynamic_dummy = () ;
      mvbdu_handler = mvbdu_handler ;
      log_info = log_info;
    }
  in
  let error, static = scan_rule init_global_static error in
  error, static, init_dynamic
