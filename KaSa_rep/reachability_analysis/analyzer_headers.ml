(**
  * analyzer_headers.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 30th of January
  * Last modification:
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
  }

type global_dynamic_information =
  {
    dynamic_dummy: unit;
    mvbdu_handler: Mvbdu_wrapper.Mvbdu.handler
  }

type event =
| Dummy
| Check_rule of Ckappa_sig.c_rule_id
| See_a_new_bond of ((Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state) 
                     * (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state))

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
  SetMap.Make (struct type t = path let compare = compare end)

module PathMap =
  (struct

    type 'a t   = 'a PathSetMap.Map.t
    
    let empty _ = PathSetMap.Map.empty
    let add     = PathSetMap.Map.add
    let find    = PathSetMap.Map.find_option
      
   end:PathMap)

type kasa_state = unit

type initial_state = Cckappa_sig.enriched_init

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

let compute_initial_state error static =
  let parameter = get_parameter static in
  let compil = get_cc_code static in
  let error, init =
    (Int_storage.Nearly_inf_Imperatif.fold
       parameter
       error
       (fun parameter error _ i l -> error, i :: l)
       compil.Cckappa_sig.init
       [])
  in
  error, List.rev init

let get_mvbdu_handler dynamic = dynamic.mvbdu_handler
let set_mvbdu_handler handler dynamic = {dynamic with mvbdu_handler = handler}

let scan_rule static error =
  let parameter = get_parameter static in
  let kappa_handler = get_kappa_handler static in
  let compilation = get_cc_code static in
  let error, store_result =
    Common_static.scan_rule_set parameter error kappa_handler compilation
  in
  let static = set_bdu_common_static store_result static in
  error, static

let initialize_global_information parameter error mvbdu_handler compilation kappa_handler =
  let init_common = Common_static.init_bdu_common_static in
  let init_global_static =
    {
      global_compilation_result =
        {
	  cc_code = compilation;
	  kappa_handler = kappa_handler;
        };
      global_parameter     = parameter;
      global_bdu_common_static = init_common
    }
  in
  let init_dynamic =
    {
      dynamic_dummy = () ;
      mvbdu_handler = mvbdu_handler ;
    }
  in
  let error, static = scan_rule init_global_static error in
  error, static, init_dynamic
