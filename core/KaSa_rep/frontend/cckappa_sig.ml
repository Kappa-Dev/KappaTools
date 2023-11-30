(**
  * cckappa_sig.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: January, the 17th of 2011
  * Last modification: Time-stamp: <Mar 19 2020>
  * *
  * Signature for prepreprocessing language ckappa
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type site =
  ( Ckappa_sig.c_site_name,
    Ckappa_sig.c_site_name,
    Ckappa_sig.c_site_name )
  Ckappa_sig.site_type

type state_dic = (unit, unit) Ckappa_sig.Dictionary_of_States.dictionary

type kappa_handler = {
  nrules: int;
  nvars: int;
  nagents: Ckappa_sig.c_agent_name;
  agents_dic: Ckappa_sig.agent_dic;
  agents_annotation:
    (string * Loc.t list)
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.t;
  interface_constraints:
    Ckappa_sig.agent_specification
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.t;
  sites:
    Ckappa_sig.site_dic Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.t;
  states_dic:
    state_dic
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t;
  dual:
    (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
    Ckappa_sig
    .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
    .t;
}

type 'a interval = { min: 'a option; max: 'a option }

type 'state port = {
  site_name: Ckappa_sig.c_site_name;
  site_position: Ckappa_sig.position;
  site_free: bool option;
  site_state: 'state;
}

type 'state interface = 'state port Ckappa_sig.Site_map_and_set.Map.t

type 'interface proper_agent = {
  agent_kasim_id: Ckappa_sig.c_agent_id;
  agent_name: Ckappa_sig.c_agent_name;
  agent_interface: 'interface;
  agent_position: Ckappa_sig.position;
  is_created: bool;
}

type site_address = {
  agent_index: Ckappa_sig.c_agent_id;
  site: Ckappa_sig.c_site_name;
  agent_type: Ckappa_sig.c_agent_name;
}

type delta = int

module Address_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = site_address

  let compare = compare
  let print _ _ = ()
end))

type bond = site_address * site_address

module KaSim_Site_map_and_set = Map_wrapper.Make (SetMap.Make (struct
  type t = (string, string, string) Ckappa_sig.site_type

  let compare = compare
  let print _ _ = ()
end))

type agent =
  | Ghost
  | Agent of Ckappa_sig.c_state interval interface proper_agent
  | Dead_agent of
      Ckappa_sig.c_state interval interface proper_agent
      * KaSim_Site_map_and_set.Set.t
      * (string option, unit, unit) Ckappa_sig.site_type
        Ckappa_sig.Site_map_and_set.Map.t
      * Ckappa_sig.link Ckappa_sig.Site_map_and_set.Map.t
  (* agent with a site or state that never occur in the rhs or an initial
     state, set of the undefined sites, map of sites with undefined
     internal states, map of sites with undefined binding states*)
  | Unknown_agent of (string * Ckappa_sig.c_agent_id)
(* agent with a type that never occur in rhs or initial states *)

type agent_sig = Ckappa_sig.c_state list interface proper_agent
type views = agent Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.t

type diff_views =
  Ckappa_sig.c_state interval port Ckappa_sig.Site_map_and_set.Map.t
  proper_agent
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.t

type mixture = {
  c_mixture: Ckappa_sig.mixture;
  views: views;
  bonds:
    site_address Ckappa_sig.Site_map_and_set.Map.t
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.t;
  plus: (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_id) list;
  dot: (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_id) list;
}

type enriched_variable = {
  e_id: string * Ckappa_sig.position;
  e_id_dot: string * Ckappa_sig.position;
  c_variable: (Ckappa_sig.mixture, string) Alg_expr.e;
  e_variable: (mixture, string) Ast.variable_def;
}

type counter_action = {
  precondition: Ckappa_sig.c_state interval;
  postcondition: Ckappa_sig.c_state interval;
  increment: delta;
}

type actions = {
  creation: (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name) list;
  remove:
    (Ckappa_sig.c_agent_id
    * unit interface proper_agent
    * Ckappa_sig.c_site_name list)
    list;
  release: bond list;
  bind: bond list;
  half_break: (site_address * Ckappa_sig.c_state interval option) list;
  translate_counters: (site_address * counter_action) list;
  removed_counters: (site_address * Ckappa_sig.c_state interval) list;
  new_counters: (site_address * Ckappa_sig.c_state) list;
  binder: (string * site_address) list;
}

type rule = {
  prefix: int;
  delta: int;
  rule_lhs: mixture;
  rule_rhs: mixture;
  diff_direct: diff_views;
  diff_reverse: diff_views;
  actions: actions;
}

type modif_expr =
  | APPLY of ((mixture, string) Alg_expr.e * rule * Ckappa_sig.position)
  | UPDATE of
      (string
      * Ckappa_sig.position
      * (mixture, string) Alg_expr.e
      * Ckappa_sig.position)
  (*TODO: pause*)
  | STOP of Ckappa_sig.position
  | SNAPSHOT of Ckappa_sig.position (*maybe later of mixture too*)

type perturbation =
  (((mixture, string) Alg_expr.bool * Ckappa_sig.position)
  * modif_expr list
  * ((mixture, string) Alg_expr.bool * Ckappa_sig.position) option)
  * Ckappa_sig.position

type enriched_rule = {
  e_rule_label: (string * Ckappa_sig.position) option;
  e_rule_label_dot: (string * Ckappa_sig.position) option;
  e_rule_initial_direction: Ckappa_sig.direction;
  e_rule_rule: Ckappa_sig.mixture Ckappa_sig.rule;
  e_rule_c_rule: rule;
}

type enriched_init = {
  e_init_factor: (Ckappa_sig.mixture, string) Alg_expr.e;
  e_init_c_factor: (mixture, string) Alg_expr.e;
  e_init_mixture: Ckappa_sig.mixture;
  e_init_c_mixture: mixture;
}

type compil = {
  variables:
    enriched_variable Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.t;
  (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
  signatures: agent_sig (*position*) Int_storage.Nearly_inf_Imperatif.t;
  (*agent signature declaration*)
  counter_default:
    Ckappa_sig.c_state option Ckappa_sig.AgentSite_map_and_set.Map.t;
  rules: enriched_rule Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.t;
  (*rules (possibly named)*)
  observables:
    (mixture, string) Alg_expr.e Loc.annoted Int_storage.Nearly_inf_Imperatif.t;
  (*list of patterns to plot*)
  init: enriched_init Int_storage.Nearly_inf_Imperatif.t;
  (*initial graph declaration*)
  perturbations:
    (mixture, rule) Ckappa_sig.perturbation Int_storage.Nearly_inf_Imperatif.t;
}

(*******************************************************)
(*EMPTY*)
(*******************************************************)

let empty_actions =
  {
    creation = [];
    remove = [];
    release = [];
    bind = [];
    half_break = [];
    translate_counters = [];
    removed_counters = [];
    new_counters = [];
    binder = [];
  }

let dummy_init parameters error =
  let error, views =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.create parameters
      error 0
  in
  let error, bonds =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.create parameters
      error 0
  in
  ( error,
    {
      e_init_factor = Alg_expr.CONST (Nbr.I 0);
      e_init_c_factor = Alg_expr.CONST (Nbr.I 0);
      e_init_mixture = Ckappa_sig.EMPTY_MIX;
      e_init_c_mixture =
        { c_mixture = Ckappa_sig.EMPTY_MIX; views; bonds; plus = []; dot = [] };
    } )

(*******************************************************)
(*JOIN*)
(*******************************************************)

let state_equal a b = compare a b = 0

let _port_equal port1 port2 =
  port1.site_name = port2.site_name
  && port1.site_free = port2.site_free
  && state_equal port1.site_state port2.site_state

let join_port parameters error a b =
  if a.site_name == b.site_name then (
    let error, site_free =
      match a.site_free, b.site_free with
      | None, x | x, None -> error, x
      | Some b1, Some b2 when b1 = b2 -> error, a.site_free
      | Some _, Some _ -> Exception.warn parameters error __POS__ Exit None
    in
    let site_state =
      {
        min = max a.site_state.min b.site_state.min;
        max = min a.site_state.max b.site_state.max;
      }
    in
    error, { a with site_free; site_state }
  ) else
    Exception.warn parameters error __POS__ Exit a

let _join_interface parameters error interface1 interface2 =
  Ckappa_sig.Site_map_and_set.Map.fold2 parameters error
    Ckappa_sig.Site_map_and_set.Map.add Ckappa_sig.Site_map_and_set.Map.add
    (fun parameters error a b c map ->
      if b = c then
        Ckappa_sig.Site_map_and_set.Map.add parameters error a b map
      else
        Exception.warn parameters error __POS__ Exit map)
    interface1 interface2 Ckappa_sig.Site_map_and_set.Map.empty

let join_interface' = KaSim_Site_map_and_set.Set.union

(*exception False of Exception.method_handle*)

let join_proper_agent parameters error agent1 agent2 =
  let bool =
    (*agent1.agent_kasim_id = agent2.agent_kasim_id
      &&*)
    agent1.agent_name = agent2.agent_name
  in
  if bool then (
    let error, map =
      Ckappa_sig.Site_map_and_set.Map.fold2 parameters error
        Ckappa_sig.Site_map_and_set.Map.add Ckappa_sig.Site_map_and_set.Map.add
        (fun parameters error key a b map ->
          let error, c = join_port parameters error a b in
          Ckappa_sig.Site_map_and_set.Map.add parameters error key c map)
        agent1.agent_interface agent2.agent_interface
        Ckappa_sig.Site_map_and_set.Map.empty
    in
    error, { agent1 with agent_interface = map }
  ) else
    Exception.warn parameters error __POS__ Exit agent1

let join_props parameters error map1 map2 =
  Ckappa_sig.Site_map_and_set.Map.fold2 parameters error
    Ckappa_sig.Site_map_and_set.Map.add Ckappa_sig.Site_map_and_set.Map.add
    (fun parameters error key value1 value2 map ->
      if value1 = value2 then
        Ckappa_sig.Site_map_and_set.Map.add parameters error key value1 map
      else
        Exception.warn parameters error __POS__ Exit map)
    map1 map2 Ckappa_sig.Site_map_and_set.Map.empty

let join_bonds' parameters error map1 map2 =
  Ckappa_sig.Site_map_and_set.Map.fold2 parameters error
    Ckappa_sig.Site_map_and_set.Map.add Ckappa_sig.Site_map_and_set.Map.add
    (fun parameters error key value1 value2 map ->
      let error, value3 = Ckappa_sig.join_link parameters error value1 value2 in
      Ckappa_sig.Site_map_and_set.Map.add parameters error key value3 map)
    map1 map2 Ckappa_sig.Site_map_and_set.Map.empty

let join_agent parameters error agent1 agent2 =
  match agent1, agent2 with
  | Ghost, _ -> error, agent2
  | _, Ghost -> error, agent1
  | Agent proper_agent1, Agent proper_agent2 ->
    let error, proper_agent =
      join_proper_agent parameters error proper_agent1 proper_agent2
    in
    error, Agent proper_agent
  | ( Dead_agent (proper_agent1, intf1, props1, bonds1),
      Dead_agent (proper_agent2, intf2, props2, bonds2) ) ->
    let error, proper_agent3 =
      join_proper_agent parameters error proper_agent1 proper_agent2
    in
    let error, intf3 = join_interface' parameters error intf1 intf2 in
    let error, props3 = join_props parameters error props1 props2 in
    let error, bonds3 = join_bonds' parameters error bonds1 bonds2 in
    error, Dead_agent (proper_agent3, intf3, props3, bonds3)
  | Dead_agent (proper_agent1, intf1, props1, bonds1), Agent proper_agent
  | Agent proper_agent, Dead_agent (proper_agent1, intf1, props1, bonds1) ->
    let error, proper_agent =
      join_proper_agent parameters error proper_agent proper_agent1
    in
    error, Dead_agent (proper_agent, intf1, props1, bonds1)
  | Unknown_agent (string1, id1), Unknown_agent (string2, id2)
    when string1 = string2 && id1 = id2 ->
    error, agent1
  | ( (Agent _ | Dead_agent _ | Unknown_agent _),
      (Agent _ | Dead_agent _ | Unknown_agent _) ) ->
    Exception.warn parameters error __POS__ Exit agent1

let join_views parameters error views1 views2 =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error id agent1 v ->
      let error, agent2 =
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
          parameters error id views2
      in
      let error, agent3 =
        match agent2 with
        | None -> error, agent1
        | Some agent2 -> join_agent parameters error agent1 agent2
      in
      let error', v =
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set
          parameters error id agent3 v
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      error, v)
    views1 views2

let join_bonds parameters error bond1 bond2 =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error id bond1 b ->
      let error, bond2 =
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
          parameters error id bond2
      in
      let error, bond3 =
        match bond2 with
        | None -> error, bond1
        | Some bond2 ->
          Ckappa_sig.Site_map_and_set.Map.fold2 parameters error
            Ckappa_sig.Site_map_and_set.Map.add
            Ckappa_sig.Site_map_and_set.Map.add
            (fun parameters error key value1 value2 map ->
              if value1 = value2 then
                Ckappa_sig.Site_map_and_set.Map.add parameters error key value1
                  map
              else
                Exception.warn parameters error __POS__ Exit map)
            bond1 bond2 Ckappa_sig.Site_map_and_set.Map.empty
      in
      let error', b =
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set
          parameters error id bond3 b
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      error, b)
    bond1 bond2

let join_rel error l1 l2 =
  let l = List.rev (List.append l1 l2) in
  let rec clean l current output =
    match l with
    | t :: q when t = current -> clean q current output
    | t :: q -> clean q t (t :: output)
    | [] -> List.rev output
  in
  let clean l =
    match l with
    | [] -> l
    | t :: q -> clean q t [ t ]
  in
  error, clean l

(*******************************************************)
(* UPGRADE *)
(*******************************************************)

let upgrade_interface ag agent_interface = { ag with agent_interface }

let map_agent f ag =
  upgrade_interface ag
    (Ckappa_sig.Site_map_and_set.Map.map
       (fun port ->
         {
           site_free = port.site_free;
           site_name = port.site_name;
           site_position = port.site_position;
           site_state = f port.site_state;
         })
       ag.agent_interface)

let build_address k agent site = { agent_index = k; site; agent_type = agent }

(*******************************************************)
(* RENAME *)
(*******************************************************)

let rename_proper_agent parameters error f agent =
  let error, id = f parameters error agent.agent_kasim_id in
  error, { agent with agent_kasim_id = id }

let rename_site_address parameters error f site_address =
  let error, agent_index = f parameters error site_address.agent_index in
  error, { site_address with agent_index }

let rename_agent parameters error f agent =
  match agent with
  | Ghost -> error, Ghost
  | Agent proper_agent ->
    let error, proper_agent =
      rename_proper_agent parameters error f proper_agent
    in
    error, Agent proper_agent
  | Dead_agent (proper_agent, intf, props, bonds) ->
    let error, proper_agent =
      rename_proper_agent parameters error f proper_agent
    in
    let error, bonds =
      Ckappa_sig.Site_map_and_set.Map.fold
        (fun key value (error, map) ->
          let error, value = Ckappa_sig.rename_link parameters error f value in
          Ckappa_sig.Site_map_and_set.Map.add parameters error key value map)
        bonds
        (error, Ckappa_sig.Site_map_and_set.Map.empty)
    in
    error, Dead_agent (proper_agent, intf, props, bonds)
  | Unknown_agent (string, id) ->
    let error, id = f parameters error id in
    error, Unknown_agent (string, id)

let rename_views parameters error f views =
  let error, v_empty =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.create parameters
      error 0
  in
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error id agent v ->
      let error, id = f parameters error id in
      let error, agent = rename_agent parameters error f agent in
      let error', v =
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set
          parameters error id agent v
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      error, v)
    views v_empty

let rename_bonds parameters error f bonds =
  let error, empty =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.create parameters
      error 0
  in
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters
    error
    (fun parameters error key value map ->
      let error, key = f parameters error key in
      let error, value =
        Ckappa_sig.Site_map_and_set.Map.fold
          (fun site site_address (error, value) ->
            let error, site_address =
              rename_site_address parameters error f site_address
            in
            Ckappa_sig.Site_map_and_set.Map.add parameters error site
              site_address value)
          value
          (error, Ckappa_sig.Site_map_and_set.Map.empty)
      in
      let error', map =
        Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set
          parameters error key value map
      in
      let error =
        Exception.check_point Exception.warn parameters error error' __POS__
          Exit
      in
      error, map)
    bonds empty

let rename_rel parameters error f l =
  List.fold_left
    (fun (error, list) (a, b) ->
      let error, a = f parameters error a in
      let error, b = f parameters error b in
      error, (a, b) :: list)
    (error, []) (List.rev l)

let rename_mixture parameters error f mixture =
  let error, c_mixture =
    Ckappa_sig.rename_mixture parameters error f mixture.c_mixture
  in
  let error, views = rename_views parameters error f mixture.views in
  let error, bonds = rename_bonds parameters error f mixture.bonds in
  let error, plus = rename_rel parameters error f mixture.plus in
  let error, dot = rename_rel parameters error f mixture.dot in
  error, { c_mixture; views; bonds; plus; dot }

(*******************************************************)
(* JOIN MIXTURE *)
(*******************************************************)

let join_mixture parameters error f g mixture1 mixture2 =
  let error', mixture1 = rename_mixture parameters error f mixture1 in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  let error', mixture2 = rename_mixture parameters error g mixture2 in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  let error', c_mixture =
    Ckappa_sig.join_mixture parameters error mixture1.c_mixture
      mixture2.c_mixture
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  let error', views =
    join_views parameters error mixture1.views mixture2.views
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  let error', bonds =
    join_bonds parameters error mixture1.bonds mixture2.bonds
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  let error', plus = join_rel error mixture1.plus mixture2.plus in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  let error', dot = join_rel error mixture1.dot mixture2.dot in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  error, { c_mixture; views; bonds; plus; dot }

(*******************************************************)
(* ADD *)
(*******************************************************)

let empty_port site =
  let empty_interval =
    {
      min = Some Ckappa_sig.dummy_state_index;
      max = Some Ckappa_sig.dummy_state_index;
    }
  in
  let empty_port =
    {
      site_name = site;
      site_free = None;
      site_state = empty_interval;
      site_position = Loc.dummy;
    }
  in
  empty_port

let get_state_port_interval parameters error site agent_interface =
  match
    Ckappa_sig.Site_map_and_set.Map.find_option_without_logs parameters error
      site agent_interface
  with
  | error, None ->
    Exception.warn parameters error __POS__ Exit (empty_port site)
  | error, Some value -> error, value

let max_state_index_option_min a b =
  if Ckappa_sig.compare_state_index_option_min a b <= 0 then
    b
  else
    a

let min_state_index_option_max a b =
  if Ckappa_sig.compare_state_index_option_max a b <= 0 then
    a
  else
    b

let add_port parameters error site state_min state_max port =
  let old_min = port.site_state.min in
  let old_max = port.site_state.max in
  if
    Ckappa_sig.compare_state_index_option_min state_min old_min <= 0
    || Ckappa_sig.compare_state_index_option_max old_max state_max <= 0
  then (
    let new_min = max_state_index_option_min state_min old_min in
    let new_max = min_state_index_option_max state_max old_max in
    if new_min = old_min && new_max = old_max then
      error, port
    else (
      let site_state = { min = new_min; max = new_max } in
      let port = { port with site_state } in
      error, port
    )
  ) else
    Exception.warn parameters error __POS__ ~message:"incompatible states" Exit
      (empty_port site)

let add_state parameters error site state port =
  add_port parameters error site state state port

let add_agent_interface parameters error site agent_interface =
  let error, state =
    get_state_port_interval parameters error site agent_interface
  in
  (*TODO: add_state_interval?*)
  let error', agent_interface =
    Ckappa_sig.Site_map_and_set.Map.add_or_overwrite parameters error site state
      agent_interface
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__
      ~message:"this agent interface is already used" Exit
  in
  error, agent_interface
