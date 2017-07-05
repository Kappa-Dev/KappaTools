(**
  * cckappa_sig.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: January, the 17th of 2011
  * Last modification: Time-stamp: <Jul 05 2017>
  * *
  * Signature for prepreprocessing language ckappa
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)


type site  =
  (Ckappa_sig.c_site_name, Ckappa_sig.c_site_name) Ckappa_sig.site_type

type state_dic = (unit, unit) Ckappa_sig.Dictionary_of_States.dictionary

type kappa_handler =
  {
      nrules                : int;
      nvars                 : int;
      nagents               : Ckappa_sig.c_agent_name;
      agents_dic            : Ckappa_sig.agent_dic;
      interface_constraints : Ckappa_sig.agent_specification
        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.t;
      sites                 : Ckappa_sig.site_dic
        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.t;
      states_dic            : state_dic
        Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t;
      dual                  :
        (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
          Ckappa_sig.Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.t;

      agents_sites_dic : Ckappa_sig.agent_site_dic
  }

type 'a interval = {min:'a; max:'a}

let state_equal a b = compare a b = 0

type 'state port =
  {
    site_name     : Ckappa_sig.c_site_name;
    site_position : Ckappa_sig.position;
    site_free     : bool option;
    site_state    : 'state
  }

let port_equal port1 port2 =
  port1.site_name = port2.site_name
  && port1.site_free = port2.site_free
  && state_equal port1.site_state port2.site_state

let join_port parameters error a b =
  if a.site_name == b.site_name
  then
    let error, site_free =
      match a.site_free, b.site_free with
      | None, x | x,None  -> error, x
      | Some b1, Some b2 when b1=b2 -> error, a.site_free
      | Some _ , Some _ ->
        Exception.warn parameters error __POS__ Exit None
    in
    let site_state =
      {min = max a.site_state.min b.site_state.min ;
       max = min a.site_state.max b.site_state.max
      }
    in
    error,
    {
      a
      with
        site_free = site_free ;
        site_state = site_state }
  else
    Exception.warn parameters error __POS__ Exit a

type 'state interface = 'state port Ckappa_sig.Site_map_and_set.Map.t

type 'interface proper_agent =
  {
    agent_kasim_id  : Ckappa_sig.c_agent_id;
    agent_name      : Ckappa_sig.c_agent_name;
    agent_interface : 'interface;
    agent_position  : Ckappa_sig.position
  }

let join_interface parameters error interface1 interface2 =
  Ckappa_sig.Site_map_and_set.Map.fold2
    parameters error
    Ckappa_sig.Site_map_and_set.Map.add
    Ckappa_sig.Site_map_and_set.Map.add
    (fun parameters error a b c map ->
       if b=c then
         Ckappa_sig.Site_map_and_set.Map.add
           parameters error a b map
       else
         Exception.warn parameters error __POS__ Exit map
    )
    interface1
    interface2
    Ckappa_sig.Site_map_and_set.Map.empty

module KaSim_Site_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = (string, string) Ckappa_sig.site_type
         let compare = compare
         let print _ _ = ()
       end))

let join_interface' =
  KaSim_Site_map_and_set.Set.union

let rename_proper_agent parameters error f agent =
  let error, id = f parameters error agent.agent_kasim_id in
  error,
  {
    agent with agent_kasim_id = id
  }

exception False of Exception.method_handler

let join_proper_agent parameters error agent1 agent2 =
  let bool =
    (*agent1.agent_kasim_id = agent2.agent_kasim_id
      &&*) agent1.agent_name = agent2.agent_name
  in
  if bool then
    let error, map =
      Ckappa_sig.Site_map_and_set.Map.fold2
        parameters error
        Ckappa_sig.Site_map_and_set.Map.add
        Ckappa_sig.Site_map_and_set.Map.add
        (fun parameters error key a b map ->
           let error, c = join_port parameters error a b in
           Ckappa_sig.Site_map_and_set.Map.add parameters error key c map)
        agent1.agent_interface agent2.agent_interface
        Ckappa_sig.Site_map_and_set.Map.empty
    in
    error, {agent1 with agent_interface = map}
  else
      Exception.warn parameters error __POS__ Exit agent1

let upgrade_interface ag interface  =
  {
    agent_kasim_id  = ag.agent_kasim_id;
    agent_name      = ag.agent_name;
    agent_interface = interface;
    agent_position  = ag.agent_position
  }

let map_agent f ag =
  upgrade_interface
    ag
    begin
      Ckappa_sig.Site_map_and_set.Map.map
         (fun port ->
           {
             site_free     = port.site_free;
             site_name     = port.site_name;
             site_position = port.site_position;
             site_state    = f port.site_state
           })
        ag.agent_interface
    end

let upgrade_some_interface ag =
  upgrade_interface ag
    begin
      Ckappa_sig.Site_map_and_set.Map.map (fun x -> Some x) ag.agent_interface
    end

type site_address =
    {
      agent_index : Ckappa_sig.c_agent_id;
      site        : Ckappa_sig.c_site_name;
      agent_type  : Ckappa_sig.c_agent_name
    }

let rename_site_address parameters error f site_address =
  let error, agent_index = f parameters error site_address.agent_index in
  error, {site_address with agent_index = agent_index}

type bond = site_address * site_address

let build_address k agent site =
  {
    agent_index = k;
    site        = site;
    agent_type  = agent
  }

module Address_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = site_address
         let compare = compare
         let print _ _ = ()
       end))

type agent =
| Ghost
| Agent of Ckappa_sig.c_state interval interface proper_agent
| Dead_agent of Ckappa_sig.c_state interval interface proper_agent * KaSim_Site_map_and_set.Set.t * ((string, unit) Ckappa_sig.site_type) Ckappa_sig.Site_map_and_set.Map.t  * Ckappa_sig.link Ckappa_sig.Site_map_and_set.Map.t
(* agent with a site or state that never occur in the rhs or an initial
   state, set of the undefined sites, map of sites with undefined
   internal states, map of sites with undefined binding states*)
| Unknown_agent of (string * Ckappa_sig.c_agent_id)
(* agent with a type that never occur in rhs or initial states *)

let join_props parameters error map1 map2 =
  Ckappa_sig.Site_map_and_set.Map.fold2
    parameters error
    Ckappa_sig.Site_map_and_set.Map.add
    Ckappa_sig.Site_map_and_set.Map.add
    (fun parameters error key value1 value2 map ->
       if value1=value2
       then
         Ckappa_sig.Site_map_and_set.Map.add parameters error key value1 map
       else
         Exception.warn parameters error __POS__ Exit map)
    map1
    map2
    Ckappa_sig.Site_map_and_set.Map.empty


let join_bonds' parameters error map1 map2 =
  Ckappa_sig.Site_map_and_set.Map.fold2
    parameters error
    Ckappa_sig.Site_map_and_set.Map.add
    Ckappa_sig.Site_map_and_set.Map.add
    (fun parameters error key value1 value2 map ->
       let error, value3 = Ckappa_sig.join_link parameters error value1 value2 in
          Ckappa_sig.Site_map_and_set.Map.add parameters error key value3 map
      )
    map1
    map2
    Ckappa_sig.Site_map_and_set.Map.empty

let rename_agent parameters error f agent =
  match
    agent
  with
  | Ghost -> error, Ghost
  | Agent proper_agent ->
    let error, proper_agent =
      rename_proper_agent parameters error f proper_agent
    in
    error, Agent proper_agent
  | Dead_agent (proper_agent, intf, props, bonds) ->
    let error, proper_agent = rename_proper_agent parameters error f proper_agent in
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

let join_agent parameters error agent1 agent2 =
  match
    agent1, agent2
  with
  | Ghost, _ -> error, agent2
  | _, Ghost -> error, agent1
  | Agent proper_agent1, Agent proper_agent2 ->
    let error, proper_agent =
      join_proper_agent parameters error proper_agent1 proper_agent2
    in
    error, Agent proper_agent
  | Dead_agent (proper_agent1, intf1, props1, bonds1),
    Dead_agent (proper_agent2, intf2, props2, bonds2) ->
    let error, proper_agent3 = join_proper_agent parameters error proper_agent1 proper_agent2 in
    let error, intf3 = join_interface' parameters error intf1 intf2 in
    let error, props3 = join_props parameters error props1 props2 in
    let error, bonds3 = join_bonds' parameters error bonds1 bonds2 in
    error, Dead_agent (proper_agent3, intf3, props3, bonds3)
  | Dead_agent (proper_agent1, intf1, props1, bonds1),
    Agent (proper_agent)
  | Agent (proper_agent),
    Dead_agent(proper_agent1, intf1, props1, bonds1) ->
    let error, proper_agent = join_proper_agent parameters error proper_agent proper_agent1 in
    error, Dead_agent (proper_agent, intf1, props1, bonds1)
  | Unknown_agent (string1, id1), Unknown_agent (string2, id2) when string1=string2 && id1=id2 -> error, agent1
  | (Agent _ | Dead_agent _ | Unknown_agent _),
    (Agent _ | Dead_agent _ | Unknown_agent _)
    -> Exception.warn parameters error __POS__ Exit agent1


type agent_sig = Ckappa_sig.c_state list interface proper_agent

type views = agent Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.t

let rename_views parameters error f views =
  let error, v_empty =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.create parameters error 0
  in
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameters
    error
    (fun parameters error id agent v ->
       let error, id = f parameters error id in
       let error, agent = rename_agent parameters error f agent in
       let error', v =
         Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set
           parameters error id agent v
       in
       let error =
         Exception.check_point Exception.warn parameters error error'
           __POS__ Exit
       in
       error, v)
    views
    v_empty

let join_views parameters error views1 views2 =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameters error
    (fun parameters error id agent1 v ->
       let error, agent2 =
         Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
           parameters error id views2 in
       let error, agent3 =
         match agent2 with
         | None -> error, agent1
         | Some agent2 ->
           join_agent parameters error agent1 agent2
       in
       let error', v =
         Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set
           parameters error id agent3 v in
       let error =
             Exception.check_point Exception.warn parameters error error'
               __POS__ Exit
       in
       error, v)
    views1
    views2

let rename_rel parameters error f l =
  List.fold_left
    (fun (error, list) (a,b) ->
       let error, a = f parameters error a in
       let error, b = f parameters error b in
       error, (a,b)::list
    )
    (error, [])
    (List.rev l)

type diff_views =
  Ckappa_sig.c_state
    interval
    port
    Ckappa_sig.Site_map_and_set.Map.t
    proper_agent
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.t

let join_rel parameter error l1 l2 =
  let l = List.rev (List.append l1 l2) in
  let rec clean l current output =
    match l with t::q when t = current -> clean q current output
               | t::q -> clean q t (t::output)
               | [] -> List.rev output
  in
  let clean l =
    match l with [] -> l | t::q -> clean q t [t]
  in
  error, clean l

type mixture =
  {
    c_mixture : Ckappa_sig.mixture;
    views     : views;
    bonds     : site_address Ckappa_sig.Site_map_and_set.Map.t
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.t;
    plus      : (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_id) list;
    dot       : (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_id) list
  }

let join_bonds parameters error bond1 bond2 =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameters error
    (fun parameters error id bond1 b ->
     let error, bond2 =
       Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
         parameters error id bond2 in
     let error, bond3 =
       match bond2 with
       | None -> error, bond1
       | Some bond2 ->
         Ckappa_sig.Site_map_and_set.Map.fold2
           parameters
           error
           Ckappa_sig.Site_map_and_set.Map.add
           Ckappa_sig.Site_map_and_set.Map.add
           (fun parameters error key value1 value2 map ->
              if value1=value2 then
                Ckappa_sig.Site_map_and_set.Map.add parameters error key value1 map
              else
                Exception.warn parameters error __POS__ Exit map)
           bond1
           bond2
           Ckappa_sig.Site_map_and_set.Map.empty
     in
     let error', b =
       Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set
         parameters error id bond3 b in
     let error =
           Exception.check_point Exception.warn parameters error error'
             __POS__ Exit
     in
     error, b
    )
    bond1
    bond2

module Mixture_setmap =
  SetMap.Make
    (struct
      type t = mixture
      let compare = compare
      let print _ _ = ()
    end)

module Mixture_map_and_set =
  Map_wrapper.Make (SetMap.Make
                      (struct
                        type t = mixture
                        let compare = compare
                        let print _ _ = ()
                      end
                      ))

module MixtureAgent_map_and_set =
  Map_wrapper.Make (SetMap.Make
                      (struct
                        type t = (mixture * Ckappa_sig.c_agent_id)
                        let compare = compare
                        let print _ _ = ()
                      end
                      ))

let rename_bonds parameters error f bonds =
  let error, empty =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.create
      parameters error 0
  in
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold
    parameters error
    (fun parameters error key value map ->
       let error, key = f parameters error key in
       let error, value =
         Ckappa_sig.Site_map_and_set.Map.fold
           (fun site site_address (error, value) ->
              let error, site_address =
                rename_site_address
                  parameters
                  error
                  f
                  site_address
              in
              Ckappa_sig.Site_map_and_set.Map.add
                parameters error site site_address value)
           value
           (error, Ckappa_sig.Site_map_and_set.Map.empty)
       in
       let error', map =
       Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set
         parameters error
         key value map in
         let error =
           Exception.check_point Exception.warn parameters error error'
             __POS__ Exit
       in
       error, map)
    bonds
    empty

let rename_mixture parameters error f mixture =
  let error, c_mixture =
    Ckappa_sig.rename_mixture parameters error f mixture.c_mixture
  in
  let error, views = rename_views parameters error f mixture.views in
  let error, bonds = rename_bonds parameters error f mixture.bonds in
  let error, plus = rename_rel parameters error f mixture.plus in
  let error, dot = rename_rel parameters error f mixture.dot in
  error,
  {c_mixture = c_mixture ;
   views = views ;
   bonds = bonds ;
   plus = plus ;
   dot = dot ;
  }

let join_mixture parameters error f g mixture1 mixture2 =
  let error', mixture1 = rename_mixture parameters error f mixture1 in
  let error =
    Exception.check_point Exception.warn parameters error error'
      __POS__ Exit
  in
  let error', mixture2 = rename_mixture parameters error g mixture2 in
  let error =
    Exception.check_point Exception.warn parameters error error'
      __POS__ Exit
  in
  let error', c_mixture =
    Ckappa_sig.join_mixture parameters error mixture1.c_mixture mixture2.c_mixture
  in
  let error =
    Exception.check_point Exception.warn parameters error error'
      __POS__ Exit
  in
  let error', views =
    join_views parameters error mixture1.views mixture2.views
  in
  let error =
    Exception.check_point Exception.warn parameters error error'
      __POS__ Exit
  in
  let error', bonds =
    join_bonds parameters error mixture1.bonds mixture2.bonds
  in
  let error =
    Exception.check_point Exception.warn parameters error error'
      __POS__ Exit
  in
  let error', plus = join_rel parameters error mixture1.plus mixture2.plus in
  let error =
    Exception.check_point Exception.warn parameters error error'
      __POS__ Exit
  in
  let error', dot = join_rel parameters error mixture1.dot mixture2.dot in
  let error =
    Exception.check_point Exception.warn parameters error error'
      __POS__ Exit
  in
  error,
  {c_mixture = c_mixture ;
   views = views ;
   bonds = bonds ;
   plus = plus ;
   dot = dot ;
  }

type enriched_variable =
    {
      e_id       : string;
      e_id_dot   : string;
      c_variable : (Ckappa_sig.mixture,string) Alg_expr.e;
      e_variable : (mixture,string) Ast.variable_def
    }

type actions =
    {
      creation   : (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name) list;
      remove     : (Ckappa_sig.c_agent_id * unit interface proper_agent * Ckappa_sig.c_site_name list) list;
      release    : bond list;
      bind       : bond list;
      half_break : (site_address * (Ckappa_sig.c_state interval option)) list
    }

let empty_actions =
  {
    creation   = [];
    remove     = [];
    release    = [];
    bind       = [];
    half_break = []
  }

type rule =
  {
    prefix             : int;
    delta              : int;
    rule_lhs           : mixture;
    rule_bidirectional : bool;
    rule_rhs           : mixture;
    diff_direct        : diff_views;
    diff_reverse       : diff_views;
    actions            : actions
    }

type perturbation =
  ((((mixture,string) Alg_expr.bool) * Ckappa_sig.position) *
     (modif_expr list) *
       (((mixture,string) Alg_expr.bool * Ckappa_sig.position)  option)) *
    Ckappa_sig.position

and modif_expr =
  | INTRO    of ((mixture,string) Alg_expr.e * mixture * Ckappa_sig.position)
  | DELETE   of ((mixture,string) Alg_expr.e * mixture * Ckappa_sig.position)
  | UPDATE   of (string * Ckappa_sig.position * (mixture,string) Alg_expr.e * Ckappa_sig.position)
  (*TODO: pause*)
  | STOP     of Ckappa_sig.position
  | SNAPSHOT of Ckappa_sig.position (*maybe later of mixture too*)

type enriched_rule =
    {
      e_rule_label             : (string * Ckappa_sig.position) option;
      e_rule_label_dot         : (string * Ckappa_sig.position) option;
      e_rule_initial_direction : Ckappa_sig.direction;
      e_rule_rule              : Ckappa_sig.mixture Ckappa_sig.rule;
      e_rule_c_rule            : rule
    }

type enriched_init =
    {
      e_init_factor     : (Ckappa_sig.mixture, string) Alg_expr.e;
      e_init_c_factor   : (mixture, string) Alg_expr.e;
      e_init_string_pos : string Locality.annot option;
      e_init_mixture    : Ckappa_sig.mixture;
      e_init_c_mixture  : mixture
    }

let dummy_init parameters error =
  let error,views = Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.create parameters error 0 in
  let error,bonds =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.create
      parameters error 0
  in
  error,
  {
    e_init_factor     = Alg_expr.CONST (Nbr.I 0);
    e_init_c_factor   = Alg_expr.CONST (Nbr.I 0);
    e_init_string_pos = None;
    e_init_mixture    = Ckappa_sig.EMPTY_MIX;
    e_init_c_mixture  =
      {
	c_mixture = Ckappa_sig.EMPTY_MIX;
	views     = views;
	bonds     = bonds;
	plus      = [];
	dot       = []
      };
  }

type compil =
  {
    variables : enriched_variable Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.t ;
    (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
    signatures : (agent_sig (** position*)) Int_storage.Nearly_inf_Imperatif.t;
    (*agent signature declaration*)
    rules : enriched_rule Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.t;
    (*rules (possibly named)*)
    observables :
      (mixture, string) Alg_expr.e Locality.annot Int_storage.Nearly_inf_Imperatif.t;
    (*list of patterns to plot*)
    init : enriched_init Int_storage.Nearly_inf_Imperatif.t  ;
    (*initial graph declaration*)
    perturbations :
      mixture Ckappa_sig.perturbation Int_storage.Nearly_inf_Imperatif.t
    }
