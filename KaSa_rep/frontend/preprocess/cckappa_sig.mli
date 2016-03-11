(**
  * cckappa_sig.mli
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  * 
  * Creation: January, the 17th of 2011
  * Last modification: December, the 9th of 2014
  * * 
  * Signature for prepreprocessing language ckappa 
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

(* eventually, each type will be abtract, no int will appear in type declaration *)

type position   = Ckappa_sig.position
type agent_name = Ckappa_sig.c_agent_name
type site_name  = Ckappa_sig.c_site_name
type state_index    = Ckappa_sig.c_state
type rule_id = int
type agent_id = int

val dummy_agent_name : agent_name

val string_of_agent_name : agent_name -> string
val int_of_agent_name : agent_name -> int
val agent_name_of_int : int -> agent_name

(*sites_dic*)
module Agent_type_nearly_inf_Imperatif: Int_storage.Storage
  with type key = agent_name
  and type dimension = int

module Agent_type_quick_nearly_inf_Imperatif: Int_storage.Storage
  with type key = agent_name 
   and type dimension = int
	
(*state_dic*)		  
module Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif: 
  Int_storage.Storage 
  with type key = agent_name * site_name 
  and type dimension = int * int

(*dual*)
module Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif: Int_storage.Storage
  with type key = agent_name * (site_name * state_index)
  and type dimension = int * (int * int)

module Agent_id_nearly_inf_Imperatif : Int_storage.Storage
  with type key = agent_id
  and type dimension = int
         
module Agent_id_quick_nearly_inf_Imperatif: Int_storage.Storage
  with type key = agent_id 
  and type dimension = int

type binding_state = 
| Free 
| Lnk_type of agent_name * site_name 

type site  = (site_name,site_name) Ckappa_sig.site_type

type state = (Ckappa_sig.internal_state,binding_state) Ckappa_sig.site_type  
 
module Dictionary_of_States: Dictionary.Dictionary with type key = state_index
                                                   and type value = state

type state_dic = (unit, unit) Dictionary_of_States.dictionary
  
type kappa_handler =
    {
      nrules                : int; 
      nvars                 : int; 
      nagents               : int;
      agents_dic            : Ckappa_sig.agent_dic; 
      interface_constraints : Ckappa_sig.agent_specification
                              Agent_type_nearly_inf_Imperatif.t;
      sites                 : Ckappa_sig.site_dic Agent_type_nearly_inf_Imperatif.t; 
      states_dic            : state_dic Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t;
      dual                  : (agent_name * site_name * state_index)
        Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.t
    }
     
type 'a interval = {min:'a; max:'a}
  
type 'state port = 
  { 
    site_name     : site_name; 
    site_position : position;
    site_free     : bool option; 
    site_state    : 'state
  }

module Rule_map_and_set: Map_wrapper.S_with_logs with type elt = rule_id
module Site_map_and_set: Map_wrapper.S_with_logs with type elt = site_name
module State_map_and_set: Map_wrapper.S_with_logs with type elt = state_index
module AgentSite_map_and_set: Map_wrapper.S_with_logs with type elt = agent_name * site_name
module AgentRule_map_and_set: Map_wrapper.S_with_logs with type elt = agent_name * rule_id
module RuleAgent_map_and_set: Map_wrapper.S_with_logs with type elt = rule_id * agent_id
module AgentsSite_map_and_set: Map_wrapper.S_with_logs with type elt = agent_id * agent_name * rule_id
module AgentSiteState_map_and_set: Map_wrapper.S_with_logs with type elt = agent_name * site_name * state_index
module PairAgentSiteState_map_and_set: Map_wrapper.S_with_logs with type elt = (agent_name * site_name * state_index) * (agent_name * site_name * state_index)

module Rule_setmap: SetMap.S with type elt = rule_id

type 'state interface = 'state port Site_map_and_set.Map.t
                                                                           
type 'interface proper_agent = 
  { 
    agent_kasim_id  : agent_id; (* int should be replaced with the appropriate type *)
    agent_name      : agent_name;
    agent_interface : 'interface;
    agent_position  : position
  }

val upgrade_interface: 'a interface proper_agent -> 'b interface -> 'b interface proper_agent
val map_agent: ('a -> 'b) -> 'a interface proper_agent -> 'b interface proper_agent
val upgrade_some_interface: 
  'a Site_map_and_set.Map.t proper_agent ->
  'a option Site_map_and_set.Map.t proper_agent
	      		      
type site_address =
    {
      agent_index : agent_id; (* int should be replaced with the appropriate type *)
      site        : site_name;
      agent_type  : agent_name
    }

type bond = site_address * site_address 
  
val build_address: agent_id -> agent_name -> site_name -> site_address
		     
module Address_map_and_set: Map_wrapper.S_with_logs with type elt = site_address 

module KaSim_Site_map_and_set: Map_wrapper.S_with_logs
  with type elt = (string, string) Ckappa_sig.site_type
    
type agent = 
| Ghost
| Agent of state_index interval interface proper_agent 
| Dead_agent of state_index interval interface proper_agent * KaSim_Site_map_and_set.Set.t * ((string, unit) Ckappa_sig.site_type) Site_map_and_set.Map.t  * Ckappa_sig.link Site_map_and_set.Map.t
(* agent with a site or state that never occur in the rhs or an initial
   state, set of the undefined sites, map of sites with undefined
   internal states, map of sites with undefined binding states*)
| Unknown_agent of (string*int)
(* agent with a type that never occur in rhs or initial states *)
    
type agent_sig = state_index list interface proper_agent 
  
type views = agent Agent_id_quick_nearly_inf_Imperatif.t

type diff_views =
  state_index
    interval
    port
    Site_map_and_set.Map.t
    proper_agent
    Agent_id_quick_nearly_inf_Imperatif.t
    
type mixture = 
  { 
    c_mixture : Ckappa_sig.mixture; 
    views     : views;
    bonds     : site_address Site_map_and_set.Map.t Int_storage.Quick_Nearly_inf_Imperatif.t; 
    plus      : (int * int) list; (* should be replaced with the appropriate type *)
    dot       : (int * int) list  (* should be replaced with the appropriate type *)
    }
      
type enriched_variable = 
    { 
      e_id       : string;
      e_id_dot   : string;
      c_variable : (Ckappa_sig.mixture,string) Ast.ast_alg_expr;
      e_variable : (mixture,string) Ast.variable_def
    }
      
type actions =
  {
    creation   : (agent_id * agent_name) list; (* should be replaced with the appropriate type *)
    remove     : (agent_id * unit interface proper_agent * int list) list; (* should be replaced with the appropriate type *)
    release    : bond list;
    bind       : bond list;
    half_break : (site_address * (state_index interval option)) list 
  }
    
val empty_actions: actions
    
type rule = 
  {
    prefix       : int;
    delta        : int;
    rule_lhs     : mixture; 
    rule_arrow   : Ast.arrow; 
    rule_rhs     : mixture; 
    diff_direct  : diff_views; 
    diff_reverse : diff_views;
    actions      : actions
    }
  
type perturbation =
  ((((mixture,string) Ast.ast_alg_expr Ast.bool_expr) * position) *
     (modif_expr list) *
       (((mixture,string) Ast.ast_alg_expr Ast.bool_expr*position)  option)) *
    position

and modif_expr =
  | INTRO    of ((mixture,string) Ast.ast_alg_expr * mixture * position)
  | DELETE   of ((mixture,string) Ast.ast_alg_expr * mixture * position)
  | UPDATE   of (string * position * (mixture,string) Ast.ast_alg_expr * position)
  (*TODO: pause*)
  | STOP     of position
  | SNAPSHOT of position (*maybe later of mixture too*)

type enriched_rule = 
    {
      e_rule_label             : (string * position) option; 
      e_rule_label_dot         : (string * position) option;
      e_rule_initial_direction : Ckappa_sig.direction; 
      e_rule_rule              : Ckappa_sig.mixture Ckappa_sig.rule;
      e_rule_c_rule            : rule
    }
      
type enriched_init = 
    {
      e_init_factor     : (Ckappa_sig.mixture,string) Ast.ast_alg_expr;
      e_init_c_factor   : (mixture,string) Ast.ast_alg_expr;
      e_init_string_pos : string Location.annot option;
      e_init_mixture    : Ckappa_sig.mixture;
      e_init_c_mixture  : mixture
    }
      
val dummy_init: Remanent_parameters_sig.parameters -> Exception.method_handler -> Exception.method_handler * enriched_init
		  

type compil =
  {
    variables : enriched_variable Int_storage.Nearly_inf_Imperatif.t ;
    (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
    signatures : (agent_sig (** position*)) Int_storage.Nearly_inf_Imperatif.t;
    (*agent signature declaration*)
    rules : enriched_rule Int_storage.Nearly_inf_Imperatif.t  ;
    (*rules (possibly named)*)
    observables :
      (mixture,string) Ast.ast_alg_expr Location.annot Int_storage.Nearly_inf_Imperatif.t;
    (*list of patterns to plot*)
    init : enriched_init Int_storage.Nearly_inf_Imperatif.t  ;
    (*initial graph declaration*)
    perturbations :
      mixture Ckappa_sig.perturbation Int_storage.Nearly_inf_Imperatif.t
    }
