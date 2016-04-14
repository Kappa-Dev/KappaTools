 (**
  * ckappa_sig.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  * 
  * Creation: 01/17/2011
  * Last modification: 09/12/2014
  * * 
  * Signature for prepreprocessing language ckappa 
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module Int_Set_and_Map : Map_wrapper.S_with_logs with type elt = int

(****************************************************************************************)

type position       = Location.t
type agent_name     = string
type site_name      = string 
type internal_state = string 

(****************************************************************************************)
    
type c_agent_name
type c_site_name
type c_state
type c_rule_id
type c_agent_id

(****************************************************************************************)

val dummy_agent_name : c_agent_name
val dummy_site_name : c_site_name
val dummy_state_index : c_state
val dummy_rule_id : c_rule_id
val dummy_agent_id : c_agent_id

val dummy_site_name_1 : c_site_name
val dummy_site_name_minus1 : c_site_name

val dummy_state_index_1 : c_state

val string_of_agent_name : c_agent_name -> string
val int_of_agent_name : c_agent_name -> int
val agent_name_of_int : int -> c_agent_name

val site_name_of_int : int -> c_site_name
val int_of_site_name : c_site_name -> int
val string_of_site_name : c_site_name -> string

val state_index_of_int: int -> c_state
val int_of_state_index: c_state -> int
val string_of_state_index : c_state -> string

val int_of_rule_id : c_rule_id -> int
val rule_id_of_int : int -> c_rule_id
val string_of_rule_id : c_rule_id -> string

val int_of_agent_id : c_agent_id -> int
val agent_id_of_int : int -> c_agent_id

(****************************************************************************************)

module Agent_type_nearly_Inf_Int_storage_Imperatif: Int_storage.Storage
  with type key = c_agent_name
  and type dimension = int

module Agent_type_quick_nearly_Inf_Int_storage_Imperatif: Int_storage.Storage
  with type key = c_agent_name 
  and type dimension = int

module Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif: 
  Int_storage.Storage 
  with type key = c_agent_name * c_site_name 
  and type dimension = int * int

module Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif: Int_storage.Storage
  with type key = c_agent_name * (c_site_name * c_state)
  and type dimension = int * (int * int)

module Site_type_nearly_Inf_Int_storage_Imperatif: Int_storage.Storage
  with type key = c_site_name
  and type dimension = int

module Site_type_quick_nearly_Inf_Int_storage_Imperatif: Int_storage.Storage
  with type key = c_site_name
  and type dimension = int

module State_index_nearly_Inf_Int_storage_Imperatif: Int_storage.Storage
  with type key = c_state
  and type dimension = int

module State_index_quick_nearly_Inf_Int_storage_Imperatif: Int_storage.Storage
  with type key = c_state
  and type dimension = int

module Rule_nearly_Inf_Int_storage_Imperatif: Int_storage.Storage
   with type key = c_rule_id
   and type dimension = int

module Rule_quick_nearly_Inf_Int_storage_Imperatif : Int_storage.Storage
  with type key = c_rule_id
  and type dimension = int

module Site_union_find: Union_find.Union_find
  with type t = c_site_name Site_type_nearly_Inf_Int_storage_Imperatif.t   
  and type dimension = int
  and type key = c_site_name

(****************************************************************************************)
(*FIFO*)

module Rule_FIFO : Fifo.Work_list
  with type elt = c_rule_id

(****************************************************************************************)

module Agent_id_nearly_Inf_Int_storage_Imperatif : Int_storage.Storage
  with type key = c_agent_id
  and type dimension = int
         
module Agent_id_quick_nearly_Inf_Int_storage_Imperatif: Int_storage.Storage
  with type key = c_agent_id 
  and type dimension = int

(****************************************************************************************)

module Agent_map_and_set: Map_wrapper.S_with_logs
  with type elt = c_agent_name

module Agent_id_map_and_set: Map_wrapper.S_with_logs
  with type elt = c_agent_id

module Rule_map_and_set: Map_wrapper.S_with_logs
  with type elt = c_rule_id

module State_map_and_set: Map_wrapper.S_with_logs
  with type elt = c_state

module AgentRule_map_and_set: Map_wrapper.S_with_logs
  with type elt = c_agent_name * c_rule_id

module RuleAgent_map_and_set: Map_wrapper.S_with_logs 
  with type elt = c_rule_id * c_agent_id

module AgentSiteState_map_and_set: Map_wrapper.S_with_logs 
  with type elt = c_agent_name * c_site_name * c_state

(*bonds in rhs and lhs*)
module PairAgentSiteState_map_and_set: Map_wrapper.S_with_logs
  with type elt = 
  (c_agent_name * c_site_name * c_state) * 
    (c_agent_name * c_site_name * c_state)

(*parallel*)
module PairAgentsSitesStates_map_and_set: Map_wrapper.S_with_logs
  with type elt = 
  (c_agent_id * c_agent_name * c_site_name * c_site_name * c_state * c_state) *
    (c_agent_id * c_agent_name * c_site_name * c_site_name * c_state * c_state)

module PairAgentSites_map_and_set: Map_wrapper.S_with_logs
  with type elt = 
  (c_agent_name * c_site_name * c_site_name) *
    (c_agent_name * c_site_name * c_site_name)

(*test flat_lattice*)
module PairAgent_id_SiteState_map_and_set: Map_wrapper.S_with_logs
  with type elt = (c_agent_id * c_site_name * c_state) * (c_agent_id * c_site_name * c_state)

module Rule_setmap: SetMap.S with type elt = c_rule_id

module Agent_id_setmap: SetMap.S with type elt = c_agent_id

module PairRule_setmap : SetMap.S with type elt = c_rule_id * c_rule_id

module Views_bdu: Mvbdu_wrapper.Mvbdu with type key = c_site_name and type value = c_state

(****************************************************************************************)

module Site_map_and_set: Map_wrapper.S_with_logs 
  with type elt = c_site_name

module AgentSite_map_and_set: Map_wrapper.S_with_logs 
  with type elt = c_agent_name * c_site_name

module AgentsSite_map_and_set: Map_wrapper.S_with_logs 
  with type elt = c_agent_id * c_agent_name * c_site_name

(****************************************************************************************)

type binding_state = 
  | Free 
  | Lnk_type of agent_name * site_name 

type mixture = 
  | SKIP  of mixture 
  | COMMA of agent * mixture 
  | DOT   of c_agent_id * agent * mixture 
  | PLUS  of c_agent_id * agent * mixture 
  | EMPTY_MIX
      
and agent =
    {
      ag_nme     : string;
      ag_intf    : interface;
      ag_nme_pos : position (*; ag_pos:position*)
    }

and interface = 
  | EMPTY_INTF
  | PORT_SEP of port * interface

and port =
    {
      port_nme  : string;
      port_int  : internal;
      port_lnk  : link;
      (*port_pos: position ;*)
      port_free : bool option
    }

and internal = string list
    
and link = 
    | LNK_VALUE of (c_agent_id * agent_name * site_name * c_agent_id * position)
    | FREE 
    | LNK_ANY   of position
    | LNK_SOME  of position
    | LNK_TYPE  of (string Location.annot * string Location.annot)

type 'mixture rule = 
  {
    prefix: int;
    delta: int;
      lhs   : 'mixture; 
      arrow : Ast.arrow; 
      rhs   : 'mixture; 
      k_def : ('mixture,string) Ast.ast_alg_expr Location.annot;
      k_un  : ('mixture,string) Ast.ast_alg_expr Location.annot option
    }

type 'mixture perturbation = ('mixture,string) Ast.perturbation

type 'mixture modif_expr   = ('mixture,string) Ast.modif_expr

type 'mixture variable     = ('mixture,string) Ast.variable_def

type direction = Direct | Reverse 

type ('agent,'mixture,'rule) compil =
  ('agent, 'mixture, string, 'rule) Ast.compil
  
type ('a,'b) site_type = 
  | Internal of 'a 
  | Binding  of 'b 
      
type site  = (site_name, site_name) site_type     

type state = (internal_state, binding_state) site_type

type c_binding_state = 
| C_Free 
| C_Lnk_type of c_agent_name * c_site_name 

type state' = (internal_state, c_binding_state) site_type  

module Dictionary_of_States: Dictionary.Dictionary 
  with type key = c_state
  and type value = state'
 
type internal_state_specification =
  {
    string : internal_state;
  }

module Dictionary_of_agents: Dictionary.Dictionary 
  with type key = c_agent_name 
  and type value = agent_name

module Dictionary_of_sites : Dictionary.Dictionary 
  with type key = c_site_name 
  and type value = site

type site_list = 
    {
      used     : (site_name list * position) list;
      declared : (site_name list * position) list;
      creation : (site_name list * position) list 
    }

type agent_dic = (unit,unit) Dictionary_of_agents.dictionary
type site_dic  = (unit,unit) Dictionary_of_sites.dictionary
type state_dic = (unit,unit) Dictionary_of_States.dictionary
  
type agent_specification = 
    { 
      binding_sites_usage : site_list; 
      marked_sites_usage  : site_list
    }
      
type kappa_handler = 
    {
      agents_dic            : agent_dic; 
      interface_constraints : agent_specification Int_storage.Nearly_inf_Imperatif.t;
      sites                 : site_dic Int_storage.Nearly_inf_Imperatif.t;
      states_dic            : state_dic Int_storage.Nearly_inf_Imperatif.t
                              Int_storage.Nearly_inf_Imperatif.t
    }

type 'a interval  = {min:'a; max:'a}
  
type c_port = 
    { 
      c_site_name     : c_site_name; 
      c_site_position : position;
      c_site_interval : c_state interval
    }

type c_interface = c_port Site_map_and_set.Map.t
                                                                           
type c_proper_agent = 
    { 
      c_agent_kasim_id  : c_agent_id; 
      c_agent_name      : c_agent_name;
      c_agent_interface : c_interface;
      c_agent_position  : position
    }

type site_address =
    {
      agent_index : c_agent_id;
      site        : c_site_name
    }

type c_bond = site_address * site_address 

type c_agent = 
   | C_ghost
   | C_agent of c_proper_agent                               

type c_mixture =
  {
    c_views : c_agent Int_storage.Quick_Nearly_inf_Imperatif.t;
    c_bonds :
      site_address Site_map_and_set.Map.t Int_storage.Nearly_inf_Imperatif.t;
    c_plus  : (int * int) list;
    c_dot   : (int * int) list
  }
      
type c_variable = (c_mixture,string) Ast.ast_alg_expr

type action =
  | Release    of c_bond
  | Bind       of c_bond
  | Half_breaf of site_address   

type c_rule = 
    {
      c_rule_lhs     : c_mixture; 
      c_rule_arrow   : Ast.arrow; 
      c_rule_rhs     : c_mixture; 
      c_diff_direct  : c_mixture;
      c_diff_reverse : c_mixture;
      c_side_effects : action list
    }

type c_perturbation =
  ((((c_mixture,string) Ast.ast_alg_expr Ast.bool_expr) * position)
   * (c_modif_expr list)
   * ((c_mixture,string) Ast.ast_alg_expr Ast.bool_expr * position) option)
  * position

and c_modif_expr =
  | C_INTRO    of ((c_mixture,string) Ast.ast_alg_expr * c_mixture * position)
  | C_DELETE   of ((c_mixture,string) Ast.ast_alg_expr * c_mixture * position)
  | C_UPDATE   of
      (string * (c_mixture,string) Ast.ast_alg_expr * position) (*TODO: pause*)
  | C_STOP     of position
  | C_SNAPSHOT of position (*maybe later of mixture too*)

type enriched_rule =
    {
      e_rule_label  : (string * position) option; 
      e_rule_direct : bool; 
      e_rule_rule   : c_mixture rule;
      e_rule_c_rule : c_rule
    }
      
type enriched_init = 
    {
      e_init_factor    : int; 
      e_init_mixture   : mixture;
      e_init_c_mixture : c_mixture; 
      e_init_pos       : position
    } 
      
type c_compil =
  {
    c_variables : c_variable Int_storage.Nearly_inf_Imperatif.t;
    (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
    c_signatures : (agent * position) Int_storage.Nearly_inf_Imperatif.t;
    (*agent signature declaration*)
    c_rules : enriched_rule Int_storage.Nearly_inf_Imperatif.t;
    (*rules (possibly named)*)
    c_observables : (c_mixture,string) Ast.ast_alg_expr Int_storage.Nearly_inf_Imperatif.t;
    (*list of patterns to plot*)
    c_init : enriched_init Int_storage.Nearly_inf_Imperatif.t  ;
    (*initial graph declaration*)
    c_perturbations :
      c_mixture Location.annot perturbation Int_storage.Nearly_inf_Imperatif.t
  }
