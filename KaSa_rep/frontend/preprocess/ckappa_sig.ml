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

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "ckappa_sig") message exn (fun () -> default) 
  
module Int_Set_and_Map = Map_wrapper.Make(SetMap.Make (struct type t = int let compare = compare end))

let local_trace = true 

type position       = Location.t
type agent_name     = string
type site_name      = string 
type internal_state = string 
    
type binding_state = 
  | Free 
  | Lnk_type of agent_name * site_name 

type mixture = 
  | SKIP  of mixture 
  | COMMA of agent * mixture 
  | DOT   of int * agent * mixture 
  | PLUS  of int * agent * mixture 
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
    | LNK_VALUE of (int * agent_name * site_name * int * position)
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

module State = 
struct
  type t = state 
  let compare = compare
end 

module Dictionary_of_States = Dictionary.Dictionary_of_Ord(State)
 
type internal_state_specification =
    {
      string : internal_state;
    }
  
module Site = 
struct 
  type t = site 
  let compare = compare 
end
  
module Kasim_agent_name = 
struct 
  type t = agent_name 
  let compare = compare 
end 
  
module Dictionary_of_agents = Dictionary.Dictionary_of_Ord(Kasim_agent_name)
module Dictionary_of_sites  = Dictionary.Dictionary_of_Ord(Site) 
  
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

type c_agent_name = int
type c_site_name  = int 
type c_state      = int 
type 'a interval  = {min:'a; max:'a}
  
type c_port = 
    { 
      c_site_name     : c_site_name; 
      c_site_position : position;
      c_site_interval : c_state interval
    }

module C_site_map_and_set = SetMap.Make
  (struct
    type t = c_site_name
    let compare = compare
   end)

type c_interface = c_port C_site_map_and_set.Map.t
                                                                           
type c_proper_agent = 
    { 
      c_agent_kasim_id  : int; 
      c_agent_name      : c_agent_name;
      c_agent_interface : c_interface;
      c_agent_position  : position
    }

type site_address =
    {
      agent_index : int;
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
      site_address C_site_map_and_set.Map.t Int_storage.Nearly_inf_Imperatif.t;
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
