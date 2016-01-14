(**
  * cckappa_sig.ml
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

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "cckappa.sig") message exn (fun () -> default) 
  
type position   = Ckappa_sig.position
type agent_name = int 
type site_name  = int

type binding_state = 
    | Free 
    | Lnk_type of agent_name * site_name 

type site  = (site_name,site_name) Ckappa_sig.site_type
type state = (Ckappa_sig.internal_state,binding_state) Ckappa_sig.site_type  
 
module State = 
struct
  type t = state 
  let compare = compare
end 
    
type state_index = int

module Dictionary_of_States = Dictionary.Dictionary_of_Ord (State)
  
type state_dic = (unit,unit) Dictionary_of_States.dictionary
  
type kappa_handler = 
    {
      nrules                : int; 
      nvars                 : int; 
      nagents               : int;
      agents_dic            : Ckappa_sig.agent_dic; 
      interface_constraints : Ckappa_sig.agent_specification
                              Int_storage.Nearly_inf_Imperatif.t;
      sites                 : Ckappa_sig.site_dic Int_storage.Nearly_inf_Imperatif.t;
      states_dic            : state_dic
                              Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t;
      dual                  : (agent_name * site_name * state_index)
                 Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.t;
    }
     
type 'a interval = {min:'a; max:'a}
  
type 'state port = 
  { 
    site_name     : site_name; 
    site_position : position;
    site_free     : bool option; 
    site_state    : 'state
  }

module Site_map_and_set = Map_wrapper.Make(SetMap.Make 
  (struct
    type t      = site_name
    let compare = compare
   end))

type 'state interface = 'state port Site_map_and_set.Map.t
                                                                           
type 'interface proper_agent = 
  { 
    agent_kasim_id  : int; 
    agent_name      : agent_name;
    agent_interface : 'interface;
    agent_position  : position
  }

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
      Site_map_and_set.Map.map
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
      Site_map_and_set.Map.map (fun x -> (*Some*) x) ag.agent_interface
    end 

type site_address =
    {
      agent_index : int;
      site        : site_name;
      agent_type  : agent_name
    }

type bond = site_address * site_address 
  
let build_address k agent site =
  {
    agent_index = k;
    site        = site;
    agent_type  = agent
  }
  
module Address_map_and_set = 
  Map_wrapper.Make (SetMap.Make
                      (struct
                        type t = site_address
                        let compare = compare
                       end))

module KaSim_Site_map_and_set = 
  Map_wrapper.Make( SetMap.Make
		      (struct 
			type t = (string, string) Ckappa_sig.site_type
			let compare = compare
		       end))
    
type agent = 
| Ghost
| Agent of state_index interval interface proper_agent 
| Dead_agent of state_index interval interface proper_agent * KaSim_Site_map_and_set.Set.t * ((string, unit) Ckappa_sig.site_type) Site_map_and_set.Map.t  * Ckappa_sig.link Site_map_and_set.Map.t
   (* agent with a site or state that never occur in the rhs or an initial state, 
      set of the undefined sites, map of sites with undefined internal states, map of sites with undefined binding states*)																   
| Unknown_agent of (string*int) (* agent with a type that never occur in rhs or initial states *)
    
type agent_sig = state_index list interface proper_agent 
  
type views = agent Int_storage.Quick_Nearly_inf_Imperatif.t 

type diff_views =
    state_index
      interval
      port
      Site_map_and_set.Map.t
      proper_agent
      Int_storage.Quick_Nearly_inf_Imperatif.t

type mixture = 
    { 
      c_mixture : Ckappa_sig.mixture; 
      views     : views;
      bonds     : site_address Site_map_and_set.Map.t Int_storage.Quick_Nearly_inf_Imperatif.t; 
      plus      : (int * int) list;
      dot       : (int * int) list
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
      creation   : (int * agent_name) list;
      remove     : (int * unit interface proper_agent * int list) list; 
      release    : bond list;
      bind       : bond list;
      half_break : (site_address * (state_index interval option)) list 
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
    prefix         : int;
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
      
let dummy_init parameters error =
  let error,views = Int_storage.Quick_Nearly_inf_Imperatif.create parameters error 0 in 
  let error,bonds = Int_storage.Quick_Nearly_inf_Imperatif.create parameters error 0 in 
  error,
  {
    e_init_factor     = Ast.CONST (Nbr.I 0);
    e_init_c_factor   = Ast.CONST (Nbr.I 0);
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
