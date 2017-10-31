(**
 * ckappa_sig.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: 01/17/2011
 * Last modification: Time-stamp: <Oct 26 2017>
 * *
 * Signature for prepreprocessing language ckappa
 *
 * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

module Int_Set_and_Map : Map_wrapper.S_with_logs with type elt = int

(***************************************************************************)

type position       = Locality.t
type agent_name     = string
type site_name      = string
type internal_state = string

(*****************************************************************************)

type c_agent_name
type c_site_name
type c_state
type c_rule_id
type c_agent_id

(****************************************************************************)

val rule_id_to_json : c_rule_id -> Yojson.Basic.json
val rule_id_of_json : Yojson.Basic.json -> c_rule_id
val write_c_rule_id : Bi_outbuf.t -> c_rule_id -> unit
val string_of_c_rule_id : ?len:int -> c_rule_id -> string
val read_c_rule_id : Yojson.Safe.lexer_state -> Lexing.lexbuf -> c_rule_id
val c_rule_id_of_string : string -> c_rule_id

val dummy_agent_name : c_agent_name
val dummy_site_name : c_site_name
val dummy_state_index : c_state
val dummy_rule_id : c_rule_id
val dummy_agent_id : c_agent_id

val dummy_site_name_1 : c_site_name
val dummy_site_name_minus1 : c_site_name

val fst_site : c_site_name
val snd_site : c_site_name

val dummy_state_index_1 : c_state

val string_of_agent_name : c_agent_name -> string
val int_of_agent_name : c_agent_name -> int
val agent_name_of_int : int -> c_agent_name
val string_of_agent_id: c_agent_id -> string
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
val add_agent_id: c_agent_id -> int -> c_agent_id
val sub_rule_id: c_rule_id -> int -> c_rule_id
val add_rule_id: c_rule_id -> int -> c_rule_id
val next_agent_id: c_agent_id -> c_agent_id
val next_agent_name: c_agent_name -> c_agent_name
val next_rule_id: c_rule_id -> c_rule_id
val next_site_name: c_site_name -> c_site_name
val next_state_index: c_state -> c_state
val pred_site_name: c_site_name -> c_site_name
val pred_agent_name: c_agent_name -> c_agent_name
val pred_state_index: c_state -> c_state
val compare_agent_id: c_agent_id -> c_agent_id -> int
val compare_rule_id: c_rule_id -> c_rule_id -> int
val compare_site_name: c_site_name -> c_site_name -> int
val compare_state_index: c_state -> c_state -> int
val compare_agent_name: c_agent_name -> c_agent_name -> int

val get_agent_shape: c_site_name -> Remanent_parameters_sig.parameters -> string
val get_agent_color: c_site_name -> Remanent_parameters_sig.parameters -> string

val compare_unit : unit -> unit -> int
val compare_unit_agent_name: unit -> unit -> c_agent_name
val compare_unit_site_name: unit -> unit -> c_site_name
val compare_unit_state_index: unit -> unit -> c_state
val compare_unit_agent_site : unit -> unit -> int

(****************************************************************************)

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
    port_free : bool option
  }

and internal = string list

and link =
  | LNK_VALUE of (c_agent_id * agent_name * site_name * c_agent_id * position)
  | FREE
  | LNK_ANY   of position
  | LNK_SOME  of position
  | LNK_TYPE  of (string Locality.annot * string Locality.annot)

val rename_link:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  (Remanent_parameters_sig.parameters ->
   Exception.method_handler ->
   c_agent_id ->
   Exception.method_handler * c_agent_id) ->
  link ->
  Exception.method_handler * link

val rename_mixture:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  (Remanent_parameters_sig.parameters ->
   Exception.method_handler ->
   c_agent_id ->
   Exception.method_handler * c_agent_id) ->
  mixture ->
  Exception.method_handler * mixture

val join_link:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  link -> link ->
  Exception.method_handler * link

val join_mixture:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  mixture -> mixture ->
  Exception.method_handler * mixture

type direction = Direct | Reverse

type 'pattern rule =
  {
    position: Locality.t;
    prefix: int;
    interprete_delta: direction ;
    delta: int;
    (* to go from Ckappa id to KaSim id: *)
    (* in direct mode:
        substract delta to agents with id >= prefix in the rhs *)
    (* in reverse mode:
        substract delta to agents with id >= prefix in the lhs *)
    lhs   : 'pattern;
    rhs   : 'pattern;
    k_def : ('pattern,string) Alg_expr.e Locality.annot;
    k_un  : ('pattern,string) Alg_expr.e Locality.annot option;
    ast: string ;
    ast_no_rate: string ;
    original_ast: string ;
    original_ast_no_rate: string ;
    from_a_biderectional_rule: bool;
  }

type 'pattern perturbation = ('pattern,'pattern,string) Ast.perturbation

type 'pattern modif_expr   = ('pattern,'pattern,string) Ast.modif_expr

type 'pattern variable     = ('pattern,string) Ast.variable_def

type ('agent,'pattern,'mixture,'rule) compil =
  ('agent, 'pattern, 'mixture, string, 'rule) Ast.compil

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

module Site_map_and_set: Map_wrapper.S_with_logs
  with type elt = c_site_name

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

type c_variable = (c_mixture,string) Alg_expr.e

type action =
  | Release    of c_bond
  | Bind       of c_bond
  | Half_breaf of site_address

type c_rule =
  {
    c_rule_lhs           : c_mixture;
    c_rule_bidirectional : bool;
    c_rule_rhs           : c_mixture;
    c_diff_direct        : c_mixture;
    c_diff_reverse       : c_mixture;
    c_side_effects       : action list
  }

type c_perturbation =
  ((((c_mixture,string) Alg_expr.bool) * position)
   * (c_modif_expr list)
   * ((c_mixture,string) Alg_expr.bool * position) option)
  * position

and c_modif_expr =
  | C_INTRO    of ((c_mixture,string) Alg_expr.e * c_mixture * position)
  | C_DELETE   of ((c_mixture,string) Alg_expr.e * c_mixture * position)
  | C_UPDATE   of
      (string * (c_mixture,string) Alg_expr.e * position) (*TODO: pause*)
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
    c_observables : (c_mixture,string) Alg_expr.e Int_storage.Nearly_inf_Imperatif.t;
    (*list of patterns to plot*)
    c_init : enriched_init Int_storage.Nearly_inf_Imperatif.t  ;
    (*initial graph declaration*)
    c_perturbations :
      c_mixture Locality.annot perturbation Int_storage.Nearly_inf_Imperatif.t
  }

module Rule_nearly_Inf_Int_storage_Imperatif: Int_storage.Storage
  with type key = c_rule_id
   and type dimension = int

val array_of_list_rule_id:
  (Rule_nearly_Inf_Int_storage_Imperatif.dimension,
   'a Rule_nearly_Inf_Int_storage_Imperatif.t)
    Int_storage.unary ->
  (Rule_nearly_Inf_Int_storage_Imperatif.key, 'a,
   'a Rule_nearly_Inf_Int_storage_Imperatif.t,
   'a Rule_nearly_Inf_Int_storage_Imperatif.t)
    Int_storage.ternary ->
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  'a list ->
  Exception.method_handler * 'a Rule_nearly_Inf_Int_storage_Imperatif.t

val introduceable_species_in_pertubation:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  (Remanent_parameters_sig.parameters ->
   Exception.method_handler ->
   string Locality.annot option -> ('a, string) Alg_expr.e Locality.annot ->
   'a Locality.annot -> Exception.method_handler * 'b) -> 'a perturbation ->
  Exception.method_handler * 'b list

(***************************************************************************)

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

module Agent_type_site_quick_nearly_Inf_Int_Int_storage_Imperatif_Imperatif:
  Int_storage.Storage
  with type key = c_agent_name * c_site_name
   and type dimension = int * int

module
  Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif: Int_storage.Storage
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

module Rule_quick_nearly_Inf_Int_storage_Imperatif : Int_storage.Storage
  with type key = c_rule_id
   and type dimension = int

module Site_union_find: Union_find.Union_find
  with type t = c_site_name Site_type_nearly_Inf_Int_storage_Imperatif.t
   and type dimension = int
   and type key = c_site_name

(******************************************************************************)
(*FIFO*)

module Rule_FIFO : Working_list.Work_list
  with type elt = c_rule_id

(******************************************************************************)

module Agent_id_nearly_Inf_Int_storage_Imperatif : Int_storage.Storage
  with type key = c_agent_id
   and type dimension = int

module Agent_id_quick_nearly_Inf_Int_storage_Imperatif: Int_storage.Storage
  with type key = c_agent_id
   and type dimension = int

module Rule_id_quick_nearly_Inf_Int_storage_Imperatif:
  Int_storage.Storage
  with type key = c_rule_id
   and type dimension = int

(****************************************************************************)

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

(*use in site_accross_bonds_domain*)

module SiteState_map_and_set : Map_wrapper.S_with_logs
  with type elt = c_site_name * c_state

module AgentSiteState_map_and_set: Map_wrapper.S_with_logs
  with type elt = c_agent_name * c_site_name * c_state

module Rule_setmap: SetMap.S with type elt = c_rule_id

module Agent_id_setmap: SetMap.S with type elt = c_agent_id

module PairRule_setmap : SetMap.S with type elt = c_rule_id * c_rule_id

module PairAgentSite_map_and_set : Map_wrapper.S_with_logs  with type elt = (c_agent_name * c_site_name) * (c_agent_name * c_site_name)

module AgentSite_map_and_set: Map_wrapper.S_with_logs
  with type elt = c_agent_name * c_site_name

module Agents_map_and_set: Map_wrapper.S_with_logs
  with type elt = c_agent_id * c_agent_name

module AgentsSite_map_and_set: Map_wrapper.S_with_logs
  with type elt = c_agent_id * c_agent_name * c_site_name

module AgentsSiteState_map_and_set: Map_wrapper.S_with_logs
  with type elt = c_agent_id * c_agent_name * c_site_name * c_state

type pair_of_states = c_state * c_state

module AgentsSitePState_map_and_set: Map_wrapper.S_with_logs
  with type elt = c_agent_id * c_agent_name * c_site_name * pair_of_states

module Views_bdu: Mvbdu_wrapper.Mvbdu with type key = c_site_name and type value = c_state

module Views_intbdu: Mvbdu_wrapper.Internalized_mvbdu
  with type key = c_site_name and type value = c_state  and type mvbdu = Views_bdu.mvbdu

(***************************************************************************)

(* JF: This file is the API for KaSa frontend *)
(* JF: Please do not put domain specific module here, put them with the definition of the corresponding abstract domain *)

(*bonds in rhs and lhs: use in Common_static.ml*)

module PairAgentsSiteState_map_and_set: Map_wrapper.S_with_logs
  with type elt =
         (c_agent_id * c_agent_name * c_site_name * c_state) * (c_agent_id * c_agent_name * c_site_name * c_state)

module PairAgentSiteState_map_and_set: Map_wrapper.S_with_logs
  with type elt =
         (c_agent_name * c_site_name * c_state) * (c_agent_name * c_site_name * c_state)

module PairAgentSitesState_map_and_set: Map_wrapper.S_with_logs
  with type elt =
         (c_agent_name * c_site_name * c_site_name * c_state) *
         (c_agent_name * c_site_name * c_site_name * c_state)
