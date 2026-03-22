(**
  * export_to_KaSa.ml
  * openkappa
  * Jérôme Feret, project Antique, INRIA Paris
  *
  * Creation: June 30 2016
  * Last modification: Time-stamp: <Dec 04 2018>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Type = sig
  type state
  type parameters = Remanent_parameters_sig.parameters
  type errors = Exception.exceptions_caught_and_uncaught
  type internal_contact_map
  type internal_scc_decomposition = Remanent_state.internal_scc_decomposition
  type contact_map = Public_data.contact_map
  type internal_influence_map = Remanent_state.internal_influence_map
  type bidirectional_influence_map
  type internal_constraint_list = Remanent_state.internal_constraint_list

  val empty_constraint_list : internal_constraint_list

  type handler = Cckappa_sig.kappa_handler
  type compilation = Ast.parsing_compil
  type c_compilation = Cckappa_sig.compil
  type reachability_analysis
  type ode_flow
  type ctmc_flow

  val init : unit -> state
  val set_errors : errors -> state -> state
  val set_parameters : parameters -> state -> state
  val get_parameters : state -> parameters
  val get_handler : state -> state * handler
  val get_errors : state -> errors
  val get_env : state -> state * Model.t option
  val get_compilation : state -> state * compilation
  val get_c_compilation : state -> state * c_compilation

  (**************************************************)
  (*work in process*)

  val get_contact_map :
    ?accuracy_level:Public_data.accuracy_level -> state -> state * contact_map

  val dump_contact_map : Public_data.accuracy_level -> state -> unit

  val get_scc_decomposition :
    ?accuracy_level_cm:Public_data.accuracy_level ->
    ?accuracy_level_scc:Public_data.accuracy_level ->
    state ->
    state * internal_scc_decomposition

  val output_scc_decomposition :
    ?accuracy_level_cm:Public_data.accuracy_level ->
    ?accuracy_level_scc:Public_data.accuracy_level ->
    state ->
    state

  (**************************************************)

  val get_internal_contact_map :
    ?accuracy_level:Public_data.accuracy_level ->
    state ->
    state * internal_contact_map

  val get_influence_map :
    ?accuracy_level:Public_data.accuracy_level ->
    state ->
    state * internal_influence_map

  val get_local_influence_map :
    ?accuracy_level:Public_data.accuracy_level ->
    ?fwd:int ->
    ?bwd:int ->
    total:int ->
    Ckappa_sig.c_rule_id ->
    state ->
    state * internal_influence_map

  val get_reachability_analysis : state -> state * reachability_analysis
  val get_constraint_list : state -> state * internal_constraint_list
  val get_ctmc_flow : state -> state * ctmc_flow
  val get_ode_flow : state -> state * ode_flow

  val get_symmetric_sites :
    ?accuracy_level:Public_data.accuracy_level ->
    state ->
    state * Remanent_state.symmetric_sites

  val dump_c_compil : state -> c_compilation -> state

  val output_internal_contact_map :
    ?logger:Loggers.t ->
    ?accuracy_level:Public_data.accuracy_level ->
    state ->
    state

  val output_influence_map :
    ?logger:Loggers.t ->
    ?accuracy_level:Public_data.accuracy_level ->
    state ->
    state

  val output_local_influence_map :
    ?logger:Loggers.t ->
    ?accuracy_level:Public_data.accuracy_level ->
    ?fwd:int ->
    ?bwd:int ->
    total:int ->
    Ckappa_sig.c_rule_id ->
    state ->
    state

  val output_constraint_list : ?logger:Loggers.t -> state -> state

  val output_symmetries :
    ?logger:Loggers.t ->
    ?accuracy_level:Public_data.accuracy_level ->
    state ->
    state

  val output_reachability_result : state -> state

  val get_data :
    state ->
    Cckappa_sig.kappa_handler option
    * Public_data.dead_rules option
    * Remanent_state.separating_transitions option
    * int list option

  val enable_rule : string -> state -> state
  val disable_rule : string -> state -> state
  val enable_rule_index : int list -> state -> state
  val disable_rule_index : int list -> state -> state

  val summarize_from_ast: state -> state * (Ast.rule Ast.compil_rule,
          (Ast.mixture, Ast.mixture, string) Ast.init_statement) Diff.summary

  (*val summarize_from_ckappa: state -> state * 
  (Ckappa_sig.enriched_rule, Ckappa_sig.enriched_init) Diff.summary *)

(*val summarize_from_cckappa: state -> state * (Cckappa_sig.enriched_rule, Cckappa_sig.enriched_init)  Diff.summary *)

  val dump_summary: ('a,'b) Diff.summary -> state -> state 

  val update_file: string -> state -> state 

  val modify_pos_of_rules: (int -> Loc.position option) -> state -> state 
  val modify_pos_of_init_states: (int -> Loc.position option) -> state -> state 
  val add_rule: Ast.rule Ast.compil_rule -> state -> state 
  val add_init: (Ast.mixture, Ast.mixture, string) Ast.init_statement -> state -> state 
 end

module Export =
functor
  (A : Analyzer.Analyzer)
  ->
  struct
    include Export.Export (A)

    let init () = init ~called_from:Remanent_parameters_sig.KaSa ()
    let get_contact_map = get_contact_map
    let dump_contact_map = dump_contact_map
    let get_internal_contact_map = get_internal_contact_map
    let get_influence_map = get_internal_influence_map
    let get_local_influence_map = get_local_internal_influence_map
    let get_scc_decomposition = get_internal_scc_decomposition
    let get_constraint_list = get_internal_constraint_list
    let output_internal_contact_map = output_internal_contact_map
    let output_influence_map = output_internal_influence_map
    let output_local_influence_map = output_local_internal_influence_map
    let output_constraint_list = output_internal_constraint_list
    let output_scc_decomposition = dump_internal_scc_decomposition
    let empty_constraint_list = []
    let enable_rule = enable_rule
    let disable_rule = disable_rule
    let enable_rule_index = enable_rule_index
    let disable_rule_index = disable_rule_index

    let summarize_from_ast = summarize_from_ast 
    let dump_summary = dump_summary 

    let update_file = update_file 
    
    let modify_pos_of_rules = modify_pos_of_rules  
    let modify_pos_of_init_states = modify_pos_of_init_states 
    let  add_rule = add_rule 
    let  add_init = add_init 
    
  end
