(**
  * remanent_state.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: June, the 25th of 2016
  * Last modification: June, the 25th of 2016
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type init = Compil of ((string Location.annot) * Ast.port list, Ast.mixture, string, Ast.rule) Ast.compil | Files of string list
type accuracy_level = Low | Medium | High | Full

module AccuracyMap: SetMap.Map with type elt = accuracy_level

type internal_contact_map = Cckappa_sig.kappa_handler

type contact_map =
  ((string list) * (string*string) list) Mods.StringMap.t Mods.StringMap.t

type quark_map = Quark_type.quarks

type rule_id = int
type var_id =  int

type compilation = ((string Location.annot) * Ast.port list, Ast.mixture, string, Ast.rule) Ast.compil

type refined_compilation = (Ckappa_sig.agent, Ckappa_sig.mixture, string, Ckappa_sig.direction * Ckappa_sig.mixture Ckappa_sig.rule) Ast.compil

type influence_node =
  | Rule of rule_id
  | Var of var_id

module InfluenceNodeMap: SetMap.Map with type elt = influence_node

type location =
  | Direct of int
  | Side_effect of int

type 'a pair = 'a * 'a

type influence_map =
  {
    positive: location pair list InfluenceNodeMap.t InfluenceNodeMap.t ;
    negative: location pair list InfluenceNodeMap.t InfluenceNodeMap.t ;
  }

type internal_influence_map =
  Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t * Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t

type state

val create_state:
  ?errors:Exception.method_handler -> Remanent_parameters_sig.parameters -> init -> state

val set_parameters: Remanent_parameters_sig.parameters -> state -> state
val get_parameters: state -> Remanent_parameters_sig.parameters
val add_event: StoryProfiling.step_kind -> (unit -> int) option -> state -> state
val close_event: StoryProfiling.step_kind -> (unit -> int) option -> state -> state
val get_init: state -> init
val set_compilation: compilation -> state -> state
val get_compilation: state -> compilation option
val set_handler: Cckappa_sig.kappa_handler -> state -> state
val get_handler: state -> Cckappa_sig.kappa_handler option
val set_refined_compil: refined_compilation -> state -> state
val get_refined_compil:
  state
  -> refined_compilation option
val set_c_compil: Cckappa_sig.compil -> state -> state
val get_c_compil: state -> Cckappa_sig.compil option
val get_errors: state -> Exception.method_handler
val set_errors: Exception.method_handler -> state -> state

val set_internal_contact_map: accuracy_level -> internal_contact_map -> state -> state
val get_internal_contact_map: accuracy_level -> state -> internal_contact_map option
val set_contact_map: accuracy_level -> contact_map -> state -> state
val get_contact_map: accuracy_level -> state -> contact_map option
val set_signature: Signature.s -> state -> state
val get_signature: state -> Signature.s option
val set_quark_map: quark_map -> state -> state
val get_quark_map: state -> quark_map option
val set_internal_influence_map: accuracy_level -> internal_influence_map -> state -> state
val get_internal_influence_map: accuracy_level -> state -> internal_influence_map option
val set_influence_map: accuracy_level -> influence_map -> state -> state
val get_influence_map: accuracy_level -> state -> influence_map option
val get_influence_map_map: state -> influence_map AccuracyMap.t
val get_contact_map_map: state -> contact_map AccuracyMap.t
val get_log_info: state -> StoryProfiling.StoryStats.log_info
