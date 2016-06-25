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

type accuracy_level = Low | Medium | High | Full

module AccuracyMap: SetMap.Map with type elt = accuracy_level

type contact_map =
  ((string list) * (string*string) list) Mods.StringMap.t Mods.StringMap.t

type rule_id = int
type var_id =  int
type compilation = Cckappa_sig.compil

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

type state

val create_state:
  Remanent_parameters_sig.parameters ->
  ((string Location.annot) * Ast.port list, Ast.mixture, string, Ast.rule) Ast.compil
  -> state

val get_parameters: state -> Remanent_parameters_sig.parameters
val get_compilation: state -> ((string Location.annot) * Ast.port list, Ast.mixture, string, Ast.rule) Ast.compil
val set_handler: Cckappa_sig.kappa_handler -> state -> state
val get_handler: state -> Cckappa_sig.kappa_handler option
val set_refined_compil:
  (Ckappa_sig.agent, Ckappa_sig.mixture, string,
   Ckappa_sig.direction * Ckappa_sig.mixture Ckappa_sig.rule)
    Ast.compil
  -> state
  -> state
val get_refined_compil:
  state
  -> (Ckappa_sig.agent, Ckappa_sig.mixture, string,
      Ckappa_sig.direction * Ckappa_sig.mixture Ckappa_sig.rule)
    Ast.compil option
val get_errors: state -> Exception.method_handler
val set_errors: Exception.method_handler -> state -> state
