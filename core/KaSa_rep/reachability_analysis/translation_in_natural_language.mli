(**
 * translation_in_natural_language.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: 2016
 * Last modification: Time-stamp: <Dec 22 2018>
 * *
 * Signature for prepreprocessing language ckappa
 *
 * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

type token =
  | Range of Ckappa_sig.c_site_name * Ckappa_sig.c_state list
  | Equiv of
      (Ckappa_sig.c_site_name * Ckappa_sig.c_state)
      * (Ckappa_sig.c_site_name * Ckappa_sig.c_state)
  | Imply of
      (Ckappa_sig.c_site_name * Ckappa_sig.c_state)
      * (Ckappa_sig.c_site_name * Ckappa_sig.c_state)
  | Partition of
      (Ckappa_sig.c_site_name * (Ckappa_sig.c_state * token list) list)
  | No_known_translation of
      (Ckappa_sig.c_site_name * Ckappa_sig.c_state) list list

type rename_sites =
  Remanent_parameters_sig.parameters ->
  Exception.exceptions_caught_and_uncaught ->
  Ckappa_sig.Site_map_and_set.Map.elt ->
  Exception.exceptions_caught_and_uncaught * Ckappa_sig.Site_map_and_set.Map.elt

val non_relational :
  Remanent_parameters_sig.parameters ->
  Ckappa_sig.Views_bdu.handler ->
  Exception.exceptions_caught_and_uncaught ->
  Ckappa_sig.Views_bdu.mvbdu ->
  Exception.exceptions_caught_and_uncaught * Ckappa_sig.Views_bdu.handler * bool

val translate :
  Remanent_parameters_sig.parameters ->
  Ckappa_sig.Views_bdu.handler ->
  Exception.exceptions_caught_and_uncaught ->
  rename_sites ->
  Ckappa_sig.Views_bdu.mvbdu ->
  Exception.exceptions_caught_and_uncaught
  * (Ckappa_sig.Views_bdu.handler * token)

val print :
  ?beginning_of_sentence:bool ->
  ?prompt_agent_type:bool ->
  ?html_mode:bool ->
  show_dep_with_dimmension_higher_than:int ->
  Remanent_parameters_sig.parameters ->
  Cckappa_sig.kappa_handler ->
  Exception.exceptions_caught_and_uncaught ->
  string ->
  Ckappa_sig.c_agent_name ->
  token ->
  Exception.exceptions_caught_and_uncaught

val convert_views_internal_constraints_list :
  show_dep_with_dimmension_higher_than:int ->
  Remanent_parameters_sig.parameters ->
  Cckappa_sig.kappa_handler ->
  Exception.exceptions_caught_and_uncaught ->
  string ->
  Ckappa_sig.c_agent_name ->
  token ->
  Site_graphs.KaSa_site_graph.t Public_data.lemma list ->
  Exception.exceptions_caught_and_uncaught
  * Site_graphs.KaSa_site_graph.t Public_data.lemma list

(*show_dep_with_dimmension_higher_than:int ->
  Remanent_parameters_sig.parameters ->
  Cckappa_sig.kappa_handler ->
  Exception.exceptions_caught_and_uncaught ->
  string ->
  Ckappa_sig.c_agent_name ->
  token ->
  (string *
   (string option *
    Site_graphs.KaSa_site_graph.binding_state option)
     Wrapped_modules.LoggedStringMap.t)
    list Remanent_state.lemma list ->
  Exception.exceptions_caught_and_uncaught *
  (string *
   (string option *
    Site_graphs.KaSa_site_graph.binding_state option)
     Wrapped_modules.LoggedStringMap.t)
    list Remanent_state.lemma list*)
