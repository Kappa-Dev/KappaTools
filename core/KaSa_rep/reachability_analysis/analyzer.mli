(*
  * analyzer.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Antique, INRIA Paris
  *
  * Creation: 2016, the 30th of January
  * Last modification: Time-stamp: <Dec 20 2016>
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

(** Analyzer entry point *)

module type Analyzer = sig
  type static_information
  type dynamic_information

  val main :
    Remanent_parameters_sig.parameters ->
    StoryProfiling.StoryStats.log_info ->
    Exception.exceptions_caught_and_uncaught ->
    Ckappa_sig.Views_bdu.handler ->
    Cckappa_sig.compil ->
    Cckappa_sig.kappa_handler ->
    Exception.exceptions_caught_and_uncaught
    * StoryProfiling.StoryStats.log_info
    * static_information
    * dynamic_information

  val export :
    static_information ->
    dynamic_information ->
    Exception.exceptions_caught_and_uncaught ->
    ('static, 'dynamic) Analyzer_headers.kasa_state ->
    Exception.exceptions_caught_and_uncaught
    * dynamic_information
    * ('static, 'dynamic) Analyzer_headers.kasa_state

  val print :
    static_information ->
    dynamic_information ->
    Exception.exceptions_caught_and_uncaught ->
    Loggers.t ->
    Exception.exceptions_caught_and_uncaught * dynamic_information

  val maybe_reachable :
    static_information ->
    dynamic_information ->
    Exception.exceptions_caught_and_uncaught ->
    Analyzer_headers.pattern_matching_flag ->
    Cckappa_sig.mixture ->
    Exception.exceptions_caught_and_uncaught * dynamic_information * bool
end

module Make : functor (Domain : Composite_domain.Composite_domain) -> Analyzer
