(*
  * analyzer.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Antique, INRIA Paris
  *
  * Creation: 2016, the 30th of January
  * Last modification:
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

(** Analyzer entry point *)

module type Analyzer =
  sig

    type static_information
    type dynamic_information

    val main:
      Remanent_parameters_sig.parameters ->
      Exception.method_handler ->
      Mvbdu_wrapper.Mvbdu.handler ->
      Cckappa_sig.compil ->
      Cckappa_sig.kappa_handler ->
      Exception.method_handler * static_information * dynamic_information

    val export:
      static_information ->
      dynamic_information ->
      Exception.method_handler ->
      Analyzer_headers.kasa_state ->
      Exception.method_handler * dynamic_information * Analyzer_headers.kasa_state

    val print:
      static_information ->
      dynamic_information ->
      Exception.method_handler ->
      Loggers.t list ->
      Exception.method_handler * dynamic_information

  end

module Make : functor (Domain:Composite_domain.Composite_domain) -> Analyzer
