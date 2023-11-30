(*
  * composite_domain.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 30th of January
  * Last modification: Time-stamp: <Aug 17 2018>
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

(** composite abstract domain (no longer with communications which have been internalized) *)

module type Composite_domain = sig
  type static_information
  type dynamic_information

  val initialize :
    Analyzer_headers.global_static_information ->
    Analyzer_headers.global_dynamic_information ->
    Exception.method_handler ->
    Exception.method_handler * static_information * dynamic_information

  type 'a zeroary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    Exception.method_handler * dynamic_information * 'a

  type ('a, 'b) unary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    Exception.method_handler * dynamic_information * 'b

  type ('a, 'b, 'c) binary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    'b ->
    Exception.method_handler * dynamic_information * 'c

  type ('a, 'b, 'c, 'd) ternary =
    static_information ->
    dynamic_information ->
    Exception.method_handler ->
    'a ->
    'b ->
    'c ->
    Exception.method_handler * dynamic_information * 'd

  val next_rule : Ckappa_sig.c_rule_id option zeroary
  val add_initial_state : (Analyzer_headers.initial_state, unit) unary

  val is_enabled :
    (Ckappa_sig.c_rule_id, Communication.precondition option) unary

  val apply_rule :
    (Ckappa_sig.c_rule_id, Communication.precondition, unit) binary

  val stabilize : unit zeroary

  val export :
    ( ('static, 'dynamic) Analyzer_headers.kasa_state,
      ('static, 'dynamic) Analyzer_headers.kasa_state )
    unary

  val print : (Loggers.t, unit) unary

  val maybe_reachable :
    ( Analyzer_headers.pattern_matching_flag,
      Cckappa_sig.mixture,
      Communication.precondition option )
    binary

  val get_global_dynamic_information :
    dynamic_information -> Analyzer_headers.global_dynamic_information

  val set_global_dynamic_information :
    Analyzer_headers.global_dynamic_information ->
    dynamic_information ->
    dynamic_information
end

module Make : functor (Domain : Analyzer_domain_sig.Domain) -> Composite_domain
