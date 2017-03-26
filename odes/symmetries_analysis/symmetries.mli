(**
   * symmetries.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 5th of December
   * Last modification: Time-stamp: <Mar 26 2017>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

(*******************************************************************)
(*TYPE*)
(*******************************************************************)

type symmetries =
  {
    rules : int Symmetries_sig.site_partition array;
    rules_and_initial_states :
      int Symmetries_sig.site_partition array option;
    rules_and_alg_expr :
      int Symmetries_sig.site_partition array option;
  }

(*******************************************************************)
(*PARTITION THE CONTACT MAP*)
(*******************************************************************)

val detect_symmetries:
  Remanent_parameters_sig.parameters ->
  Model.t ->
  LKappa_auto.cache ->
  Remanent_parameters_sig.rate_convention ->
  Pattern.cc list ->
  Primitives.elementary_rule list ->
  (string list * (string * string) list) Mods.StringMap.t
    Mods.StringMap.t -> LKappa_auto.cache * symmetries

val print_symmetries:
  Remanent_parameters_sig.parameters -> Model.t -> symmetries -> unit

type cache

val empty_cache: unit -> cache

val representant:
  ?parameters:Remanent_parameters_sig.parameters ->
  Signature.s -> cache -> LKappa_auto.cache -> Pattern.PreEnv.t -> symmetries -> Pattern.cc
  ->  cache * LKappa_auto.cache * Pattern.PreEnv.t * Pattern.cc
