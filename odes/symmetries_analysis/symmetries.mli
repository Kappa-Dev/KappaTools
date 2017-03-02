(**
   * symmetries.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 5th of December
   * Last modification: Time-stamp: <Mar 02 2017>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

(***************************************************************************)
(*TYPE*)
(***************************************************************************)

type symmetries = ((int list list) * (int list list)) array

(***************************************************************************)
(*PARTITION THE CONTACT MAP*)
(***************************************************************************)

val detect_symmetries:
  Remanent_parameters_sig.parameters -> Model.t -> LKappa_auto.cache  ->
  (LKappa_auto.RuleCache.hashed_list * LKappa.rule) list ->
  (bool array  * int array * ('a, 'b) Alg_expr.e Locality.annot Rule_modes.RuleModeMap.t array * int array) ->
  (string list * (string * string) list) Mods.StringMap.t Mods.StringMap.t ->
  LKappa_auto.cache * symmetries

val build_array_for_symmetries:
  LKappa_auto.RuleCache.hashed_list list ->
  bool array * int array * 'a Rule_modes.RuleModeMap.t array * int array

val print_cannonic_form_from_syntactic_rules:
  compil:'a ->
  Loggers.t ->
  (compil:'a -> Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'c -> unit) ->
  'c ->
  'b ->
  int ->
  LKappa_auto.RuleCache.hashed_list ->
  (Alg_expr.t * 'd) option list ->
  unit

val print_symmetries:
  Remanent_parameters_sig.parameters -> Model.t -> symmetries -> unit 
type cache
val empty_cache: unit -> cache
val representant: cache -> symmetries -> Pattern.cc ->  cache * Pattern.cc
