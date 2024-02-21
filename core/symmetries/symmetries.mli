(**
   * symmetries.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 5th of December
   * Last modification: Time-stamp: <Jul 18 2017>
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

type equivalence_classes = int Symmetries_sig.site_partition array

type symmetries = {
  rules: equivalence_classes;
  rules_and_initial_states: equivalence_classes option;
  rules_and_alg_expr: equivalence_classes option;
}

type reduction =
  | Ground
  | Forward of equivalence_classes
  | Backward of equivalence_classes

(*******************************************************************)
(*PARTITION THE CONTACT MAP*)
(*******************************************************************)

val refine_partitioned_contact_map_in_lkappa_representation :
  'a ->
  ('a -> int -> 'b -> 'b -> 'a * bool) ->
  ('a -> int -> 'b -> 'b -> 'a * bool) ->
  ('a -> int -> 'b -> 'b -> 'a * bool) ->
  'b Symmetries_sig.site_partition array ->
  'a * 'b Symmetries_sig.site_partition array

val detect_symmetries :
  Remanent_parameters_sig.parameters ->
  Model.t ->
  LKappa_auto.cache ->
  Remanent_parameters_sig.rate_convention ->
  Pattern.cc list ->
  Primitives.elementary_rule list ->
  Public_data.contact_map ->
  LKappa_auto.cache * symmetries

val print_symmetries :
  Remanent_parameters_sig.parameters -> Model.t -> symmetries -> unit

type cache

val empty_cache : unit -> cache

val representative :
  ?parameters:Remanent_parameters_sig.parameters ->
  sigs:Signature.s ->
  cache ->
  LKappa_auto.cache ->
  Pattern.PreEnv.t ->
  reduction ->
  Pattern.cc ->
  cache * LKappa_auto.cache * Pattern.PreEnv.t * Pattern.cc

val equiv_class :
  ?parameters:Remanent_parameters_sig.parameters ->
  Model.t ->
  bool Mods.DynArray.t ->
  cache ->
  LKappa_auto.cache ->
  Pattern.PreEnv.t ->
  reduction ->
  Pattern.id ->
  cache
  * LKappa_auto.cache
  * Pattern.PreEnv.t
  * bool Mods.DynArray.t
  * (int * (Pattern.id * int) list)
