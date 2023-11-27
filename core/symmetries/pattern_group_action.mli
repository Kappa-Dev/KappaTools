(**
   * pattern_group_action.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 5th of December
   * Last modification: Time-stamp: <May 13 2017>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

val normalize_species :
  ?parameters:Remanent_parameters_sig.parameters ->
  sigs:Signature.s ->
  LKappa_auto.cache ->
  Pattern.PreEnv.t ->
  int Symmetries_sig.site_partition array ->
  Pattern.cc ->
  LKappa_auto.cache * Pattern.PreEnv.t * Pattern.cc

val is_pattern_invariant_internal_states_permutation :
  ?parameters:Remanent_parameters_sig.parameters ->
  env:Model.t ->
  agent_type:int ->
  site1:int ->
  site2:int ->
  Pattern.id ->
  LKappa_auto.cache ->
  LKappa_auto.cache * bool

val is_pattern_invariant_binding_states_permutation :
  ?parameters:Remanent_parameters_sig.parameters ->
  env:Model.t ->
  agent_type:int ->
  site1:int ->
  site2:int ->
  Pattern.id ->
  LKappa_auto.cache ->
  LKappa_auto.cache * bool

val is_pattern_invariant_full_states_permutation :
  ?parameters:Remanent_parameters_sig.parameters ->
  env:Model.t ->
  agent_type:int ->
  site1:int ->
  site2:int ->
  Pattern.id ->
  LKappa_auto.cache ->
  LKappa_auto.cache * bool

val equiv_class_of_a_pattern :
  ?parameters:Remanent_parameters_sig.parameters ->
  env:Model.t ->
  partitions_internal_states:(int -> int list list) ->
  partitions_binding_states:(int -> int list list) ->
  partitions_full_states:(int -> int list list) ->
  LKappa_auto.cache ->
  Pattern.PreEnv.t ->
  bool Mods.DynArray.t ->
  Pattern.id ->
  LKappa_auto.cache
  * Pattern.PreEnv.t
  * bool Mods.DynArray.t
  * (Pattern.id * int) list

val equiv_class_of_a_species :
  ?parameters:Remanent_parameters_sig.parameters ->
  sigs:Signature.s ->
  partitions_internal_states:(int -> int list list) ->
  partitions_binding_states:(int -> int list list) ->
  partitions_full_states:(int -> int list list) ->
  LKappa_auto.cache ->
  Pattern.PreEnv.t ->
  bool Mods.DynArray.t ->
  Pattern.cc ->
  LKappa_auto.cache * Pattern.PreEnv.t * bool Mods.DynArray.t * Pattern.cc list
