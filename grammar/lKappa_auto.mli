(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type cache

module CannonicCache : Hashed_list.Hash

module CannonicSet_and_map : SetMap.S with type elt = CannonicCache.hashed_list

module CannonicMap : SetMap.Map with type elt = CannonicCache.hashed_list

module RuleCache : Hashed_list.Hash

val init_cache: unit -> cache

val mixture_to_species_map : Remanent_parameters_sig.rate_convention -> cache ->
  LKappa.rule -> cache * (int * int) CannonicMap.t

val nauto: Remanent_parameters_sig.rate_convention -> cache ->
  LKappa.rule -> cache * int

val n_cc: cache -> LKappa.rule -> cache * int

val cannonic_form: cache -> LKappa.rule ->
  cache * RuleCache.hashed_list


type bwd_bisim_info =
  int Symmetries_sig.site_partition array * bool Mods.DynArray.t * Signature.s * (cache ref)

val swap_binding_state_regular : int -> int -> int -> LKappa.rule_agent -> unit
val swap_internal_state_regular : int -> int -> int -> LKappa.rule_agent -> unit
val swap_full_regular : int -> int -> int -> LKappa.rule_agent -> unit
val swap_binding_state_created : int -> int -> int -> Raw_mixture.agent -> unit
val swap_internal_state_created : int -> int -> int -> Raw_mixture.agent -> unit
val swap_full_created : int -> int -> int -> Raw_mixture.agent -> unit

val equiv_class:
  cache ->
  bool Mods.DynArray.t ->
  LKappa.rule ->
  partitions_internal_states:(int -> int list list) ->
  partitions_binding_states:(int -> int list list) ->
  partitions_full_states:(int -> int list list) ->
  convention:Remanent_parameters_sig.rate_convention ->
  cache * bool Mods.DynArray.t * (LKappa.rule * int) list

val saturate_domain_with_symmetric_patterns:
  compileModeOn:bool -> ?origin:Operator.rev_dep -> Contact_map.t ->
  bwd_bisim_info -> (('a * Pattern.cc) array * 'b) list ->
  Pattern.PreEnv.t -> Pattern.PreEnv.t
