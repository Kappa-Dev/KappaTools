(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type cache

module CannonicCache : Hashed_list.Hash
module CannonicSet_and_map : SetMap.S with type elt = CannonicCache.hashed_list
module CannonicMap : SetMap.Map with type elt = CannonicCache.hashed_list
module RuleCache : Hashed_list.Hash

val init_cache : unit -> cache

val mixture_to_species_map :
  Remanent_parameters_sig.rate_convention ->
  cache ->
  LKappa.rule ->
  cache * (int * int) CannonicMap.t

val nauto :
  Remanent_parameters_sig.rate_convention -> cache -> LKappa.rule -> cache * int

val n_cc : cache -> LKappa.rule -> cache * int
val cannonic_form : cache -> LKappa.rule -> cache * RuleCache.hashed_list
