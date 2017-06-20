(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  of_patterns : IntCollection.t Pattern.ObsMap.t;
  of_unary_patterns : Mods.IntSet.t Mods.IntMap.t Pattern.ObsMap.t;
}

type mod_ccs_cache = (int, unit) Hashtbl.t

val empty : Model.t -> t

val incorporate_extra_pattern : t -> Pattern.id -> IntCollection.t -> unit

val break_apart_cc : t -> Edges.t -> mod_ccs_cache -> (int * int) option -> t
  
val merge_cc : t -> mod_ccs_cache -> (int * int) option -> t

val update_roots :
  t -> bool -> Pattern.Set.t -> Edges.t ->
  mod_ccs_cache -> Pattern.Set.elt -> int -> unit

val number : t -> Pattern.id -> int

val debug_print : Format.formatter -> t -> unit