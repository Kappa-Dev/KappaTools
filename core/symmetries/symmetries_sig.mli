(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Abstract domain to record relations between pair of sites in
    connected agents. *)

type 'a site_partition = {
  over_binding_states: 'a list list;
  over_internal_states: 'a list list;
  over_full_states: 'a list list;
}

val empty : 'a site_partition
val map : ('a -> 'b) -> 'a site_partition -> 'b site_partition
val clean : 'a site_partition -> 'a site_partition

val print :
  Loggers.t ->
  ('agent -> Format.formatter -> 'site -> unit) ->
  (Format.formatter -> 'agent -> unit) ->
  'agent ->
  'site site_partition ->
  unit
