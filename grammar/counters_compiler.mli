(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val remove_variables_in_counters :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  debugMode:bool -> Ast.parsing_compil -> Ast.parsing_compil * bool

val build_created_counter :
  int -> int -> int -> Ast.counter_test Locality.annot option ->
  Raw_mixture.t -> (Raw_mixture.t * int)
(** [build_created_counter min_value nb_agents value_link_id value acc] *)

val build_erased_counter :
  int -> int -> (int * int) -> Ast.counter_test Locality.annot option ->
  LKappa.rule_mixture -> (LKappa.rule_mixture * int)
(** [build_erased_counter nb_agents value_link_id link_value_info test acc] *)

val build_maintained_counter :
  int -> int * int -> int -> Ast.counter_test Locality.annot option -> int ->
  LKappa.rule_mixture -> (LKappa.rule_mixture * int * bool)
(** [build_maintained_counter
    value_link_id link_value_info new_value_link_id test diff acc] *)
