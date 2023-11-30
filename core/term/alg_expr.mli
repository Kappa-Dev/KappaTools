(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type pervasives_bool = bool

type ('mix, 'id) e =
  | BIN_ALG_OP of
      Operator.bin_alg_op
      * ('mix, 'id) e Locality.annot
      * ('mix, 'id) e Locality.annot
  | UN_ALG_OP of Operator.un_alg_op * ('mix, 'id) e Locality.annot
  | STATE_ALG_OP of Operator.state_alg_op
  | ALG_VAR of 'id
  | KAPPA_INSTANCE of 'mix
  | TOKEN_ID of 'id
  | CONST of Nbr.t
  | IF of
      ('mix, 'id) bool Locality.annot
      * ('mix, 'id) e Locality.annot
      * ('mix, 'id) e Locality.annot
  | DIFF_TOKEN of (('mix, 'id) e Locality.annot * 'id)
  | DIFF_KAPPA_INSTANCE of (('mix, 'id) e Locality.annot * 'mix)

and ('mix, 'id) bool =
  | TRUE
  | FALSE
  | BIN_BOOL_OP of
      Operator.bin_bool_op
      * ('mix, 'id) bool Locality.annot
      * ('mix, 'id) bool Locality.annot
  | UN_BOOL_OP of Operator.un_bool_op * ('mix, 'id) bool Locality.annot
  | COMPARE_OP of
      Operator.compare_op
      * ('mix, 'id) e Locality.annot
      * ('mix, 'id) e Locality.annot

val e_to_yojson :
  filenames:int Mods.StringMap.t ->
  ('a -> Yojson.Basic.t) ->
  ('b -> Yojson.Basic.t) ->
  ('a, 'b) e ->
  Yojson.Basic.t

val e_of_yojson :
  filenames:string array ->
  (Yojson.Basic.t -> 'a) ->
  (Yojson.Basic.t -> 'b) ->
  Yojson.Basic.t ->
  ('a, 'b) e

val print :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  ('a, 'b) e ->
  unit

val bool_to_yojson :
  filenames:int Mods.StringMap.t ->
  ('a -> Yojson.Basic.t) ->
  ('b -> Yojson.Basic.t) ->
  ('a, 'b) bool ->
  Yojson.Basic.t

val bool_of_yojson :
  filenames:string array ->
  (Yojson.Basic.t -> 'a) ->
  (Yojson.Basic.t -> 'b) ->
  Yojson.Basic.t ->
  ('a, 'b) bool

val print_bool :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  ('a, 'b) bool ->
  unit

val const : Nbr.t -> ('a, 'b) e Locality.annot
(** {2 Smart constructor } *)

val int : int -> ('a, 'b) e Locality.annot
val float : float -> ('a, 'b) e Locality.annot

val add :
  ('a, 'b) e Locality.annot ->
  ('a, 'b) e Locality.annot ->
  ('a, 'b) e Locality.annot

val minus :
  ('a, 'b) e Locality.annot ->
  ('a, 'b) e Locality.annot ->
  ('a, 'b) e Locality.annot

val mult :
  ('a, 'b) e Locality.annot ->
  ('a, 'b) e Locality.annot ->
  ('a, 'b) e Locality.annot

val div :
  ('a, 'b) e Locality.annot ->
  ('a, 'b) e Locality.annot ->
  ('a, 'b) e Locality.annot

val pow :
  ('a, 'b) e Locality.annot ->
  ('a, 'b) e Locality.annot ->
  ('a, 'b) e Locality.annot

val ln : ('a, 'b) e Locality.annot -> ('a, 'b) e Locality.annot
val uminus : ('a, 'b) e Locality.annot -> ('a, 'b) e Locality.annot
val sin : ('a, 'b) e Locality.annot -> ('a, 'b) e Locality.annot
val cos : ('a, 'b) e Locality.annot -> ('a, 'b) e Locality.annot
val sqrt : ('a, 'b) e Locality.annot -> ('a, 'b) e Locality.annot

val add_dep :
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array ->
  Operator.rev_dep ->
  ('a, int) e Locality.annot ->
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array
(** depend in time, depend in event number, depend in given var *)

val add_dep_bool :
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array ->
  Operator.rev_dep ->
  ('a, int) bool Locality.annot ->
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array

val setup_alg_vars_rev_dep :
  unit NamedDecls.t ->
  (string Locality.annot * ('a, int) e Locality.annot) array ->
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array

val has_mix : ?var_decls:('b -> ('c, 'b) e) -> ('a, 'b) e -> pervasives_bool
val is_constant : ('a, 'b) e Locality.annot -> pervasives_bool

val is_time_homogeneous : ('a, 'b) e Locality.annot -> pervasives_bool
(** does not take into account symbolic propagation of expression *)

val has_progress_dep :
  only_time:pervasives_bool ->
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array ->
  ('a, int) e Locality.annot ->
  pervasives_bool

val extract_connected_components : ('a, 'b) e Locality.annot -> 'a list
val extract_connected_components_bool : ('a, 'b) bool Locality.annot -> 'a list

val propagate_constant :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  ?max_time:float ->
  ?max_events:int ->
  int list ->
  (string Locality.annot * ('a, int) e Locality.annot) array ->
  ('a, int) e Locality.annot ->
  ('a, int) e Locality.annot

val propagate_constant_bool :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  ?max_time:float ->
  ?max_events:int ->
  int list ->
  (string Locality.annot * ('a, int) e Locality.annot) array ->
  ('a, int) bool Locality.annot ->
  ('a, int) bool Locality.annot

val is_equality_test_time :
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array ->
  ('a, int) bool ->
  pervasives_bool

val map_on_mixture :
  ('a -> ('c, 'b) e) -> ('a, 'b) e Locality.annot -> ('c, 'b) e Locality.annot

val map_bool_on_mixture :
  ('a -> ('c, 'b) e) ->
  ('a, 'b) bool Locality.annot ->
  ('c, 'b) bool Locality.annot

val fold_on_mixture : ('a -> 'b -> 'a) -> 'a -> ('b, 'c) e Locality.annot -> 'a

val fold_bool_on_mixture :
  ('a -> 'b -> 'a) -> 'a -> ('b, 'c) bool Locality.annot -> 'a

val equal :
  ('a, 'b) e Locality.annot -> ('a, 'b) e Locality.annot -> pervasives_bool
(** Syntactic equality up to positions but not associativity and comutativity *)

val equal_bool :
  ('a, 'b) bool Locality.annot ->
  ('a, 'b) bool Locality.annot ->
  pervasives_bool
