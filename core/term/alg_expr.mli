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
      * ('mix, 'id) e Locality.annoted
      * ('mix, 'id) e Locality.annoted
  | UN_ALG_OP of Operator.un_alg_op * ('mix, 'id) e Locality.annoted
  | STATE_ALG_OP of Operator.state_alg_op
  | ALG_VAR of 'id
  | KAPPA_INSTANCE of 'mix
  | TOKEN_ID of 'id
  | CONST of Nbr.t
  | IF of
      ('mix, 'id) bool Locality.annoted
      * ('mix, 'id) e Locality.annoted
      * ('mix, 'id) e Locality.annoted
  | DIFF_TOKEN of (('mix, 'id) e Locality.annoted * 'id)
  | DIFF_KAPPA_INSTANCE of (('mix, 'id) e Locality.annoted * 'mix)

and ('mix, 'id) bool =
  | TRUE
  | FALSE
  | BIN_BOOL_OP of
      Operator.bin_bool_op
      * ('mix, 'id) bool Locality.annoted
      * ('mix, 'id) bool Locality.annoted
  | UN_BOOL_OP of Operator.un_bool_op * ('mix, 'id) bool Locality.annoted
  | COMPARE_OP of
      Operator.compare_op
      * ('mix, 'id) e Locality.annoted
      * ('mix, 'id) e Locality.annoted

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

val const : Nbr.t -> ('a, 'b) e Locality.annoted
(** {2 Smart constructor } *)

val int : int -> ('a, 'b) e Locality.annoted
val float : float -> ('a, 'b) e Locality.annoted

val add :
  ('a, 'b) e Locality.annoted ->
  ('a, 'b) e Locality.annoted ->
  ('a, 'b) e Locality.annoted

val minus :
  ('a, 'b) e Locality.annoted ->
  ('a, 'b) e Locality.annoted ->
  ('a, 'b) e Locality.annoted

val mult :
  ('a, 'b) e Locality.annoted ->
  ('a, 'b) e Locality.annoted ->
  ('a, 'b) e Locality.annoted

val div :
  ('a, 'b) e Locality.annoted ->
  ('a, 'b) e Locality.annoted ->
  ('a, 'b) e Locality.annoted

val pow :
  ('a, 'b) e Locality.annoted ->
  ('a, 'b) e Locality.annoted ->
  ('a, 'b) e Locality.annoted

val ln : ('a, 'b) e Locality.annoted -> ('a, 'b) e Locality.annoted
val uminus : ('a, 'b) e Locality.annoted -> ('a, 'b) e Locality.annoted
val sin : ('a, 'b) e Locality.annoted -> ('a, 'b) e Locality.annoted
val cos : ('a, 'b) e Locality.annoted -> ('a, 'b) e Locality.annoted
val sqrt : ('a, 'b) e Locality.annoted -> ('a, 'b) e Locality.annoted

val add_dep :
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array ->
  Operator.rev_dep ->
  ('a, int) e Locality.annoted ->
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
  ('a, int) bool Locality.annoted ->
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array

val setup_alg_vars_rev_dep :
  unit NamedDecls.t ->
  (string Locality.annoted * ('a, int) e Locality.annoted) array ->
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array

val has_mix : ?var_decls:('b -> ('c, 'b) e) -> ('a, 'b) e -> pervasives_bool
val is_constant : ('a, 'b) e Locality.annoted -> pervasives_bool

val is_time_homogeneous : ('a, 'b) e Locality.annoted -> pervasives_bool
(** does not take into account symbolic propagation of expression *)

val has_progress_dep :
  only_time:pervasives_bool ->
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array ->
  ('a, int) e Locality.annoted ->
  pervasives_bool

val extract_connected_components : ('a, 'b) e Locality.annoted -> 'a list
val extract_connected_components_bool : ('a, 'b) bool Locality.annoted -> 'a list

val propagate_constant :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  ?max_time:float ->
  ?max_events:int ->
  int list ->
  (string Locality.annoted * ('a, int) e Locality.annoted) array ->
  ('a, int) e Locality.annoted ->
  ('a, int) e Locality.annoted

val propagate_constant_bool :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  ?max_time:float ->
  ?max_events:int ->
  int list ->
  (string Locality.annoted * ('a, int) e Locality.annoted) array ->
  ('a, int) bool Locality.annoted ->
  ('a, int) bool Locality.annoted

val is_equality_test_time :
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array ->
  ('a, int) bool ->
  pervasives_bool

val map_on_mixture :
  ('a -> ('c, 'b) e) -> ('a, 'b) e Locality.annoted -> ('c, 'b) e Locality.annoted

val map_bool_on_mixture :
  ('a -> ('c, 'b) e) ->
  ('a, 'b) bool Locality.annoted ->
  ('c, 'b) bool Locality.annoted

val fold_on_mixture : ('a -> 'b -> 'a) -> 'a -> ('b, 'c) e Locality.annoted -> 'a

val fold_bool_on_mixture :
  ('a -> 'b -> 'a) -> 'a -> ('b, 'c) bool Locality.annoted -> 'a

val equal :
  ('a, 'b) e Locality.annoted -> ('a, 'b) e Locality.annoted -> pervasives_bool
(** Syntactic equality up to positions but not associativity and comutativity *)

val equal_bool :
  ('a, 'b) bool Locality.annoted ->
  ('a, 'b) bool Locality.annoted ->
  pervasives_bool
