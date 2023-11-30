(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Our own implementattion of Set and Map

    Purely functionnal.
    Functions without _with_logs do NOT raise any exception.*)

module type OrderedType = sig
  type t

  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
end

type ('parameters, 'error, 'a) with_log_wrap =
  ('parameters -> 'error -> string -> string option -> exn -> 'error) ->
  'parameters ->
  'error ->
  'a

module type Set = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val singleton : elt -> t
  val is_singleton : t -> bool
  val add : elt -> t -> t

  val add_with_logs :
    ('parameters, 'error, elt -> t -> 'error * t) with_log_wrap

  val remove : elt -> t -> t

  val add_while_testing_freshness :
    ('parameters, 'error, elt -> t -> 'error * bool * t) with_log_wrap

  val remove_while_testing_existence :
    ('parameters, 'error, elt -> t -> 'error * bool * t) with_log_wrap

  val remove_with_logs :
    ('parameters, 'error, elt -> t -> 'error * t) with_log_wrap

  val split : elt -> t -> t * bool * t
  val union : t -> t -> t
  val disjoint_union : t -> t -> t option
  val inter : t -> t -> t

  val minus : t -> t -> t
  (** [minus a b] contains elements of [a] that are not in [b] *)

  val diff : t -> t -> t
  (** [diff a b] = [minus (union a b) (inter a b)] *)

  val minus_with_logs :
    ('parameters, 'error, t -> t -> 'error * t) with_log_wrap

  val union_with_logs :
    ('parameters, 'error, t -> t -> 'error * t) with_log_wrap

  val disjoint_union_with_logs :
    ('parameters, 'error, t -> t -> 'error * t) with_log_wrap

  val inter_with_logs :
    ('parameters, 'error, t -> t -> 'error * t) with_log_wrap

  val diff_with_logs : ('parameters, 'error, t -> t -> 'error * t) with_log_wrap
  val size : t -> int
  val mem : elt -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t

  val filter_with_logs :
    ('parameters, 'error, (elt -> bool) -> t -> 'error * t) with_log_wrap

  val for_all : (elt -> bool) -> t -> bool
  val partition : (elt -> bool) -> t -> t * t

  val partition_with_logs :
    ('parameters, 'error, (elt -> bool) -> t -> 'error * t * t) with_log_wrap

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_inv : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val elements : t -> elt list
  val print : Format.formatter -> t -> unit
  val choose : t -> elt option
  val random : Random.State.t -> t -> elt option
  val min_elt : t -> elt option
  val max_elt : t -> elt option
end

module type Map = sig
  type elt
  type set
  type +'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val size : 'a t -> int
  val root : 'a t -> (elt * 'a) option
  val max_key : 'a t -> elt option
  val add : elt -> 'a -> 'a t -> 'a t
  val remove : elt -> 'a t -> 'a t

  val add_while_testing_freshness :
    ( 'parameters,
      'error,
      elt -> 'a -> 'a t -> 'error * bool * 'a t )
    with_log_wrap

  val remove_while_testing_existence :
    ('parameters, 'error, elt -> 'a t -> 'error * bool * 'a t) with_log_wrap

  val pop : elt -> 'a t -> 'a option * 'a t
  val merge : 'a t -> 'a t -> 'a t
  val min_elt : 'a t -> (elt * 'a) option
  val find_option : elt -> 'a t -> 'a option
  val find_default : 'a -> elt -> 'a t -> 'a

  val find_option_with_logs :
    ('parameters, 'error, elt -> 'a t -> 'error * 'a option) with_log_wrap

  val find_default_with_logs :
    ('parameters, 'error, 'a -> elt -> 'a t -> 'error * 'a) with_log_wrap

  val mem : elt -> 'a t -> bool
  val diff : 'a t -> 'a t -> 'a t * 'a t
  val union : 'a t -> 'a t -> 'a t
  val update : 'a t -> 'a t -> 'a t
  val diff_pred : ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t * 'a t

  val add_with_logs :
    ('parameters, 'error, elt -> 'a -> 'a t -> 'error * 'a t) with_log_wrap

  val remove_with_logs :
    ('parameters, 'error, elt -> 'a t -> 'error * 'a t) with_log_wrap

  val join_with_logs :
    ( 'parameters,
      'error,
      'a t -> elt -> 'a -> 'a t -> 'error * 'a t )
    with_log_wrap

  val split_with_logs :
    ( 'parameters,
      'error,
      elt -> 'a t -> 'error * ('a t * 'a option * 'a t) )
    with_log_wrap

  val update_with_logs :
    ('parameters, 'error, 'a t -> 'a t -> 'error * 'a t) with_log_wrap

  val map2_with_logs :
    ( 'parameters,
      'error,
      ('parameters -> 'error -> 'a -> 'error * 'c) ->
      ('parameters -> 'error -> 'b -> 'error * 'c) ->
      ('parameters -> 'error -> 'a -> 'b -> 'error * 'c) ->
      'a t ->
      'b t ->
      'error * 'c t )
    with_log_wrap

  val map2z_with_logs :
    ( 'parameters,
      'error,
      ('parameters -> 'error -> 'a -> 'a -> 'error * 'a) ->
      'a t ->
      'a t ->
      'error * 'a t )
    with_log_wrap

  val fold2z_with_logs :
    ( 'parameters,
      'error,
      ('parameters -> 'error -> elt -> 'a -> 'b -> 'c -> 'error * 'c) ->
      'a t ->
      'b t ->
      'c ->
      'error * 'c )
    with_log_wrap

  val fold2_with_logs :
    ( 'parameters,
      'error,
      ('parameters -> 'error -> elt -> 'a -> 'c -> 'error * 'c) ->
      ('parameters -> 'error -> elt -> 'b -> 'c -> 'error * 'c) ->
      ('parameters -> 'error -> elt -> 'a -> 'b -> 'c -> 'error * 'c) ->
      'a t ->
      'b t ->
      'c ->
      'error * 'c )
    with_log_wrap

  val fold2_sparse_with_logs :
    ( 'parameters,
      'error,
      ('parameters -> 'error -> elt -> 'a -> 'b -> 'c -> 'error * 'c) ->
      'a t ->
      'b t ->
      'c ->
      'error * 'c )
    with_log_wrap

  val iter2_sparse_with_logs :
    ( 'parameters,
      'error,
      ('parameters -> 'error -> elt -> 'a -> 'b -> 'error) ->
      'a t ->
      'b t ->
      'error )
    with_log_wrap

  val diff_with_logs :
    ('parameters, 'error, 'a t -> 'a t -> 'error * 'a t * 'a t) with_log_wrap

  val diff_pred_with_logs :
    ( 'parameters,
      'error,
      ('a -> 'a -> bool) -> 'a t -> 'a t -> 'error * 'a t * 'a t )
    with_log_wrap

  val merge_with_logs :
    ('parameters, 'error, 'a t -> 'a t -> 'error * 'a t) with_log_wrap

  val union_with_logs :
    ('parameters, 'error, 'a t -> 'a t -> 'error * 'a t) with_log_wrap

  val fold_restriction_with_logs :
    ( 'parameters,
      'error,
      (elt -> 'a -> 'error * 'b -> 'error * 'b) ->
      set ->
      'a t ->
      'b ->
      'error * 'b )
    with_log_wrap

  val fold_restriction_with_missing_associations_with_logs :
    ( 'parameters,
      'error,
      (elt -> 'a -> 'error * 'b -> 'error * 'b) ->
      (elt -> 'error * 'b -> 'error * 'b) ->
      set ->
      'a t ->
      'b ->
      'error * 'b )
    with_log_wrap

  val iter : (elt -> 'a -> unit) -> 'a t -> unit
  val fold : (elt -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val fold_with_interruption :
    (elt -> 'a -> 'b -> ('b, 'c) Stop.stop) -> 'a t -> 'b -> ('b, 'c) Stop.stop

  val monadic_fold2 :
    'parameters ->
    'method_handler ->
    ('parameters ->
    'method_handler ->
    elt ->
    'a ->
    'b ->
    'c ->
    'method_handler * 'c) ->
    ('parameters -> 'method_handler -> elt -> 'a -> 'c -> 'method_handler * 'c) ->
    ('parameters -> 'method_handler -> elt -> 'b -> 'c -> 'method_handler * 'c) ->
    'a t ->
    'b t ->
    'c ->
    'method_handler * 'c

  val monadic_fold2_sparse :
    'parameters ->
    'method_handler ->
    ('parameters ->
    'method_handler ->
    elt ->
    'a ->
    'b ->
    'c ->
    'method_handler * 'c) ->
    'a t ->
    'b t ->
    'c ->
    'method_handler * 'c

  val monadic_iter2_sparse :
    'parameters ->
    'method_handler ->
    ('parameters -> 'method_handler -> elt -> 'a -> 'b -> 'method_handler) ->
    'a t ->
    'b t ->
    'method_handler

  val monadic_fold_restriction :
    'parameters ->
    'method_handler ->
    ('parameters -> 'method_handler -> elt -> 'a -> 'b -> 'method_handler * 'b) ->
    set ->
    'a t ->
    'b ->
    'method_handler * 'b

  val mapi : (elt -> 'a -> 'b) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val for_all : (elt -> 'a -> bool) -> 'a t -> bool
  val filter_one : (elt -> 'a -> bool) -> 'a t -> (elt * 'a) option
  (* returns an element that respects the predicate (if any) *)

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val bindings : 'a t -> (elt * 'a) list

  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val of_json :
    ?lab_key:string ->
    ?lab_value:string ->
    ?error_msg:string ->
    (Yojson.Basic.t -> elt) ->
    (Yojson.Basic.t -> 'value) ->
    Yojson.Basic.t ->
    'value t

  val to_json :
    ?lab_key:string ->
    ?lab_value:string ->
    (elt -> Yojson.Basic.t) ->
    ('value -> Yojson.Basic.t) ->
    'value t ->
    Yojson.Basic.t
end

module type S = sig
  type elt

  module Set : Set with type elt = elt
  module Map : Map with type elt = elt and type set = Set.t
end

module Make (Ord : OrderedType) : S with type elt = Ord.t

module type Projection = sig
  type elt_a
  type elt_b
  type 'a map_a
  type 'a map_b
  type set_a
  type set_b

  val proj_map :
    (elt_a -> elt_b) -> 'b -> ('b -> 'a -> 'b) -> 'a map_a -> 'b map_b
  (** proj_map f init merge map is a map mapping each element b
      to the result of the itteration of the function merge over the image in map of the element a in f such that f(a)=b, starting with the element init. *)

  val proj_map_monadic :
    'parameters ->
    'method_handler ->
    (elt_a -> elt_b) ->
    'b ->
    ('parameters -> 'method_handler -> 'b -> 'a -> 'method_handler * 'b) ->
    'a map_a ->
    'method_handler * 'b map_b

  val proj_set : (elt_a -> elt_b) -> set_a -> set_b
  (** proj_set f set is the set \{f(a) | a\in S\} *)

  val proj_set_monadic :
    'parameters ->
    'method_handler ->
    ('parameters -> 'method_handler -> elt_a -> 'method_handler * elt_b) ->
    set_a ->
    'method_handler * set_b

  val partition_set : (elt_a -> elt_b) -> set_a -> set_a map_b
  (** partition_set f set is the map mapping any element b with an antecedent for f in the set set, into the set of its antecedents, ie
      to the set \{a\in set | f(a)=b\}. *)

  val partition_set_monadic :
    'parameters ->
    'method_handler ->
    ('parameters -> 'method_handler -> elt_a -> 'method_handler * elt_b) ->
    set_a ->
    'method_handler * set_a map_b
end

module Proj (A : S) (B : S) :
  Projection
    with type elt_a = A.elt
     and type elt_b = B.elt
     and type 'a map_a = 'a A.Map.t
     and type 'a map_b = 'a B.Map.t

module type Projection2 = sig
  type elt_a
  type elt_b
  type elt_c
  type 'a map_a
  type 'a map_b
  type 'a map_c

  val proj2 :
    (elt_a -> elt_b) ->
    (elt_a -> elt_c) ->
    'b ->
    ('b -> 'a -> 'b) ->
    'a map_a ->
    'b map_c map_b

  val proj2_monadic :
    'parameters ->
    'method_handler ->
    (elt_a -> elt_b) ->
    (elt_a -> elt_c) ->
    'b ->
    ('parameters -> 'method_handler -> 'b -> 'a -> 'method_handler * 'b) ->
    'a map_a ->
    'method_handler * 'b map_c map_b
end

module Proj2 (A : S) (B : S) (C : S) :
  Projection2
    with type elt_a = A.elt
     and type elt_b = B.elt
     and type 'a map_a = 'a A.Map.t
     and type 'a map_b = 'a B.Map.t
     and type elt_c = C.elt
     and type 'a map_c = 'a C.Map.t
