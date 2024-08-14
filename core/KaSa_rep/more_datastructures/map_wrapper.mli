(**
   Time-stamp: <Feb 22 2018>
*)

module type Set_with_logs = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val singleton : elt -> t
  val is_singleton : t -> bool

  val add :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    t ->
    Exception.exceptions_caught_and_uncaught * t

  val add_when_not_in :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    t ->
    Exception.exceptions_caught_and_uncaught * t

  val remove :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    t ->
    Exception.exceptions_caught_and_uncaught * t

  val minus :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    t ->
    t ->
    Exception.exceptions_caught_and_uncaught * t

  val union :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    t ->
    t ->
    Exception.exceptions_caught_and_uncaught * t

  val disjoint_union :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    t ->
    t ->
    Exception.exceptions_caught_and_uncaught * t

  val inter :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    t ->
    t ->
    Exception.exceptions_caught_and_uncaught * t

  val diff :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    t ->
    t ->
    Exception.exceptions_caught_and_uncaught * t

  val cardinal : t -> int
  val mem : elt -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val for_all : (elt -> bool) -> t -> bool
  val partition : (elt -> bool) -> t -> t * t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_inv : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val elements : t -> elt list
  val choose : t -> elt option
  val min_elt : t -> elt option
  val max_elt : t -> elt option
end

module type Map_with_logs = sig
  type elt
  type set
  type +'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val min_elt : 'a t -> (elt * 'a) option
  val mem : elt -> 'a t -> bool

  val find_option :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a option

  val find_default :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'a ->
    elt ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a

  val find_default_without_logs :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'a ->
    elt ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a

  val find_option_without_logs :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a option

  val add :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a t

  val overwrite :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a t

  val add_or_overwrite :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a t

  val remove :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a t

  val remove_or_not :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a t

  val update :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'a t ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a t

  val map2 :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'a ->
    Exception.exceptions_caught_and_uncaught * 'c) ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'b ->
    Exception.exceptions_caught_and_uncaught * 'c) ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'a ->
    'b ->
    Exception.exceptions_caught_and_uncaught * 'c) ->
    'a t ->
    'b t ->
    Exception.exceptions_caught_and_uncaught * 'c t

  val map2z :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'a ->
    'a ->
    Exception.exceptions_caught_and_uncaught * 'a) ->
    'a t ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a t

  val fold2z :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a ->
    'b ->
    'c ->
    Exception.exceptions_caught_and_uncaught * 'c) ->
    'a t ->
    'b t ->
    'c ->
    Exception.exceptions_caught_and_uncaught * 'c

  val fold2 :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a ->
    'c ->
    Exception.exceptions_caught_and_uncaught * 'c) ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'b ->
    'c ->
    Exception.exceptions_caught_and_uncaught * 'c) ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a ->
    'b ->
    'c ->
    Exception.exceptions_caught_and_uncaught * 'c) ->
    'a t ->
    'b t ->
    'c ->
    Exception.exceptions_caught_and_uncaught * 'c

  val fold2_sparse :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a ->
    'b ->
    'c ->
    Exception.exceptions_caught_and_uncaught * 'c) ->
    'a t ->
    'b t ->
    'c ->
    Exception.exceptions_caught_and_uncaught * 'c

  val iter2_sparse :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a ->
    'b ->
    Exception.exceptions_caught_and_uncaught) ->
    'a t ->
    'b t ->
    Exception.exceptions_caught_and_uncaught

  val diff :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'a t ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a t * 'a t

  val diff_pred :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    ('a -> 'a -> bool) ->
    'a t ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a t * 'a t

  val merge :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'a t ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a t

  val union :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'a t ->
    'a t ->
    Exception.exceptions_caught_and_uncaught * 'a t

  val fold_restriction :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    (elt ->
    'a ->
    Exception.exceptions_caught_and_uncaught * 'b ->
    Exception.exceptions_caught_and_uncaught * 'b) ->
    set ->
    'a t ->
    'b ->
    Exception.exceptions_caught_and_uncaught * 'b

  val fold_restriction_with_missing_associations :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    (elt ->
    'a ->
    Exception.exceptions_caught_and_uncaught * 'b ->
    Exception.exceptions_caught_and_uncaught * 'b) ->
    (elt -> Exception.exceptions_caught_and_uncaught * 'b -> Exception.exceptions_caught_and_uncaught * 'b) ->
    set ->
    'a t ->
    'b ->
    Exception.exceptions_caught_and_uncaught * 'b

  val iter : (elt -> 'a -> unit) -> 'a t -> unit

  val iter2 :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a ->
    Exception.exceptions_caught_and_uncaught) ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'b ->
    Exception.exceptions_caught_and_uncaught) ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt ->
    'a ->
    'b ->
    Exception.exceptions_caught_and_uncaught) ->
    'a t ->
    'b t ->
    Exception.exceptions_caught_and_uncaught

  val fold : (elt -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val mapi : (elt -> 'a -> 'b) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val for_all : (elt -> 'a -> bool) -> 'a t -> bool
  val filter_one : (elt -> 'a -> bool) -> 'a t -> (elt * 'a) option
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val bindings : 'a t -> (elt * 'a) list

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

module type S_with_logs = sig
  type elt

  module Set : Set_with_logs with type elt = elt
  module Map : Map_with_logs with type elt = elt and type set = Set.t
end

module Make (S_both : SetMap.S) :
  S_with_logs
    with type elt = S_both.elt
     and type 'a Map.t = 'a S_both.Map.t
     and type Set.t = S_both.Set.t
     and type Map.elt = S_both.elt
     and type Set.elt = S_both.elt

module type Projection = sig
  type elt_a
  type elt_b
  type set_a
  type set_b
  type 'a map_a
  type 'a map_b

  val proj_map :
    (elt_a -> elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'b ->
    ('b -> 'a -> 'b) ->
    'a map_a ->
    Exception.exceptions_caught_and_uncaught * 'b map_b
  (** proj_map f init merge map is a map mapping each element b
      to the result of the itteration of the function merge over the image in map of the element a in f such that f(a)=b, starting with the element init. *)

  val monadic_proj_map :
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt_a ->
    Exception.exceptions_caught_and_uncaught * elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'b ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'b ->
    'a ->
    Exception.exceptions_caught_and_uncaught * 'b) ->
    'a map_a ->
    Exception.exceptions_caught_and_uncaught * 'b map_b

  val monadic_proj_map_i :
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt_a ->
    Exception.exceptions_caught_and_uncaught * elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'b ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'b ->
    elt_a ->
    'a ->
    Exception.exceptions_caught_and_uncaught * 'b) ->
    'a map_a ->
    Exception.exceptions_caught_and_uncaught * 'b map_b

  val proj_set :
    (elt_a -> elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    set_a ->
    Exception.exceptions_caught_and_uncaught * set_b
  (** proj_set f set is the set \{f(a) | a\in S\} *)

  val monadic_proj_set :
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt_a ->
    Exception.exceptions_caught_and_uncaught * elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    set_a ->
    Exception.exceptions_caught_and_uncaught * set_b

  val partition_set :
    (elt_a -> elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    set_a ->
    Exception.exceptions_caught_and_uncaught * set_a map_b
  (** partition_set f set is the map mapping any element b with an antecedent for f in the set set, into the set of its antecedents, ie
      to the set \{a\in set | f(a)=b\}. *)

  val monadic_partition_set :
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    elt_a ->
    Exception.exceptions_caught_and_uncaught * elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    set_a ->
    Exception.exceptions_caught_and_uncaught * set_a map_b
end

module Proj (A : S_with_logs) (B : S_with_logs) :
  Projection
    with type elt_a = A.elt
     and type elt_b = B.elt
     and type set_a = A.Set.t
     and type set_b = B.Set.t
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
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    (elt_a -> elt_b) ->
    (elt_a -> elt_c) ->
    'b ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'b ->
    'a ->
    'b) ->
    'a map_a ->
    Exception.exceptions_caught_and_uncaught * 'b map_c map_b

  val proj2_monadic :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'mvbdu_handler ->
    (elt_a -> elt_b) ->
    (elt_a -> elt_c) ->
    'b ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'mvbdu_handler ->
    'b ->
    'a ->
    Exception.exceptions_caught_and_uncaught * 'mvbdu_handler * 'b) ->
    'a map_a ->
    Exception.exceptions_caught_and_uncaught * 'mvbdu_handler * 'b map_c map_b
end

module Proj2 (A : S_with_logs) (B : S_with_logs) (C : S_with_logs) :
  Projection2
    with type elt_a = A.elt
     and type elt_b = B.elt
     and type elt_c = C.elt
     and type 'a map_a = 'a A.Map.t
     and type 'a map_b = 'a B.Map.t
     and type 'a map_c = 'a C.Map.t
