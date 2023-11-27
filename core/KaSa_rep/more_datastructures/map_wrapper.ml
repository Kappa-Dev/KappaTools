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
    Exception.method_handler ->
    elt ->
    t ->
    Exception.method_handler * t

  val add_when_not_in :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    t ->
    Exception.method_handler * t

  val remove :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    t ->
    Exception.method_handler * t

  val minus :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    t ->
    t ->
    Exception.method_handler * t

  val union :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    t ->
    t ->
    Exception.method_handler * t

  val disjoint_union :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    t ->
    t ->
    Exception.method_handler * t

  val inter :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    t ->
    t ->
    Exception.method_handler * t

  val diff :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    t ->
    t ->
    Exception.method_handler * t

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
    Exception.method_handler ->
    elt ->
    'a t ->
    Exception.method_handler * 'a option

  val find_default :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'a ->
    elt ->
    'a t ->
    Exception.method_handler * 'a

  val find_default_without_logs :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'a ->
    elt ->
    'a t ->
    Exception.method_handler * 'a

  val find_option_without_logs :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a t ->
    Exception.method_handler * 'a option

  val add :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a ->
    'a t ->
    Exception.method_handler * 'a t

  val overwrite :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a ->
    'a t ->
    Exception.method_handler * 'a t

  val add_or_overwrite :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a ->
    'a t ->
    Exception.method_handler * 'a t

  val remove :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a t ->
    Exception.method_handler * 'a t

  val remove_or_not :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a t ->
    Exception.method_handler * 'a t

  val update :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'a t ->
    'a t ->
    Exception.method_handler * 'a t

  val map2 :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'a ->
    Exception.method_handler * 'c) ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'b ->
    Exception.method_handler * 'c) ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'a ->
    'b ->
    Exception.method_handler * 'c) ->
    'a t ->
    'b t ->
    Exception.method_handler * 'c t

  val map2z :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'a ->
    'a ->
    Exception.method_handler * 'a) ->
    'a t ->
    'a t ->
    Exception.method_handler * 'a t

  val fold2z :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a ->
    'b ->
    'c ->
    Exception.method_handler * 'c) ->
    'a t ->
    'b t ->
    'c ->
    Exception.method_handler * 'c

  val fold2 :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a ->
    'c ->
    Exception.method_handler * 'c) ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'b ->
    'c ->
    Exception.method_handler * 'c) ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a ->
    'b ->
    'c ->
    Exception.method_handler * 'c) ->
    'a t ->
    'b t ->
    'c ->
    Exception.method_handler * 'c

  val fold2_sparse :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a ->
    'b ->
    'c ->
    Exception.method_handler * 'c) ->
    'a t ->
    'b t ->
    'c ->
    Exception.method_handler * 'c

  val iter2_sparse :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a ->
    'b ->
    Exception.method_handler) ->
    'a t ->
    'b t ->
    Exception.method_handler

  val diff :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'a t ->
    'a t ->
    Exception.method_handler * 'a t * 'a t

  val diff_pred :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    ('a -> 'a -> bool) ->
    'a t ->
    'a t ->
    Exception.method_handler * 'a t * 'a t

  val merge :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'a t ->
    'a t ->
    Exception.method_handler * 'a t

  val union :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'a t ->
    'a t ->
    Exception.method_handler * 'a t

  val fold_restriction :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (elt ->
    'a ->
    Exception.method_handler * 'b ->
    Exception.method_handler * 'b) ->
    set ->
    'a t ->
    'b ->
    Exception.method_handler * 'b

  val fold_restriction_with_missing_associations :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (elt ->
    'a ->
    Exception.method_handler * 'b ->
    Exception.method_handler * 'b) ->
    (elt -> Exception.method_handler * 'b -> Exception.method_handler * 'b) ->
    set ->
    'a t ->
    'b ->
    Exception.method_handler * 'b

  val iter : (elt -> 'a -> unit) -> 'a t -> unit

  val iter2 :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a ->
    Exception.method_handler) ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'b ->
    Exception.method_handler) ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    'a ->
    'b ->
    Exception.method_handler) ->
    'a t ->
    'b t ->
    Exception.method_handler

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

let lift f = f Exception.wrap

module Make (S_both : SetMap.S) :
  S_with_logs
    with type elt = S_both.elt
     and type 'a Map.t = 'a S_both.Map.t
     and type Set.t = S_both.Set.t = struct
  type elt = S_both.elt

  module Set :
    Set_with_logs with type elt = S_both.elt and type t = S_both.Set.t = struct
    type elt = S_both.elt
    type t = S_both.Set.t

    let empty = S_both.Set.empty
    let is_empty = S_both.Set.is_empty
    let singleton = S_both.Set.singleton
    let is_singleton = S_both.Set.is_singleton
    let add = lift S_both.Set.add_with_logs

    let add_when_not_in p e x s =
      let e, _, s = (lift S_both.Set.add_while_testing_freshness) p e x s in
      e, s

    let remove = lift S_both.Set.remove_with_logs
    let union = lift S_both.Set.union_with_logs
    let disjoint_union = lift S_both.Set.disjoint_union_with_logs
    let inter = lift S_both.Set.inter_with_logs
    let diff = lift S_both.Set.diff_with_logs
    let minus = lift S_both.Set.minus_with_logs
    let cardinal = S_both.Set.size
    let mem = S_both.Set.mem
    let exists = S_both.Set.exists
    let filter = S_both.Set.filter
    let for_all = S_both.Set.for_all
    let partition = S_both.Set.partition
    let compare = S_both.Set.compare
    let equal = S_both.Set.equal
    let subset = S_both.Set.subset
    let iter = S_both.Set.iter
    let fold = S_both.Set.fold
    let fold_inv = S_both.Set.fold_inv
    let elements = S_both.Set.elements
    let choose = S_both.Set.choose
    let min_elt = S_both.Set.min_elt
    let max_elt = S_both.Set.max_elt
  end

  module Map :
    Map_with_logs
      with type elt = S_both.elt
       and type 'a t = 'a S_both.Map.t
       and type set = S_both.Set.t
       and type set = Set.t = struct
    type elt = S_both.elt
    type set = S_both.Set.t
    type +'data t = 'data S_both.Map.t

    let empty = S_both.Map.empty
    let is_empty = S_both.Map.is_empty
    let min_elt = S_both.Map.min_elt
    let mem = S_both.Map.mem
    let find_option a b c d = lift S_both.Map.find_option_with_logs a b c d
    let find_default a b c d = lift S_both.Map.find_default_with_logs a b c d
    let find_option_without_logs _a b c d = b, S_both.Map.find_option c d
    let find_default_without_logs _a b c d e = b, S_both.Map.find_default c d e
    let add a b c d = lift S_both.Map.add_with_logs a b c d

    let overwrite parameter error c d e =
      let error, bool, map =
        lift S_both.Map.add_while_testing_freshness parameter error c d e
      in
      if bool then
        Exception.warn parameter error __POS__
          ~message:"attempt to overwrite an association that does not exist"
          (Failure "Attempt to overwrite an association that does not exist")
          map
      else
        error, map

    let add_or_overwrite a b c d e =
      let error, _, map =
        lift S_both.Map.add_while_testing_freshness a b c d e
      in
      error, map

    let remove a b c d = lift S_both.Map.remove_with_logs a b c d

    let remove_or_not a b c d =
      let error, _, map =
        lift S_both.Map.remove_while_testing_existence a b c d
      in
      error, map

    let update a b c = lift S_both.Map.update_with_logs a b c
    let map2 a b c = lift S_both.Map.map2_with_logs a b c
    let map2z a b c = lift S_both.Map.map2z_with_logs a b c
    let fold2z a b c = lift S_both.Map.fold2z_with_logs a b c
    let fold2 a b c = lift S_both.Map.fold2_with_logs a b c

    let iter2 parameter error f g h mapf mapg =
      fst
        (S_both.Map.fold2_with_logs Exception.wrap parameter error
           (fun a b c d () -> f a b c d, ())
           (fun a b c d () -> g a b c d, ())
           (fun a b c d e () -> h a b c d e, ())
           mapf mapg ())

    let fold2_sparse a b c = lift S_both.Map.fold2_sparse_with_logs a b c
    let iter2_sparse a b c = lift S_both.Map.iter2_sparse_with_logs a b c
    let diff a b c = lift S_both.Map.diff_with_logs a b c
    let diff_pred a b c = lift S_both.Map.diff_pred_with_logs a b c
    let merge a b c = lift S_both.Map.merge_with_logs a b c
    let union a b c = lift S_both.Map.union_with_logs a b c

    let fold_restriction a b c =
      lift S_both.Map.fold_restriction_with_logs a b c

    let fold_restriction_with_missing_associations a b c =
      lift S_both.Map.fold_restriction_with_missing_associations_with_logs a b c

    let iter = S_both.Map.iter
    let fold = S_both.Map.fold
    let mapi = S_both.Map.mapi
    let map = S_both.Map.map
    let for_all = S_both.Map.for_all
    let filter_one = S_both.Map.filter_one
    let compare = S_both.Map.compare
    let equal = S_both.Map.equal
    let bindings = S_both.Map.bindings
    let to_json = S_both.Map.to_json
    let of_json = S_both.Map.of_json
  end
end

module type Projection = sig
  type elt_a
  type elt_b
  type 'a map_a
  type 'a map_b
  type set_a
  type set_b

  val proj_map :
    (elt_a -> elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'b ->
    ('b -> 'a -> 'b) ->
    'a map_a ->
    Exception.method_handler * 'b map_b

  val monadic_proj_map :
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt_a ->
    Exception.method_handler * elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'b ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'b ->
    'a ->
    Exception.method_handler * 'b) ->
    'a map_a ->
    Exception.method_handler * 'b map_b

  val monadic_proj_map_i :
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt_a ->
    Exception.method_handler * elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'b ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'b ->
    elt_a ->
    'a ->
    Exception.method_handler * 'b) ->
    'a map_a ->
    Exception.method_handler * 'b map_b

  val proj_set :
    (elt_a -> elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    set_a ->
    Exception.method_handler * set_b

  val monadic_proj_set :
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt_a ->
    Exception.method_handler * elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    set_a ->
    Exception.method_handler * set_b

  val partition_set :
    (elt_a -> elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    set_a ->
    Exception.method_handler * set_a map_b

  val monadic_partition_set :
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt_a ->
    Exception.method_handler * elt_b) ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    set_a ->
    Exception.method_handler * set_a map_b
end

module Proj (A : S_with_logs) (B : S_with_logs) :
  Projection
    with type elt_a = A.elt
     and type elt_b = B.elt
     and type 'a map_a = 'a A.Map.t
     and type 'a map_b = 'a B.Map.t
     and type set_a = A.Set.t
     and type set_b = B.Set.t = struct
  module MA = A.Map
  module MB = B.Map
  module SA = A.Set
  module SB = B.Set

  type elt_a = MA.elt
  type elt_b = MB.elt
  type set_a = SA.t
  type set_b = SB.t
  type 'a map_a = 'a MA.t
  type 'a map_b = 'a MB.t

  let proj_map f parameter error identity_elt merge map =
    MA.fold
      (fun key_a data_a (error, map_b) ->
        let key_b = f key_a in
        match MB.find_option_without_logs parameter error key_b map_b with
        | error, None ->
          MB.add_or_overwrite parameter error key_b
            (merge identity_elt data_a)
            map_b
        | error, Some old ->
          MB.add_or_overwrite parameter error key_b (merge old data_a) map_b)
      map (error, MB.empty)

  let monadic_proj_map_i f parameter error identity_elt merge map =
    MA.fold
      (fun key_a data_a (error, map_b) ->
        let error, key_b = f parameter error key_a in
        match MB.find_option_without_logs parameter error key_b map_b with
        | error, None ->
          let error, data' = merge parameter error identity_elt key_a data_a in
          MB.add_or_overwrite parameter error key_b data' map_b
        | error, Some old ->
          let error, data' = merge parameter error old key_a data_a in
          MB.add_or_overwrite parameter error key_b data' map_b)
      map (error, MB.empty)

  let monadic_proj_map f parameter error identity_elt merge map =
    monadic_proj_map_i f parameter error identity_elt
      (fun parameter error old _ data_a -> merge parameter error old data_a)
      map

  let partition_set f parameter error set =
    SA.fold
      (fun key_a (error, map_b) ->
        let key_b = f key_a in
        match MB.find_option_without_logs parameter error key_b map_b with
        | error, None ->
          MB.add_or_overwrite parameter error key_b (SA.singleton key_a) map_b
        | error, Some old ->
          let error, newset = SA.add parameter error key_a old in
          MB.add_or_overwrite parameter error key_b newset map_b)
      set (error, MB.empty)

  let monadic_partition_set f parameter error set =
    SA.fold
      (fun key_a (error, map_b) ->
        let error, key_b = f parameter error key_a in
        match MB.find_option_without_logs parameter error key_b map_b with
        | error, None ->
          let error, data' = error, SA.singleton key_a in
          MB.add_or_overwrite parameter error key_b data' map_b
        | error, Some old ->
          let error, data' = SA.add_when_not_in parameter error key_a old in
          MB.add_or_overwrite parameter error key_b data' map_b)
      set (error, MB.empty)

  let proj_set f parameter error set_a =
    SA.fold
      (fun key_a (error, set_b) ->
        SB.add_when_not_in parameter error (f key_a) set_b)
      set_a (error, SB.empty)

  let monadic_proj_set f parameter error set_a =
    SA.fold
      (fun key_a (error, set_b) ->
        let error, key_b = f parameter error key_a in
        SB.add_when_not_in parameter error key_b set_b)
      set_a (error, SB.empty)
end

module type Projection2 = sig
  type elt_a
  type elt_b
  type elt_c
  type 'a map_a
  type 'a map_b
  type 'a map_c

  val proj2 :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (elt_a -> elt_b) ->
    (elt_a -> elt_c) ->
    'b ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'b ->
    'a ->
    'b) ->
    'a map_a ->
    Exception.method_handler * 'b map_c map_b

  val proj2_monadic :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'mvbdu_handler ->
    (elt_a -> elt_b) ->
    (elt_a -> elt_c) ->
    'b ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'mvbdu_handler ->
    'b ->
    'a ->
    Exception.method_handler * 'mvbdu_handler * 'b) ->
    'a map_a ->
    Exception.method_handler * 'mvbdu_handler * 'b map_c map_b
end

module Proj2 (A : S_with_logs) (B : S_with_logs) (C : S_with_logs) :
  Projection2
    with type elt_a = A.elt
     and type elt_b = B.elt
     and type elt_c = C.elt
     and type 'a map_a = 'a A.Map.t
     and type 'a map_b = 'a B.Map.t
     and type 'a map_c = 'a C.Map.t = struct
  module MA = A.Map
  module MB = B.Map
  module MC = C.Map

  type elt_a = MA.elt
  type elt_b = MB.elt
  type elt_c = MC.elt
  type 'a map_a = 'a MA.t
  type 'a map_b = 'a MB.t
  type 'a map_c = 'a MC.t

  let proj2 parameter error f g identity_elt merge map =
    MA.fold
      (fun key_a data_a (error, map_b) ->
        let key_b = f key_a in
        let key_c = g key_a in
        let error, submap =
          MB.find_default_without_logs parameter error MC.empty key_b map_b
        in
        let error, find_default =
          MC.find_default_without_logs parameter error identity_elt key_c submap
        in
        let error, submap =
          MC.add parameter error key_c
            (merge parameter error find_default data_a)
            submap
        in
        MB.add_or_overwrite parameter error key_b submap map_b)
      map (error, MB.empty)

  let proj2_monadic parameter handler mvbdu_handler f g identity_elt merge map =
    MA.fold
      (fun key_a data_a (handler, mvbdu_handler, map_b) ->
        let key_b = f key_a in
        let key_c = g key_a in
        let handler, submap =
          MB.find_default_without_logs parameter handler MC.empty key_b map_b
        in
        let handler, find_default =
          MC.find_default_without_logs parameter handler identity_elt key_c
            submap
        in
        let handler, mvbdu_handler, data' =
          merge parameter handler mvbdu_handler find_default data_a
        in
        let handler, submap = MC.add parameter handler key_c data' submap in
        let handler, add =
          MB.add_or_overwrite parameter handler key_b submap map_b
        in
        handler, mvbdu_handler, add)
      map
      (handler, mvbdu_handler, MB.empty)
end
