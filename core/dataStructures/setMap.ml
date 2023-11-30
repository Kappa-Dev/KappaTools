(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

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
  (* val split_with_logs:
        ('parameters,'error,elt -> t -> 'error * ( t * bool * t)) with_log_wrap *)

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

exception DeadCodeIsNotDead of string

module Make (Ord : OrderedType) : S with type elt = Ord.t = struct
  type elt = Ord.t

  module Set = struct
    type elt = Ord.t

    module Private : sig
      type t = private Empty | Node of t * elt * t * int * int

      val empty : t
      val height : t -> int
      val size : t -> int
      val node : t -> elt -> t -> t
    end = struct
      type t = Empty | Node of t * elt * t * int * int

      let empty = Empty

      let height = function
        | Empty -> 0
        | Node (_, _, _, h, _) -> h

      let size = function
        | Empty -> 0
        | Node (_, _, _, _, s) -> s

      let node left value right =
        let hl = height left in
        let hr = height right in
        Node
          ( left,
            value,
            right,
            (if hl > hr then
               hl
             else
               hr)
            + 1,
            size left + size right + 1 )
    end

    type t = Private.t

    let empty = Private.empty
    let height = Private.height
    let size = Private.size
    let node = Private.node

    let is_empty = function
      | Private.Empty -> true
      | Private.Node _ -> false

    let singleton value = node empty value empty

    let is_singleton set =
      match set with
      | Private.Empty -> false
      | Private.Node (set1, _, set2, _, _) -> is_empty set1 && is_empty set2

    let balance left value right =
      let height_left = height left in
      let height_right = height right in
      if height_left > height_right + 2 then (
        match left with
        | Private.Empty -> raise (DeadCodeIsNotDead "SetMap line 222")
        (* height_left > height_right + 2 >= 2 *)
        | Private.Node (leftleft, leftvalue, leftright, _, _) ->
          if height leftleft >= height leftright then
            node leftleft leftvalue (node leftright value right)
          else (
            match leftright with
            | Private.Empty -> raise (DeadCodeIsNotDead "SetMap line 229")
            (* 0 <= height leftleft < height leftright *)
            | Private.Node (leftrightleft, leftrightvalue, leftrightright, _, _)
              ->
              node
                (node leftleft leftvalue leftrightleft)
                leftrightvalue
                (node leftrightright value right)
          )
      ) else if height_right > height_left + 2 then (
        match right with
        | Private.Empty -> raise (DeadCodeIsNotDead "SetMap line 238")
        (* height_right > height_left + 2 >= 2 *)
        | Private.Node (rightleft, rightvalue, rightright, _, _) ->
          if height rightright >= height rightleft then
            node (node left value rightleft) rightvalue rightright
          else (
            match rightleft with
            | Private.Empty -> raise (DeadCodeIsNotDead "SetMap line 245")
            (* 0 <= height rightright < height rightleft *)
            | Private.Node (rightleftleft, rightleftvalue, rightleftright, _, _)
              ->
              node
                (node left value rightleftleft)
                rightleftvalue
                (node rightleftright rightvalue rightright)
          )
      ) else
        node left value right

    let balance_with_logs warn parameters error left value right =
      try error, balance left value right
      with DeadCodeIsNotDead loc ->
        let error =
          warn parameters error "setMap.ml"
            (Some (loc ^ " Set invariant is broken, keep on with unbalanced set"))
            (Failure "Set_and_Map.SET.balance")
        in
        error, node left value right

    (** Beware some code relies on the invariant [add x s == s] iff [mem x s] *)
    let rec add x = function
      | Private.Empty -> singleton x
      | Private.Node (l, v, r, _, _) as t ->
        let c = Ord.compare x v in
        if c = 0 then
          t
        else if c < 0 then (
          let o = add x l in
          if o == l then
            t
          else
            balance o v r
        ) else (
          let o = add x r in
          if o == r then
            t
          else
            balance l v o
        )

    let rec add_while_testing_freshness warn parameters error new_val set =
      match set with
      | Private.Empty -> error, true, singleton new_val
      | Private.Node (left, value_set, right, _, _) ->
        let c = Ord.compare new_val value_set in
        if c = 0 then
          error, false, set
        else if c < 0 then (
          let error, bool, left' =
            add_while_testing_freshness warn parameters error new_val left
          in
          let error, set =
            balance_with_logs warn parameters error left' value_set right
          in
          error, bool, set
        ) else (
          let error, bool, right' =
            add_while_testing_freshness warn parameters error new_val right
          in
          let error, set =
            balance_with_logs warn parameters error left value_set right'
          in
          error, bool, set
        )

    let add_with_logs warn parameters error new_value set =
      let error, bool, set =
        add_while_testing_freshness warn parameters error new_value set
      in
      let error =
        if bool then
          error
        else
          warn parameters error "setMap.ml"
            (Some ("SetMap line 300" ^ " an already elt has been added to a set"))
            (Failure "Set_and_Map.SET.add")
      in
      error, set

    let add_even_if_it_exists warn parameters error new_value set =
      let error, _, set =
        add_while_testing_freshness warn parameters error new_value set
      in
      error, set

    let rec join left value right =
      match left, right with
      | Private.Empty, _ -> add value right
      | _, Private.Empty -> add value left
      | ( Private.Node (leftleft, leftvalue, leftright, leftheight, _),
          Private.Node (rightleft, rightvalue, rightright, rightheight, _) ) ->
        if leftheight > rightheight + 2 then (
          let right' = join leftright value right in
          balance leftleft leftvalue right'
        ) else if rightheight > leftheight + 2 then (
          let left' = join left value rightleft in
          balance left' rightvalue rightright
        ) else
          node left value right

    let rec safe_extract_min_elt left value right =
      match left with
      | Private.Empty -> value, right
      | Private.Node (leftleft, leftvalue, leftright, _, _) ->
        let min, left' = safe_extract_min_elt leftleft leftvalue leftright in
        min, balance left' value right

    let rec min_elt_with_logs warn parameters error set =
      match set with
      | Private.Empty ->
        let error =
          warn parameters error "setMap.ml" (Some "min_elt_with_logs, line 303")
            Not_found
        in
        error, None
      | Private.Node (Private.Empty, v, _, _, _) -> error, Some v
      | Private.Node (left, _, _, _, _) ->
        min_elt_with_logs warn parameters error left

    let rec remove_min_elt_with_logs warn parameters error set =
      match set with
      | Private.Empty ->
        let error =
          warn parameters error "setMap.ml"
            (Some "remove_min_elt_with_logs, line 311") Not_found
        in
        error, empty
      | Private.Node (Private.Empty, _, right, _, _) -> error, right
      | Private.Node (left, value, right, _, _) ->
        let error, left' =
          remove_min_elt_with_logs warn parameters error left
        in
        balance_with_logs warn parameters error left' value right

    let merge set1 set2 =
      match set1, set2 with
      | Private.Empty, _ -> set2
      | _, Private.Empty -> set1
      | Private.Node _, Private.Node (left2, value2, right2, _, _) ->
        let min2, set2' = safe_extract_min_elt left2 value2 right2 in
        balance set1 min2 set2'

    let merge_with_logs warn parameters error set1 set2 =
      match set1, set2 with
      | Private.Empty, _ -> error, set2
      | _, Private.Empty -> error, set1
      | Private.Node _, Private.Node _ ->
        let error, left2 =
          remove_min_elt_with_logs warn parameters error set2
        in
        let error, elt_opt = min_elt_with_logs warn parameters error set2 in
        (match elt_opt with
        | None ->
          let error =
            warn parameters error "setMap.ml" (Some "merge_with_logs,line 339")
              Not_found
          in
          error, set1
        | Some elt -> balance_with_logs warn parameters error set1 elt left2)

    let rec join_with_logs warn parameters error left value right =
      match left, right with
      | Private.Empty, _ -> add_with_logs warn parameters error value right
      | _, Private.Empty -> add_with_logs warn parameters error value left
      | ( Private.Node (leftleft, leftvalue, leftright, leftheight, _),
          Private.Node (rightleft, rightvalue, rightright, rightheight, _) ) ->
        if leftheight > rightheight + 2 then (
          let error, right' =
            join_with_logs warn parameters error leftright value right
          in
          balance_with_logs warn parameters error leftleft leftvalue right'
        ) else if rightheight > leftheight + 2 then (
          let error, left' =
            join_with_logs warn parameters error left value rightleft
          in
          balance_with_logs warn parameters error left' rightvalue rightright
        ) else
          error, node left value right

    let concat set1 set2 =
      match set1, set2 with
      | Private.Empty, _ -> set2
      | _, Private.Empty -> set1
      | Private.Node _, Private.Node (left2, value2, right2, _, _) ->
        let min2, set2' = safe_extract_min_elt left2 value2 right2 in
        join set1 min2 set2'

    let concat_with_logs warn parameters error set1 set2 =
      match set1, set2 with
      | Private.Empty, _ -> error, set2
      | _, Private.Empty -> error, set1
      | Private.Node _, Private.Node _ ->
        let error, left2 =
          remove_min_elt_with_logs warn parameters error set2
        in
        let error, elt_opt = min_elt_with_logs warn parameters error set2 in
        (match elt_opt with
        | None ->
          let error =
            warn parameters error "setMap.ml" (Some "concat_with_logs,line 390")
              Not_found
          in
          error, set1
        | Some elt -> join_with_logs warn parameters error set1 elt left2)

    let rec split_with_logs warn parameters error split_val set =
      match set with
      | Private.Empty -> error, (empty, false, empty)
      | Private.Node (left, set_val, right, _, _) ->
        let c = Ord.compare split_val set_val in
        if c = 0 then
          error, (left, true, right)
        else if c < 0 then (
          let error, (leftleft, bool, rightleft) =
            split_with_logs warn parameters error split_val left
          in
          let error, rightright =
            join_with_logs warn parameters error rightleft set_val right
          in
          error, (leftleft, bool, rightright)
        ) else (
          let error, (leftright, bool, rightright) =
            split_with_logs warn parameters error split_val right
          in
          let error, leftleft =
            join_with_logs warn parameters error left set_val leftright
          in
          error, (leftleft, bool, rightright)
        )

    let rec remove value = function
      | Private.Empty as set -> set
      | Private.Node (left, value_set, right, _, _) as set ->
        let c = Ord.compare value value_set in
        if c = 0 then
          merge left right
        else if c < 0 then (
          let left' = remove value left in
          if left == left' then
            set
          else
            balance left' value_set right
        ) else (
          let right' = remove value right in
          if right == right' then
            set
          else
            balance left value_set right'
        )

    let rec remove_while_testing_existence warn parameters error value =
      function
      | Private.Empty as set -> error, false, set
      | Private.Node (left, value_set, right, _, _) as set ->
        let c = Ord.compare value value_set in
        if c = 0 then (
          let error, set = merge_with_logs warn parameters error left right in
          error, true, set
        ) else if c < 0 then (
          let error, bool, left' =
            remove_while_testing_existence warn parameters error value left
          in
          if left == left' then (
            let error =
              if bool then
                warn parameters error "setMap.ml" (Some "SetMap line 454")
                  (failwith "Invariant is broken")
              else
                error
            in
            error, bool, set
          ) else (
            let error, set =
              balance_with_logs warn parameters error left' value_set right
            in
            error, bool, set
          )
        ) else (
          let error, bool, right' =
            remove_while_testing_existence warn parameters error value right
          in
          if right == right' then (
            let error =
              if bool then
                warn parameters error "setMap.ml" (Some "SetMap line 467")
                  Not_found
              else
                error
            in
            error, bool, set
          ) else (
            let error, set =
              balance_with_logs warn parameters error left value_set right'
            in
            error, bool, set
          )
        )

    let remove_with_logs warn parameters error value set =
      let error, bool, set =
        remove_while_testing_existence warn parameters error value set
      in
      if bool then
        error, set
      else
        ( warn parameters error "setMap.ml"
            (Some
               ("SetMap line 481"
              ^ "Attempt to remove an elt that does not exist"))
            Not_found,
          set )

    let rec split split_value set =
      match set with
      | Private.Empty -> empty, false, empty
      | Private.Node (left, set_value, right, _, _) ->
        let c = Ord.compare split_value set_value in
        if c = 0 then
          left, true, right
        else if c < 0 then (
          let leftleft, bool, rightleft = split split_value left in
          let rightright = join rightleft set_value right in
          leftleft, bool, rightright
        ) else (
          let leftright, bool, rightright = split split_value right in
          let leftleft = join left set_value leftright in
          leftleft, bool, rightright
        )

    let rec union set1 set2 =
      match set1, set2 with
      | Private.Empty, _ -> set2
      | _, Private.Empty -> set1
      | ( Private.Node (left1, value1, right1, height1, _),
          Private.Node (left2, value2, right2, height2, _) ) ->
        if height1 > height2 then
          if height2 = 1 then
            add value2 set1
          else (
            let left2, _, right2 = split value1 set2 in
            let left' = union left1 left2 in
            let right' = union right1 right2 in
            join left' value1 right'
          )
        else if height1 = 1 then
          add value1 set2
        else (
          let left1, _, right1 = split value2 set1 in
          let left' = union left1 left2 in
          let right' = union right1 right2 in
          join left' value2 right'
        )

    let rec disjoint_union set1 set2 =
      match set1, set2 with
      | Private.Empty, _ -> Some set2
      | _, Private.Empty -> Some set1
      | ( Private.Node (left1, value1, right1, height1, _),
          Private.Node (left2, value2, right2, height2, _) ) ->
        if height1 > height2 then
          if height2 = 1 then (
            let out = add value2 set1 in
            if out == set1 then
              None
            else
              Some out
          ) else (
            let left2, _, right2 = split value1 set2 in
            match disjoint_union left1 left2, disjoint_union right1 right2 with
            | Some left', Some right' -> Some (join left' value1 right')
            | _, _ -> None
          )
        else if height1 = 1 then (
          let out = add value1 set2 in
          if set2 == out then
            None
          else
            Some out
        ) else (
          let left1, _, right1 = split value2 set1 in
          match disjoint_union left1 left2, disjoint_union right1 right2 with
          | Some left', Some right' -> Some (join left' value2 right')
          | _, _ -> None
        )

    let rec union_gen add_gen warn parameters error set1 set2 =
      match set1, set2 with
      | Private.Empty, _ -> error, set2
      | _, Private.Empty -> error, set1
      | ( Private.Node (left1, value1, right1, height1, _),
          Private.Node (left2, value2, right2, height2, _) ) ->
        if height1 > height2 then
          if height2 = 1 then
            add_gen warn parameters error value2 set1
          else (
            let error, (left2, _, right2) =
              split_with_logs warn parameters error value1 set2
            in
            let error, left' =
              union_gen add_gen warn parameters error left1 left2
            in
            let error, right' =
              union_gen add_gen warn parameters error right1 right2
            in
            join_with_logs warn parameters error left' value1 right'
          )
        else if height1 = 1 then
          add_gen warn parameters error value1 set2
        else (
          let error, (left1, _, right1) =
            split_with_logs warn parameters error value2 set1
          in
          let error, left' =
            union_gen add_gen warn parameters error left1 left2
          in
          let error, right' =
            union_gen add_gen warn parameters error right1 right2
          in
          join_with_logs warn parameters error left' value2 right'
        )

    let union_with_logs w p e s s' = union_gen add_even_if_it_exists w p e s s'
    let disjoint_union_with_logs w p e s s' = union_gen add_with_logs w p e s s'

    let suture (left1, value1, right1) (left2, bool, right2) f =
      let left' = f left1 left2 in
      let right' = f right1 right2 in
      if bool then
        join left' value1 right'
      else
        concat left' right'

    let suture_not (left1, value1, right1) (left2, bool, right2) f =
      let left' = f left1 left2 in
      let right' = f right1 right2 in
      if bool then
        concat left' right'
      else
        join left' value1 right'

    let rec inter set1 set2 =
      match set1, set2 with
      | Private.Empty, _ | _, Private.Empty -> empty
      | Private.Node (left1, value1, right1, _, _), _ ->
        let triple2 = split value1 set2 in
        suture (left1, value1, right1) triple2 inter

    let suture_with_logs warn parameters error (left1, value1, right1)
        (left2, bool, right2) f =
      let error, left' = f warn parameters error left1 left2 in
      let error, right' = f warn parameters error right1 right2 in
      if bool then
        join_with_logs warn parameters error left' value1 right'
      else
        concat_with_logs warn parameters error left' right'

    let suture_not_with_logs warn parameters error (left1, value1, right1)
        (left2, bool, right2) f =
      let error, left' = f warn parameters error left1 left2 in
      let error, right' = f warn parameters error right1 right2 in
      if bool then
        concat_with_logs warn parameters error left' right'
      else
        join_with_logs warn parameters error left' value1 right'

    let rec inter_with_logs warn parameters error set1 set2 =
      match set1, set2 with
      | Private.Empty, _ | _, Private.Empty -> error, empty
      | Private.Node (left1, value1, right1, _, _), _ ->
        let mh', triple2 = split_with_logs warn parameters error value1 set2 in
        suture_with_logs warn parameters mh' (left1, value1, right1) triple2
          inter_with_logs

    let rec diff set1 set2 =
      match set1, set2 with
      | Private.Empty, _ -> set2
      | _, Private.Empty -> set1
      | Private.Node (left1, value1, right1, _, _), _ ->
        let triple2 = split value1 set2 in
        suture_not (left1, value1, right1) triple2 diff

    let rec diff_with_logs warn parameters error set1 set2 =
      match set1, set2 with
      | Private.Empty, _ -> error, empty
      | _, Private.Empty -> error, set1
      | Private.Node (left1, value1, right1, _, _), _ ->
        let error, triple2 =
          split_with_logs warn parameters error value1 set2
        in
        suture_not_with_logs warn parameters error (left1, value1, right1)
          triple2 diff_with_logs

    let rec minus set1 set2 =
      match set1, set2 with
      | Private.Empty, _ -> empty
      | _, Private.Empty -> set1
      | Private.Node (left1, value1, right1, _, _), _ ->
        let triple2 = split value1 set2 in
        suture_not (left1, value1, right1) triple2 minus

    let rec minus_with_logs warn parameters error set1 set2 =
      match set1, set2 with
      | Private.Empty, _ -> error, empty
      | _, Private.Empty -> error, set1
      | Private.Node (left1, value1, right1, _, _), _ ->
        let error, triple2 =
          split_with_logs warn parameters error value1 set2
        in
        suture_not_with_logs warn parameters error (left1, value1, right1)
          triple2 minus_with_logs

    let rec mem searched_value = function
      | Private.Empty -> false
      | Private.Node (left, set_value, right, _, _) ->
        let c = Ord.compare searched_value set_value in
        c == 0
        || mem searched_value
             (if c < 0 then
                left
              else
                right)

    let filter p set =
      let rec filt accu = function
        | Private.Empty -> accu
        | Private.Node (left, value, right, _, _) ->
          filt
            (filt
               (if p value then
                  add value accu
                else
                  accu)
               left)
            right
      in
      filt empty set

    let filter_with_logs warn parameters error p set =
      let rec filt accu set =
        match set with
        | Private.Empty -> accu
        | Private.Node (left, value, right, _, _) ->
          let error, list = accu in
          filt
            (filt
               (if p value then
                  add_with_logs warn parameters error value list
                else
                  accu)
               left)
            right
      in
      filt (error, empty) set

    let partition p set =
      let rec part ((t, f) as accu) = function
        | Private.Empty -> accu
        | Private.Node (left, value, right, _, _) ->
          part
            (part
               (if p value then
                  add value t, f
                else
                  t, add value f)
               left)
            right
      in
      part (empty, empty) set

    let partition_with_logs warn parameters error p set =
      let rec part ((rh, t, f) as accu) set =
        match set with
        | Private.Empty -> accu
        | Private.Node (left, value, right, _, _) ->
          part
            (part
               (if p value then (
                  let a, b = add_with_logs warn parameters rh value t in
                  a, b, f
                ) else (
                  let a, c = add_with_logs warn parameters rh value f in
                  a, t, c
                ))
               left)
            right
      in
      part (error, empty, empty) set

    type enumeration = End | More of elt * t * enumeration

    let rec cons_enum enum = function
      | Private.Empty -> enum
      | Private.Node (left, value, right, _, _) ->
        cons_enum (More (value, right, enum)) left

    let rec compare_aux e1 e2 =
      match e1, e2 with
      | End, End -> 0
      | End, _ -> -1
      | _, End -> 1
      | More (v1, r1, e1), More (v2, r2, e2) ->
        let c = Ord.compare v1 v2 in
        if c <> 0 then
          c
        else
          compare_aux (cons_enum e1 r1) (cons_enum e2 r2)

    let compare set1 set2 =
      compare_aux (cons_enum End set1) (cons_enum End set2)

    let equal set1 set2 = compare set1 set2 == 0

    let rec subset set1 set2 =
      match set1, set2 with
      | Private.Empty, _ -> true
      | _, Private.Empty -> false
      | ( Private.Node (left1, value1, right1, _, _),
          Private.Node (left2, value2, right2, _, _) ) ->
        let c = Ord.compare value1 value2 in
        if c = 0 then
          subset left1 left2 && subset right1 right2
        else if c < 0 then
          subset (node left1 value1 empty) left2 && subset right1 set2
        else
          subset (node empty value1 right1) right2 && subset left1 set2

    let rec iter f = function
      | Private.Empty -> ()
      | Private.Node (left, value, right, _, _) ->
        let () = iter f left in
        let () = f value in
        iter f right

    let rec fold f set accu =
      match set with
      | Private.Empty -> accu
      | Private.Node (left, value, right, _, _) ->
        fold f right (f value (fold f left accu))

    let rec fold_inv f s accu =
      match s with
      | Private.Empty -> accu
      | Private.Node (l, v, r, _, _) -> fold_inv f l (f v (fold_inv f r accu))

    let rec for_all p = function
      | Private.Empty -> true
      | Private.Node (left, value, right, _, _) ->
        p value && for_all p left && for_all p right

    let rec exists p = function
      | Private.Empty -> false
      | Private.Node (left, value, right, _, _) ->
        p value || exists p left || exists p right

    let elements set =
      let rec elements_aux accu = function
        | Private.Empty -> accu
        | Private.Node (left, value, right, _, _) ->
          elements_aux (value :: elements_aux accu right) left
      in
      elements_aux [] set

    let rec aux_print f = function
      | Private.Empty -> ()
      | Private.Node (Private.Empty, key, Private.Empty, _, _) ->
        Format.fprintf f "@[%a@]" Ord.print key
      | Private.Node (Private.Empty, key, right, _, _) ->
        Format.fprintf f "@[%a@],@ %a" Ord.print key aux_print right
      | Private.Node (left, key, Private.Empty, _, _) ->
        Format.fprintf f "%a,@ @[%a@]" aux_print left Ord.print key
      | Private.Node (left, key, right, _, _) ->
        Format.fprintf f "%a,@ @[%a@],@ %a" aux_print left Ord.print key
          aux_print right

    let print f = function
      | Private.Empty -> Format.pp_print_string f "\xE2\x88\x85"
      | Private.Node _ as m -> Format.fprintf f "@[{%a}@]" aux_print m

    let rec min_elt = function
      | Private.Empty -> None
      | Private.Node (Private.Empty, v, _, _, _) -> Some v
      | Private.Node (left, _, _, _, _) -> min_elt left

    let rec max_elt = function
      | Private.Empty -> None
      | Private.Node (_, v, Private.Empty, _, _) -> Some v
      | Private.Node (_, _, right, _, _) -> max_elt right

    let choose = function
      | Private.Empty -> None
      | Private.Node (_, v, _, _, _) -> Some v

    let rec find_acc aim_acc = function
      | Private.Empty -> None
      | Private.Node (l, key, r, _, acc) ->
        if aim_acc >= acc then
          None
        else (
          let acc_l = size l in
          let acc_r = size r in
          if acc_l > aim_acc then
            find_acc aim_acc l
          else if acc_r + acc_l > aim_acc then
            find_acc (aim_acc - acc_l) r
          else
            Some key
        )

    (* let rec find_acc k m = *)
    (*   match m with *)
    (*     Private.Empty -> None *)
    (*   | Private.Node (l, key, r, _, _) -> *)
    (*      let s = size l in *)
    (*      if k < s then find_acc k l *)
    (*      else if k = s then Some key *)
    (*      else find_acc (k - s - 1) r *)

    let random rs m =
      let s = size m in
      if s = 0 then
        None
      else
        find_acc (Random.State.int rs (size m)) m

    (* let add = Lift_error_logs.lift_generic_binary_for_KaSim add_with_logs
       let split = Lift_error_logs.lift_generic_binary_for_KaSim split_with_logs
       let remove =  Lift_error_logs.lift_generic_binary_for_KaSim remove_with_logs
       let union = Lift_error_logs.lift_generic_binary_for_KaSim union_with_logs
       let inter = Lift_error_logs.lift_generic_binary_for_KaSim inter_with_logs
       let diff = Lift_error_logs.lift_generic_binary_for_KaSim diff_with_logs
       let minus = Lift_error_logs.lift_generic_binary_for_KaSim minus_with_logs
       let filter = Lift_error_logs.lift_generic_binary_for_KaSim filter_with_logs
       let partition = Lift_error_logs.lift_generic_binary_binary_for_KaSim partition_with_logs
    *)
  end

  (**************************************************************************)
  (* Map implementation*)

  module Map = struct
    type elt = Ord.t

    module Private : sig
      type +'data t = private
        | Empty
        | Node of 'data t * elt * 'data * 'data t * int * int

      val empty : 'a t
      val height : 'a t -> int
      val size : 'a t -> int
      val node : 'a t -> elt -> 'a -> 'a t -> 'a t
    end = struct
      type +'data t =
        | Empty
        | Node of 'data t * elt * 'data * 'data t * int * int

      let empty = Empty

      let height = function
        | Empty -> 0
        | Node (_, _, _, _, h, _) -> h

      let size = function
        | Empty -> 0
        | Node (_, _, _, _, _, s) -> s

      let node left key0 data right =
        let hl = height left in
        let hr = height right in
        Node
          ( left,
            key0,
            data,
            right,
            (1
            +
            if hl > hr then
              hl
            else
              hr),
            1 + size left + size right )
    end

    type +'a t = 'a Private.t

    let empty = Private.empty
    let height = Private.height
    let size = Private.size
    let node = Private.node

    type set = Set.t

    let is_empty = function
      | Private.Empty -> true
      | Private.Node _ -> false

    let root = function
      | Private.Empty -> None
      | Private.Node (_, x, d, _, _, _) -> Some (x, d)

    let rec max_key = function
      | Private.Empty -> None
      | Private.Node (_, k, _, Private.Empty, _, _) -> Some k
      | Private.Node (_, _, _, m, _, _) -> max_key m

    let balance left key data right =
      let height_left = height left in
      let height_right = height right in
      if height_left > height_right + 2 then (
        match left with
        | Private.Empty -> raise (DeadCodeIsNotDead "SetMap line 828")
        (* height_left > height_right + 2 >= 2 *)
        | Private.Node (left0, key0, data0, right0, _, _) ->
          if height left0 >= height right0 then
            node left0 key0 data0 (node right0 key data right)
          else (
            match right0 with
            | Private.Empty -> raise (DeadCodeIsNotDead "SetMap line 835")
            (* 0 <= height left0 < height right0 *)
            | Private.Node (left1, key1, data1, right1, _, _) ->
              node
                (node left0 key0 data0 left1)
                key1 data1
                (node right1 key data right)
          )
      ) else if height_right > height_left + 2 then (
        match right with
        | Private.Empty -> raise (DeadCodeIsNotDead "SetMap line 844")
        (* height_right > height_left + 2 >= 2 *)
        | Private.Node (left0, key0, data0, right0, _, _) ->
          if height right0 >= height left0 then
            node (node left key data left0) key0 data0 right0
          else (
            match left0 with
            | Private.Empty -> raise (DeadCodeIsNotDead "SetMap line 851")
            (* 0 <= height right0 < height left0 *)
            | Private.Node (left1, key1, data1, right1, _, _) ->
              node (node left key data left1) key1 data1
                (node right1 key0 data0 right0)
          )
      ) else
        node left key data right

    let balance_with_logs warn parameters error left key data right =
      try error, balance left key data right
      with DeadCodeIsNotDead loc ->
        let error =
          warn parameters error "setMap.ml"
            (Some (loc ^ " Map invariant is broken, keep on with unbalanced map"))
            (Failure "Set_and_Map.Map.balance")
        in
        error, node left key data right

    let rec add key data = function
      | Private.Empty -> node empty key data empty
      | Private.Node (left, key_map, data_map, right, _, _) ->
        let cmp = Ord.compare key key_map in
        if cmp = 0 then
          node left key_map data right
        else if cmp < 0 then
          balance (add key data left) key_map data_map right
        else
          balance left key_map data_map (add key data right)

    let rec add_while_testing_freshness warn parameter error key data = function
      | Private.Empty -> error, true, node empty key data empty
      | Private.Node (left, key_map, data_map, right, _, _) ->
        let cmp = Ord.compare key key_map in
        if cmp = 0 then
          error, false, node left key_map data right
        else if cmp < 0 then (
          let error, bool, left' =
            add_while_testing_freshness warn parameter error key data left
          in
          let error, map =
            balance_with_logs warn parameter error left' key_map data_map right
          in
          error, bool, map
        ) else (
          let error, bool, right' =
            add_while_testing_freshness warn parameter error key data right
          in
          let error, map =
            balance_with_logs warn parameter error left key_map data_map right'
          in
          error, bool, map
        )

    let add_with_logs warn parameter error key data map =
      let error, bool, map =
        add_while_testing_freshness warn parameter error key data map
      in
      if bool then
        error, map
      else (
        let a, b, _, _ = __POS__ in
        ( warn parameter error "setMap.ml "
            (Some
               (a ^ " line: " ^ string_of_int b
              ^ ": Attempt to add an association over a former one in a map"))
            (Failure "Set_and_Map.Map.add"),
          map )
      )

    let rec extract_min_binding map key data map' =
      match map with
      | Private.Empty -> (key, data), map'
      | Private.Node (left2, key2, data2, right2, _, _) ->
        let min, left' = extract_min_binding left2 key2 data2 right2 in
        min, balance left' key data map'

    let rec extract_min_binding_with_logs warn parameters error map key data
        map' =
      match map with
      | Private.Empty -> error, ((key, data), map')
      | Private.Node (left2, key2, data2, right2, _, _) ->
        let error, (min, left') =
          extract_min_binding_with_logs warn parameters error left2 key2 data2
            right2
        in
        error, (min, balance left' key data map')

    let merge map1 map2 =
      match map1 with
      | Private.Empty -> map2
      | Private.Node _ ->
        (match map2 with
        | Private.Empty -> map1
        | Private.Node (left2, key2, data2, right2, _, _) ->
          let (key3, data3), left' =
            extract_min_binding left2 key2 data2 right2
          in
          balance map1 key3 data3 left')

    let merge_with_logs warn parameters error map1 map2 =
      match map1 with
      | Private.Empty -> error, map2
      | Private.Node _ ->
        (match map2 with
        | Private.Empty -> error, map1
        | Private.Node (left2, key2, data2, right2, _, _) ->
          let error, ((key3, data3), left') =
            extract_min_binding_with_logs warn parameters error left2 key2 data2
              right2
          in
          balance_with_logs warn parameters error map1 key3 data3 left')

    let rec remove key = function
      | Private.Empty -> empty
      | Private.Node (left, key_map, data, right, _, _) ->
        let cmp = Ord.compare key key_map in
        if cmp = 0 then
          merge left right
        else if cmp < 0 then
          balance (remove key left) key_map data right
        else
          balance left key_map data (remove key right)

    let rec remove_while_testing_existence warn parameters error key map =
      match map with
      | Private.Empty -> error, false, empty
      | Private.Node (left, key_map, data, right, _, _) ->
        let cmp = Ord.compare key key_map in
        if cmp = 0 then (
          let error, map = merge_with_logs warn parameters error left right in
          error, true, map
        ) else if cmp < 0 then (
          let error, bool, left' =
            remove_while_testing_existence warn parameters error key left
          in
          if left' == left then (
            let error =
              if bool then
                warn parameters error "setMap.ml" (Some "SetMap line 961")
                  (failwith "Invariant is broken")
              else
                error
            in
            error, bool, map
          ) else (
            let error, map =
              balance_with_logs warn parameters error left' key_map data right
            in
            error, bool, map
          )
        ) else (
          let error, bool, right' =
            remove_while_testing_existence warn parameters error key right
          in
          if right' == right then (
            let error =
              if bool then
                warn parameters error "setMap.ml" (Some "SetMap line 978")
                  (failwith "Invariant is broken")
              else
                error
            in
            error, bool, map
          ) else (
            let error, map =
              balance_with_logs warn parameters error left key_map data right'
            in
            error, bool, map
          )
        )

    let remove_with_logs warn parameters error key map =
      let error, bool, map =
        remove_while_testing_existence warn parameters error key map
      in
      if bool then
        error, map
      else
        ( warn parameters error "setMap.ml"
            (Some
               ("SetMap line 994"
              ^ "Try to remove an association that is not defined in Map.remove"
               ))
            (failwith
               "Try to remove an association that is not defined in Map.remove"),
          map )

    let rec pop x = function
      | Private.Empty as m -> None, m
      | Private.Node (l, v, d, r, _, _) as m ->
        let c = Ord.compare x v in
        if c = 0 then
          Some d, merge l r
        else if c < 0 then (
          match pop x l with
          | (None as o), _ -> o, m
          | (Some _ as o), t -> o, balance t v d r
        ) else (
          match pop x r with
          | (None as o), _ -> o, m
          | (Some _ as o), t -> o, balance l v d t
        )

    let rec join left key value right =
      match balance left key value right with
      | Private.Empty ->
        raise (DeadCodeIsNotDead "SetMap line 1013") (* By case analysis *)
      | Private.Node (left2, key2, data2, right2, _, _) as map2 ->
        let h = height left2 - height right2 in
        if h > 2 || h < -2 then
          join left2 key2 data2 right2
        else
          map2

    let rec join_with_logs warn parameters error left key value right =
      match balance_with_logs warn parameters error left key value right with
      | error, Private.Empty ->
        let error =
          warn parameters error "setMap.ml"
            (Some
               "Map.join_with_logs, line 986, the output of balance should not \
                be empty")
            (failwith "the output of balance should not be empty")
        in
        error, empty
      | error, (Private.Node (left2, key2, data2, right2, _, _) as map2) ->
        let h = height left2 - height right2 in
        if h > 2 || h < -2 then
          join_with_logs warn parameters error left2 key2 data2 right2
        else
          error, map2

    let rec split value = function
      | Private.Empty -> empty, None, empty
      | Private.Node (left1, key1, data1, right1, _, _) ->
        let cmp = Ord.compare value key1 in
        if cmp = 0 then
          left1, Some data1, right1
        else if cmp < 0 then (
          let left2, data2, right2 = split value left1 in
          let right2' = join right2 key1 data1 right1 in
          left2, data2, right2'
        ) else (
          let left2, data2, right2 = split value right1 in
          let left2' = join left1 key1 data1 left2 in
          left2', data2, right2
        )

    let rec split_with_logs warn parameters error value map =
      match map with
      | Private.Empty -> error, (empty, None, empty)
      | Private.Node (left1, key1, data1, right1, _, _) ->
        let cmp = Ord.compare value key1 in
        if cmp = 0 then
          error, (left1, Some data1, right1)
        else if cmp < 0 then (
          let error, (left2, data2, right2) =
            split_with_logs warn parameters error value left1
          in
          let error, right2' =
            join_with_logs warn parameters error right2 key1 data1 right1
          in
          error, (left2, data2, right2')
        ) else (
          let error, (left2, data2, right2) =
            split_with_logs warn parameters error value right1
          in
          let error, left2' =
            join_with_logs warn parameters error left1 key1 data1 left2
          in
          error, (left2', data2, right2)
        )

    let rec diff map1 map2 =
      match map1 with
      | Private.Empty -> empty, map2
      | Private.Node (left1, key1, data1, right1, _, _) ->
        let left2, data2, right2 = split key1 map2 in
        let oleft1, oleft2 = diff left1 left2 in
        let oright1, oright2 = diff right1 right2 in
        (match data2 with
        | Some x when x = data1 -> merge oleft1 oright1, merge oleft2 oright2
        | Some data2 ->
          join oleft1 key1 data1 oright1, join oleft2 key1 data2 oright2
        | None -> join oleft1 key1 data1 oright1, merge oleft2 oright2)

    let rec union map1 map2 =
      match map1, map2 with
      | Private.Empty, _ -> map2
      | _, Private.Empty -> map1
      | ( Private.Node (left1, value1, data1, right1, height1, _),
          Private.Node (left2, value2, data2, right2, height2, _) ) ->
        if height1 >= height2 then (
          let left2, op_data2, right2 = split value1 map2 in
          join (union left1 left2) value1
            (match op_data2 with
            | None -> data1
            | Some d2 -> d2)
            (union right1 right2)
        ) else (
          let left1, op_data1, right1 = split value2 map1 in
          join (union left1 left2) value1
            (match op_data1 with
            | None -> data2
            | Some d1 -> d1)
            (union right1 right2)
        )

    let rec union_with_logs warn parameters error map1 map2 =
      match map1, map2 with
      | Private.Empty, _ -> error, map2
      | _, Private.Empty -> error, map1
      | ( Private.Node (left1, value1, data1, right1, height1, _),
          Private.Node (left2, value2, data2, right2, height2, _) ) ->
        if height1 >= height2 then (
          let error, (left2, op_data2, right2) =
            split_with_logs warn parameters error value1 map2
          in
          let error, left' =
            union_with_logs warn parameters error left1 left2
          in
          let error, right' =
            union_with_logs warn parameters error right1 right2
          in
          join_with_logs warn parameters error left' value1
            (match op_data2 with
            | None -> data1
            | Some d2 -> d2)
            right'
        ) else (
          let error, (left1, op_data1, right1) =
            split_with_logs warn parameters error value2 map1
          in
          let error, left' =
            union_with_logs warn parameters error left1 left2
          in
          let error, right' =
            union_with_logs warn parameters error right1 right2
          in
          join_with_logs warn parameters error left' value1
            (match op_data1 with
            | None -> data2
            | Some d1 -> d1)
            right'
        )

    let rec update map1 map2 =
      if map1 == map2 then
        map2
      else (
        match map1 with
        | Private.Empty -> map2
        | Private.Node (left1, key1, data1, right1, _, _) ->
          let left2, data2, right2 = split key1 map2 in
          join (update left1 left2) key1
            (match data2 with
            | None -> data1
            | Some d2 -> d2)
            (update right1 right2)
      )

    let rec update_with_logs warn parameters error map1 map2 =
      if map1 == map2 then
        error, map2
      else (
        match map1 with
        | Private.Empty -> error, map2
        | Private.Node (left1, key1, data1, right1, _, _) ->
          let error, (left2, data2, right2) =
            split_with_logs warn parameters error key1 map2
          in
          let error, left' =
            update_with_logs warn parameters error left1 left2
          in
          let error, right' =
            update_with_logs warn parameters error right1 right2
          in
          join_with_logs warn parameters error left' key1
            (match data2 with
            | None -> data1
            | Some d2 -> d2)
            right'
      )

    let rec diff_pred pred map1 map2 =
      match map1 with
      | Private.Empty -> empty, map2
      | Private.Node (left1, key1, data1, right1, _, _) ->
        let left2, data2, right2 = split key1 map2 in
        let oleft1, oleft2 = diff_pred pred left1 left2 in
        let oright1, oright2 = diff_pred pred right1 right2 in
        (match data2 with
        | Some x when pred x data1 -> merge oleft1 oright1, merge oleft2 oright2
        | Some data2 ->
          join oleft1 key1 data1 oright1, join oleft2 key1 data2 oright2
        | None -> join oleft1 key1 data1 oright1, merge oleft2 oright2)

    let rec min_elt = function
      | Private.Empty -> None
      | Private.Node (Private.Empty, key, data, _, _, _) -> Some (key, data)
      | Private.Node (left, _, _, _, _, _) -> min_elt left

    let rec find_option key = function
      | Private.Empty -> None
      | Private.Node (left, key_map, data, right, _, _) ->
        let cmp = Ord.compare key key_map in
        if cmp = 0 then
          Some data
        else
          find_option key
            (if cmp < 0 then
               left
             else
               right)

    let rec find_default d key = function
      | Private.Empty -> d
      | Private.Node (left, key_map, data, right, _, _) ->
        let cmp = Ord.compare key key_map in
        if cmp = 0 then
          data
        else
          find_default d key
            (if cmp < 0 then
               left
             else
               right)

    let rec find_option_with_logs warn parameter error key = function
      | Private.Empty ->
        let error =
          warn parameter error "setMap.ml" (Some "line 659") Not_found
        in
        error, None
      | Private.Node (left, key_map, data, right, _, _) ->
        let cmp = Ord.compare key key_map in
        if cmp = 0 then
          error, Some data
        else
          find_option_with_logs warn parameter error key
            (if cmp < 0 then
               left
             else
               right)

    let rec find_default_with_logs warn parameter error d key = function
      | Private.Empty ->
        let error =
          warn parameter error "setMap.ml" (Some "line 669") Not_found
        in
        error, d
      | Private.Node (left, key_map, data, right, _, _) ->
        let cmp = Ord.compare key key_map in
        if cmp = 0 then
          error, data
        else
          find_default_with_logs warn parameter error d key
            (if cmp < 0 then
               left
             else
               right)

    let rec mem key = function
      | Private.Empty -> false
      | Private.Node (left, key_map, _, right, _, _) ->
        let cmp = Ord.compare key key_map in
        cmp == 0
        ||
        if cmp > 0 then
          mem key right
        else
          mem key left

    let rec filter_one p = function
      | Private.Empty -> None
      | Private.Node (left, key, value, right, _, _) ->
        if p key value then
          Some (key, value)
        else (
          match filter_one p left with
          | None -> filter_one p right
          | out -> out
        )

    let rec iter f = function
      | Private.Empty -> ()
      | Private.Node (left, key, data, right, _, _) ->
        let () = iter f left in
        let () = f key data in
        iter f right

    let rec fold f map value =
      match map with
      | Private.Empty -> value
      | Private.Node (left, key, data, right, _, _) ->
        fold f right (f key data (fold f left value))

    let rec fold_with_interruption f map value =
      match map with
      | Private.Empty -> false, Stop.success value
      | Private.Node (left, key, data, right, _, _) ->
        let outputl = fold_with_interruption f left value in
        let interrupted, value = outputl in
        if interrupted then
          outputl
        else
          Stop.success_or_stop
            (fun value ->
              let val_opt =
                try Some (f key data value) with Sys.Break -> None
              in
              match val_opt with
              | None -> true, Stop.success value
              | Some v ->
                Stop.success_or_stop
                  (fun v -> fold_with_interruption f right v)
                  (fun v -> false, Stop.stop v)
                  v)
            (fun x -> false, Stop.stop x)
            value

    let fold_with_interruption f map value =
      snd (fold_with_interruption f map value)

    let rec monadic_fold param err f map value =
      match map with
      | Private.Empty -> err, value
      | Private.Node (left, key, data, right, _, _) ->
        let err', value' = monadic_fold param err f left value in
        let err'', value'' = f param err' key data value' in
        monadic_fold param err'' f right value''

    let rec monadic_fold2 parameters rh f g h map1 map2 res =
      match map1, map2 with
      | Private.Empty, Private.Empty -> rh, res
      | Private.Empty, _ -> monadic_fold parameters rh h map2 res
      | _, Private.Empty -> monadic_fold parameters rh g map1 res
      | Private.Node (left1, key1, data1, right1, _, _), _ ->
        let left2, data2, right2 = split key1 map2 in
        (match data2 with
        | None ->
          let rh', res' = monadic_fold2 parameters rh f g h left1 left2 res in
          let rh'', res'' = g parameters rh' key1 data1 res' in
          monadic_fold2 parameters rh'' f g h right1 right2 res''
        | Some data2 ->
          let rh', res' = monadic_fold2 parameters rh f g h left1 left2 res in
          let rh'', res'' = f parameters rh' key1 data1 data2 res' in
          monadic_fold2 parameters rh'' f g h right1 right2 res'')

    let monadic_fold2_sparse parameters rh f map1 map2 res =
      let id _ x _ _ y = x, y in
      monadic_fold2 parameters rh f id id map1 map2 res

    let monadic_iter2_sparse parameters rh f map1 map2 =
      let error, () =
        monadic_fold2_sparse parameters rh
          (fun p e k a b () -> f p e k a b, ())
          map1 map2 ()
      in
      error

    let rec monadic_fold_restriction parameters rh f set map res =
      match set with
      | Set.Private.Empty -> rh, res
      | Set.Private.Node (left1, key1, right1, _, _) ->
        let left2, data2, right2 = split key1 map in
        (match data2 with
        | None ->
          let rh', res' =
            monadic_fold_restriction parameters rh f left1 left2 res
          in
          monadic_fold_restriction parameters rh' f right1 right2 res'
        | Some data2 ->
          let rh', res' =
            monadic_fold_restriction parameters rh f left1 left2 res
          in
          let rh'', res'' = f parameters rh' key1 data2 res' in
          monadic_fold_restriction parameters rh'' f right1 right2 res'')

    let rec mapi f = function
      | Private.Empty -> empty
      | Private.Node (left, key, data, right, _, _) ->
        node (mapi f left) key (f key data) (mapi f right)

    let map f s = mapi (fun _ x -> f x) s

    let rec map_with_logs warn parameters errors f map =
      match map with
      | Private.Empty -> errors, empty
      | Private.Node (left, key, data, right, _, _) ->
        let errors, left' = map_with_logs warn parameters errors f left in
        let errors, data' = f parameters errors data in
        let error, right' = map_with_logs warn parameters errors f right in
        error, node left' key data' right'

    let rec map2 f map map' =
      match map with
      | Private.Empty -> map'
      | Private.Node (left1, key1, data1, right1, _, _) ->
        let left2, data2, right2 = split key1 map' in
        join (map2 f left1 left2) key1
          (match data2 with
          | None -> data1
          | Some d2 -> f data1 d2)
          (map2 f right1 right2)

    let rec map2_with_logs warn parameters errors f g h map1 map2 =
      match map1 with
      | Private.Empty ->
        (match map2 with
        | Private.Empty -> errors, empty
        | Private.Node _ -> map_with_logs warn parameters errors g map2)
      | Private.Node (left1, key1, data1, right1, _, _) ->
        let errors, (left2, data2, right2) =
          split_with_logs warn parameters errors key1 map2
        in
        let errors, left' =
          map2_with_logs warn parameters errors f g h left1 left2
        in
        let error, right' =
          map2_with_logs warn parameters errors f g h right1 right2
        in
        let error, data' =
          match data2 with
          | None -> f parameters error data1
          | Some d2 -> h parameters errors data1 d2
        in
        join_with_logs warn parameters error left' key1 data' right'

    let map2z_with_logs warn parameters errors =
      map2_with_logs warn parameters errors
        (fun parameters error a ->
          let error =
            warn parameters error "setMap.ml"
              (Some "line 1248, incompatible maps in map2z_safe") Not_found
          in
          error, a)
        (fun parameters error a ->
          let error =
            warn parameters error "setMap.ml"
              (Some "line 1251, incompatible maps in map2z_safe") Not_found
          in
          error, a)

    let rec fold2_with_logs warn parameters error f g h map1 map2 res =
      match map1, map2 with
      | Private.Empty, Private.Empty -> error, res
      | Private.Empty, _ -> monadic_fold parameters error g map2 res
      | _, Private.Empty -> monadic_fold parameters error f map1 res
      | Private.Node (left1, key1, data1, right1, _, _), _ ->
        let error, (left2, data2, right2) =
          split_with_logs warn parameters error key1 map2
        in
        (match data2 with
        | None ->
          let error, res' =
            fold2_with_logs warn parameters error f g h left1 left2 res
          in
          let error, res'' = f parameters error key1 data1 res' in
          fold2_with_logs warn parameters error f g h right1 right2 res''
        | Some data2 ->
          let error, res' =
            fold2_with_logs warn parameters error f g h left1 left2 res
          in
          let error, res'' = h parameters error (key1 : elt) data1 data2 res' in
          fold2_with_logs warn parameters error f g h right1 right2 res'')

    let fold2z_with_logs warn parameters error =
      fold2_with_logs warn parameters error
        (fun parameters error _ _ a ->
          let error =
            warn parameters error "setMap.ml"
              (Some "line 1248, incompatible maps in fold2z_safe") Not_found
          in
          error, a)
        (fun parameters error _ _ a ->
          let error =
            warn parameters error "setMap.ml"
              (Some "line 1251, incompatible maps in fold2z_safe") Not_found
          in
          error, a)

    let rec fold2_sparse_with_logs warn parameters error f map1 map2 res =
      match map1, map2 with
      | Private.Empty, _ | _, Private.Empty -> error, res
      | Private.Node (left1, key1, data1, right1, _, _), _ ->
        let error, (left2, data2, right2) =
          split_with_logs warn parameters error key1 map2
        in
        (match data2 with
        | None ->
          let error, res' =
            fold2_sparse_with_logs warn parameters error f left1 left2 res
          in
          fold2_sparse_with_logs warn parameters error f right1 right2 res'
        | Some data2 ->
          let error, res' =
            fold2_sparse_with_logs warn parameters error f left1 left2 res
          in
          let error, res'' = f parameters error key1 data1 data2 res' in
          fold2_sparse_with_logs warn parameters error f right1 right2 res'')

    let iter2_sparse_with_logs warn parameters error f map1 map2 =
      let error, _ =
        fold2_sparse_with_logs warn parameters error
          (fun par err a b c _ -> f par err a b c, ())
          map1 map2 ()
      in
      error

    let rec for_all p = function
      | Private.Empty -> true
      | Private.Node (left, key, data, right, _, _) ->
        p key data && for_all p right && for_all p left

    type 'a enumeration = End | More of elt * 'a * 'a t * 'a enumeration

    let rec cons_enum m e =
      match m with
      | Private.Empty -> e
      | Private.Node (l, v, d, r, _, _) -> cons_enum l (More (v, d, r, e))

    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
        match e1, e2 with
        | End, End -> 0
        | End, _ -> -1
        | _, End -> 1
        | More (v1, d1, r1, e1), More (v2, d2, r2, e2) ->
          let c = Ord.compare v1 v2 in
          if c <> 0 then
            c
          else (
            let c = cmp d1 d2 in
            if c <> 0 then
              c
            else
              compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
          )
      in
      compare_aux (cons_enum m1 End) (cons_enum m2 End)

    let equal cmp m1 m2 =
      compare
        (fun x y ->
          if cmp x y then
            0
          else
            1)
        m1 m2
      == 0

    let rec bindings_aux accu = function
      | Private.Empty -> accu
      | Private.Node (l, v, d, r, _, _) ->
        bindings_aux ((v, d) :: bindings_aux accu r) l

    let bindings s = bindings_aux [] s

    let rec aux_print pr f = function
      | Private.Empty -> ()
      | Private.Node (Private.Empty, key, data, Private.Empty, _, _) ->
        Format.fprintf f "@[%a->@,%a@]" Ord.print key pr data
      | Private.Node (Private.Empty, key, data, right, _, _) ->
        Format.fprintf f "@[%a->%a@],@ %a" Ord.print key pr data (aux_print pr)
          right
      | Private.Node (left, key, data, Private.Empty, _, _) ->
        Format.fprintf f "%a,@ @[%a->%a@]" (aux_print pr) left Ord.print key pr
          data
      | Private.Node (left, key, data, right, _, _) ->
        Format.fprintf f "%a,@ @[%a->%a@],@ %a" (aux_print pr) left Ord.print
          key pr data (aux_print pr) right

    let print pr f = function
      | Private.Empty -> Format.pp_print_string f "\xE2\x88\x85"
      | Private.Node _ as m -> Format.fprintf f "@[{%a}@]" (aux_print pr) m

    let rec diff_with_logs warn parameters error map1 map2 =
      match map1 with
      | Private.Empty -> error, empty, map2
      | Private.Node (left1, key1, data1, right1, _, _) ->
        let error, (left2, data2, right2) =
          split_with_logs warn parameters error key1 map2
        in
        let error, oleft1, oleft2 =
          diff_with_logs warn parameters error left1 left2
        in
        let error, oright1, oright2 =
          diff_with_logs warn parameters error right1 right2
        in
        (match data2 with
        | Some x when x = data1 ->
          let error, o1 =
            merge_with_logs warn parameters error oleft1 oright1
          in
          let error, o2 =
            merge_with_logs warn parameters error oleft2 oright2
          in
          error, o1, o2
        | Some data2 ->
          let error, o1 =
            join_with_logs warn parameters error oleft1 key1 data1 oright1
          in
          let error, o2 =
            join_with_logs warn parameters error oleft2 key1 data2 oright2
          in
          error, o1, o2
        | None ->
          let error, o1 =
            join_with_logs warn parameters error oleft1 key1 data1 oright1
          in
          let error, o2 =
            merge_with_logs warn parameters error oleft2 oright2
          in
          error, o1, o2)

    let rec diff_pred_with_logs warn parameters error pred map1 map2 =
      match map1 with
      | Private.Empty -> error, empty, map2
      | Private.Node (left1, key1, data1, right1, _, _) ->
        let error, (left2, data2, right2) =
          split_with_logs warn parameters error key1 map2
        in
        let error, oleft1, oleft2 =
          diff_pred_with_logs warn parameters error pred left1 left2
        in
        let error, oright1, oright2 =
          diff_pred_with_logs warn parameters error pred right1 right2
        in
        (match data2 with
        | Some x when pred x data1 ->
          let error, o1 =
            merge_with_logs warn parameters error oleft1 oright1
          in
          let error, o2 =
            merge_with_logs warn parameters error oleft2 oright2
          in
          error, o1, o2
        | Some data2 ->
          let error, o1 =
            join_with_logs warn parameters error oleft1 key1 data1 oright1
          in
          let error, o2 =
            join_with_logs warn parameters error oleft2 key1 data2 oright2
          in
          error, o1, o2
        | None ->
          let error, o1 =
            join_with_logs warn parameters error oleft1 key1 data1 oright1
          in
          let error, o2 =
            merge_with_logs warn parameters error oleft2 oright2
          in
          error, o1, o2)

    let rec fold_restriction_with_missing_associations_with_logs warn parameters
        error f g set map res =
      match set, map with
      | Set.Private.Empty, _ -> error, res
      | Set.Private.Node (left1, key1, right1, _, _), _ ->
        let error, (left2, data2, right2) =
          split_with_logs warn parameters error key1 map
        in
        (match data2 with
        | None ->
          let error, res' =
            fold_restriction_with_missing_associations_with_logs warn parameters
              error f g left1 left2 res
          in
          let error, res'' = g key1 (error, res') in
          fold_restriction_with_missing_associations_with_logs warn parameters
            error f g right1 right2 res''
        | Some data2 ->
          let error, res' =
            fold_restriction_with_missing_associations_with_logs warn parameters
              error f g left1 left2 res
          in
          let error, res'' = f key1 data2 (error, res') in
          fold_restriction_with_missing_associations_with_logs warn parameters
            error f g right1 right2 res'')

    let fold_restriction_with_logs warn parameters error f set map res =
      fold_restriction_with_missing_associations_with_logs warn parameters error
        f
        (fun _ x -> x)
        set map res

    let to_json ?(lab_key = "key") ?(lab_value = "value") =
      JsonUtil.of_map ~lab_key ~lab_value ~fold

    let of_json ?(lab_key = "key") ?(lab_value = "value")
        ?(error_msg = JsonUtil.build_msg "map") =
      JsonUtil.to_map ~lab_key ~lab_value ~error_msg ~add ~empty
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
    (elt_a -> elt_b) -> 'b -> ('b -> 'a -> 'b) -> 'a map_a -> 'b map_b

  val proj_map_monadic :
    'parameters ->
    'method_handler ->
    (elt_a -> elt_b) ->
    'b ->
    ('parameters -> 'method_handler -> 'b -> 'a -> 'method_handler * 'b) ->
    'a map_a ->
    'method_handler * 'b map_b

  val proj_set : (elt_a -> elt_b) -> set_a -> set_b

  val proj_set_monadic :
    'parameters ->
    'method_handler ->
    ('parameters -> 'method_handler -> elt_a -> 'method_handler * elt_b) ->
    set_a ->
    'method_handler * set_b

  val partition_set : (elt_a -> elt_b) -> set_a -> set_a map_b

  val partition_set_monadic :
    'parameters ->
    'method_handler ->
    ('parameters -> 'method_handler -> elt_a -> 'method_handler * elt_b) ->
    set_a ->
    'method_handler * set_a map_b
end

module Proj (A : S) (B : S) = struct
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

  let proj_map f identity_elt merge map =
    MA.fold
      (fun key_a data_a map_b ->
        let key_b = f key_a in
        MB.add key_b
          (merge (MB.find_default identity_elt key_b map_b) data_a)
          map_b)
      map MB.empty

  let proj_map_monadic parameter handler f identity_elt monadic_merge map =
    MA.fold
      (fun key_a data_a (handler, map_b) ->
        let key_b = f key_a in
        let handler, data' =
          monadic_merge parameter handler
            (MB.find_default identity_elt key_b map_b)
            data_a
        in
        handler, MB.add key_b data' map_b)
      map (handler, MB.empty)

  let proj_set f set_a = SA.fold (fun key_a -> SB.add (f key_a)) set_a SB.empty

  let proj_set_monadic parameter handler f set_a =
    SA.fold
      (fun key_a (handler, set_b) ->
        let handler, key_b = f parameter handler key_a in
        handler, SB.add key_b set_b)
      set_a (handler, SB.empty)

  let partition_set f set_a =
    SA.fold
      (fun key_a map_b ->
        let key_b = f key_a in
        MB.add key_b (SA.add key_a (MB.find_default SA.empty key_b map_b)) map_b)
      set_a MB.empty

  let partition_set_monadic parameter handler f set_a =
    SA.fold
      (fun key_a (handler, map_b) ->
        let handler, key_b = f parameter handler key_a in
        ( handler,
          MB.add key_b
            (SA.add key_a (MB.find_default SA.empty key_b map_b))
            map_b ))
      set_a (handler, MB.empty)
end
(* todo: add the following  test to the sanity tests *)

(*
(* for instance, the following code: *)

module IntMap = Make(struct type t = int let compare = compare end)
module P = Proj(IntMap)(IntMap)


let f = List.fold_left
(fun map (a,b) -> IntMap.Map.add a b map)
IntMap.Map.empty
[1,[2;3];2,[3;4];5,[6;7];8,[12;13]]

(* bad implementation (quadratic time complexity) *)
let g = P.proj (fun i -> i mod 2) [] (List.append) f

(* good implementation (linear time complexity)*)
let g' = IntMap.Map.map List.rev (P.proj (fun i -> i mod 2) [] (fun x y -> List.append (List.rev y) x) f)


let dump (s:string) f =
  let _ = Printf.fprintf stderr "%s: \n" s in
  let _ = IntMap.Map.iter
(fun a l ->
let _ = Printf.fprintf stderr "  %i:" a in
let _ = List.iter (Printf.fprintf stderr "%i,") l in
let _ = Printf.fprintf stderr "\n" in ())
f in
  ()
let _ = dump "f" f
let _ = dump "g" g
let _ = dump "g'" g'

(* should dump: *)
(*

f:
  1:2,3,
  2:3,4,
  5:6,7,
  8:12,13,
g:
  0:3,4,12,13,
  1:2,3,6,7,
 *)
 *)

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

module Proj2 (A : S) (B : S) (C : S) = struct
  module MA = A.Map
  module MB = B.Map
  module MC = C.Map

  type elt_a = MA.elt
  type elt_b = MB.elt
  type elt_c = MC.elt
  type 'a map_a = 'a MA.t
  type 'a map_b = 'a MB.t
  type 'a map_c = 'a MC.t

  let proj2 f g identity_elt merge map =
    MA.fold
      (fun key_a data_a map_b ->
        let key_b = f key_a in
        let key_c = g key_a in
        let submap = MB.find_default MC.empty key_b map_b in
        let submap =
          MC.add key_c
            (merge (MC.find_default identity_elt key_c submap) data_a)
            submap
        in
        MB.add key_b submap map_b)
      map MB.empty

  let proj2_monadic parameter handler f g identity_elt merge map =
    MA.fold
      (fun key_a data_a (handler, map_b) ->
        let key_b = f key_a in
        let key_c = g key_a in
        let submap = MB.find_default MC.empty key_b map_b in
        let handler, data' =
          merge parameter handler
            (MC.find_default identity_elt key_c submap)
            data_a
        in
        let submap = MC.add key_c data' submap in
        handler, MB.add key_b submap map_b)
      map (handler, MB.empty)
end
