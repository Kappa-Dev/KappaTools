(**
  * cache.ml
  *
  * a module for KaSim
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * Creation: 27/03/2012
  * Last modification: 27/03/2012
  * *
  *
  * It uses imperative styles to ensure compatibility with other modules
  *
  * Copyright 2011,2012  Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Cache = sig
  module O : SetMap.OrderedType

  type t

  val last : t -> O.t option
  val create : int option -> t
  val add : O.t -> t -> t
  val fold : (O.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (O.t -> unit) -> t -> unit
end

module Cache =
functor
  (OO : SetMap.OrderedType)
  ->
  (
    struct
      module O = OO
      module SM = SetMap.Make (O)
      module S = SM.Set

      type finite_cache = {
        size: int;
        offset: int;
        cache: O.t option array;
        bag: S.t;
        last: O.t option;
      }

      let create_finite n =
        {
          size = n;
          offset = 0;
          cache = Array.make n None;
          bag = S.empty;
          last = None;
        }

      let next t =
        let offset = t.offset in
        if offset = t.size - 1 then
          { t with offset = 0 }
        else
          { t with offset = offset + 1 }

      let add_finite key t =
        let t = { t with last = Some key } in
        if S.mem key t.bag then
          t
        else (
          let t = { t with bag = S.add key t.bag } in
          let t = next t in
          let overwritten_value = t.cache.(t.offset) in
          let t =
            match overwritten_value with
            | None -> t
            | Some overwritten_value ->
              { t with bag = S.remove overwritten_value t.bag }
          in
          let _ = t.cache.(t.offset) <- Some key in
          t
        )

      let last_finite t = t.last

      type infinite_cache = { inf_bag: S.t; last: O.t option }
      type t = Finite of finite_cache | Infinite of infinite_cache

      let create_infinite = { inf_bag = S.empty; last = None }

      let add_infinite key t =
        { inf_bag = S.add key t.inf_bag; last = Some key }

      let create size =
        match size with
        | None -> Infinite create_infinite
        | Some a -> Finite (create_finite a)

      let last t =
        match t with
        | Finite t -> last_finite t
        | Infinite t -> t.last

      let add key t =
        match t with
        | Finite t -> Finite (add_finite key t)
        | Infinite t -> Infinite (add_infinite key t)

      let fold f t a =
        match t with
        | Finite t -> S.fold f t.bag a
        | Infinite t -> S.fold f t.inf_bag a

      let iter f t =
        match t with
        | Finite t -> S.iter f t.bag
        | Infinite t -> S.iter f t.inf_bag
    end :
      Cache with type O.t = OO.t)
