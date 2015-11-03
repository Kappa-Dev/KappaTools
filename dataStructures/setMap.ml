(**
   * setMap.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   * KaSim
   * Pierre Boutillier, PPS, CNRS - Univ Paris Diderot
   *
   * Creation: 2010, the 7th of July
   * Last modification: 2015, November 3rd
   *
   * This library provides primitives to deal with Set and Maps of of ordered
   * elements, in the fashion of Ocaml's Map; It provides efficient iterators
   *
   * Copyright 2010,2011,2012,2013 Institut National de Recherche en
   * Informatique et en Automatique.
   * Copyright 2015 Havard Medical School
   * All rights reserved.  This file is distributed under the terms of the GNU
   * Library General Public License *)

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module type S = sig
    type elt

    module Set :
    sig
      type t

      val empty: t
      val is_empty: t -> bool
      val singleton: elt -> t
      val is_singleton: t -> bool

      val add: elt -> t -> t
      val remove: elt -> t -> t
      val split: elt -> t -> (t * bool * t)
      val union: t -> t -> t
      val inter: t -> t -> t
      val diff: t -> t -> t

      val cardinal: t -> int

      val mem: elt -> t -> bool
      val exists: (elt -> bool) -> t -> bool
      val filter: (elt -> bool) -> t -> t
      val for_all: (elt -> bool) -> t -> bool
      val partition: (elt -> bool) -> t -> t * t

      val compare: t -> t -> int
      val equal: t -> t -> bool
      val subset: t -> t -> bool

      val iter: (elt -> unit) -> t -> unit
      val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a

      val elements: t -> elt list
      val choose: t -> elt option
      val min_elt: t -> elt option
      val max_elt: t -> elt option
    end

  (*------------------------------------------------------------------------------*)
  (*map interface*)
    module Map:
    sig
      type 'a t

      val empty: 'a t
      val is_empty: 'a t -> bool
      val cardinal : 'a t -> int

      val add: elt -> 'a -> 'a t -> 'a t
      val remove: elt -> 'a t -> 'a t
      val merge: 'a t -> 'a t -> 'a t
      val min_elt: (elt -> 'a -> bool) -> 'a t -> elt option
      val find_option: elt -> 'a t -> 'a option
      val find_default: 'a -> elt -> 'a t -> 'a
      val mem:  elt -> 'a t -> bool
      val diff: 'a t -> 'a t -> 'a t * 'a t
      val union: 'a t -> 'a t -> 'a t
      val update: 'a t -> 'a t -> 'a t
      val diff_pred: ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t * 'a t

      val iter: (elt -> 'a -> unit) -> 'a t -> unit
      val fold: (elt -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val monadic_fold2:
	'parameters -> 'method_handler ->
	('parameters -> 'method_handler ->
	 elt -> 'a -> 'b -> 'c -> ('method_handler * 'c)) ->
	('parameters -> 'method_handler ->
	 elt -> 'a -> 'c -> ('method_handler * 'c)) ->
	('parameters -> 'method_handler ->
	 elt -> 'b -> 'c -> ('method_handler * 'c)) ->
	'a t -> 'b t -> 'c -> ('method_handler * 'c)
      val monadic_fold2_sparse:
	'parameters -> 'method_handler ->
	('parameters -> 'method_handler ->
	 elt -> 'a -> 'b -> 'c -> ('method_handler * 'c)) ->
	'a t -> 'b t -> 'c -> ('method_handler * 'c)
      val monadic_iter2_sparse:
	'parameters -> 'method_handler ->
	('parameters -> 'method_handler ->
	 elt -> 'a -> 'b -> 'method_handler) ->
	'a t -> 'b t -> 'method_handler
      val monadic_fold_restriction:
	'parameters -> 'method_handler ->
	('parameters -> 'method_handler ->
	 elt -> 'a -> 'b -> ('method_handler * 'b)) ->
	Set.t -> 'a t -> 'b -> 'method_handler * 'b

      val mapi: (elt -> 'a -> 'b) -> 'a t -> 'b t
      val map: ('a -> 'b) -> 'a t -> 'b t
      val map2: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

      val for_all: (elt -> 'a -> bool) -> 'a t -> bool
      val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
      val bindings : 'a t -> (elt * 'a) list
    end
  end

module Make(Ord:OrderedType): S with type elt = Ord.t =
  struct
    type elt = Ord.t

    module Set =
      struct
	type t = Empty | Node of t * elt * t * int

	let empty = Empty
	let is_empty set = set == Empty
	let singleton value = Node(Empty,value,Empty,1)
	let is_singleton set =
	  match set with
            Empty -> false
          | Node (set1,_,set2,_) -> is_empty set1 && is_empty set2

	let height t =
	  match t with
          | Empty -> 0
          | Node(_,_,_,h) -> h
	let rec cardinal = function
	  | Empty -> 0
	  | Node(left,_,right,_) -> cardinal left + 1 + cardinal right

	let node left value right =
	  Node(left,value,right,(max (height left) (height right))+1)

	let balance left value right =
	  let height_left = height left in
	  let height_right = height right in
	  if height_left > height_right + 2 then
            match left with
            | Empty ->
	       assert false (* height_left > height_right + 2 >= 2 *)
            | Node(leftleft,leftvalue,leftright,_) ->
	       if height leftleft >= height leftright then
		 node leftleft leftvalue (node leftright value right)
	       else
		 match leftright with
                 | Empty ->
		    assert false (* 0 <= height leftleft < height leftright *)
                 | Node(leftrightleft,leftrightvalue,leftrightright,_) ->
		    node
		      (node leftleft leftvalue leftrightleft)
		      leftrightvalue
		      (node leftrightright value right)
	  else if height_right > height_left + 2 then
            match right with
            | Empty ->
	       assert false (* height_right > height_left + 2 >= 2 *)
            | Node(rightleft,rightvalue,rightright,_) ->
	       if height rightright >= height rightleft then
		 node (node left value rightleft) rightvalue rightright
	       else
		 match rightleft with
                 | Empty ->
		    assert false (* 0 <= height rightright < height rightleft *)
                 | Node(rightleftleft,rightleftvalue,rightleftright,_) ->
		    node
		      (node left value rightleftleft)
		      rightleftvalue
		      (node rightleftright rightvalue rightright)
	    else Node (left,value,right,1+(max height_left height_right))

	let rec add new_value set =
	  match set with
          | Empty -> Node(Empty,new_value,Empty,1)
          | Node(left,value_set,right,_) ->
             let c = Ord.compare new_value value_set in
             if c = 0 then set
             else if c<0 then balance (add new_value left) value_set right
             else balance left value_set (add new_value right)

	let rec join left value right =
	  match left,right with
          | Empty,_ -> add value right
          | _,Empty -> add value left
          | Node(leftleft,leftvalue,leftright,leftheight),
            Node(rightleft,rightvalue,rightright,rightheight) ->
             if leftheight > rightheight + 2 then
               let right' = join leftright value right in
               balance leftleft leftvalue right'
             else if rightheight > leftheight +2 then
               let left' = join left value rightleft in
               balance left' rightvalue rightright
             else node left value right

	let rec safe_extract_min_elt left value right =
	  match left with
	  | Empty -> value,right
	  | Node (left,value,right,_) ->
	     let min,left' = safe_extract_min_elt left value right in
	     min,balance left' value right

	let merge set1 set2 =
	  match set1,set2 with
          | Empty,_ -> set2
          | _,Empty -> set1
          | Node _, Node (left2,value2,right2,_) ->
             let min2,set2' = safe_extract_min_elt left2 value2 right2 in
             balance set1 min2 set2'

	let concat set1 set2 =
	  match set1,set2 with
	  |   Empty,_ -> set2
	  | _,Empty -> set1
	  | Node _, Node (left2,value2,right2,_) ->
             let min2,set2' = safe_extract_min_elt left2 value2 right2 in
             join set1 min2 set2'

	let rec remove value set =
	  match set with
          | Empty -> Empty
          | Node(left,value_set,right,_) ->
             let c = Ord.compare value value_set in
             if c = 0 then merge left right
             else if c < 0 then balance (remove value left) value_set right
             else balance left value_set (remove value right)

	let rec split split_value set =
	  match set with
          | Empty -> (Empty,false,Empty)
          | Node(left,set_value,right,_) ->
             let c = Ord.compare split_value set_value in
             if c=0 then (left,true,right)
             else if c<0 then
               let (leftleft,bool,rightleft) = split split_value left in
               let rightright = join rightleft set_value right in
               (leftleft,bool,rightright)
             else
               let (leftright,bool,rightright) = split split_value right in
               let leftleft = join left set_value leftright in
               (leftleft,bool,rightright)

	let rec union set1 set2 =
	  match set1,set2 with
          | Empty,_ -> set2
          | _,Empty -> set1
          | Node(left1,value1,right1,height1),
	    Node(left2,value2,right2,height2) ->
             if height1 > height2 then
               if height2 = 1 then add value2 set1
               else
		 let (left2,_,right2) = split value1 set2 in
		 let left' = union left1 left2 in
		 let right' = union right1 right2 in
		 join left' value1 right'
             else
               if height1 = 1 then add value1 set2
               else
		 let (left1,_,right1) = split value2 set1 in
		 let left' = union left1 left2 in
		 let right' = union right1 right2 in
		 join left' value2 right'

	let suture (left1,value1,right1) (left2,bool,right2) f =
	  let left' = f left1 left2 in
	  let right' = f right1 right2 in
	  if bool then join left' value1 right' else concat left' right'

	let suture_not (left1,value1,right1) (left2,bool,right2) f =
	  let left' = f left1 left2 in
	  let right' = f right1 right2 in
	  if bool then concat left' right' else join left' value1 right'

	let rec inter set1 set2 =
	  match set1,set2 with
          | Empty,_
          | _,Empty -> Empty
          | Node(left1,value1,right1,_),_ ->
             let triple2 = split value1 set2 in
             suture (left1,value1,right1) triple2 inter

	let rec diff set1 set2 =
	  match set1,set2 with
	  | Empty,_ -> Empty
	  | _,Empty -> set1
	  | Node(left1,value1,right1,_),_ ->
             let triple2 = split value1 set2 in
             suture_not (left1,value1,right1) triple2 diff

	let rec mem searched_value = function
          | Empty -> false
          | Node(left,set_value,right,_) ->
             let c = Ord.compare searched_value set_value in
             c==0 || mem searched_value (if c < 0 then left else right)

	let filter p set =
	  let rec filt accu set =
            match set with
            | Empty -> accu
            | Node(left,value,right,_) ->
               filt (filt (if p value then add value accu else accu) left) right
	  in filt Empty set

	let partition p set =
	  let rec part (t,f as accu) set =
            match set with
            | Empty -> accu
            | Node(left,value,right,_) ->
               part
		 (part
                    (if p value then add value t,f else t,add value f)
                    left)
		 right
	  in part (Empty,Empty) set

	type enumeration = End | More of elt * t * enumeration

	let rec cons_enum enum = function
          | Empty -> enum
          | Node(left,value,right,_) -> cons_enum (More(value,right,enum)) left

	let rec compare_aux e1 e2 =
	  match e1,e2 with
          | End,End -> 0
          | End,_ -> -1
          | _ , End -> 1
          | More(v1,r1,e1),More(v2,r2,e2) ->
             let c = Ord.compare v1 v2 in
             if c<>0 then c
             else compare_aux (cons_enum e1 r1) (cons_enum e2 r2)

	let compare set1 set2 =
	  compare_aux (cons_enum End set1) (cons_enum End set2)

	let equal set1 set2 = compare set1 set2 == 0

	let rec subset set1 set2 =
	  match set1,set2 with
	  | Empty,_ -> true
	  | _,Empty -> false
	  | Node(left1,value1,right1,_),Node(left2,value2,right2,_) ->
             let c = Ord.compare value1 value2 in
             if c=0 then
               subset left1 left2 && subset right1 right2
             else if c < 0 then
               subset (Node(left1,value1,Empty,0)) left2 && subset right1 set2
             else
               subset (Node(Empty,value1,right1,0)) right2 && subset left1 set2

	let rec iter f = function
	  | Empty -> ()
	  | Node(left,value,right,_) ->
             let () = iter f left in let () = f value in iter f right

	let rec fold f set accu =
	  match set with
	  | Empty -> accu
	  | Node(left,value,right,_) ->
	     fold f right (f value (fold f left accu))

	let rec for_all p = function
	  | Empty -> true
	  | Node(left,value,right,_) ->
	     p value && for_all p left && for_all p right

	let rec exists p = function
	  | Empty -> false
	  | Node(left,value,right,_) ->
	     p value || exists p left || exists p right

	let elements set =
	  let rec elements_aux accu = function
            | Empty -> accu
            | Node(left,value,right,_) ->
	       elements_aux (value::(elements_aux accu right)) left
	  in elements_aux [] set

	let choose = function
	  | Empty -> None
	  | Node (_,v,_,_) -> Some v
	let rec min_elt = function
	  | Empty -> None
	  | Node(Empty,v,_,_) -> Some v
	  | Node(left,_,_,_) -> min_elt left
	let rec max_elt = function
	  | Empty -> None
	  | Node(_,v,Empty,_) -> Some v
	  | Node(_,_,right,_) -> max_elt right
      end

    (************************************************************************************)
    (* Map implementation*)

    module Map =
      struct
	type 'data t =
	  | Empty
	  | Node of 'data t * elt * 'data * 'data t * int

	let empty = Empty
	let is_empty x = x==empty

	let height = function
          | Empty -> 0
          | Node(_,_,_,_,h) -> h

	let node left key0 data right  =
	  Node (left,key0,data,right, 1 + max (height left) (height right))

	let balance left key data right =
	  let height_left = height left in
	  let height_right = height right in
	  if height_left > height_right + 2 then
	    match left with
            | Empty ->
	       assert false (* height_left > height_right + 2 >= 2 *)
            | Node (left0,key0,data0,right0,_) ->
               if height right0 <= height left0 then
		 node left0 key0 data0 (node right0 key data right)
               else
		 match right0 with
		 | Empty ->
		    assert false (* height right0 > height left0 >= 0 *)
		 | Node (left1,key1,data1,right1,_) ->
                    node (node left0 key0 data0 left1)
			 key1 data1
			 (node right1 key data right)
	  else
            if height_right > height_left + 2 then
              match right with
              | Empty ->
		 assert false (* height_right > height_left + 2 >= 2 *)
              | Node (left0,key0,data0,right0,_) ->
		 if height left0 <= height right0 then
		   node (node left key data left0) key0 data0 right0
		 else
		   match left0 with
		   | Empty ->
		      assert false (* height left0 > height right0 >= 0 *)
		   | Node (left1,key1,data1,right1,_) ->
                      node (node left key data left1)
			   key1 data1
			   (node right1 key0 data0 right0)
            else node left key data right

	let rec add key data = function
	  | Empty -> Node (empty,key,data,empty,1)
	  | Node (left,key_map,data_map,right,height) ->
             let cmp = compare key key_map in
             if cmp = 0 then Node(left,key_map,data,right,height)
             else if cmp < 0 then
	       balance (add key data left) key_map data_map right
             else balance left key_map data_map (add key data right)

	let rec extract_min_binding map key data map' =
	  match map with
	  | Empty -> (key,data),map'
	  | Node (left2,key2,data2,right2,_) ->
             let min, left' = extract_min_binding left2 key2 data2 right2 in
             min,balance left' key data map'

	let merge map1 map2 =
	  match map1 with
	  | Empty -> map2
	  | Node _ ->
             match map2 with
             | Empty -> map1
             | Node(left2,key2,data2,right2,_) ->
		let (key3,data3), left' =
		  extract_min_binding left2 key2 data2 right2 in
		balance map1 key3 data3 left'

	let rec remove key = function
	  | Empty -> empty
	  | Node (left,key_map,data,right,_) ->
             let cmp = compare key key_map in
             if cmp = 0 then merge left right
             else if cmp < 0 then balance (remove key left) key_map data right
             else balance left key_map data (remove key right)

	let rec join left key value right =
	  match balance left key value right with
          | Empty -> assert false (* By case analysis *)
          | Node (left2,key2,data2,right2,_) as map2 ->
             let h = height left2 - height right2 in
             if h > 2 || h< -2 then join left2 key2 data2 right2 else map2

	let rec split value = function
	  | Empty -> (empty,None,empty)
	  | Node (left1,key1,data1,right1,_) ->
             let cmp = Ord.compare value key1 in
             if cmp = 0 then (left1,Some data1,right1)
             else if cmp < 0 then
               let (left2,data2,right2) = split value left1 in
               let right2' = join right2 key1 data1 right1 in
               (left2,data2,right2')
             else
               let (left2,data2,right2) = split value right1 in
               let left2' = join left1 key1 data1 left2 in
               (left2',data2,right2)

	let rec diff map1 map2 =
	  match map1 with
	  | Empty -> Empty,map2
	  | Node(left1,key1,data1,right1,_) ->
             let left2,data2,right2 = split key1 map2 in
             let oleft1,oleft2 = diff left1 left2 in
             let oright1,oright2 = diff right1 right2 in
             match data2 with
             | Some x when x = data1 ->
		merge oleft1 oright1, merge oleft2 oright2
             | Some data2  ->
		join oleft1 key1 data1 oright1, join oleft2 key1 data2 oright2
             | None ->
		join oleft1 key1 data1 oright1, merge oleft2 oright2

	let rec union map1 map2 =
	  match map1, map2 with
          | Empty, _ -> map2
          | _, Empty -> map1
          | Node (left1, value1, data1, right1, height1),
            Node (left2, value2, data2, right2, height2) ->
             if height1 >= height2 then
	       let left2, op_data2, right2 = split value1 map2 in
	       join (union left1 left2)
		    value1 (match op_data2 with None -> data1 | Some d2 -> d2)
		    (union right1 right2)
             else
	       let left1, op_data1, right1 = split value2 map1 in
	       join (union left1 left2)
		    value1 (match op_data1 with None -> data2 | Some d1 -> d1)
		    (union right1 right2)

	let rec update map1 map2 =
	  if map1==map2 then map2
	  else
            match map1 with
            | Empty -> map2
            | Node(left1,key1,data1,right1,_) ->
               let left2,data2,right2 = split key1 map2 in
               join (update left1 left2)
		    key1 (match data2 with None -> data1 | Some d2 -> d2)
		    (update right1 right2)

	let rec diff_pred pred map1 map2 =
	  match map1 with
          | Empty -> Empty,map2
          | Node(left1,key1,data1,right1,_) ->
             let left2,data2,right2 = split key1 map2 in
             let oleft1,oleft2 = diff_pred pred left1 left2 in
             let oright1,oright2 = diff_pred pred right1 right2 in
             match data2 with
             | Some x when pred x data1 ->
                merge oleft1 oright1, merge oleft2 oright2
             | Some data2  ->
                join oleft1 key1 data1 oright1, join oleft2 key1 data2 oright2
             | None ->
                join oleft1 key1 data1 oright1, merge oleft2 oright2

	let rec cardinal = function
	  | Empty -> 0
	  | Node (l, _, _, r, _) -> cardinal l + 1 + cardinal r

	let rec min_elt p = function
          | Empty -> None
          | Node(left,key,data,right,_) ->
             match min_elt p left with
             | None -> if p key data then Some key else min_elt p right
             | some -> some

	let rec find_option key = function
	  | Empty -> None
	  | Node (left,key_map,data,right,_) ->
             let cmp = compare key key_map in
             if cmp = 0 then Some data
             else if cmp>0 then find_option key right
             else find_option key left

	let rec find_default d key = function
	  | Empty -> d
	  | Node (left,key_map,data,right,_) ->
             let cmp = compare key key_map in
             if cmp = 0 then data
             else if cmp>0 then find_default d key right
             else find_default d key left

	let rec mem key = function
          | Empty -> false
          | Node (left,key_map,_,right,_) ->
             let cmp = compare key key_map in
             cmp == 0 ||
	       if cmp>0 then mem key right else mem key left

	let rec iter f = function
          | Empty -> ()
          | Node(left,key,data,right,_) ->
             let () = iter f left in let () = f key data in iter f right

	let rec fold f map value =
	  match map with
          | Empty -> value
          | Node(left,key,data,right,_) ->
             fold f right (f key data (fold f left value))

	let rec monadic_fold param err f map value =
	  match map with
	  | Empty -> err,value
	  | Node(left,key,data,right,_) ->
	     let err',value' = monadic_fold param err f left value in
	     let err'',value'' = f param err' key data value' in
	     monadic_fold param err'' f right value''

	let rec monadic_fold2 parameters rh f g h map1 map2 res =
	  match map1,map2 with
          | Empty,Empty -> rh,res
          | Empty , _ -> monadic_fold parameters rh h map2 res
	  | _ , Empty -> monadic_fold parameters rh g map1 res
          | Node(left1,key1,data1,right1,_),_ ->
             let (left2,data2,right2) = split key1 map2 in
             match data2 with
             | None ->
		let rh', res' =
		  monadic_fold2 parameters rh f g h left1 left2 res in
                let rh'',res'' = g parameters rh' key1 data1 res' in
                monadic_fold2 parameters rh'' f g h right1 right2 res''
             | Some data2 ->
                let rh', res' = monadic_fold2 parameters rh f g h left1 left2 res in
                let rh'',res'' = f parameters rh' key1 data1 data2 res' in
                monadic_fold2 parameters rh'' f g h right1 right2 res''

	let monadic_fold2_sparse parameters rh f map1 map2 res =
	  let id _ x _ _ y = (x,y) in
	  monadic_fold2 parameters rh f id id map1 map2 res

	let monadic_iter2_sparse parameters rh f map1 map2 =
	  let error,() =
	    monadic_fold2_sparse
	      parameters rh
	      (fun p e k a b () -> (f p e k a b,())) map1 map2 () in
	  error

	let rec monadic_fold_restriction parameters rh f set map res =
	  match set with
	  | Set.Empty -> rh,res
	  | Set.Node(left1,key1,right1,_) ->
             let left2,data2,right2 = split key1 map in
             match data2 with
             | None ->
		let rh', res' =
		  monadic_fold_restriction parameters rh f left1 left2 res in
		monadic_fold_restriction parameters rh' f right1 right2 res'
             | Some data2 ->
		let rh', res' =
		  monadic_fold_restriction parameters rh f left1 left2 res in
		let rh'',res'' = f parameters rh' key1 data2 res' in
		monadic_fold_restriction parameters rh'' f right1 right2 res''

	let rec mapi f = function
	  | Empty -> empty
	  | Node(left,key,data,right,height) ->
             Node(mapi f left,key,f key data,mapi f right,height)

	let map f s = mapi (fun _ x -> f x) s

	let rec map2 f map map' =
	  match map with
	  | Empty -> map'
	  | Node(left1,key1,data1,right1,_) ->
             let left2,data2,right2 = split key1 map' in
             join (map2 f left1 left2)
		  key1 (match data2 with None -> data1 | Some d2 -> f data1 d2)
		  (map2 f right1 right2)

	let rec cut_opt value map =
	  match map with
	  | Empty -> None
	  | Node (left1,key1,data1,right1,_) ->
             let cmp = Ord.compare value key1 in
             if cmp = 0 then Some (left1,data1,right1)
             else if cmp < 0 then
               match cut_opt value left1 with
               | None -> None
               |Some (left2,data2,right2) ->
		 Some (left2,data2,node right2 key1 data1 right1)
             else
               match cut_opt value right1 with
               | None -> None
               | Some (left2,data2,right2) ->
		  Some (node left1 key1 data1 left2,data2,right2)

	let rec for_all p = function
          | Empty -> true
          | Node(left,key,data,right,_) ->
             p key data && for_all p right && for_all p left

	let rec forall2iz p map1 map2 =
	  map1==map2 ||
            match map1 with
            | Empty -> false
            | Node(left1,key1,data1,right1,_) ->
               match cut_opt key1 map2 with
               | Some (left2,data2,right2) ->
		  (data1==data2 || p key1 data1 data2)
		  && forall2iz p left1 left2
		  && forall2iz p right1 right2
               | None -> false

	let equal p = forall2iz (fun _ -> p)

	let rec bindings_aux accu = function
	  | Empty -> accu
	  | Node (l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

	let bindings s = bindings_aux [] s
      end
  end
