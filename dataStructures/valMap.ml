module type Content =
  sig
    type t
    val to_f: t -> float
  end

module type ValMap =
	sig
		type key = int
		type tree
		type content
		val to_string : tree -> string
		val size : tree -> int
		val random_val : tree -> (key*content)
		val empty: tree
		val is_empty: tree -> bool
		val add: key -> content -> tree -> tree
		val find : key -> tree -> content
		val total : tree -> float
	end

module Make(C:Content) = 
  (struct
		type content = C.t
		type key = int
    type tree =
        Empty
      | Node of tree * key * C.t * tree * int * int * float (*Node(left,key,value,right,height,size,acc)*)
	  
		let rec to_string = function
			| Empty -> "Empty"
			| Node (l,k,content,r,_,_,acc) -> Printf.sprintf "<%d,%f(%f)>[%s|%s]\n" k acc (C.to_f content) (to_string l) (to_string r) 
		
		let height = function
        Empty -> 0
      | Node(_,_,_,_,h,_,_) -> h

    let size = function
        Empty -> 0
      | Node(_,_,_,_,_,s,_) -> s

    let accval = function
				Empty -> 0.0
      | Node(_,_,_,_,_,_,acc) -> acc
	  
		let total = accval
		
    let create l key value r =
      let hl = height l and hr = height r in
      let acc1 = accval l and acc2 = accval r in
      Node(l, key, value, r, (if hl >= hr then hl + 1 else hr + 1), (size l) + (size r) + 1, (C.to_f value) +. acc1 +. acc2)

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h,_,_) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h,_,_) -> h in
      if hl > hr + 2 then 
				begin
	        match l with
	          Empty -> invalid_arg "Val_map.bal"
	        | Node(ll, lv, ld, lr, _,_,_) ->
	            if height ll >= height lr then
	              create ll lv ld (create lr x d r)
	            else 
								begin
		              match lr with
		                Empty -> invalid_arg "Val_map.bal"
		              | Node(lrl, lrv, lrd, lrr, _,_,_)->
		                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
	            	end
      	end 
			else 
				if hr > hl + 2 then 
					begin
		        match r with
		          Empty -> invalid_arg "Val_map.bal"
		        | Node(rl, rv, rd, rr, _,_,_) ->
		            if height rr >= height rl then
		              create (create l x d rl) rv rd rr
		            else 
									begin
			              match rl with
			                Empty -> invalid_arg "Val_map.bal"
			              | Node(rll, rlv, rld, rlr, _,_,_) ->
			                  create (create l x d rll) rlv rld (create rlr rv rd rr)
		            	end
	      	end 
				else
					let acc1 = accval l and acc2 = accval r in
          	Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1), (size l) + (size r) + 1, (C.to_f d) +. acc1 +. acc2)

    let empty = Empty
    let is_empty = function Empty -> true | _ -> false

    let rec add key data = function
        Empty -> Node(Empty,key, data, Empty,1,1,C.to_f data)
      | Node(l, key', d, r, h,s,acc) ->
          if key = key' then
            Node(l, key, data, r, h,s,acc -. (C.to_f d) +. (C.to_f data))
          else 
						if key < key' then
            bal (add key data l) key' d r
          else
            bal l key' d (add key data r)

    let rec find x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _,_,_) ->
          let c = compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec find_acc aim_acc = function
				Empty -> raise Not_found
      | Node(l,key,d,r,_,_,acc) -> 
			  if aim_acc >= acc then raise Not_found 
			  else 
			    let acc_l = accval l and acc_r = accval r in
			      if acc_l > aim_acc then find_acc aim_acc l
			      else 
							if (acc_r +. acc_l) > aim_acc then find_acc (aim_acc -. acc_l) r
							else (key,d)  

    let rec mem key = function
        Empty -> false
      | Node(l, key',_, r, _,_,_) ->
          let c = compare key key' in
          c = 0 || (mem key (if c < 0 then l else r))

    let rec min_binding = function
        Empty -> raise Not_found
      | Node(Empty, x, d, r, _,_,_) -> (x, d)
      | Node(l, x, d, r, _,_,_) -> min_binding l

    let rec remove_min_binding = function
        Empty -> invalid_arg "Val_map.remove_min_elt"
      | Node(Empty, x, d, r, _,_,_) -> r
      | Node(l, x, d, r, _,_,_) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove x = function
        Empty ->
          Empty
      | Node(l, v, d, r, _,_,_) ->
          let c = compare x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (remove x l) v d r
          else
            bal l v d (remove x r)

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _,_,_) ->
          iter f l; f v d; iter f r

    let rec map f = function
        Empty               -> Empty
      | Node(l, v, d, r, h,s,acc) -> Node(map f l, v, f d, map f r, h,s,acc)

    let rec mapi f = function
        Empty               -> Empty
      | Node(l, v, d, r, h,s,acc) -> Node(mapi f l, v, f v d, mapi f r, h,s,acc)

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _,_,_) ->
          fold f r (f v d (fold f l accu))
		
		(**Returns (key,value) at random in the tree*)
    let random_val (m:tree) = 
    	try 
				let r = Random.float (accval m) in
					find_acc (Random.float r) m 
			with 
				| Invalid_argument "Val_map.find_acc" -> invalid_arg "Val_map.random_val"

	end:ValMap with type content=C.t)
