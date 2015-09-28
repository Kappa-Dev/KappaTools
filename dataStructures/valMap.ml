type 'a tree =
  Empty
| Node of 'a tree * 'a * 'a tree * int * int
    (*Node(left,key,right,height,size)*)

let rec print pr f = function
  | Empty -> Format.pp_print_string f "Empty"
  | Node (l,k,r,_,acc) ->
    Format.fprintf f "<%a,%i>[%a|%a]@," pr k acc
      (print pr) l (print pr) r

let height = function
Empty -> 0
  | Node(_,_,_,h,_) -> h

let size = function
Empty -> 0
  | Node(_,_,_,_,s) -> s

let total = size

let create l key r =
  let hl = height l and hr = height r in
  let acc1 = size l and acc2 = size r in
  Node(l, key, r, (if hl >= hr then hl + 1 else hr + 1),
       acc1 + acc2 + 1)

let bal l d r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    begin
      match l with
	Empty -> invalid_arg "Val_map.bal"
      | Node(ll, ld, lr, _,_) ->
	if height ll >= height lr then
	  create ll ld (create lr d r)
	else
	  begin
	    match lr with
	      Empty -> invalid_arg "Val_map.bal"
	    | Node(lrl, lrd, lrr,_,_)->
	      create (create ll ld lrl) lrd (create lrr d r)
	  end
    end
  else
    if hr > hl + 2 then
      begin
	match r with
	  Empty -> invalid_arg "Val_map.bal"
	| Node(rl, rd, rr, _,_) ->
	  if height rr >= height rl then
	    create (create l d rl) rd rr
	  else
	    begin
	      match rl with
		Empty -> invalid_arg "Val_map.bal"
	      | Node(rll, rld, rlr,_,_) ->
		create (create l d rll) rld (create rlr rd rr)
	    end
      end
    else
      let acc1 = size l in
      let acc2 = size r in
      Node(l, d, r, (if hl >= hr then hl + 1 else hr + 1),
	   1 + acc1 + acc2)

let empty = Empty
let is_empty = function Empty -> true | Node _ -> false

let rec add key = function
Empty -> Node(Empty,key, Empty,1,1)
  | Node(l, key', r, h,s) ->
    if key = key' then
      Node(l, key, r, h, s)
    else
      if key < key' then
        bal (add key l) key' r
      else
        bal l key' (add key r)

let rec find_acc aim_acc = function
Empty -> raise Not_found
  | Node(l,key,r,_,acc) ->
    if aim_acc >= acc then raise Not_found
    else
      let acc_l = size l in
      let acc_r = size r in
      if acc_l > aim_acc then find_acc aim_acc l
      else if (acc_r + acc_l) > aim_acc
      then find_acc (aim_acc - acc_l) r
      else key

let rec mem key = function
Empty -> false
  | Node(l, key', r, _,_) ->
    let c = compare key key' in
    c = 0 || (mem key (if c < 0 then l else r))

let rec min_binding = function
Empty -> raise Not_found
  | Node(Empty, x, _, _,_) -> x
  | Node(l, _, _, _,_) -> min_binding l

let rec remove_min_binding = function
Empty -> invalid_arg "Val_map.remove_min_elt"
  | Node(Empty, _, r, _,_) -> r
  | Node(l, x, r, _,_) -> bal (remove_min_binding l) x r

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) | (t, Empty) -> t
  | (Node _, Node _) ->
    let x = min_binding t2 in
    bal t1 x (remove_min_binding t2)

let rec remove x = function
Empty -> Empty
  | Node(l, v, r, _,_) ->
    let c = compare x v in
    if c = 0 then merge l r
    else if c < 0 then bal (remove x l) v r
    else bal l v (remove x r)

let rec iter f = function
Empty -> ()
  | Node(l, v, r, _,_) -> iter f l; f v; iter f r

let rec fold f m accu =
  match m with
    Empty -> accu
  | Node(l, v, r, _,_) ->
    fold f r (f v (fold f l accu))

(**Returns (key,value) at random in the tree*)
let random_val m =
  try
(*    let r = Random.int (size m) in
    find_acc (Random.int r) m *)
    find_acc (Random.int (size m)) m
  with Invalid_argument "Val_map.find_acc" ->
    invalid_arg "Val_map.random_val"
