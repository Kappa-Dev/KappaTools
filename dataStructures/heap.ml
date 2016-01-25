module type Content =
  sig
    type t
    val allocate : t -> int -> unit
  end

module type T =
  sig
    type content
    type t (*Heap type*)
    val size : t -> int (*Number of elements allocated in the heap*)
    val dimension : t -> int (*Total size of the heap in memory*)
    val create : int -> t
    val remove : int -> t -> t (*Deallocate address*)
    val alloc : content -> t -> t (*Allocate an element in the heap --call for Content.allocate*)
    val next_alloc : t -> content option
    val find : int -> t -> content (*Returns the element at address key*)
    val iteri : (int -> content -> unit) -> t -> unit (*iter on the heap*)
    val fold : (int -> content -> 'a ->'a) -> t -> 'a -> 'a
    val random : t -> content (*returns a random element from the heap --constant time*)
    val is_empty : t -> bool (*is size=0*)
    val mem : int -> t -> bool
  end


module Make(C:Content) =
  (struct
    type content = C.t
    type t = {
      mutable next_address : int ;
      mutable cemetery : content list ; (*where to put keys that points to addresses that can be recycled*)
      ar: content option LargeArray.t ; (*Unfragmented array*)
    }

    let bury h value = C.allocate value (-1) ; h.cemetery <- value::h.cemetery

    let get = LargeArray.get
    let set = LargeArray.set

    let safe_get txt = fun ar i ->
      try get ar i with Invalid_argument msg -> invalid_arg (txt^": "^msg)
    let safe_set txt = fun ar i j ->
      try set ar i j with Invalid_argument msg -> invalid_arg (txt^": "^msg)

    let size h = h.next_address (*virtual size of the extensible array*)
    let dimension h = LargeArray.length h.ar (*real length of the array*)

    let create size =
      if (size < 0) || (size >= Sys.max_array_length)
      then invalid_arg "Heap.create"
      else
	{ar = LargeArray.create size None ;
	 next_address = 0 ;
	 cemetery = [] ; (*shouldn't be too big*)
	}

    let remove i h =
      if h.next_address = 0 then h (*h is empty*)
      else
	match safe_get "Heap.remove: getting value" h.ar i with
	| None -> h (*already removed*)
	| Some v_rm ->
	   let last_address = h.next_address-1 in
	   let v_last =
	     match safe_get "Heap.remove 1" h.ar last_address with
	     | None -> invalid_arg "Heap.remove: Fragmented heap"
	     | Some v -> v
	   in (*get value of last element in array*)
	   safe_set "Heap.remove 2" h.ar i (Some v_last) ;
	   (*one replaces the values of the erased cell*)
	   (*safe_set "" h.ar last_address None ;*)
	   C.allocate v_last i ;
	   let (_:unit) = bury h v_rm in
	   h.next_address <- last_address ;
	   h

    let alloc v h =
      let i = h.next_address in
      C.allocate v i ;
      let ar =
	if (i = LargeArray.length h.ar) then (*h.ar is full*)
	  let size' = 2 * (i + 1) in
	  LargeArray.init
	    size'
	    (fun j -> if j < i
		      then safe_get "Heap.alloc 0" h.ar j (*copying old values*)
		      else None (*default value*)
	    )
	else h.ar
      in
      let h = {h with ar = ar} in
      safe_set "Heap.alloc 1" h.ar i (Some v);
      h.next_address <- (i + 1) ;
      h

    let next_alloc h =
      match h.cemetery with
      | [] -> None
      | v::tl -> (h.cemetery <- tl ; Some v)

    let find i h = (*may raise Not_found*)
      match safe_get "Heap.find" h.ar i with
      | Some v -> v
      | None -> invalid_arg "Heap.find: heap is fragmented"

    exception End_of_Array

    let iteri f h =
      try
	LargeArray.iteri
	  (fun cpt -> function
		   | None -> raise End_of_Array
		   | Some content ->
		      if cpt = h.next_address then raise End_of_Array
		      else f cpt content) h.ar
      with End_of_Array -> ()

    let fold f h cont =
      let c = ref cont in
      iteri (fun k v -> c:=f k v !c) h ;
      !c

    let random h =
      if h.next_address < 1 then raise Not_found
      else
	match safe_get "Heap.random" h.ar (Random.int h.next_address) with
	| Some content -> content
	| None -> invalid_arg "Heap.random: heap is fragmented"

    let is_empty h = (h.next_address = 0)

    let mem i h =
      match safe_get "Heap.mem" h.ar i with
      | None -> false
      | Some _ -> true

  end:T with type content = C.t)
