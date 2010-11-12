open LargeArray

module type Content = 
	sig
		type t
		val allocate : t -> int -> unit
	end

module type FragHeap = 
    sig
      type t 
			type content
			exception Not_allocated
			val allocated : t -> int
			val dimension : t -> int
			val fragmentation : t -> int
      val create : int -> t
			val next_alloc : t -> content option
			val free : t -> int -> unit
			val alloc : t -> content -> t
      val get : t -> int -> content
			val set : t -> int -> content -> unit
      val iteri : (int -> content -> unit) -> t -> unit
			val flush : t -> unit
    end

module Make(T:Content) = 
(struct
	type content = T.t
	type memory = Mem of content | Empty 
	type t = {mutable fresh: int ; mutable elements : int ; ar : memory GenArray.t ; mutable free_cells : int list}
  
	exception Not_allocated
	
	let allocated h = h.elements
	let dimension h = GenArray.length h.ar
	
	let flush h = 
		h.fresh <- 0 ;
		h.elements <- 0 ; 
		h.free_cells <- []
			
	let fragmentation h = List.length h.free_cells
	
	let create s =
		try 
			{fresh = 0 ; elements = 0 ; ar = GenArray.create s Empty ; free_cells = []}
		with
			| Invalid_argument str -> invalid_arg ("Heap error: "^str)
	
	
  let internal_get ar = fun x -> try GenArray.get ar x with Invalid_argument msg -> invalid_arg ("MemoryManagement.internal_set: "^msg) 
  let internal_set ar = fun x y -> try GenArray.set ar x y with Invalid_argument msg -> invalid_arg ("MemoryManagement.internal_set: "^msg)
	
	let alloc h elt = 
		try
			match h.free_cells with
				| addr::tl -> 
					(T.allocate elt addr ; 
					internal_set h.ar addr (Mem elt); 
					h.elements <- (h.elements+1) ; 
					h.free_cells <- tl ;
					h)
				| [] -> 
					if h.fresh = dimension h (*If heap has reached the actual size of the array*)
					then
						let size' = 2 * (dimension h + 1) in
						let ar' = 
							GenArray.init size' 
							(fun i -> if i < h.fresh then internal_get h.ar i (*copying old values*)
						  					else Empty (*default value*)
							) 
		   			in
							begin
								T.allocate elt h.fresh ;
								internal_set ar' h.fresh (Mem elt) ;
								{h with ar = ar' ; fresh = h.fresh+1 ; elements = h.elements+1} 
							end
					else
						begin
							T.allocate elt h.fresh ; 
							internal_set h.ar h.fresh (Mem elt) ; 
							h.fresh <- h.fresh + 1 ; 
							h.elements <- (h.elements+1) ; 
							h
						end
		with
			| Invalid_argument msg -> invalid_arg (Printf.sprintf "MemoryManagement.alloc: %d/%d" h.fresh (dimension h))
					
	let next_alloc h = 
		match h.free_cells with
			| addr::_ ->
				begin
					let mem = internal_get h.ar addr in
						match mem with
							| Mem e -> Some e
							| Empty -> None
				end
			| [] -> None

	let free h addr = (internal_set h.ar addr Empty ; h.elements <- (h.elements-1) ; h.free_cells <- (addr::h.free_cells) )

	let set h addr elt = internal_set h.ar addr (Mem elt)

	let get h addr = 
		match (internal_get h.ar addr) with
			| Empty -> raise Not_allocated
			| Mem elt -> elt

	let iteri f h = 
		let cpt = ref 0 in
			while !cpt < h.fresh do
				begin
					match internal_get h.ar !cpt with
						| Mem elt -> (f !cpt elt)  
						| Empty -> ()
				end ;  
				cpt := !cpt+1 ; 
		  done 
end:FragHeap with type content=T.t)


	
	