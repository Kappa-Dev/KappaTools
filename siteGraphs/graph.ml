open ExceptionDefn

open Mods

module type NodeMemoryModel =
sig
	type t
	exception Not_allocated
	val allocated : t -> int
	val dimension : t -> int
	val create : int -> t
	val free : t -> int -> unit
	val alloc : t -> Node.t -> t
	val get : t -> int -> Node.t
	val set : t -> int -> Node.t -> unit
	val iteri : (int -> Node.t -> unit) -> t -> unit

end

module type SG =
sig
	type t
	val fold : (int -> Node.t -> 'a -> 'a) -> t -> 'a -> 'a
	val init : int -> t
	val node_of_id : t -> int -> Node.t
	val add : t -> Node.t -> t
	val add_nodes : t -> Node.t Mods.IntMap.t -> t
	val dump : ?with_lift: bool -> t -> Environment.t -> unit
	val remove : t -> int -> unit
	val ( & ) : Node.t -> int
	val neighborhood :?interrupt_with: Mods.IntSet.t -> t -> int -> int -> int Mods.IntMap.t
	val add_lift : t -> Injection.t -> ((int * int) list) Mods.IntMap.t -> t
	val marshalize : t -> Node.t Mods.IntMap.t
	val size : t -> int
end

module Make (A : NodeMemoryModel) : SG =
struct
	type t = A.t
	
	let size sg = A.allocated sg 
	
	(* A is a fragmented heap so iteri might not be very efficient *)
	let fold f sg cont =
		let cont = ref cont
		in (A.iteri (fun i node -> cont := f i node !cont) sg; !cont)
	
	let init size = A.create size
	
	let node_of_id = try A.get with | exn -> raise Not_found
	
	let add sg node =
		try
			let sg = A.alloc sg node in sg
		with Invalid_argument msg -> invalid_arg ("Graph.add: "^msg)
	
	let add_nodes sg nodes =
		IntMap.fold (fun _ node sg -> add sg node) nodes sg
	
	let dump ?(with_lift = false) sg env =
		let hsh_lnk = Hashtbl.create (A.dimension sg)
		
		and fresh = ref 0
		in
		A.iteri
			(fun i node ->
						if Node.is_empty node
						then (Debug.tag "!")
						else
							(let (str, c) =
									Node.to_string with_lift (hsh_lnk, (!fresh)) node env
								in (fresh := c; print_string str; print_newline ())))
			sg
	
	let remove sg i = A.free sg i
	
	let ( & ) node = Node.get_address node
	
	let neighborhood ?(interrupt_with = IntSet.empty) sg id radius =
		let address node =
			try ( & ) node
			with | Not_found -> invalid_arg "SG.neighborhood: not allocated" in
		let add_min id d map =
			if IntMap.mem id map
			then (true, map)
			else (false, (IntMap.add id d map)) in
		let mark_next addr depth d_map to_do =
			let node = node_of_id sg addr
			in
			Node.fold_status
				(fun _ (_, lnk) (d_map, to_do) ->
							match lnk with
							| Node.FPtr _ -> invalid_arg "SG.neighborhood"
							| Node.Null -> (d_map, to_do)
							| Node.Ptr (node', _) ->
									let id' = address node' in
									let (visited, d_map) = add_min id' depth d_map
									in
									if visited
									then (d_map, to_do)
									else (d_map, (id' :: to_do)))
				node (d_map, to_do) in
		let rec iter to_do dist_map =
			match to_do with
			| [] -> dist_map
			| addr :: to_do ->
					let depth =
						(try IntMap.find addr dist_map
						with
						| Not_found ->
								invalid_arg "SG.neighborhood: invariant violation")
					in
					if (radius >= 0) && ((depth + 1) > radius)
					then iter to_do dist_map
					else
						(let (dist_map', to_do') =
								mark_next addr (depth + 1) dist_map to_do
							in
							if
							IntSet.exists (fun id -> IntMap.mem id dist_map')
								interrupt_with
							then dist_map'
							else iter to_do' dist_map')
		in iter [ id ] (IntMap.add id 0 IntMap.empty)
	
	let add_lift sg phi port_map =
		(IntMap.iter
				(fun u_i port_list ->
							let node_i =
								try node_of_id sg u_i
								with | Not_found -> invalid_arg "Graph.add_lift"
							in A.set sg u_i (Node.add_dep phi port_list node_i))
				port_map;
			sg)
	
	let marshalize sg =
		fold
			(fun id node map ->
						IntMap.add id (Node.marshalize node) map
			) sg IntMap.empty
end

module SiteGraph : SG = Make(Node.NodeHeap)
