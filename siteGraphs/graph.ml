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
	exception Is_connex
	val fold : (int -> Node.t -> 'a -> 'a) -> t -> 'a -> 'a
	val init : int -> t
	val node_of_id : t -> int -> Node.t
	val add : t -> Node.t -> t
	val add_nodes : t -> Node.t Mods.IntMap.t -> t
	val dump : ?with_lift: bool -> t -> Environment.t -> unit
	val remove : t -> int -> unit
	val ( & ) : Node.t -> int
	val neighborhood :
			?interrupt_with: Mods.IntSet.t -> 
			?check_connex: Mods.IntSet.t * Mods.IntSet.t -> 
			?complete_construction:bool -> 
			?d_map:int IntMap.t -> 
			?filter_elements: (Node.t -> bool) ->  
			t -> int -> int -> (bool * int Mods.IntMap.t * IntSet.t * IntSet.t)
	
	val add_lift : t -> Injection.t -> ((int * int) list) Mods.IntMap.t -> Environment.t -> t
	val marshalize : t -> Node.t Mods.IntMap.t
	val size : t -> int
	val to_dot : ?with_heap:bool -> t -> string -> Environment.t -> unit
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
	
	let node_of_id sg i = try A.get sg i with _ -> raise Not_found
	
	let add sg node =
		try
			let sg = A.alloc sg node in sg
		with Invalid_argument msg -> invalid_arg ("Graph.add: "^msg)
	
	let add_nodes sg nodes =
		IntMap.fold (fun _ node sg -> add sg node) nodes sg
		
	let remove sg i = A.free sg i
	
	let ( & ) node = Node.get_address node
	
	exception Is_connex
	
	(*Should re-implement this function*)
	let neighborhood 
			?(interrupt_with = IntSet.empty) (**[interrrupt_with set] Interrupts function call if neighborhood of node [id] contains an identifier in the specified set*) 
			?check_connex (**[check_connex=set option] if [Some set] then will additionally check whether the cc of node [id] is connected to a node id in [set] *)
			?(complete_construction = false) (**if [complete=false] then the construction of the cc will stop whenever [check_connex] has been verified -if asked for*)
			?(d_map = IntMap.empty) 
			?filter_elements 
			sg 
			id 
			radius 
		=
		let address node =
			try ( & ) node
			with | Not_found -> invalid_arg "Graph.neighborhood: not allocated" in
		let add_min codomain id d map =
			let d = try min (d+1) (IntMap.find id map) with Not_found -> if IntSet.mem id codomain then 0 else d+1 in
			IntMap.add id d map	 
		in
		let mark_next (root,codomain) depth_of_root d_map to_do complete interrupt_with =
			let node = node_of_id sg root
			in
			let d_map,todo,stop =
  			Node.fold_status
  				(fun _ (_, lnk) (d_map, to_do, stop) ->
  							match lnk with
  							| Node.FPtr _ -> invalid_arg "Graph.neighborhood"
  							| Node.Null -> (d_map, to_do, stop)
  							| Node.Ptr (node', _) ->
  									let id' = address node' in
  									let stop = if IntSet.mem id' interrupt_with then true else stop in
  									let d_map = add_min codomain id' depth_of_root d_map
  									in
  									if IntSet.mem id' complete then (d_map, to_do, stop)
  									else (d_map, (id' :: to_do),stop))
  				node (d_map, to_do, false)
			in
			(d_map,todo,stop,IntSet.add root complete)
		in
		
		let rec iter check_connex (remaining_roots,codomain) to_do (complete:Mods.IntSet.t) dist_map component is_connex =
			match to_do with
			| [] -> (is_connex,dist_map,component,remaining_roots) 
			| root :: to_do -> (*adding addr to cc -only if addr is the root of a nl injection if filtering is enabled*)
					let component = 
						match filter_elements with (*if root is not a non-local root then skip*)
							| None -> IntSet.add root component 
							| Some predicate_over_node -> 
								let n = node_of_id sg root 
								in 
								if (predicate_over_node n) then IntSet.add root component else component
					in
					(*checking connectivity of remaining_roots*)
					let remaining_roots,is_connex = 
						if not check_connex then (remaining_roots,is_connex) 
						else 
							let set = IntSet.remove root remaining_roots in
							let is_connex = IntSet.is_empty set in
							if complete_construction || (not is_connex) then
								(set,is_connex)
							else raise Is_connex
					in
					let depth =
						(try IntMap.find root dist_map
						with
						| Not_found ->
								invalid_arg "Graph.neighborhood: invariant violation")
					in
					if (radius >= 0) && ((depth + 1) > radius) (*do not try to mark next if depth is too big*)
					then iter check_connex (remaining_roots,codomain) to_do complete dist_map component is_connex
					else
						(let (dist_map', to_do',stop, complete') =
								mark_next (root,codomain) depth dist_map to_do complete interrupt_with
							in
							if stop then (is_connex,dist_map',component,remaining_roots)
							else 
								iter check_connex (remaining_roots,codomain) to_do' complete' dist_map' component is_connex)
		in 
		
		let check_connex,roots,codomain = match check_connex with None -> (false,IntSet.empty,IntSet.empty) | Some (roots,codomain) -> (true,roots,codomain) in
		
		try iter check_connex (roots,codomain) [ id ] IntSet.empty (IntMap.add id 0 d_map) IntSet.empty false with Is_connex -> (true,IntMap.empty,IntSet.empty,IntSet.empty)
			
	
	let add_lift sg phi port_map env =
		(IntMap.iter
				(fun u_i port_list ->
							let node_i =
								try node_of_id sg u_i
								with | Not_found -> invalid_arg "Graph.add_lift"
							in A.set sg u_i (Node.add_dep phi port_list node_i env))
				port_map;
			sg)
					
		
	let marshalize sg =
		fold
			(fun id node map ->
						IntMap.add id (Node.marshalize node) map
			) sg IntMap.empty
	
	let to_dot ?(with_heap=false) sg dotfile env =
		let header = "digraph G{\n\t nodesep=.05;\n\t rankdir=LR;\n\t node [shape=record];\n" in
		let hp_ls, nodes_ls, bonds_ls =
			fold 
			(fun i node (hp_ls,nodes_ls, bonds_ls) ->
				let hp_ls = LongString.concat ~sep:'|' (Printf.sprintf "<n%d> %d" i i) hp_ls
				and nodes_ls = 
						let l = 
							Node.fold_status 
							(fun site_id status cont -> 
								if site_id = 0 then (Printf.sprintf "<s%d> %s" site_id (Environment.name (Node.name node) env))::cont
								else
								let int_str = 
									match status with 
										| (Some i,_) -> "~"^(Environment.state_of_id (Node.name node) site_id i env) 
										| _ -> "" 
								in
								let label = (Environment.site_of_id (Node.name node) site_id env)^int_str in
								(Printf.sprintf "<s%d> %s" site_id label)::cont
								) node [] 
						in
						LongString.concat (Printf.sprintf "\tnode%d [label = \"{%s}\"];\n" i (String.concat "|" (List.rev l))) nodes_ls
				and bonds_ls = 
					let l = 
							Node.fold_status 
							(fun site_id status cont -> 
								match status with 
									| (_,Node.Ptr (node',j)) -> (Printf.sprintf "node%d:s%d -> node%d:s%d;" i site_id (Node.get_address node') j)::cont
									| (_,Node.FPtr (n,j)) -> (Printf.sprintf "node%d:s%d -> node%d:s%d;" i site_id n j)::cont
									| _ -> cont
							) node [] 
					in
					if with_heap then
						LongString.concat (Printf.sprintf "heap:n%d -> node%d:s0\n" i i) (LongString.concat (String.concat "\n" l) bonds_ls)
					else
						LongString.concat (String.concat "\n" l) bonds_ls
				in
				(hp_ls,nodes_ls,bonds_ls)
			)
			sg (LongString.empty,LongString.empty,LongString.empty)
		in
		let d = open_out dotfile in
		Printf.fprintf d "%s" header ;
		if with_heap then
			begin
				Printf.fprintf d "heap [label = \"" ; 
				LongString.printf d hp_ls ;
				Printf.fprintf d "\",height=%f];\n" (float_of_int (size sg) /. 1.5)
			end ;
		LongString.printf d nodes_ls ;
		LongString.printf d bonds_ls ;
		Printf.fprintf d "}\n" ;
		close_out d
		
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
								in (fresh := c; print_string ("#"^(string_of_int i)^":"^str); print_newline ())))
			sg

end

module SiteGraph : SG = Make(Node.NodeHeap)
