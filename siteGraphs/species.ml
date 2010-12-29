open Mods
open ExceptionDefn
open Graph

module Int64Map = MapExt.Make (Int64)

type t = { nodes : Node.t IntMap.t ; views : IntSet.t Int64Map.t } (*view_id -> {agent_id,...}*)
type table = (int list, (t * int) list) Hashtbl.t

let empty_table () = Hashtbl.create 10

let to_string spec env =
	let hsh_lnk = Hashtbl.create 0
	in
	let _, l =
		IntMap.fold
			(fun id node (fresh, cont) ->
						let (str, c) = Node.to_string false (hsh_lnk, fresh) node env in
						(c, str:: cont)
			) spec.nodes (0,[])
	in
	String.concat "," (List.rev l)

let print desc spec env = 
	let hsh_lnk = Hashtbl.create 0
	and mx = IntMap.size spec.nodes in
	let _ =
		IntMap.fold
		(fun id node (fresh,cpt) ->
					let (str, fresh) = Node.to_string false (hsh_lnk, fresh) node env in
					Printf.fprintf desc "%s" str ;
					if cpt = (mx-1) then () else Printf.fprintf desc "," ;
					(fresh,cpt+1)
		) spec.nodes (0,0) 
	in
	()

let to_dot k cpt spec desc env = 
	let header = 
		Printf.sprintf 
		"subgraph cluster%d{\n\t node [style=filled,color=lightblue,shape=Mrecord,];\n\t edge [arrowhead=none];\n\t label = \"#%d\";\n" cpt k in
		let hp_ls, nodes_ls, bonds_ls =
			IntMap.fold 
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
									| (_,Node.Ptr (node',j)) -> 
										let i' = Node.get_address node' in
										if i<i' || (i=i' && site_id<j) then 
											(Printf.sprintf "node%d:s%d -> node%d:s%d;" i site_id i' j)::cont
										else cont
									| (_,Node.FPtr (n,j)) -> 
										if i<n || (i=n && site_id<j) then 
											(Printf.sprintf "node%d:s%d -> node%d:s%d;" i site_id n j)::cont
										else cont
									| _ -> cont
							) node [] 
					in
					LongString.concat (String.concat "\n" l) bonds_ls
				in
				(hp_ls,nodes_ls,bonds_ls)
			)
			spec.nodes (LongString.empty,LongString.empty,LongString.empty)
		in
		Printf.fprintf desc "%s" header ;
		LongString.printf desc nodes_ls ;
		LongString.printf desc bonds_ls ;
		Printf.fprintf desc "}\n"

(**[of_node sg root visited env] produces the species anchored at node [root] allocated in the graph [sg] and *)
(** returns a pair [(spec,visited')] where [visited'=visited U node_id] of [spec]*)
let of_node sg root visited env =
	let rec iter todo spec visited =
		match todo with
		| [] -> (spec, visited)
		| id:: tl ->
				let node = try SiteGraph.node_of_id sg id with Not_found -> invalid_arg "Species.of_node: Not found" in
				let todo', spec'=
					Node.fold_status
						(fun site_id (_, lnk_state) (todo, spec) ->
								match lnk_state with
								| Node.Null -> (todo, spec)
								| Node.Ptr (node', site_id') ->
										let id' = Node.get_address node' in
										if IntMap.mem id' spec.nodes then (todo, spec)
										else
											let view = Node.bit_encode node' env in
											let set =
												try IntSet.add id' (Int64Map.find view spec.views) with
												| Not_found -> IntSet.singleton id'
											in
											(id':: todo,
												{ nodes = IntMap.add id' (Node.marshalize node') spec.nodes ;
													views = Int64Map.add view set spec.views }
											)
								| Node.FPtr _ -> invalid_arg "Species.of_node"
						) node (tl, spec)
				in
				iter todo' spec' (IntSet.add id visited)
	in
	let view_root = Node.bit_encode root env in
	iter [Node.get_address root]
	{ nodes = IntMap.add (Node.get_address root) (Node.marshalize root) IntMap.empty ;
		views = Int64Map.add view_root (IntSet.singleton (Node.get_address root)) Int64Map.empty ;
	} visited 
	
let iso spec1 spec2 env =
	let rec reco embedding todo_list checked =
		match todo_list with
		| [] -> embedding
		| (id, id'):: tl ->
				let node = IntMap.find id spec1.nodes
				and node' = IntMap.find id' spec2.nodes
				in
				if not (Node.name node = Node.name node') then raise False
				else
					let todo, checked =
						Node.fold_status
							(fun site_id (int, lnk) (todo_list, checked) ->
										let int' = Node.internal_state (node', site_id)
										and lnk' = Node.link_state (node', site_id)
										in
										if not (int'= int) then raise False
										else
											match (lnk, lnk') with
											| (Node.Null , Node.Null) -> (todo_list, checked)
											| (Node.FPtr (i, j), Node.FPtr (i', j')) ->
													if not (j = j') then raise False
													else
													if Int2Set.mem (i, i') checked then (todo_list, checked)
													else
														((i, i'):: todo_list, Int2Set.add (i, i') checked)
											| _ -> raise False
							) node (tl, checked)
					in
					reco (IntMap.add id id' embedding) todo checked
	in
	
	try
		let opt = Int64Map.root spec1.views in
		match opt with
		| None -> Int64Map.is_empty spec2.views
		| Some (view, ids) ->
				let id = IntSet.choose ids in (*cannot fail*)
				let ids' = try Int64Map.find view spec2.views with Not_found -> raise False
				in
				IntSet.iter
					(fun id' ->
								try
									let _ = reco IntMap.empty [(id, id')] Int2Set.empty
									in
									raise True
								with False -> ()
					) ids' ; false
	with
	| True -> true

let of_graph sg env =
	let species, _ =
		SiteGraph.fold
			(fun id node (species, visited) ->
						if IntSet.mem id visited then (species, visited)
						else
							let spec, visited = of_node sg node visited env in
							let sign =
								List.fast_sort
									compare
									(Int64Map.fold
											(fun view _ cont ->
														view:: cont
											) spec.views [])
							in
							let specs = try Hashtbl.find species sign with Not_found -> [] in
							let specs, already_there =
								List.fold_left
									(fun (cont, b) (spec', n) ->
												if iso spec spec' env then ((spec', n + 1):: cont, true)
												else ((spec', n):: cont, b)
									) ([], false) specs
							in
							if already_there then Hashtbl.replace species sign specs
							else Hashtbl.replace species sign ((spec, 1):: specs) ;
							(species, visited)
			) sg (empty_table(), IntSet.empty)
	in
	species

let dump desc table env =
	Printf.fprintf desc "digraph G{\n" ;
	let _ = 
		Hashtbl.fold
		(fun _ specs cpt ->
					List.iter
					(fun (spec, k) ->
								to_dot k cpt spec desc env
					) specs ;
					cpt+1
		) table 0
	in
	Printf.fprintf desc "}\n" 
	
let dump_table table env =
	Hashtbl.iter
		(fun _ specs ->
					List.iter
						(fun (spec, k) ->
									Printf.printf "%d instances of species: %s\n" k (to_string spec env);
									Printf.printf "with signature %s\n" (Misc.string_of_map Int64.to_string (fun _ -> "") Int64Map.fold spec.views) ;
									Printf.printf "******\n"
						) specs
		) table