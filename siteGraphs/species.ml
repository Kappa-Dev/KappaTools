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
	

let to_dot hr palette k cpt spec desc env = 
	let rand_rgb () = 
		(fun (r,g,b) -> (string_of_float r^","^string_of_float g^","^string_of_float b)) 
		((Random.float 0.5)+.0.5,(Random.float 0.5)+.0.5, (Random.float 0.5)+.0.5)
	in
	let get_color lbl = 
		if not !Parameter.useColor then "white"
		else
		let rgb = try Hashtbl.find palette lbl with Not_found -> rand_rgb()
		in
			Hashtbl.replace palette lbl rgb ; rgb
	in
		
	Printf.fprintf desc "subgraph cluster%d{\n" cpt ;
	Printf.fprintf desc "\tcounter%d [label = \"%d instance(s)\", shape=none];\n" cpt k ;
	let bonds = 
		IntMap.fold
		(fun i node bonds ->
			let label = Node.label node env
			in
			Printf.fprintf desc "\tnode%d_%d [label = \"%s\", color = \"%s\", style=filled];\n" cpt i label (get_color label)  ; 
			Printf.fprintf desc "\tnode%d_%d -> counter%d [style=invis];\n" cpt i cpt ; 
			Node.fold_status 
			(fun site_id status cont -> 
				match status with
					| (int_opt,Node.FPtr (j,k)) -> 
						if j < i then cont
						else
							let node' = try IntMap.find j spec.nodes with Not_found -> invalid_arg "Species.to_dot: Node not found"
							in
							let int_opt'= Node.internal_state (node',k) in
							let nme node_name site_id int_opt =
								match int_opt with
									| Some int -> 
										let str = Environment.state_of_id node_name site_id int env 
										in
										let n = Environment.site_of_id node_name site_id env in
										(n^"~"^str)
									| None -> Environment.site_of_id node_name site_id env
							in
							(i,nme (Node.name node) site_id int_opt,j,nme (Node.name node') k int_opt')::cont
						| _ -> cont
			) node bonds
		) spec.nodes []
	in
	List.iter
	(fun (i,s_i,j,s_j) ->
		if hr then
			Printf.fprintf desc "\t node%d_%d -> node%d_%d [taillabel=\"%s\", headlabel=\"%s\", dir=none];\n" cpt i cpt j s_i s_j
		else
			Printf.fprintf desc "\t node%d_%d -> node%d_%d [dir=none];\n" cpt i cpt j 
	) bonds ;
	Printf.fprintf desc "}\n" 


(**[of_node sg root visited env] produces the species anchored at node [root] allocated in the graph [sg] and *)
(** returns a pair [(spec,visited')] where [visited'=visited U node_id] of [spec]*)
let of_node sg root visited env =
	let rec iter todo spec visited =
		match todo with
		| [] -> (spec, visited)
		| id:: tl ->
				let node = 
					try SiteGraph.node_of_id sg id with 
						| Not_found -> invalid_arg (Printf.sprintf "Species.of_node: Node %d is no longer in the graph" id) in
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
	
	let check i i' assoc = 
		let i_opt = try Some (IntMap.find i assoc) with Not_found -> None
		and i_opt' = try Some (IntMap.find i' assoc) with Not_found -> None
		in
		match (i_opt,i_opt') with
			| (Some j,Some j') -> (if (j=i') && (j'=i) then false else raise False)
			| (None,None) -> true
			| _ -> raise False
	in
	
	let rec reco embedding todo_list assoc =
		match todo_list with
		| [] -> embedding
		| (id, id'):: tl ->
				let node = IntMap.find id spec1.nodes
				and node' = IntMap.find id' spec2.nodes
				in
				if not (Node.name node = Node.name node') then raise False
				else
					let todo,assoc =
						Node.fold_status
							(fun site_id (int, lnk) (todo_list,assoc) ->
									let int' = Node.internal_state (node', site_id)
									and lnk' = Node.link_state (node', site_id)
									in
									if not (int'= int) then raise False
									else
										match (lnk, lnk') with
										| (Node.Null , Node.Null) -> (todo_list,assoc)
										| (Node.FPtr (i, j), Node.FPtr (i', j')) ->
												if not (j = j') then raise False
												else
													let is_new = check i i' assoc	in
													if is_new then ((i,i')::todo_list,IntMap.add i i' (IntMap.add i' i assoc))
													else (todo_list,assoc)
										| _ -> raise False
							) node (tl,assoc)
					in
					reco (IntMap.add id id' embedding) todo assoc
	in
	
	try
		let opt = Int64Map.root spec1.views in
		match opt with
		| None -> Int64Map.is_empty spec2.views
		| Some (view, ids) ->
				let id = IntSet.choose ids in (*cannot fail*)
				let ids' = try Int64Map.find view spec2.views with Not_found -> raise False
				in
				(IntSet.iter
					(fun id' ->
						try
							let _ = reco IntMap.empty [(id, id')] (IntMap.add id id' (IntMap.add id' id IntMap.empty))
							in
								raise True
						with False -> ()
					) ids' ; 
				false)
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
													view::cont
										) spec.views []
								)
							in
							let specs = try Hashtbl.find species sign with Not_found -> [] in
							let specs, already_there =
								List.fold_left
									(fun (cont, b) (spec', n) ->
												if iso spec spec' env then 
													((spec', n + 1):: cont, true)
												else ((spec', n):: cont, b)
									) ([], false) specs
							in
							if already_there then Hashtbl.replace species sign specs
							else Hashtbl.replace species sign ((spec, 1):: specs) ;
							(species, visited)
			) sg (empty_table(), IntSet.empty)
	in
	species

let dump desc table hr token_vector env =
	let palette = Hashtbl.create 10 in
	
	Printf.fprintf desc "digraph G{\n" ;
	let _ = 
		Hashtbl.fold
		(fun _ specs cpt ->
				let c = ref cpt in
				List.iter
				(fun (spec, k) ->
					to_dot hr palette k !c spec desc env ;
					c := !c + 1
				) specs ;
				!c+1
		) table 0
	in
	Array.iteri
	(fun tk_id v ->
		let tk = Environment.token_of_num tk_id env in
		Printf.fprintf desc "token_%d [label = \"%s (%E)\" , shape=none]" tk_id tk v
	) token_vector ;
	Printf.fprintf desc "}\n" 
	
let dump_table table env =
	Hashtbl.iter
		(fun _ specs ->
					List.iter
						(fun (spec, k) ->
									Printf.printf "%d instances of species: %s\n" k (to_string spec env);
									Printf.printf "with signature %s\n" (Tools.string_of_map Int64.to_string (fun _ -> "") Int64Map.fold spec.views) ;
									Printf.printf "******\n"
						) specs
		) table