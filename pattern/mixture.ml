open Mods
open ExceptionDefn
open Ast
open Tools

type agent = {name:int ; interface : (int option * Node.lnk_t) IntMap.t}
type covering = {span: (int*int) Int2Map.t ; internal: (int*int) Int2Map.t} 
type t = {
	agents:agent IntMap.t ; 
	site_number : int ;
	graph : (int*int) Int2Map.t ;
	enum_cov : (int,covering) Hashtbl.t option ;
	ids_of_name : IntSet.t Int2Map.t ; (*(nm,cc_id) -> id if agent(id) has name nm in con. comp. cc_id*)
	component_of_id : int array option ; (*id -> cc_id starting at 0*)
	arity : int option (*number of explicitly connected components*) ;
	mix_id : int option ;
	size_of_cc : int array ;
	root_of_cc : int array ;
	unary : bool
	}

let graph m = m.graph 

let interface ag = ag.interface

let is_empty m = (IntMap.size m.agents) = 0	

let empty_agent = {name = 0 ; interface = IntMap.empty}

let get_id mix =
	match mix.mix_id with 
		| None -> invalid_arg "State.get_id: Not found" 
		| Some id -> id


let span root mix = 
	match mix.enum_cov with
		| Some hsh -> (try (fun cov -> cov.span) (Hashtbl.find hsh root) with Not_found -> Int2Map.empty)
		| None -> invalid_arg "Mixture.span: covering not computed"
			
let internal_edges root mix = 
	match mix.enum_cov with
		| Some hsh -> (try (fun cov -> cov.internal) (Hashtbl.find hsh root) with Not_found -> Int2Map.empty)
		| None -> invalid_arg "Mixture.span: covering not computed"

let arity mix = 
	if is_empty mix then 0 
	else
		match mix.arity with
			| Some arity -> arity
			| None -> invalid_arg "Mixture.arity: arity not computed"

let component_of_id id mix = 
	match mix.component_of_id  with
		| Some ar -> 
			begin
				try ar.(id) with Invalid_argument msg -> invalid_arg ("Mixture.component_of_id: "^msg)
			end
		| None -> invalid_arg "Mixture.component_of_id: component_of_id not computed"

let set_root_of_cc mix = 
	match mix.component_of_id with
				| None -> invalid_arg "Mixture.set_root_of_cc"
				| Some ar -> 
					let root_of_cc = Array.create (arity mix) (-1) in 
					Array.iteri (fun a_i cc_i -> if root_of_cc.(cc_i) < a_i then root_of_cc.(cc_i) <- a_i) ar ;
					{mix with root_of_cc = root_of_cc}

let root_of_cc mix cc_id =
	try
		Some (mix.root_of_cc.(cc_id)) 
	with
		| _ -> None
			
let agent_of_id i mix = IntMap.find i mix.agents
let name ag = ag.name
let agents mix = mix.agents
let ids_of_name (nm,cc_id) mix = try Int2Map.find (nm,cc_id) mix.ids_of_name with Not_found -> IntSet.empty 

let is_bound (a_i,s_i) mix = 
	let ag_i = agent_of_id a_i mix in
		let (_,lnk) = IntMap.find s_i ag_i.interface in
			match lnk with
				| Node.BND | Node.TYPE _ -> true
				| _ -> false

let fold_interface f ag = IntMap.fold f ag.interface 
let create_agent name intf = {name=name ; interface = intf}

let empty id_opt = {
	agents = IntMap.empty ;
	site_number = 0 ; 
	graph = Int2Map.empty ;
	enum_cov = None ;
	ids_of_name = Int2Map.empty ;
	component_of_id = None ;
	arity = None ;
	mix_id = id_opt ;
	size_of_cc = Array.create 0 0 ;
	root_of_cc = Array.create 0 0 ;
	unary = false
	} 

let unary mix = mix.unary
let set_unary mix = {mix with unary = true}

let size_of_cc cc_id mix = 
	try
		mix.size_of_cc.(cc_id)
	with
		| Invalid_argument msg -> invalid_arg ("Mixture.size_of_cc "^msg)
	
let compose id agent mixture new_edges = 
	let graph,site_num = 
		Int2Map.fold 
		(fun (a,i) (b,j) (graph,site_num) -> 
			if not (id=a) then invalid_arg "Mixture.compose: invariant violation 1"
			else if a<b then invalid_arg "Mixture.compose: invariant violation 2" (*spanning edge a->b*)
			else 
				(Int2Map.add (b,j) (a,i) (Int2Map.add (a,i) (b,j) graph),site_num+2)
		) 
		new_edges (mixture.graph,mixture.site_number)
	in
	{mixture with
	graph = graph ;
	site_number = site_num ;
	agents = IntMap.add id agent mixture.agents ; 
	}

let follow_in_spanning_tree root_ag (i,site_id) mix =
	let span = try (span root_ag mix) with 
		| Not_found -> 
			invalid_arg (Printf.sprintf "Mixture.follow_in_spanning_tree: span not precompiled for root %d" root_ag)
	in 
		try
			Some (Int2Map.find (i,site_id) span)
		with
			| Not_found -> None

let follow (i,site_id) mix = 
	try Some (Int2Map.find (i,site_id) mix.graph) with Not_found -> None 
	
let to_kappa with_number mix env = 
	let bnd = Hashtbl.create 10
	and fresh = ref 0 
	in
	let string_of_intf name_id agent_id interface = 
		let l = 
			IntMap.fold 
			(fun site_id (opt_v,opt_l) l ->
				let site_name = Environment.site_of_id name_id site_id env in
					if site_name = "_" then l (*skipping existential port*)
					else 
						let s_int = match opt_v with 
							| (Some x) -> ("~"^(Environment.state_of_id name_id site_id x env))
							| None -> ""
						 in
								let s_lnk = match opt_l with
									| Node.BND -> 
										let opt = follow (agent_id,site_id) mix in
											begin
												match opt with
													| Some (agent_id',site_id') -> 
														begin
															let lnk = 
																try Hashtbl.find bnd (agent_id,site_id) with 
																	| Not_found -> 
																		(Hashtbl.replace bnd (agent_id',site_id') !fresh ;
																		let i = !fresh in
																			fresh := !fresh+1 ;
																			i)
															in
																Hashtbl.replace bnd (agent_id,site_id) lnk ;
																"!"^(string_of_int lnk)
														end
													| None -> 
														if not (Hashtbl.mem bnd (agent_id,site_id)) then "!_"
														else
															let lnk = Hashtbl.find bnd (agent_id,site_id) in
																"!"^(string_of_int lnk)
											end
									| Node.FREE -> ""
									| Node.WLD -> "?"
									| Node.TYPE (i,nme) -> 
										let s = Environment.site_of_id nme i env 
										and n = Environment.name nme env in
										 ("!"^s^"."^n) 
								in
									(site_name^s_int^s_lnk)::l
			) interface []
		in
			String.concat "," (List.rev l) 
	in
		let l = 
			IntMap.fold 
			(fun id agent l -> 
				let name = if with_number then (Environment.name agent.name env)^"#"^(string_of_int id) 
				else Environment.name agent.name env 
				in
					let s = Printf.sprintf "%s(%s)" name (string_of_intf agent.name id agent.interface) in
						s::l
			) mix.agents []
		in
			Printf.sprintf "%s" (String.concat "," (List.rev l)) 

let create_sptr_from id mix = 
	let rec depth_first queue viewed m2_span m2_internal component =
		match queue with
			| a_i::tl ->
				begin
					let component' = if a_i < component then a_i else component
					and (queue',viewed',m2_span',m2_internal') = 
						fold_interface 
						(fun s_i _ (queue,viewed,m2_span,m2_internal) ->
							let opt = follow (a_i,s_i) mix in
								match opt with
								| Some (b_j,s_j) -> 
										if (IntSet.mem b_j viewed) then	
											if (a_i,s_i) < (b_j,s_j) then 
												(queue,viewed,m2_span,Int2Map.add (a_i,s_i) (b_j,s_j) m2_internal)
											else (queue,viewed,m2_span,m2_internal)
										else
											let m2_span' = Int2Map.add (a_i,s_i) (b_j,s_j) m2_span
											in
												(b_j::queue,IntSet.add b_j viewed,m2_span',m2_internal)
								| None -> (queue,viewed,m2_span,m2_internal)
						) (agent_of_id a_i mix) (tl,viewed,m2_span,m2_internal)
					in
						depth_first queue' viewed' m2_span' m2_internal' component'
			end
		| [] -> ({span=m2_span; internal=m2_internal},component)
	in
		depth_first [id] (IntSet.singleton id) Int2Map.empty Int2Map.empty id

let enum_alternate_anchors mix = 
	let sptrs = Hashtbl.create mix.site_number in
		let comp_map,comp_num = 
			IntMap.fold 
			(fun id ag (component_map,comp_num) -> 
				let span,component = create_sptr_from id mix in (*component: smallest agent id belonging to CC(id)*)
					Hashtbl.replace sptrs id span ; 
					let comp_num' = if component = id then comp_num+1 else comp_num in
						(IntMap.add id component component_map,comp_num')						
			) mix.agents (IntMap.empty,0) 
		in
			let ar,_,_,ids_of_name,size_of_cc = 
				IntMap.fold 
				(fun id id_min (ar,m,fresh,ids_of_name,size_of_cc) -> 
					let (cc_id,fresh) = try (IntMap.find id_min m,fresh) with Not_found -> (fresh,fresh+1)
					in
						let ag = agent_of_id id mix in
							ar.(id) <- cc_id ;
							let n = try IntMap.find cc_id size_of_cc with Not_found -> 0 in
							let size_of_cc = IntMap.add cc_id (n+1) size_of_cc in
							let ids_of_name = 
								let set = try Int2Map.find (ag.name,cc_id) ids_of_name with Not_found -> IntSet.empty 
								in
									Int2Map.add (ag.name,cc_id) (IntSet.add id set) ids_of_name 
							in
								(ar,IntMap.add id_min cc_id m,fresh,ids_of_name,size_of_cc) 
				) comp_map (Array.create (IntMap.size comp_map) 0,IntMap.empty,0,Int2Map.empty,IntMap.empty)
			in
			let size_of_cc = Array.init (comp_num) (fun cc_id -> IntMap.find cc_id size_of_cc) in
				{mix with enum_cov = Some sptrs ; 
				component_of_id = Some ar ; 
				arity = Some comp_num ; 
				ids_of_name = ids_of_name ;
				size_of_cc = size_of_cc}

let site_defined site_id ag is_added env =
	try
		let (int,lnk) = IntMap.find site_id ag.interface
		in
			let tested = 
				match int with
					| Some _ -> true
					| None -> false
			in
				if tested then Some (int,lnk)
				else
					match lnk with
						| Node.WLD -> None
						| _ -> Some (int,lnk)
	with
		| Not_found -> 
			if not is_added then None
			else 
				try
					Some (Environment.default_state (name ag) site_id env,Node.FREE)
				with
					| Not_found -> invalid_arg "Mixture.site_defined: invariant violation"


let dump_span mix = 
	Printf.printf "Arity: %d\n" (arity mix) ;
	let ar = match mix.component_of_id with Some ar -> ar | None -> invalid_arg "Mixture.dump_span: component_of_id not computed"
	in
		Printf.printf "component map: %s\n" (string_of_array string_of_int ar) ;
		let hsh = (match mix.enum_cov with Some hsh -> hsh | None -> invalid_arg "Mixture.dump_span: hsh not computed")
		in
			Hashtbl.iter 
			(fun root_id cov ->
					Printf.printf "SPTR[%d]: " root_id;
					let str =
						string_of_map 
						(fun (i,j) -> Printf.sprintf "(%d,%d)" i j) 
						(fun (i,j) -> Printf.sprintf "(%d,%d)" i j) 
						Int2Map.fold cov.span
					in
						print_string str ; print_newline() 
			) hsh

