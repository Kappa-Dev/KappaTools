open Mods
open ExceptionDefn

(**node: (name,array[site_name,internal_state,link])*)
(**ptr(i,a) pointer to a node and a site at offset i*)

type lnk_t = WLD | BND | FREE | TYPE of (int*int) (*(site_id,nme)*)
type port_status = (int option * ptr) (*internal state,link state*)
and port = {status:port_status ; mutable dep : (LiftSet.t * LiftSet.t)} (*dep : (lifts for int,lifts for lnk)*)
and t = {name:int ; interface : port array ; mutable address : int option} 
(*prob_connect: mix_id -> {u | v....u and v,u are the image of the roots of connected components of mix_id} *)
and ptr = Null | Ptr of (t * int) | FPtr of (int * int)
and node = t (*just alias for Node.t*)

let name node = node.name
let interface node = node.interface
let set_address node i = node.address <- Some i
let get_address node = match node.address with None -> raise Not_found | Some addr -> addr
let is_empty node = (node.name = -1)

let get_lifts u i = 
	let port_i = try u.interface.(i) with Invalid_argument msg -> invalid_arg (Printf.sprintf "Node.get_lifts: node %d has no site %d" (name u) i)
	in
	port_i.dep 
	
	
let fold_status f node cont = 
	let cont = ref cont in
		Array.iteri (fun site_id port -> cont := f site_id port.status !cont) (interface node) ;
		!cont

let fold_dep f node cont =
	let cont = ref cont in
		Array.iteri (fun site_id port -> cont := f site_id port.dep !cont) (interface node) ;
		!cont
		
let fold_interface f node cont = 
	let cont = ref cont in
		Array.iteri (fun site_id port -> cont := f site_id port !cont) (interface node) ;
		!cont
	
let label node env =
	let str_intf = 
		fold_interface
		(fun i port cont ->
				let s_i = Environment.site_of_id node.name i env in
				if (s_i = "_") then cont
				else
					let (int_state,_) = port.status
					in 
					match int_state with 
						| None -> cont 
						| Some x -> 
							let s_int = ("~"^(Environment.state_of_id node.name i x env))
							in
							(Printf.sprintf "%s%s" s_i s_int)::cont
		) node [] 
	in
	match str_intf with
		| [] -> Environment.name node.name env
		| _ -> Printf.sprintf "%s(%s)" (Environment.name node.name env) (String.concat "," (List.rev str_intf)) 

	
let to_string with_detail (hsh_lnk,fresh) node env =
	let intf_l,fresh' = 
		fold_interface
		(fun i port (cont,fresh) ->
			let s_i = Environment.site_of_id node.name i env in
				if not with_detail && (s_i = "_") then (cont,fresh) (*Skipping existential port*)
				else
				let (int_state,lnk_state) = port.status
				and (lifts_int,lifts_lnk) = port.dep 
				in 
					let s_int = match int_state with None -> "" | Some x -> ("~"^(Environment.state_of_id node.name i x env))
					and s_lnk,fresh = 
						match lnk_state with 
							| FPtr (u',j) -> 
								let edge_id,fresh' = 
									try (Hashtbl.find hsh_lnk (u',j),fresh) 
									with 
										| Not_found -> 
											let addr = try get_address node with Not_found -> invalid_arg "Node.to_string: not allocated"
											in
												Hashtbl.add hsh_lnk (addr,i) fresh ;
												(fresh,fresh+1)
								in
									("!"^(string_of_int edge_id),fresh')
							| Null -> ("",fresh) 
							| Ptr (node',j) -> 
								let u' = try get_address node' with Not_found -> invalid_arg "Node.to_string: not allocated"
								in
									let edge_id,fresh' = 
										try (Hashtbl.find hsh_lnk (u',j),fresh) 
										with 
											| Not_found -> 
												let addr = try get_address node with Not_found -> invalid_arg "Node.to_string: not allocated"
												in
													Hashtbl.add hsh_lnk (addr,i) fresh ;
													(fresh,fresh+1)
									in
										("!"^(string_of_int edge_id),fresh')
					in
						let string_of_lift = 
							fun inj -> 
								let (m,c) = Injection.get_coordinate inj in
								Printf.sprintf "(%s,%s)" (string_of_int m) (string_of_int c)  
						in
							let lift_int = Tools.string_of_set string_of_lift LiftSet.fold lifts_int
							and lift_lnk = Tools.string_of_set string_of_lift LiftSet.fold lifts_lnk
							in
								if with_detail then
									((Printf.sprintf "%s%s%s%s%s" s_i s_int lift_int s_lnk lift_lnk)::cont,fresh)
								else
									((Printf.sprintf "%s%s%s" s_i s_int s_lnk)::cont,fresh) 
		) node ([],fresh) 
	in
		let s_addr = try string_of_int (get_address node) with Not_found -> "na" in
		if with_detail then
			(Printf.sprintf "%s_%s:[%s]" (Environment.name node.name env) s_addr (String.concat "," (List.rev intf_l)) ,fresh')
		else
			(Printf.sprintf "%s(%s)" (Environment.name node.name env) (String.concat "," (List.rev intf_l)),fresh') 

let add_dep phi port_list node env =
	let add lift int_lnk = (fun (x,y) -> if int_lnk = 0 then (LiftSet.add x lift,y) else (x,LiftSet.add y lift))
	in
	List.iter
	(fun (int_lnk,p_k) ->
		let port_k = node.interface.(p_k) 
		in 
		let dep = add phi int_lnk port_k.dep in node.interface.(p_k).dep <- dep
	) port_list ;
	node


let iteri f node  = Array.iteri f (interface node) 

let marshalize node =
	let f_intf =
		fold_interface
		(fun site_id port intf ->
			let (int,lnk) = port.status in
				let f_lnk =
					match lnk with
						| FPtr (i,j) -> FPtr (i,j)
						| Null -> Null
						| Ptr (node',site_id') -> FPtr ((try get_address node' with Not_found -> invalid_arg "Node.marshalize"),site_id')
				in
					intf.(site_id) <- {dep = port.dep ; status = (int,f_lnk)} ;
					intf
		) node (Array.copy node.interface)
	in
		{node with address = Some (get_address node) ; interface = f_intf}
		
let is_bound ?with_type (n,i) =
	let intf = n.interface.(i).status 
	in
	match intf with
		| (_,FPtr _) -> invalid_arg "Node.is_bound"
		| (_,Null) -> false
		| (_,Ptr (u,j)) -> 
			match with_type with
				| Some (site_id,nme) -> (name u = nme) && (j = site_id)
				| None -> true

exception TooBig

let bit_encode node env =
	try
  	let tot_offset = ref 0 in 
  	let sign = Environment.get_sig (name node) env in
  	let max_val = fun site_id -> Signature.internal_states_number site_id sign in
  	let bit_rep = 
  		fold_status
  		(fun site_id (int,lnk) bit_rep ->
  			let n = max_val site_id in 
  			let bit_rep = 
  				match int with
  					| None -> if n = 0 then bit_rep else invalid_arg "Node.bit_encode" 
  					| Some i -> 
  						if i>n then invalid_arg "Node.bit_encode"
  						else
  							let offset = Tools.bit_rep_size n in
  							let bit_rep = Int64.shift_left bit_rep offset in
  							(tot_offset := !tot_offset + offset ;
  							if !tot_offset > 63 then raise TooBig
  							else
  								Int64.logor bit_rep (Int64.of_int i))
  			in
  			match lnk with
  					|	FPtr _ | Ptr _ -> let bit_rep = (Int64.shift_left bit_rep 1) in (tot_offset := !tot_offset + 1 ; (Int64.shift_left bit_rep 1))
  					| Null -> (tot_offset := !tot_offset + 1 ; Int64.shift_left bit_rep 1)
  		) node Int64.zero
  	in
  	let offset = Tools.bit_rep_size (Environment.name_number env) in
  	tot_offset := !tot_offset + offset ;
  	if !tot_offset > 63 then raise TooBig
  	else 
  		Int64.logor (Int64.shift_left bit_rep offset) (Int64.of_int (name node)) 
	with TooBig ->
		Int64.of_int (name node) (*If the interface is too big, retaining only the name of the agent as a view*)
	
let internal_state (n,i) = 
	match n.interface.(i).status with
		| (s,_) -> s

let link_state (n,i) = 
	match n.interface.(i).status with
		| (_,i) -> i

let set_ptr (u,i) value = 
	let intf = interface u in
		let port = intf.(i) in
			let int,_ = port.status in
				intf.(i) <- {port with status = (int,value)}

let set_int (u,i) value = 
	let intf = interface u in
		let port = intf.(i) in
			let _,lnk = port.status in
				intf.(i) <- {port with status = (value,lnk)}

let create ?with_interface name_id env =
	try 
		if name_id<0 then (*Adding the empty agent*)
			invalid_arg "Node.create: null agent"
			(*{name="%" ; interface = Array.make 0 {status = (None,Null) ; dep = (LiftSet.create 0,LiftSet.create 0)}; address=None ; species_id = None}*)
		else
			let sign = try Environment.get_sig name_id env with Not_found -> invalid_arg "Node.create 1"
			in
			let size = Signature.arity sign in
			let intf = 
				Array.init size
				(fun i -> 
					let def_int = Signature.default_num_value i sign in
						{status = (def_int,Null) ; dep = (LiftSet.create !Parameter.defaultLiftSetSize,LiftSet.create !Parameter.defaultLiftSetSize)}
				)
			in 
				let u = {name = name_id ; interface = intf ; address = None} in
					match with_interface with
						| None -> u
						| Some m ->
							let (_:unit) =  
								IntMap.iter 
								(fun i (int,_) -> 
									match int with
										| None -> ()
										| Some _ -> set_int (u,i) int
								) m 
							in
								u	
	with
		| Invalid_argument str -> (invalid_arg ("Node.create: "^str))
		| Not_found -> (invalid_arg "Node.create: not found")

(*Tests whether status of port i of node n is compatible with (int,lnk) raises False if not otherwise returns completes*)
(*the list port_list with (0,i) if i is int-tested or (1,i) if i is lnk-tested *)
let test (n,i) (int,lnk) port_list = 
	let intf_n = interface n in
		let (state,link) = try intf_n.(i).status with exn -> (Debug.tag (Printf.sprintf "Node %d has no site %d" (name n) i) ; raise exn) in
			let b,port_list = 
				match int with
					| None -> (true,port_list) (*None in pattern is compatible with anything in the node*)
					| _ as s -> (s=state,(0,i)::port_list)
			in
				if not b then raise False
				else
					match lnk with
						| WLD -> port_list (*None in pattern is compatible with anything in the node*)
						| BND -> (match link with FPtr _ -> invalid_arg "Node.test" | Ptr _ -> (1,i)::port_list | Null -> raise False)
						| TYPE (sid,nme) -> 
							begin
								match link with
									| FPtr _ -> invalid_arg "Node.test"
									| Ptr (u,j) -> if (name u = nme) && (j = sid) then (1,i)::port_list else raise False
									| Null -> raise False
							end
						| FREE -> (match link with FPtr _ -> invalid_arg "Node.test" | Ptr _ -> raise False | Null -> (1,i)::port_list)

let follow (u,i) = 
	let intf_u = interface u in
		let (_,lnk) = intf_u.(i).status in
			match lnk with
				| Ptr (v,j) -> Some (v,j)
				| _ -> None

module NodeHeap = MemoryManagement.Make (struct type t = node let allocate node i = node.address <- (Some i) end)

