open Mods
open Dynamics
open Graph
open State
open LargeArray

type atom_state = FREE | BND of int*int | INT of int | UNDEF
type event_kind = OBS of int | RULE of int | INIT | PERT of int

type atom = 
	{before:atom_state ; (*attribute state before the event*)
	after:atom_state; (*attribute state after the event*) 
	locked:bool ; (*whether this node can be removed by compression*)
	causal_impact : int ; (*(1) tested (2) modified, (3) tested + modified*) 
	eid:int (*event identifier*) ;
	label:event_kind
	}

type attribute = atom list (*vertical sequence of atoms*)
	
type grid = {flow: (int*int*int,attribute) Hashtbl.t}  (*(n_i,s_i,q_i) -> att_i with n_i: node_id, s_i: site_id, q_i: link (1) or internal state (0) *)

let empty_grid () = {flow = Hashtbl.create !Parameter.defaultExtArraySize }

let grid_find (node_id,site_id,quark) grid = Hashtbl.find grid.flow (node_id,site_id,quark)

let grid_add (node_id,site_id,quark) attribute grid = 
	Hashtbl.replace grid.flow (node_id,site_id,quark) attribute ;
	grid
		
let is i c = (i land c = i)

let impact q c = 
	if q = 1 (*link*) 
	then 
		if (is _LINK_TESTED c) && (is _LINK_MODIF c) then 3 
		else 
			if (is _LINK_MODIF c) then 2 else 1
	else (*internal state*)
		if (is _INTERNAL_TESTED c) && (is _INTERNAL_MODIF c) then 3 
		else 
			if (is _INTERNAL_MODIF c) then 2 else 1

let before attribute = 
	match attribute with
		| atom::_ -> atom.after
		| [] -> UNDEF 

let last_event attribute = 
	match attribute with
		| [] -> None
		| a::_ -> (Some a.eid)

let rec find_opposite atom attribute = 
	match attribute with
		| [] -> raise Not_found
		| a::att -> 
			if a.causal_impact = 1 (*a is a pure test*) then find_opposite atom att
			else 
				if a.before = atom.after then a
				else raise Not_found

(*adds atom a to attribute att. Collapses last atom if if bears the same id as a --in the case of a non atomic action*)
let push (a:atom) (att:atom list) = 
	match att with
		| [] -> ([a],None)
		| a'::att' -> 
			if a'.eid = a.eid then (a::att',None) (*if rule has multiple effect on the same attribute, only the last one is recorded*) 
			else
				begin
					if a.causal_impact = 1 then (a::att,None) (*atom is a pure test, no need to compress loop locally*) 
					else
						let opt = try Some (find_opposite a att) with Not_found -> None in 
						(a::att,opt)
				end 

let add (node_id,site_id) c state grid event_number kind locked =
 
	(*adding a link modification*)
	let grid = 
		if (is _LINK_TESTED c) || (is _LINK_MODIF c) then
			let att = try grid_find (node_id,site_id,1) grid with Not_found -> [] in
			let after = 
				(*if is _LINK_MODIF c then*)
					let opt_node = try Some (SiteGraph.node_of_id state.graph node_id) with Not_found -> None (*node deleted*) in
					match opt_node with
						| Some node ->
							begin
								match Node.link_state (node,site_id) with
									| Node.Ptr (node',site_id') -> BND (Node.get_address node',site_id') 
									| Node.FPtr _ -> invalid_arg "Causal.add"
									| Node.Null -> FREE
							end
						| None -> UNDEF
							
				(*else 
					before att*)
			in
			let att,opt_opposite = push {before = before att ; after = after ; locked = locked ; causal_impact = impact 1 c ; eid = event_number ; label = kind} att
			in
			grid_add (node_id,site_id,1) att grid
		else
			grid 
	in
	if (is _INTERNAL_TESTED c) || (is _INTERNAL_MODIF c) then
		(*adding an internal state modification*)
		let att = try grid_find (node_id,site_id,0) grid with Not_found -> [] in
		let after = 
			(*if is _INTERNAL_MODIF c then*)
				let opt_node = try Some (SiteGraph.node_of_id state.graph node_id) with Not_found -> None in
				match opt_node with
					| Some node ->
						begin
							match Node.internal_state (node,site_id) with
								| Some i -> INT i 
								| None -> invalid_arg "Causal.add"
						end
					| None -> UNDEF
			(*else before att*)
		in
		let att,opt_opposite = push {before = before att ; after = after ; locked = locked ; causal_impact = impact 0 c ; eid = event_number ; label = kind} att
		in
		grid_add (node_id,site_id,0) att grid
	else 
		grid
		
(**side_effect Int2Set.t: pairs (agents,ports) that have been freed as a side effect --via a DEL or a FREE action*)
(*NB no internal state modif as side effect*)
let record mix opt_rule embedding state counter locked grid env = 
	
	let im state embedding fresh_map id grid =
		try
			match id with
			| FRESH j ->  
				let im_j = (IntMap.find j fresh_map) in
				let node = SiteGraph.node_of_id state.graph im_j in
				let node_id = Node.name node in
				let grid =  (*adding attributes for new site*)
					Node.fold_status (fun site_id _ grid ->
						let int_opt = Environment.default_state node_id site_id env in
						let grid = 
							match int_opt with
								| Some i -> 
									let att = grid_find (node_id,site_id,0) grid in
									let atom = {before = UNDEF ; after = INT i ; locked = false ; causal_impact = impact 0 _INTERNAL_MODIF ; eid = Counter.event counter ; label = INIT}
									in
									let att,opt_opposite = push atom att in
									grid_add (node_id,site_id,0) att grid
								| None -> grid
						in
						let att = grid_find (node_id,site_id,1) grid in (*link state has to be modified because node is fresh*)
						let atom = {before = UNDEF ; after = FREE ; locked = false ; causal_impact = impact 1 _LINK_MODIF ; eid = Counter.event counter ; label = INIT}
						in
						let att,opt_opposite = push atom att in
						grid_add (node_id,site_id,1) att grid
					) node grid
				in
				(im_j,grid)
			| KEPT j ->
				let im_j =
					begin
						try	(IntMap.find j embedding) with Not_found -> invalid_arg "Causal.record: Not a valid embedding"
					end
				in 
				(im_j,grid)
		with 
			| Not_found -> invalid_arg "Causal.record: incomplete embedding"  
	in
	
	let grid = (*if mix is the lhs of a rule*) 
		match opt_rule with
			| Some (pre_causal,side_effects,psi,is_pert,r_id) ->
				(*adding side-effect free modifications and tests*)
				let kind = if is_pert then (PERT r_id) else (RULE r_id) in
				let grid = 
					Id2Map.fold
					(fun (id,site_id) c grid ->
						let node_id,grid = im state embedding psi id grid in
						add (node_id,site_id) c state grid (Counter.event counter) kind locked 
					) pre_causal grid
				in
				(*adding side effects modifications*)
				Int2Set.fold 
				(fun (node_id,site_id) grid -> add (node_id,site_id) _LINK_MODIF state grid (Counter.event counter) kind locked) 
				side_effects grid
			| None -> (*event is an observable occurrence*)
				let kind = OBS (Mixture.get_id mix) in
				IntMap.fold
				(fun id ag grid ->
					let node_id,grid = im state embedding IntMap.empty (Dynamics.KEPT id) grid in
					Mixture.fold_interface
					(fun site_id (int,lnk) grid ->
						let grid = 
							match int with
								| Some i -> add (node_id,site_id) _INTERNAL_TESTED state grid (Counter.event counter) kind true
								| None -> grid
						in
						match lnk with
							| Node.BND | Node.FREE | Node.TYPE _ -> add (node_id,site_id) _LINK_TESTED state grid (Counter.event counter) kind true
							| Node.WLD -> grid
					)
					ag grid
				)
				(Mixture.agents mix) grid
	in
	grid


let init state grid = 
	SiteGraph.fold
	(fun node_id node grid ->
		Node.fold_status
		(fun site_id (int,lnk) grid ->
			let grid = 
				match int with 
					| None -> grid
					| Some i -> 
						let atom = {before = UNDEF ; after = INT i ; locked = true ; causal_impact = impact 0 _INTERNAL_MODIF ; eid = 0 ; label = INIT}
						in
						grid_add (node_id,site_id,0) [atom] grid 
			in
			match lnk with
				| Node.Ptr (node',site_id') -> 
					let node_id' = try Node.get_address node' with Not_found -> invalid_arg "Causal.init" in
					let atom = {before = UNDEF ; after = BND (node_id',site_id') ; locked = true ; causal_impact = impact 1 _LINK_MODIF ; eid = 0 ;label = INIT}
					in
						grid_add (node_id,site_id,1) [atom] grid 
				| Node.Null -> 
					let atom = {before = UNDEF ; after = FREE ; locked = true ; causal_impact = impact 1 _LINK_MODIF ; eid = 0 ; label = INIT}
					in
						grid_add (node_id,site_id,1) [atom] grid
			  | _ -> invalid_arg "Causal.init"
		) node grid
	)	state.graph grid


let string_of_atom atom = 
	let string_of_atom_state state =
		match state with
			| FREE -> "..."
			| BND (i,j) -> Printf.sprintf "(%d,%d)" i j
			| INT i -> Printf.sprintf "%d" i
			| UNDEF -> "N"
	in
	let imp_str = match atom.causal_impact with 1 -> "o" | 2 -> "x" | 3 -> "%" | _ -> invalid_arg "Causal.string_of_atom" in
	Printf.sprintf "(%s%s%s)_%d" (string_of_atom_state atom.before) imp_str (string_of_atom_state atom.after) atom.eid

		
let dump grid state env =
	Hashtbl.fold 
	(fun (n_id,s_id,q) att _ ->
		let q_name = "#"^(string_of_int n_id)^"."^(string_of_int s_id)^(if q=0 then "~" else "!") in
		let att_ls =
			List.fold_right
			(fun atom ls -> LongString.concat (string_of_atom atom) ls) 
			att LongString.empty
		in
		Printf.printf "%s:" q_name ; 
		LongString.printf stdout att_ls ;
		Printf.printf "\n"
	) 
	grid.flow ()
	
