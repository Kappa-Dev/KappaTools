(**Module that manages connectedness of injection images --for dot and plus operators*)
open Mods
open Graph

let add arity mix_id inj_list inj_prod_hp =
	let inj_prod = 
		match InjProdHeap.next_alloc inj_prod_hp with
			| None -> InjProduct.create arity mix_id
			| Some ar -> ar
	in
	List.iter 
	(fun inj ->
		let (_,cc_id) = Injection.get_coordinate inj in
		InjProduct.add cc_id inj inj_prod
	) inj_list ;
	(InjProdHeap.alloc inj_prod inj_prod_hp,inj_prod)
	
let remove inj_prod h = 
	let i = try InjProduct.get_address inj_prod with Not_found -> invalid_arg "NonLocal.remove" in
	InjProdHeap.remove i h

let search_elements graph component extensions env =
	let ext,modified = 
		IntSet.fold
		(fun u_id (extensions,modified) ->
			let u = try SiteGraph.node_of_id graph u_id with Not_found -> invalid_arg "NonLocal.search_elements"
			in
			Printf.printf "Entering component at node[%d]\n" u_id ;
			let _,lifts = Node.get_lifts u 0 in
			
			LiftSet.fold
			(fun inj (extensions,modified) ->
				if Injection.is_trashed inj then invalid_arg "NonLocal.search_elements: injection should not be already invalid..."
				else
					let (mix_id,cc_id) = Injection.get_coordinate inj in
					Printf.printf "Found a lift pointing to inj[%d,%d]\n" mix_id cc_id ;
			
					if not (Environment.is_nl_rule mix_id env) then (Debug.tag "But this is not a unary rule..." ; (extensions,modified))
					else
						let _ = Debug.tag "Adding this injection as a candidate for a new intra" in
						let inj_map = try IntMap.find mix_id extensions with Not_found -> IntMap.empty in
						let inj_list = try IntMap.find cc_id inj_map with Not_found -> [] in
						let inj_map' = IntMap.add cc_id (inj::inj_list) inj_map in
						(IntMap.add mix_id inj_map' extensions,true)
			) lifts (extensions,modified)
		) component (extensions,false)
	in
	if modified then (Some ext)
	else None