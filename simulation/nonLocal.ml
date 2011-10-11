(**Module that manages connectedness of injection images --for dot and plus operators*)
open Mods

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
	
