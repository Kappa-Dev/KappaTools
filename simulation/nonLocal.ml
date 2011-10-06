(**Module that manages connectedness of injection images --for dot and plus operators*)
open Mods

module InjProdHeap = Heap.Make(InjProduct) 
module InjProdSet = Set.Make(InjProduct)

type inj_hp = InjProdHeap.t (*provides InjProdHeap.random, size, remove and alloc*)

let new_heap n = InjProdHeap.create n

let add arity mix_id inj_list inj_prod_hp =
	let inj_prod = 
		match InjProdHeap.next_alloc inj_prod_hp with
			| None -> InjProduct.empty arity mix_id
			| Some ar -> ar
	in
	List.iter 
	(fun inj ->
		let (_,cc_id) = Injection.get_coordinate inj in
		match Injection.root_image inj with
			| None -> invalid_arg "NonLocal.add"
			| Some (a_i,u_i) -> InjProduct.add cc_id (a_i,u_i) inj_prod
	) inj_list ;
	(InjProdHeap.alloc inj_prod inj_prod_hp,inj_prod)
	
let remove inj_prod h = 
	let i = try InjProduct.get_address inj_prod with Not_found -> invalid_arg "NonLocal.remove" in
	InjProdHeap.remove i h