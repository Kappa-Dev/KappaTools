(**Module that manages connectedness of injection images --for dot and plus operators*)
open Mods

(*array of pairs (a_i,u_i) being witnesses of the injection that maps a_i of CC #i of mix_id to u_i in the graph*)
type inj_product = {inj: (int*int) array ; mutable address : int}

module InjProdHeap = Heap.Make(struct type t = inj_product let allocate prod i = prod.address <- i let get_address prod = prod.address end)

type t = { 
	mix_id : int ; (*identifier of the corresponding mixture*)
	inj_product : InjProdHeap.t ; (*provides InjProdHeap.random, size, remove and alloc*)
	}
	
let add inj_list mix_id inj_prod_hp = 
	