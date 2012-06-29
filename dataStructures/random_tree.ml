open Mods

module type Random_tree =
sig
	type tree
	val create: int -> tree
	val total: tree -> float
	val copy: tree -> tree
	val copy_in: tree -> tree -> tree
	val add: int -> float -> tree -> unit
	val random: tree -> int * float
	val update_structure: tree -> tree
	val find : int -> tree -> float
	val is_infinite : int -> tree -> bool
end

module Random_tree =
	(struct
		type tree = {
			mask: (int, int) Hashtbl.t ;
			unmask: (int, int) Hashtbl.t ;
			mutable new_mask : int ;
			mutable inf_list : IntSet.t ;
			size: int;
			weight_of_nodes: float array ;
			weight_of_subtrees: float array ;
			unbalanced_events_by_layer: int list array ;
			unbalanced_events: bool array;
			layer: int array ;
			mutable consistent: bool
		}
				
		let mask t i = 
			try Hashtbl.find t.mask i with 
				| Not_found -> 
					let m = t.new_mask in
						t.new_mask <- m+1 ; 
						Hashtbl.replace t.mask i m ; 
						Hashtbl.replace	t.unmask m i ; 
						m 
			         
		let unmask t m = try Hashtbl.find t.unmask m with Not_found -> invalid_arg "Random_tree: incoherent hash"
		
		let is_infinite i t = let i = mask t i in IntSet.mem i t.inf_list

		
		let find i t = 
			let i = mask t i in t.weight_of_nodes.(i)
		
		let copy t = {
			mask = Hashtbl.copy t.mask ;
			unmask = Hashtbl.copy t.unmask ;
			new_mask = t.new_mask ;
			size = t.size;
		(*	total = t.total ;*)
			weight_of_nodes = Array.copy t.weight_of_nodes ;
			weight_of_subtrees = Array.copy t.weight_of_subtrees ;
			layer = Array.copy t.layer;
			consistent = t.consistent ;
			unbalanced_events_by_layer = Array.copy t.unbalanced_events_by_layer ;
			unbalanced_events = Array.copy t.unbalanced_events ;
			inf_list = IntSet.empty
		}
		
		let copy_vect_in t t1 =
			let _ = Array.iteri (fun i a -> t1.(i) <- a) t in
			t1
		
		let copy_in t1 t2 =
			let _ = copy_vect_in t1.weight_of_nodes t2.weight_of_nodes in
			let _ = copy_vect_in t1.weight_of_subtrees t2.weight_of_subtrees in
			let _ = copy_vect_in t1.layer t2.layer in
			let _ = copy_vect_in t1.unbalanced_events t2.unbalanced_events in
			let _ = copy_vect_in t1.unbalanced_events_by_layer t2.unbalanced_events_by_layer in
			let _ = t2.consistent <- t1.consistent in
			t2
		
		let pere i = i / 2
		let left_son i = i * 2
		let right_son i = i * 2 + 1
		
		let is_left_son i = i mod 2 = 0
		let is_right_son i = i > 1 && i mod 2 = 1
		let is_root i = i = 1
		
		let rec update_structure t =
			if t.consistent then t
			else
				let n_layer = t.layer.(t.size) in
				let weight_of_subtree k =
					if k > t.size then 0.
					else t.weight_of_subtrees.(k)
				in
				let rec aux k =
					if k = 0 then ()
					else
						let l = t.unbalanced_events_by_layer.(k) in
						t.unbalanced_events_by_layer.(k) <- [] ;
						List.iter
							(fun i ->
										t.weight_of_subtrees.(i) <- t.weight_of_nodes.(i) +. weight_of_subtree (2 * i) +. weight_of_subtree (2 * i + 1) ;
										t.unbalanced_events.(i) <- false ;
										if is_root i then ()
										else
											let father = i / 2 in
											declare_unbalanced father t
							) l ;
						aux (k - 1)
				in
				aux n_layer ;
				t.consistent <- true ;
				t
		
		and	declare_unbalanced i t =
			let _ =
				if t.unbalanced_events.(i) then ()
				else
					let l = t.layer.(i) in
					t.unbalanced_events.(i) <- true ;
					t.unbalanced_events_by_layer.(l) <- i:: (t.unbalanced_events_by_layer.(l))
			in
			t.consistent <- false
		
		let create n =
			let t_node = Array.create (n + 1) 0. in
			let t_subtree = Array.create (n + 1) 0. in
			let layer = Array.create (n + 1) 0 in
			let _ =
				let rec aux k current_layer layer_end =
					if k > n then ()
					else if k > layer_end then aux k (current_layer + 1) (2 * layer_end + 1)
					else (layer.(k) <- current_layer;
						aux (k + 1) current_layer layer_end)
				in aux 1 1 1 in
			let unbalanced_events_by_layer = Array.create (layer.(n) + 1) [] in
			let unbalanced_events = Array.create (n + 1) false in
			{ size = n;
			(*	total = 0.;*)
				new_mask = 1 ;
				mask = Hashtbl.create (n+1) ;
				unmask = Hashtbl.create (n+1) ;
				inf_list = IntSet.empty ;
				consistent = true;
				weight_of_nodes = t_node;
				weight_of_subtrees = t_subtree;
				unbalanced_events_by_layer = unbalanced_events_by_layer;
				unbalanced_events = unbalanced_events;
				layer = layer }
		
		let raz t =
			let n = t.size in
			let _ =
				let rec aux k =
					if k = 0 then ()
					else
						(t.unbalanced_events.(k) <- false ;
							t.weight_of_subtrees.(k) <- 0. ;
							t.weight_of_nodes.(k) <- 0.)
				in aux n in
			let _ =
				Array.iteri
					(fun k _ -> t.unbalanced_events_by_layer.(k) <-[])
					t.unbalanced_events_by_layer
			in
			t
		
		let add i w t =
			let i = mask t i in
			let w = 
				if w = infinity then (t.inf_list <- IntSet.add i t.inf_list ; 0.)
				else (t.inf_list <- IntSet.remove i t.inf_list ; w)
			in 
		(*	let total = t.total -. t.weight_of_nodes.(i) +. w in*)
			let _ = t.weight_of_nodes.(i) <- w
			and _ = declare_unbalanced i t
			in
			() (*t.total <- (max 0.0 total) (*not satisfactory*)*)
		
		let random t =
			try (unmask t (IntSet.choose t.inf_list),infinity) 
			with Not_found ->
				let t = update_structure t in
				let a = t.weight_of_subtrees.(1) in
				if a = 0.0
				then raise Not_found
				else
					let r = Random.float a in
					let rec find i r =
						let node = t.weight_of_nodes.(i) in
						if r < node then (i,node)
						else if 2 * i > t.size then raise Not_found
						else
							let r'= r -.node in
							let lson = 2 * i in
							let rson = 2 * i + 1 in
							let left = t.weight_of_subtrees.(lson) in
							if r'< left then find lson r'
							else
							if rson > t.size then raise Not_found
							else find rson (r'-.left)
					in
					let rep,w = find 1 r in
					(unmask t rep,w)
		
                let total t = 
                  if IntSet.is_empty t.inf_list 
                  then 
                    begin 
                      let t = update_structure t in 
                      t.weight_of_subtrees.(1)
                    end 
                  else 
                    infinity

	end: Random_tree)
