module StringMap = MapExt.Make (struct type t = string let compare = compare end)   
module IntMap = MapExt.Make (struct type t = int let compare = compare end)
module IntSet = Set.Make (struct type t = int let compare = compare end)
module Int2Map = MapExt.Make (struct type t = int*int let compare = compare end)
module StringSet = Set.Make (struct type t = string let compare = compare end)
module Int2Set = Set.Make (struct type t = int*int let compare = compare end)
module Int3Set = Set.Make (struct type t = int*int*int let compare = compare end)

type dep_type = ALG of int | KAPPA of int | EVENT | TIME | RULE of int | PERT of int | ABORT of int
module DepMap = Map.Make (struct type t = dep_type let compare = compare end) 
module DepSet = Set.Make (struct type t = dep_type let compare = compare end) 

let string_of_dep = function
	| ALG i -> "ALG("^(string_of_int i)^")"
	| KAPPA i -> "KAPPA("^(string_of_int i)^")"
	| EVENT -> "EVENT"
	| TIME -> "TIME"
	| RULE i -> "RULE("^(string_of_int i)^")"
	| PERT i -> "PERT("^(string_of_int i)^")"
	| ABORT i -> "ABORT("^(string_of_int i)^")"

module StringIntMap = MapExt.Make (struct type t = (string * int) let compare = compare end)
module Injection = 
	struct
		type t = {map : (int,int) Hashtbl.t ; mutable address : int option ; coordinate : (int*int)}
		
		let set_address addr phi = phi.address <- Some addr 
			
		let get_address phi = match phi.address with Some a -> a | None -> raise Not_found
		let get_coordinate phi = phi.coordinate 
		
		let add i j phi = Hashtbl.replace phi.map i j ; phi
		let mem i phi = Hashtbl.mem phi.map i 
		let size phi = Hashtbl.length phi.map
		let find i phi = Hashtbl.find phi.map i
		let empty n (mix_id,cc_id) = {map = Hashtbl.create n ; address = None ; coordinate = (mix_id,cc_id)}
		let flush phi (var_id,cc_id) = 
			{phi with address = None ; coordinate = (var_id,cc_id)}
		
		
		let compare phi psi = 
			try
				let a = get_address phi
				and a'= get_address psi
				and (m,c) = get_coordinate phi
				and (m',c') = get_coordinate psi
				in
					compare (m,c,a) (m',c',a') (*might be better to compare a bit rep of this triple*)
			with Not_found -> invalid_arg "Injection.compare"
		
		let fold f phi cont = Hashtbl.fold f phi.map cont

		exception Clashing
		let codomain phi cod = 
			Hashtbl.fold 
			(fun i j set -> if IntSet.mem j set then raise Clashing else IntSet.add j set) 
			phi.map cod
		
		let to_string phi = 
			Tools.string_of_map string_of_int string_of_int Hashtbl.fold phi.map 
			
		let copy phi = fold (fun i j phi' -> add i j phi') phi {map = Hashtbl.create (size phi) ; address = None ; coordinate = get_coordinate phi}
	end

module Activity:(ValMap.ValMap with type content = float) = 
	ValMap.Make 
	(struct 
		type t = float (*mix_id -> rule_activity*) 
		let to_f = fun alpha -> alpha
	end)

module InjectionHeap = 
	Heap.Make 
	(struct 
		type t = Injection.t 
		let allocate = fun inj i -> Injection.set_address i inj  
		let get_address inj = Injection.get_address inj 
	end)

module Counter = 
	struct
		type t = {
			mutable time:float ; 
			mutable events:int ; 
			mutable null_events:int ; 
			mutable null_action:int ;
			mutable last_tick : (int * float) ;
			mutable initialized : bool ;
			mutable ticks : int ; 
			init_time : float ;
			init_event : int ;
			max_time : float option ; 
			max_events : int option ;
			dE : int option ;
			dT : float option 
			}
		let inc_tick c = c.ticks <- c.ticks + 1
		let time c = c.time
		let event c = c.events
		let null_event c = c.null_events
		let null_action c = c.null_action
		let is_initial c = c.time = c.init_time
		let inc_time c dt = c.time <- (c.time +. dt)
		let inc_events c =c.events <- (c.events + 1) 
		let inc_null_events c = c.null_events <- (c.null_events + 1) 
		let inc_null_action c = c.null_action <- (c.null_action + 1)
		let check_time c = match c.max_time with None -> true | Some max -> c.time < max
		let check_events c = match c.max_events with None -> true | Some max -> c.events < max
		let dT c = c.dT
		let dE c = c.dE
		let last_tick c = c.last_tick
		let set_tick c (i,x) = c.last_tick <- (i,x)
		let dT c = c.dT
		
		let compute_dT () = 
			match !Parameter.pointNumberValue with
				| None -> None
				| Some points ->
					match !Parameter.maxTimeValue with
						| None -> None
						| Some max_t -> 
							Some (max_t /. (float_of_int points))

		let compute_dE () =
			match !Parameter.pointNumberValue with
				| None -> None
				| Some points ->
					match !Parameter.maxEventValue with
						| None -> None
						| Some max_e ->
							let m = max (max_e / points) 1 in
								Some m
								
		let tick counter time event =
			let _ = 
				if not counter.initialized then
					let c = ref !Parameter.progressBarSize in
						while !c > 0 do
							print_string "_" ;
							c:=!c-1
						done ;
						print_newline() ; 
						counter.initialized <- true ; 
			and last_event,last_time = counter.last_tick
			in
				let n_t = 
					match !Parameter.maxTimeValue with
						| None -> 0
						| Some tmax ->
							let n = int_of_float ((time -. last_time) *. (float_of_int !Parameter.progressBarSize) /. tmax) in
								n
				and n_e = 
					match !Parameter.maxEventValue with
						| None -> 0
						| Some emax -> 
							if emax = 0 then 0 
							else
								(event - last_event) * !Parameter.progressBarSize / emax
				in
					let n = ref (max n_t n_e) in
						if !n>0 then set_tick counter (event,time) ;
						while !n > 0 do
							Printf.printf "%c" !Parameter.progressBarSymbol ;
							inc_tick counter ;
							n:=!n-1 
						done ;
						flush stdout
														
		let create init_t init_e mx_t mx_e = 
			let dE = compute_dE() in
				let dT = match dE with None -> compute_dT() | Some _ -> None
				in
				{time = init_t ; 
				events = init_e ; 
				null_events = 0 ; 
				null_action = 0 ;
				max_time = mx_t ; 
				max_events = mx_e ;
				last_tick = (init_e,init_t);
				dE = dE ;
				dT = dT ;
				init_time = init_t ;
				init_event = init_e ;
				initialized = false ;
				ticks = 0
				}
		
	end