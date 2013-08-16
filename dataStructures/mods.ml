module StringMap = MapExt.Make (struct type t = string let compare = compare end)   
module IntMap = MapExt.Make (struct type t = int let compare = compare end)
module IntSet = Set_patched.Make (struct type t = int let compare = compare end)
module Int2Map = MapExt.Make (struct type t = int*int let compare = compare end)
module StringSet = Set.Make (struct type t = string let compare = compare end)
module Int2Set = Set.Make (struct type t = int*int let compare = compare end)
module Int3Set = Set.Make (struct type t = int*int*int let compare = compare end)

module DynArray = DynamicArray.DynArray(LargeArray.GenArray)

type dep_type = ALG of int | KAPPA of int | TOK of int | EVENT | TIME | RULE of int | PERT of int | ABORT of int
module DepMap = Map.Make (struct type t = dep_type let compare = compare end) 
module DepSet = Set.Make (struct type t = dep_type let compare = compare end) 


module Num =
	struct
	
	type t = I of int | F of float | I64 of Int64.t 
		
	let is_greater n1 n2 =
		match n1,n2 with
			| (F x, F y) -> x > y
			| (I x, I y) -> x > y
			| (F x, I y) -> x > (float_of_int y)
			| (I x, F y) -> (float_of_int x) > y
			| (I x, I64 y) -> (Int64.of_int x) > y
			| (I64 x, I64 y) -> x > y
			| (I64 x, I y) -> (Int64.of_int y) < x
			| (F x, I64 y) -> x > (Int64.to_float y)
			| (I64 x, F y) -> y < (Int64.to_float x)
			
			 
	let is_smaller n1 n2 = 
		match n1,n2 with
			| (F x, F y) -> x < y
			| (I x, I y) -> x < y
			| (F x, I y) -> x < (float_of_int y)
			| (I x, F y) -> (float_of_int x) < y
			| (I x, I64 y) -> (Int64.of_int x) < y
			| (I64 x, I64 y) -> x < y
			| (I64 x, I y) -> (Int64.of_int y) > x
			| (F x, I64 y) -> x < (Int64.to_float y)
			| (I64 x, F y) -> y > (Int64.to_float x)
	
	let is_equal n1 n2 = 
		match n1,n2 with
			| (F x, F y) -> x = y
			| (I x, I y) -> x = y
			| (I64 x, I64 y) -> x=y
			| (I64 x, I y) -> x = (Int64.of_int y)
			| (I x, I64 y) -> y = (Int64.of_int x)
			| _ -> false
	
	let mult n1 n2 = 
		match n1,n2 with
			| (F x, F y) -> F (x *. y)
			| (I x, I y) -> I (x * y)
			| (F x, I y) -> F (x *. (float_of_int y))
			| (I x, F y) -> F ((float_of_int x) *. y)
			| (I x, I64 y) -> I64 (Int64.mul (Int64.of_int x) y)
			| (I64 x, I64 y) -> I64 (Int64.mul x y)
			| (I64 x, I y) -> I64 (Int64.mul (Int64.of_int y) x)
			| (F x, I64 y) -> F (x *. (Int64.to_float y))
			| (I64 x, F y) -> F (y *. (Int64.to_float x))
	
	let min n1 n2 =
		match n1,n2 with
			| (F x, F y) -> F (min x y) 
			| (I x, I y) -> I (min x y)
			| (F x, I y) -> F (min x (float_of_int y))
			| (I x, F y) -> F (min (float_of_int x) y)
			| (I x, I64 y) -> I64 (min (Int64.of_int x) y)
			| (I64 x, I64 y) -> I64 (min x y)
			| (I64 x, I y) -> I64 (min (Int64.of_int y) x)
			| (F x, I64 y) -> F (min x (Int64.to_float y))
			| (I64 x, F y) -> F (min y (Int64.to_float x))
	
	
	let add n1 n2 = 
		match n1,n2 with
			| (F x, F y) -> F (x +. y)
			| (I x, I y) -> I (x + y)
			| (F x, I y) -> F (x +. (float_of_int y))
			| (I x, F y) -> F ((float_of_int x) +. y)
			| (I x, I64 y) -> I64 (Int64.add (Int64.of_int x) y)
			| (I64 x, I64 y) -> I64 (Int64.add x y)
			| (I64 x, I y) -> I64 (Int64.add (Int64.of_int y) x)
			| (F x, I64 y) -> F (x +. (Int64.to_float y))
			| (I64 x, F y) -> F (y +. (Int64.to_float x))
	
	let float_of_num n =
		match n with
			| F x -> x
			| I x -> float_of_int x
			| I64 x -> Int64.to_float x 
	
	let int_of_num n =
		match n with
			| F x -> (int_of_float x)
			| I x -> x
			| I64 x -> Int64.to_int x (*Might exceed thebiggest 32 bits integer*)

	let is_zero n = 
		match n with
		| F x -> x = 0.
		| I64 x -> x = Int64.zero
		| I x -> x = 0

	let to_string n =
		match n with
		| F x -> Printf.sprintf "%E" x
		| I64 x -> Printf.sprintf "%Ld" x
		| I x -> Printf.sprintf "%d" x

end


let string_of_dep = function
	| TOK i -> "TOK("^(string_of_int i)^")"
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
		
		exception Found of (int*int)
		let root_image phi = try (Hashtbl.iter (fun i j -> raise (Found (i,j))) phi.map ; None) with Found (i,j) -> Some (i,j)
		
		let set_address addr phi = phi.address <- Some addr 
			
		let get_address phi = match phi.address with Some a -> a | None -> raise Not_found
		let get_coordinate phi = phi.coordinate 
		
		let to_map phi = Hashtbl.fold (fun i j (map,cod) -> (IntMap.add i j map,IntSet.add j cod)) phi.map (IntMap.empty,IntSet.empty)
		let is_trashed phi = match phi.address with Some (-1) -> true | _ -> false
		
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
		let codomain phi (inj,cod) = 
			Hashtbl.fold 
			(fun i j (map,set) -> if IntSet.mem j set then raise Clashing else (IntMap.add i j map,IntSet.add j set)) 
			phi.map (inj,cod)
		
		let to_string phi = 
			Tools.string_of_map string_of_int string_of_int Hashtbl.fold phi.map 
		
		let string_of_coord phi = 
			let a = get_address phi and (m,c) = get_coordinate phi 
			in 
			Printf.sprintf "(%d,%d,%d)" m c a
		
		let copy phi = fold (fun i j phi' -> add i j phi') phi {map = Hashtbl.create (size phi) ; address = None ; coordinate = get_coordinate phi}
	end

module InjProduct =
	struct
		type t = {elements : Injection.t array ; mutable address : int option ; coordinate : int ; signature : int array}
		type key = int array
		
		let allocate phi addr = phi.address <- Some addr 
			
		let get_address phi = match phi.address with Some a -> a | None -> raise Not_found
		let get_coordinate phi = phi.coordinate 
		
		let add cc_id inj injprod = 
			try 
				injprod.elements.(cc_id) <- inj ;
				let root = (function Some (_,u) -> u | None -> invalid_arg "InjProduct.add") (Injection.root_image inj) in
				injprod.signature.(cc_id) <- root
			with Invalid_argument msg -> invalid_arg ("InjProduct.add: "^msg)
				
		let is_trashed injprod = match injprod.address with Some (-1) -> true | _ -> false
		
		exception False
		
		let is_complete injprod = 
			try 
				(Array.iteri (fun i inj_i -> let a,_ = Injection.get_coordinate inj_i in if a<0 then raise False else ()) injprod.elements ; true)
			with False -> false
			
		let size injprod = Array.length injprod.elements
		
		let find i injprod = try injprod.elements.(i) with Invalid_argument _ -> raise Not_found
		
		let create n mix_id = {elements = Array.create n (Injection.empty 0 (-1,-1)) ; address = None ; coordinate = mix_id ; signature = Array.create n (-1)}
		
		let equal phi psi =
			try 
				if (Array.length phi.signature) <> (Array.length psi.signature) then false
				else
					(
					Array.iteri (fun cc_id u -> if u <> psi.signature.(cc_id) then raise False ) phi.signature ;
					true
					) 
			with 
				| False -> false
		
		let get_key phi = phi.signature
		
		let compare phi psi = 
			try
				let a = get_address phi
				and a'= get_address psi
				and m = get_coordinate phi
				and m' = get_coordinate psi
				in
					compare (m,a) (m',a') 
			with Not_found -> invalid_arg "Injection.compare"
		
		let fold_left f cont phi = Array.fold_left f cont phi.elements
		
		let to_string phi = Tools.string_of_array Injection.to_string phi.elements 
			
	end
	
(*module Activity:(ValMap.ValMap with type content = float) = 
	ValMap.Make 
	(struct 
		type t = float (*mix_id -> rule_activity*) 
		let to_f = fun alpha -> alpha
	end)
*)

module InjectionHeap = 
	Heap.Make 
	(struct 
		type t = Injection.t 
		let allocate = fun inj i -> Injection.set_address i inj  
		let get_address inj = Injection.get_address inj 
	end)
	
module InjProdHeap = SafeHeap.Make(InjProduct) 

module InjProdSet = Set.Make(InjProduct)

module Counter = 
	struct
		type t = {
			mutable time:float ; 
			mutable events:int ; 
			mutable null_events:int ; 
			mutable cons_null_events:int;
			mutable perturbation_events:int ;
			mutable null_action:int ;
			mutable last_tick : (int * float) ;
			mutable initialized : bool ;
			mutable ticks : int ; 
			stat_null : int array ; 
			init_time : float ;
			init_event : int ;
			max_time : float option ; 
			max_events : int option ;
			dE : int option ;
			dT : float option ;
			mutable stop : bool
			}

		let stop c = c.stop
		let inc_tick c = c.ticks <- c.ticks + 1
		let time c = c.time
		let event c = c.events
		let null_event c = c.null_events
		let null_action c = c.null_action
		let is_initial c = c.time = c.init_time
		let inc_time c dt = c.time <- (c.time +. dt)
		let inc_events c =c.events <- (c.events + 1) 
		let inc_null_events c = c.null_events <- (c.null_events + 1) 
		let inc_consecutive_null_events c = (c.cons_null_events <- c.cons_null_events + 1)
		let inc_null_action c = c.null_action <- (c.null_action + 1)
		let	reset_consecutive_null_event c = c.cons_null_events <- 0 
		let check_time c = match c.max_time with None -> true | Some max -> c.time < max
		let check_output_time c ot = match c.max_time with None -> true | Some max -> ot < max
		let check_events c = match c.max_events with None -> true | Some max -> c.events < max
		let dT c = c.dT
		let dE c = c.dE
		let last_tick c = c.last_tick
		let set_tick c (i,x) = c.last_tick <- (i,x)
		
		let last_increment c = let _,t = c.last_tick in (c.time -. t) 
		
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
							  let nplus = (event * !Parameter.progressBarSize) / emax in 
                                                          let nminus = (last_event * !Parameter.progressBarSize) / emax in 
                                                          nplus-nminus 
				in
					let n = ref (max n_t n_e) in
						if !n>0 then set_tick counter (event,time) ;
						while !n > 0 do
							Printf.printf "%c" !Parameter.progressBarSymbol ;
							if !Parameter.eclipseMode then print_newline() ;
							inc_tick counter ;
							n:=!n-1 
						done ;
						flush stdout
	                                          
	 	let stat_null i c = try c.stat_null.(i) <- c.stat_null.(i) + 1 with exn -> invalid_arg "Invalid null event identifier"
              
		let create init_t init_e mx_t mx_e = 
			let dE = compute_dE() in
				let dT = match dE with None -> compute_dT() | Some _ -> None
				in
				{time = init_t ; 
				events = init_e ; 
				null_events = 0 ; 
				cons_null_events = 0;
				stat_null = Array.init 6 (fun i -> 0) ;
				perturbation_events = 0;
				null_action = 0 ;
				max_time = mx_t ; 
				max_events = mx_e ;
				last_tick = (init_e,init_t);
				dE = dE ;
				dT = dT ;
				init_time = init_t ;
				init_event = init_e ;
				initialized = false ;
				ticks = 0 ;
				stop = false
				}
		
	end
	
module Palette:
	sig
	  type t
	  type color = (float*float*float)
	  val find : int -> t -> color
	  val add : int -> color -> t -> t
	  val mem : int -> t -> bool
	  val empty : t
	  val new_color : unit -> color
	  val grey : int -> string
	  val string_of_color : color -> string
	end =
	struct
	  type color = (float*float*float)
	  type t = color IntMap.t
	  let find = IntMap.find
	  let add = IntMap.add
	  let mem = IntMap.mem
	  let empty = IntMap.empty
	  let new_color () = Random.float 1.0,Random.float 1.0,Random.float 1.0
	  let grey d = if d > 16 then "black" else ("gray"^(string_of_int (100-6*d)))
	  let string_of_color (r,g,b) = String.concat "," (List.rev_map string_of_float [b;g;r])
	end


	let tick_stories n_stories (init,last,counter) =
	  let _ = 
	    if not init then
	      let c = ref !Parameter.progressBarSize in
	      let _ = print_newline () in 
              while !c > 0 do
		print_string "_" ;
		c:=!c-1
	      done ;
	      print_newline() ; 
	  in
	  let nc = (counter * !Parameter.progressBarSize) / n_stories in 
          let nl = (last * !Parameter.progressBarSize) / n_stories in 
          let n = nc - nl in 
          let rec aux n = 
            if n<=0 then () 
            else 
              let _ = Printf.printf "%c" (!Parameter.progressBarSymbol) in 
              let _ = 
                if !Parameter.eclipseMode then print_newline() ;
			    in 
              aux (n-1)
          in 
          let _ = aux n in 
	  let _ =  flush stdout in  
          (true,counter,counter+1)
            
type 'a simulation_info = (* type of data to be given with obersables for story compression (such as date when the obs is triggered*)
    {
      story_id: int ; 
      story_time: float ;
      story_event: int ;
      profiling_info: 'a; 
    }

let update_profiling_info a info = 
  { 
    story_id = info.story_id ;
    story_time = info.story_time ;
    story_event = info.story_event ;
    profiling_info = a}

let dump_simulation_info log info = 
  Printf.fprintf log "Story: %i\nTime: %f\nEvent: %i\n" info.story_id info.story_time info.story_event 

let compare_profiling_info info1 info2 = 
  match info1,info2
  with 
    | None,None -> 0
    | None,Some _ -> -1
    | Some _,None -> +1
    | Some info1,Some info2 -> 
      compare info1.story_id info2.story_id 
