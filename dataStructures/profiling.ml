type t = (string,float) Hashtbl.t

let create n = Hashtbl.create n

let start_chrono () = Sys.time()
let stop_chrono c = Sys.time() -. c

let add_chrono fun_name p c = 
	let t = stop_chrono c in
	let t' = try Hashtbl.find p fun_name with Not_found -> 0.0 in
	Hashtbl.replace p fun_name (t+.t')

let dump p =
	let t_tot = Hashtbl.find p "Tot" in
	let control = ref 0. in
	Hashtbl.iter
	(fun fun_name t -> 
		if (fun_name = "Tot") then () 
		else 
			begin
				control := !control +. t ;
				let prct = 100. *. (t/.t_tot)
				in
				let arr = int_of_float prct in
				let p = if prct -. (float_of_int arr) > 0.5 then arr+1 else arr in
					Printf.printf "%s: %.2fs\t%d%%\n" fun_name t p 
			end
	)
	p ;
	let prct = 100. *. ((t_tot -. !control)/.t_tot)
	in
	let arr = int_of_float prct in
	let i = if prct -. (float_of_int arr) > 0.5 then arr+1 else arr in
	Printf.printf "\nProf: %.2fs\t%d%%\n" (t_tot -. !control) i 
