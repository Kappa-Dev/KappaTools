open Mods
open ExceptionDefn

type t = {control:int ; assoc : (string*(string array * int StringMap.t) option) array ; num_of_site: int StringMap.t}

let control sign = sign.control

let fold f sign cont = 
	let cont,_ = Array.fold_left (fun (cont,i) _ -> (f i cont,i+1)) (cont,0) sign.assoc
	in 
	cont

let num_of_site site_name sign = 
	StringMap.find site_name sign.num_of_site 
	
let site_of_num addr sign = 
	try
		let site_name,_ = sign.assoc.(addr) in site_name
	with
		| Invalid_argument _ -> raise Not_found

let add_site site_name sign =
	let size = Array.length sign.assoc in
	let assoc = Array.init (size+1) (fun i -> if i<size then sign.assoc.(i) else (site_name,None))
	in
		{sign with assoc = assoc ; num_of_site = StringMap.add site_name size sign.num_of_site}
	
let add_internal_state st site_id sign =
	let (nm,opt) = try sign.assoc.(site_id) with exn -> invalid_arg ("Signature.add_internal_state: "^(Printexc.to_string exn))
	in
	let opt',n = 
		match opt with
			| None -> (Some ([|st|],StringMap.add st 0 StringMap.empty),0)
			| Some (ar,map) -> 
				let n = Array.length ar in 
				let ar = Array.init (n + 1) (fun i -> if i<n then ar.(i) else st)
				in
				(Some (ar,StringMap.add st n map),n)
	in
	sign.assoc.(site_id) <- (nm,opt') ;
	(sign,n) 

let num_of_internal_state site_name state sign =
	try
		let _,values_opt = sign.assoc.(num_of_site site_name sign) in
			match values_opt with
				| None -> raise Not_found
				| Some (_,map) -> StringMap.find state map
	with
		| Invalid_argument _ -> raise Not_found 

let internal_state_of_num site_num val_num sign = 
	try
		let _,values_opt = sign.assoc.(site_num) in
			match values_opt with
				| None -> raise Not_found
				| Some (ar,_) -> ar.(val_num)
	with
		| Invalid_argument _ -> raise Not_found

let internal_states_number site_num sign = 
	try
		let _,values_opt = sign.assoc.(site_num) in
			match values_opt with
				| None -> 0
				| Some (ar,_) -> Array.length ar
	with
		| Invalid_argument _ -> raise Not_found


let arity sign = Array.length sign.assoc

let default_num_value num_site sign = 
	try
		let _,values_opt = sign.assoc.(num_site) in
			match values_opt with
				| None -> None
				| Some _ -> Some 0
	with
		| Invalid_argument _ -> invalid_arg "Signature.default_num_value: invalid site identifier"

let create ag_name intf_map =
	let assoc = Array.make (StringMap.size intf_map) ("",None) in
		let _,num_of_site = 
			StringMap.fold
			(fun site_name (int_state_list,_,pos) (cpt,num_of_site_name) -> 
				let ar_opt = 
					match int_state_list with
						| [] -> None
						| l -> 
							let ar_site = Array.make (List.length l) "" in 
								let _,map_site = 
									List.fold_left 
									(fun (i,num_of_state) v -> 
										ar_site.(i)<-v ; (i+1,StringMap.add v i num_of_state)
									) (0,StringMap.empty) l 
								in
									Some (ar_site,map_site)
				in 
					let cpt,cpt' = if site_name = "_" then (0,cpt) else (cpt,cpt+1) in (*making sure that "_" gets assigned to 0*)
					assoc.(cpt) <- (site_name,ar_opt) ;
					(cpt',StringMap.add site_name cpt num_of_site_name)
			)
			intf_map (1,StringMap.empty)
		in
			 {control = ag_name ; assoc = assoc ; num_of_site = num_of_site}

let to_string sign = 
	let str_of_assoc assoc = 
		let cont = ref [] in
			Array.iteri (fun i (name,_) -> 
				if name = "_" then () 
				else
					let int = 
						match default_num_value i sign with
							| None -> ""
							| Some n ->  "~"^(internal_state_of_num i n sign)
					in
						cont:=(name^int)::!cont
			) assoc ;
			String.concat "," (List.rev !cont)
	in
		Printf.sprintf "(%s)"	(str_of_assoc sign.assoc)