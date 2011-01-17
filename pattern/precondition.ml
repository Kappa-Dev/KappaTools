open Mods
open Tools
open ExceptionDefn

type t = (int*int*view,Int2Set.t) Hashtbl.t
and view = FREE | BND1 of int * int | BND0 | INT of int  
	
let empty () = Hashtbl.create !Parameter.defaultExtArraySize

let add m (hsh:t) =
	IntMap.iter
	(fun id ag ->
		let ag_nme = Mixture.name ag in
			Mixture.fold_interface 
			(fun site_id (int_opt,lnk_opt) () ->
				let (_:unit) = 
					match int_opt with
						| None -> ()
						| Some j ->
							let set = try Hashtbl.find hsh (ag_nme,site_id,INT j) with Not_found -> Int2Set.empty in 
								Hashtbl.replace hsh (ag_nme,site_id,INT j) (Int2Set.add (Mixture.get_id m,Mixture.component_of_id id m) set)
				in
					match lnk_opt with
						| Node.WLD -> ()
						| Node.FREE -> 
							let set = try Hashtbl.find hsh (ag_nme,site_id,FREE) with Not_found -> Int2Set.empty in 
								Hashtbl.replace hsh (ag_nme,site_id,FREE) (Int2Set.add (Mixture.get_id m,Mixture.component_of_id id m) set)
						| Node.TYPE (site_id',ag_nme') -> 
							let set = try Hashtbl.find hsh (ag_nme,site_id,BND1 (ag_nme',site_id')) with Not_found -> Int2Set.empty in 
								Hashtbl.replace 
								hsh 
								(ag_nme,site_id,BND1 (ag_nme',site_id')) 
								(Int2Set.add (Mixture.get_id m,Mixture.component_of_id id m) set)
						| Node.BND ->
							match Mixture.follow (id,site_id) m with
								| None ->
									let set = try Hashtbl.find hsh (ag_nme,site_id,BND0) with Not_found -> Int2Set.empty in  
										Hashtbl.replace hsh (ag_nme,site_id,BND0) (Int2Set.add (Mixture.get_id m,Mixture.component_of_id id m) set) 
								| Some (id',site_id') -> (*complete-link*)
									let ag_nme' = Mixture.name (Mixture.agent_of_id id' m) in
										let set = try Hashtbl.find hsh (ag_nme,site_id,BND1 (ag_nme',site_id')) with Not_found -> Int2Set.empty in 
											Hashtbl.replace hsh (ag_nme,site_id,BND1 (ag_nme',site_id')) 
											(Int2Set.add (Mixture.get_id m,Mixture.component_of_id id m) set)
			) ag ()
	)
	(Mixture.agents m) ;
	hsh

let find_all nme i int_opt lnk_opt is_free (hsh:t) = 
	let set_int =
		match int_opt with
			| None -> Int2Set.empty
			| Some u -> try Hashtbl.find hsh (nme,i,INT u) with Not_found -> Int2Set.empty
	in
		match lnk_opt with
			| None -> 
				if not is_free then set_int
				else 
					(try Int2Set.union (Hashtbl.find hsh (nme,i,FREE)) set_int with Not_found -> set_int)
			| Some (nme',i') ->
				let set_lnk = 
					try Int2Set.union (Hashtbl.find hsh (nme,i,BND0)) set_int with Not_found -> set_int
				in
					try Int2Set.union (Hashtbl.find hsh (nme,i,BND1 (nme',i'))) set_lnk with Not_found -> set_lnk
							
(*
let to_string hsh =
	let f = Misc.string_of_set (fun (x,y) -> "("^(string_of_int x)^","^(string_of_int y)^")") Int2Set.fold in 
	let l =  
		Hashtbl.fold 
		(fun k set2 cont ->
			let str = 
				match k with
					| (nme,i,INT j) -> Printf.sprintf "(%s,%d,INT %d)->%s" nme i j (f set2)
					| (nme,i,FREE) -> Printf.sprintf "(%s,%d,FREE)->%s" nme i (f set2)
					| (nme,i,BND0) -> Printf.sprintf "(%s,%d,BND0)->%s" nme i (f set2)
					| (nme,i,BND1 (nme',i')) -> Printf.sprintf "(%d,%d,BND1(%d,%d))->%s" nme i nme' i' (f set2)
			in
				str::cont
		) hsh []
	in
		String.concat "\n" l	
*)

