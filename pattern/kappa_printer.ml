let lnk_t env f = function
  | Mixture.WLD -> Printf.fprintf f "?"
  | Mixture.BND -> Printf.fprintf f "!"
  | Mixture.FREE -> Printf.fprintf f ""
  | Mixture.TYPE (site_id,sig_id) ->
     Printf.fprintf f "!%s.%s" (Environment.site_of_id sig_id site_id env)
	(Environment.name sig_id env)

let lnk_t_to_string env () = function
  | Mixture.WLD -> Printf.sprintf "?"
  | Mixture.BND -> Printf.sprintf "!"
  | Mixture.FREE -> Printf.sprintf ""
  | Mixture.TYPE (site_id,sig_id) ->
     Printf.sprintf "!%s.%s" (Environment.site_of_id sig_id site_id env)
	(Environment.name sig_id env)

let follower_string (bnd,fresh) mix uid = function
  | Mixture.BND ->
     let opt = Mixture.follow uid mix in
     begin
       match opt with
       | Some (agent_id',site_id') ->
	  begin
	    let lnk =
	      try Hashtbl.find bnd uid with
	      | Not_found ->
		 (Hashtbl.replace bnd (agent_id',site_id') !fresh ;
		  let i = !fresh in
		  fresh := !fresh+1 ;
		  i)
	    in
	    Hashtbl.replace bnd uid lnk ;
	    (string_of_int lnk)
	  end
       | None ->
	  try string_of_int (Hashtbl.find bnd uid)
	  with Not_found -> "_"
     end
  | _ -> ""

let intf_item env (bnd,fresh) mix sig_id agent_id
		    f (site_id,(opt_v,opt_l)) =
  let s_int f = match opt_v with
    | (Some x) ->
       Printf.fprintf f "~%s" (Environment.state_of_id sig_id site_id x env)
    | None -> Printf.fprintf f ""
  in
  let s_lnk f =
    Printf.fprintf f "%a%s" (lnk_t env) opt_l
       (follower_string (bnd,fresh) mix (agent_id,site_id) opt_l)
  in
  Printf.fprintf f "%s%t%t" (Environment.site_of_id sig_id site_id env)
     s_int s_lnk

let intf_item_to_string env (bnd,fresh) mix sig_id agent_id
			() (site_id,(opt_v,opt_l)) =
  let s_int f = match opt_v with
    | (Some x) ->
       Printf.sprintf "~%s" (Environment.state_of_id sig_id site_id x env)
    | None -> Printf.sprintf ""
  in
  let s_lnk f =
    Printf.sprintf "%a%s" (lnk_t_to_string env) opt_l
	(follower_string (bnd,fresh) mix (agent_id,site_id) opt_l)
  in
  Printf.sprintf "%s%t%t" (Environment.site_of_id sig_id site_id env)
     s_int s_lnk

let intf env mix sig_id agent_id (bnd,fresh) f interface =
  Pp.set Mods.IntMap.bindings (fun f -> Printf.fprintf f ",")
	 (intf_item env (bnd,fresh) mix sig_id agent_id)
	 f (Mods.IntMap.remove 0 interface)
(* Beware: removes "_" the hackish way *)

let intf_to_string env mix sig_id agent_id (bnd,fresh) () interface =
  Pp.set_to_string Mods.IntMap.bindings (fun () -> ",")
		   (intf_item_to_string env (bnd,fresh) mix sig_id agent_id)
		   () (Mods.IntMap.remove 0 interface)
(* Beware: removes "_" the hackish way *)

let agent with_number env mix (bnd,fresh) f (id,ag) =
  let sig_id = Mixture.name ag in
  let name = if with_number
	     then (Environment.name sig_id env)^"#"^(string_of_int id)
	     else Environment.name sig_id env
  in
  Printf.fprintf f "%s(%a)" name (intf env mix sig_id id (bnd,fresh))
		 (Mixture.interface ag)

let agent_to_string with_number env mix (bnd,fresh) () (id,ag) =
  let sig_id = Mixture.name ag in
  let name = if with_number
	     then (Environment.name sig_id env)^"#"^(string_of_int id)
	     else Environment.name sig_id env
  in
  Printf.sprintf "%s(%a)" name (intf_to_string env mix sig_id id (bnd,fresh))
		 (Mixture.interface ag)

let mixture with_number env f mix =
  let bnd = Hashtbl.create 7 in
  let fresh = ref 0 in
  Pp.set Mods.IntMap.bindings (fun f -> Printf.fprintf f ",")
	 (agent with_number env mix (bnd,fresh))
	 f (Mixture.agents mix)

let mixture_to_string with_number env () mix =
  let bnd = Hashtbl.create 7 in
  let fresh = ref 0 in
  Pp.set_to_string Mods.IntMap.bindings (fun () -> ",")
		   (agent_to_string with_number env mix (bnd,fresh))
		   () (Mixture.agents mix)
