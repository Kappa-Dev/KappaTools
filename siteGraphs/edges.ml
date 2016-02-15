open Mods

type agent = int * int
(** agent_id * agent_type *)

module Edge = struct
  type t = agent * int
  (** agent * site *)

  let _compare ((n,_),s) ((n',_),s') =
    let c = int_compare n n' in
    if c <> 0 then c else int_compare s s'

  (* let dummy_link = ((-1,-1),-1) *)
end

module Cache = struct
    type t = int DynArray.t
    let int_l = 30

    let create () = DynArray.make 1 0

    let mark t i =
      DynArray.set t (i / int_l)
		   ((DynArray.get t (i / int_l)) lor (1 lsl (i mod int_l)))
    let test t i = (DynArray.get t (i / int_l)) land (1 lsl (i mod int_l)) <> 0

    let reset t = DynArray.fill t 0 (DynArray.length t) 0
  end

type t =
  {
    mutable outdated : bool;
    connect : Edge.t option array DynArray.t;
    missings : Int2Set.t;
    state : int option array DynArray.t;
    sort : int option DynArray.t;
    cache : Cache.t;
    free_id : int * int list;
  }
(** (agent,site -> binding_state; missings);
    agent,site -> internal_state; agent -> sort; free_id
    the free sites are neither in missings nor in linking_destination *)

let empty () =
  {
    outdated = false;
    connect = DynArray.make 1 [||];
    missings = Int2Set.empty;
    state = DynArray.make 1 [||];
    sort = DynArray.make 1 None;
    cache = Cache.create ();
    free_id =(0,[]);
  }

let add_agent sigs ty graph =
  let ar = Signature.arity sigs ty in
  let al = Array.make ar None in
  let ai = Array.make ar None in
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  match graph.free_id with
  | new_id,h :: t ->
     let missings' = Tools.recti (fun s a -> Int2Set.add (h,s) a)
				 graph.missings ar in
     let () = DynArray.set graph.connect h al in
     let () = DynArray.set graph.state h ai in
     let () = DynArray.set graph.sort h (Some ty) in
     h,
     {
       outdated = false;
       connect = graph.connect;
       missings = missings';
       state = graph.state;
       sort = graph.sort;
       cache = graph.cache;
       free_id = (new_id,t);
     }
  | new_id,[] ->
     let missings' = Tools.recti (fun s a -> Int2Set.add (new_id,s) a)
				 graph.missings ar in
     let () = DynArray.set graph.connect new_id al in
     let () = DynArray.set graph.state new_id ai in
     let () = DynArray.set graph.sort new_id (Some ty) in
     new_id,
     {
       outdated = false;
       connect = graph.connect;
       missings = missings';
       state = graph.state;
       sort = graph.sort;
       cache = graph.cache;
       free_id = (succ new_id,[])
     }

let add_free ag s graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (DynArray.get graph.connect ag).(s) <- None in
  {
    outdated = false;
    connect = graph.connect;
    missings = Int2Set.remove (ag,s) graph.missings;
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = graph.free_id;
  }
let add_internal ag s i graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (DynArray.get graph.state ag).(s) <- Some i in
  {
    outdated = false;
    connect = graph.connect;
    missings = graph.missings;
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = graph.free_id;
  }

let add_link (ag,ty) s (ag',ty') s' graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (DynArray.get graph.connect ag).(s) <- Some ((ag',ty'),s') in
  let () = (DynArray.get graph.connect ag').(s') <- Some ((ag,ty),s) in
  {
    outdated = false;
    connect = graph.connect;
    missings = Int2Set.remove (ag,s) (Int2Set.remove (ag',s') graph.missings);
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = graph.free_id;
  }

let remove_agent ag graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = DynArray.set graph.connect ag [||] in
  let () = DynArray.set graph.state ag [||] in
  let () = DynArray.set graph.sort ag None in
  {
    outdated = false;
    connect = graph.connect;
    missings = Int2Set.filter (fun (ag',_) -> ag <> ag') graph.missings;
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = let new_id,ids = graph.free_id in (new_id,ag::ids);
  }
let remove_free ag s graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = assert ((DynArray.get graph.connect ag).(s) = None) in
  {
    outdated = false;
    connect = graph.connect;
    missings = Int2Set.add (ag,s) graph.missings;
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = graph.free_id
  }
let get_internal ag s graph =
  let () = assert (not graph.outdated) in
  match (DynArray.get graph.state ag).(s) with
  | Some i -> i
  | None ->
     failwith ("Site "^string_of_int s^ " of agent "^string_of_int ag^
		 " has no internal state to remove in the current graph.")

let remove_internal ag s graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (DynArray.get graph.state ag).(s) <- None in
  {
    outdated = false;
    connect = graph.connect;
    missings = graph.missings;
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = graph.free_id
  }

let remove_link ag s ag' s' graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (DynArray.get graph.connect ag).(s) <- None in
  let () = (DynArray.get graph.connect ag').(s') <- None in
  {
    outdated = false;
    connect = graph.connect;
    missings = Int2Set.add (ag,s) (Int2Set.add (ag',s') graph.missings);
    state = graph.state;
    sort = graph.sort;
    cache = graph.cache;
    free_id = graph.free_id;
  }

let is_agent (ag,ty) graph =
  let () = assert (not graph.outdated) in
  match DynArray.get graph.sort ag with
  | Some ty' -> let () = assert (ty = ty') in true
  | None -> false
let is_free ag s graph =
  let () = assert (not graph.outdated) in
  (DynArray.get graph.connect ag).(s) = None
  && not @@ Int2Set.mem (ag,s) graph.missings
let is_internal i ag s graph =
  let () = assert (not graph.outdated) in
  match (DynArray.get graph.state ag).(s) with
  | Some j -> j = i
  | None -> false
let link_exists ag s ag' s' graph =
  let () = assert (not graph.outdated) in
  match (DynArray.get graph.connect ag).(s) with
  | Some ((ag'',_),s'') -> ag'=ag'' && s'=s''
  | None -> false

let exists_fresh ag s ty s' graph =
  let () = assert (not graph.outdated) in
  match (DynArray.get graph.connect ag).(s) with
  | Some ((ag',ty'),s'') ->
    if ty'=ty && s'=s'' then Some ag' else None
  | None -> None

let link_destination ag s graph =
  let () = assert (not graph.outdated) in
  (DynArray.get graph.connect ag).(s)

(** The snapshot machinery *)
let one_connected_component sigs ty node graph =
  let rec build acc free_id dangling =
    function
    | [] -> acc,free_id
    | (ty,node) :: todos ->
       if Cache.test graph.cache node
       then build acc free_id dangling todos
       else match DynArray.get graph.sort node with
       | None -> failwith "Edges.one_connected_component"
       | Some _ ->
	 let () = Cache.mark graph.cache node in
	 let arity = Signature.arity sigs ty in
	 let ports = Array.make arity Raw_mixture.FREE in
	 let (free_id',dangling',todos'),ports =
	   Tools.array_fold_left_mapi
	     (fun i (free_id,dangling,todos) _ ->
	      match (DynArray.get graph.connect node).(i) with
	      | None ->
		 (free_id,dangling,todos),Raw_mixture.FREE
	      | Some ((n',ty'),s') ->
		 match Int2Map.pop (n',s') dangling with
		 | None, dangling ->
		    (succ free_id,
		     Int2Map.add (node,i) free_id dangling,
		     if n' = node || List.mem (ty',n') todos
		     then todos
		     else (ty',n')::todos),
		    Raw_mixture.VAL free_id
		 | Some id, dangling' ->
		    (free_id,dangling',todos), Raw_mixture.VAL id)
	     (free_id,dangling,todos) ports in
	 let skel =
	   { Raw_mixture.a_id = node; Raw_mixture.a_type = ty;
	     Raw_mixture.a_ports = ports;
	     Raw_mixture.a_ints = DynArray.get graph.state node; } in
	 build (skel::acc) free_id' dangling' todos'
  in build [] 1 Int2Map.empty [ty,node]

let build_snapshot sigs graph =
  let () = assert (not graph.outdated) in
  let rec increment x = function
    | [] -> [1,x]
    | (n,y as h)::t ->
       if Raw_mixture.equal x y then (succ n,y)::t
       else h::increment x t in
  let rec aux ccs node =
    if node = DynArray.length graph.sort
    then let () = Cache.reset graph.cache in ccs
    else
      if Cache.test graph.cache node
      then aux ccs (succ node)
      else match DynArray.get graph.sort node with
	| None -> aux ccs (succ node)
	| Some ty ->
	   let (out,_) =
	     one_connected_component sigs ty node graph in
	aux (increment out ccs) (succ node) in
  aux [] 0

let debug_print f graph =
  let print_sites ag =
    (Pp.array Pp.comma
	      (fun s f l ->
	       Format.fprintf
		 f "%i%t%t" s
		 (match (DynArray.get graph.state ag).(s) with
		  | Some int -> fun f -> Format.fprintf f "~%i" int
		  | None -> fun _ -> ())
		 (fun f -> match l with
			   | None ->
			      if Int2Set.mem (ag,s) graph.missings
			      then Format.pp_print_string f "?"
			   | Some ((ag',ty'),s') ->
			      Format.fprintf f "->%i:%i.%i" ag' ty' s'))) in
  DynArray.print
    Pp.empty
    (fun ag f a ->
      match DynArray.get graph.sort ag with
      | Some ty ->
	Format.fprintf
	  f "%i:%i(@[%a@])@ " ag ty (print_sites ag) a
      | None -> if a = [||] then ()
	else Format.fprintf
	  f "%i:NOTYPE(@[%a@])@ " ag (print_sites ag) a
    )
    f graph.connect

type path = ((agent * int) * (agent * int)) list
(** ((agent_id, agent_name),site_name) *)

let rec print_path ?sigs ?graph f = function
  | [] -> Pp.empty_set f
  | [((p,_),s),((p',_),s')] -> Format.fprintf f "%i.%i@,-%i.%i" p s s' p'
  | (((p,_),s),((p',_),s'))::((((p'',_),_),_)::_ as l) ->
     Format.fprintf f "%i.%i@,-%i.%t%a" p s s'
		    (fun f -> if p' <> p'' then Format.fprintf f "%i##" p')
		    (print_path ?sigs ?graph) l
let empty_path = []
let rev_path l = List.rev_map (fun (x,y) -> (y,x)) l
let is_valid_path graph l =
  List.for_all (fun (((a,_),s),((a',_),s')) -> link_exists a s a' s' graph) l

(* depth = number of edges between root and node *)
let breadth_first_traversal dist stop_on_find is_interesting sigs links cache =
  let rec look_each_site (id,ty,path as x) site (stop,out,next as acc) =
    if site = 0 then acc else
    match (DynArray.get links id).(pred site) with
    | None -> look_each_site x (pred site) acc
    | Some ((id',ty'),site') ->
       if (stop&&stop_on_find) then let () = Cache.reset cache in acc
       else if Cache.test cache id' then look_each_site x (pred site) acc
       else
	 let () = Cache.mark cache id' in
	 let path' = (((id',ty'),site'),((id,ty),pred site))::path in
	 let out',store =
	   match is_interesting id' with
	   | Some x -> ((x,id'),path')::out,true
	   | None -> out,false in
	 let next' = (id',ty',path')::next in
	 look_each_site x (pred site) (stop||store,out',next') in
  let rec aux depth out next = function
    | (_,ty,_ as x)::todos ->
       let stop,out',next' =
	 look_each_site x (Signature.arity sigs ty) (false,out,next) in
       if stop&&stop_on_find then out' else aux depth out' next' todos
    | [] -> match next with
	    | [] -> let () = Cache.reset cache in out
	    (* end when all graph traversed and return the list of paths *)
	    | _ -> match dist with
		   | Some d when d <= depth -> let () = Cache.reset cache in []
		   (* stop when the max distance is reached *)
		   | Some _ -> aux (depth+1) out [] next
		   | None -> aux depth out [] next
  in aux 1

let paths_of_interest
      is_interesting sigs graph start_ty start_point done_path =
  let () = Cache.mark graph.cache start_point in
  let () = List.iter (fun (_,((x,_),_)) -> Cache.mark graph.cache x)
		     done_path in
  let () = assert (not graph.outdated) in
  let acc = match is_interesting start_point with
    | None -> []
    | Some x -> [(x,start_point),done_path] in
  breadth_first_traversal None false is_interesting sigs graph.connect
			  graph.cache acc [] [start_point,start_ty,done_path]

(* nodes_x: agent_id list = int * int list
   nodes_y: adent_id list = int list *)
let are_connected
      ?candidate sigs graph ty_x x y nodes_x nodes_y dist store_dist =
  let () = assert (not graph.outdated) in
  (* look for the closest node in nodes_y *)
  let rec is_in_nodes_y nodes_y z = match nodes_y with
    | [] -> None
    | y::nds -> if z = y then Some () else is_in_nodes_y nds z in
  let rec prepare_node x ty site acc =
    if site = 0 then acc else
      match (DynArray.get graph.connect x).(pred site) with
      | None -> prepare_node x ty (pred site) acc
      | Some ((id',ty'),_) ->
	 if (List.mem (id',ty') nodes_x) then prepare_node x ty (pred site) acc
	 else prepare_node x ty (pred site) ((x,ty,[])::acc) in
  (* breadth first search is called on a list of sites;
     start the breadth first search with the boundaries of nodes_x,
     that is all sites that are connected to other nodes in x
     and with all nodes in nodes_x marked as done *)
  match dist with
  | None when (store_dist = false) ->
     (match candidate with
      | Some p when is_valid_path graph p -> Some p
      | (Some _ | None) ->
	 let () = Cache.mark graph.cache x in
	 (match
	     breadth_first_traversal dist true
				     (fun z -> if z = y then Some () else None)
				     sigs graph.connect graph.cache
				     [] [] [x,ty_x,[]] with
	   | [] -> None
	   | [ _,p ] -> Some p
	   | _ :: _ -> failwith "Edges.are_they_connected completely broken"))
  | _ ->
     let () =
       List.iter (fun (x,_) -> Cache.mark graph.cache x) nodes_x in
     let prepare =
       List.fold_left
	 (fun acc (x,ty) -> prepare_node x ty (Signature.arity sigs ty) acc)
	 [] nodes_x in
     match breadth_first_traversal dist true (is_in_nodes_y nodes_y) sigs
				   graph.connect graph.cache [] [] prepare
     with [] -> None
	| [ _,p ] -> Some p
	| _ :: _ -> failwith "Edges.are_they_connected completely broken"
