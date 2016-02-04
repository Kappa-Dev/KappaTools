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

type t =
  {
    mutable outdated : bool;
    connect : Edge.t option array LargeArray.t;
    missings : Int2Set.t;
    state : int option array LargeArray.t;
    sort : IntSet.t IntMap.t;
    free_id : int * int list;
  }
(** (agent,site -> binding_state; missings);
    agent,site -> internal_state; sort -> agents; free_id
    the free sites are neither in missings nor in linking_destination *)

let empty () =
  {
    outdated = false;
    connect = LargeArray.make 1 [||];
    missings = Int2Set.empty;
    state = LargeArray.make 1 [||];
    sort = IntMap.empty;
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
     let () = LargeArray.set graph.connect h al in
     let () = LargeArray.set graph.state h ai in
     h,
     {
       outdated = false;
       connect = graph.connect;
       missings = missings';
       state = graph.state;
       sort =
	 IntMap.add
	   ty (IntSet.add
		 h (IntMap.find_default IntSet.empty ty graph.sort)) graph.sort;
       free_id = (new_id,t);
     }
  | new_id,[] ->
     let missings' = Tools.recti (fun s a -> Int2Set.add (new_id,s) a)
				 graph.missings ar in
     let connect',state' =
       if LargeArray.length graph.connect = new_id
       then LargeArray.append graph.connect (LargeArray.make new_id [||]),
	    LargeArray.append graph.state (LargeArray.make new_id [||])
       else graph.connect,graph.state in
     let () = LargeArray.set connect' new_id al in
     let () = LargeArray.set state' new_id ai in
     new_id,
     {
       outdated = false;
       connect = connect';
       missings = missings';
       state = state';
       sort =
	 IntMap.add
	   ty (IntSet.add
		 new_id (IntMap.find_default IntSet.empty ty graph.sort))
	   graph.sort;
       free_id = (succ new_id,[])
     }

let add_free ag s graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (LargeArray.get graph.connect ag).(s) <- None in
  {
    outdated = false;
    connect = graph.connect;
    missings = Int2Set.remove (ag,s) graph.missings;
    state = graph.state;
    sort = graph.sort;
    free_id = graph.free_id;
  }
let add_internal ag s i graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (LargeArray.get graph.state ag).(s) <- Some i in
  {
    outdated = false;
    connect = graph.connect;
    missings = graph.missings;
    state = graph.state;
    sort = graph.sort;
    free_id = graph.free_id;
  }

let add_link (ag,ty) s (ag',ty') s' graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (LargeArray.get graph.connect ag).(s) <- Some ((ag',ty'),s') in
  let () = (LargeArray.get graph.connect ag').(s') <- Some ((ag,ty),s) in
  {
    outdated = false;
    connect = graph.connect;
    missings = Int2Set.remove (ag,s) (Int2Set.remove (ag',s') graph.missings);
    state = graph.state;
    sort = graph.sort;
    free_id = graph.free_id;
  }

let remove_agent (ag,ty) graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = LargeArray.set graph.connect ag [||] in
  let () = LargeArray.set graph.state ag [||] in
  {
    outdated = false;
    connect = graph.connect;
    missings = Int2Set.filter (fun (ag',_) -> ag <> ag') graph.missings;
    state = graph.state;
    sort =
      IntMap.add
	ty (IntSet.remove
	      ag (IntMap.find_default IntSet.empty ty graph.sort)) graph.sort;
    free_id = let new_id,ids = graph.free_id in (new_id,ag::ids);
  }
let remove_free ag s graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = assert ((LargeArray.get graph.connect ag).(s) = None) in
  {
    outdated = false;
    connect = graph.connect;
    missings = Int2Set.add (ag,s) graph.missings;
    state = graph.state;
    sort = graph.sort;
    free_id = graph.free_id
  }
let get_internal ag s graph =
  let () = assert (not graph.outdated) in
  match (LargeArray.get graph.state ag).(s) with
  | Some i -> i
  | None ->
     failwith ("Site "^string_of_int s^ " of agent "^string_of_int ag^
		 " has no internal state to remove in the current graph.")

let remove_internal ag s graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (LargeArray.get graph.state ag).(s) <- None in
  {
    outdated = false;
    connect = graph.connect;
    missings = graph.missings;
    state = graph.state;
    sort = graph.sort;
    free_id = graph.free_id
  }

let remove_link ag s ag' s' graph =
  let () = assert (not graph.outdated) in
  let () = graph.outdated <- true in
  let () = (LargeArray.get graph.connect ag).(s) <- None in
  let () = (LargeArray.get graph.connect ag').(s') <- None in
  {
    outdated = false;
    connect = graph.connect;
    missings = Int2Set.add (ag,s) (Int2Set.add (ag',s') graph.missings);
    state = graph.state;
    sort = graph.sort;
    free_id = graph.free_id;
  }

let is_agent (ag,ty) graph =
  let () = assert (not graph.outdated) in
  IntSet.mem ag (IntMap.find_default IntSet.empty ty graph.sort)
let is_free ag s graph =
  let () = assert (not graph.outdated) in
  (LargeArray.get graph.connect ag).(s) = None
  && not @@ Int2Set.mem (ag,s) graph.missings
let is_internal i ag s graph =
  let () = assert (not graph.outdated) in
  match (LargeArray.get graph.state ag).(s) with
  | Some j -> j = i
  | None -> false
let link_exists ag s ag' s' graph =
  let () = assert (not graph.outdated) in
  match (LargeArray.get graph.connect ag).(s) with
  | Some ((ag'',_),s'') -> ag'=ag'' && s'=s''
  | None -> false

let exists_fresh ag s ty s' graph =
  let () = assert (not graph.outdated) in
  match (LargeArray.get graph.connect ag).(s) with
  | Some ((ag',ty'),s'') ->
    if ty'=ty && s'=s'' then Some ag' else None
  | None -> None

let link_destination ag s graph =
  let () = assert (not graph.outdated) in
  (LargeArray.get graph.connect ag).(s)

(** The snapshot machinery *)
let one_connected_component sigs ty node rsorts graph =
  let rec build acc free_id dangling sorts =
    function
    | [] -> acc,free_id,sorts
    | (ty,node) :: todos ->
       let nodes_of_type = IntMap.find_default IntSet.empty ty sorts in
       let nodes_of_type' = IntSet.remove node nodes_of_type in
       if nodes_of_type == nodes_of_type' then
	 build acc free_id dangling sorts todos
       else
	 let sorts' = IntMap.add ty nodes_of_type' sorts in
	 let arity = Signature.arity sigs ty in
	 let ports = Array.make arity Raw_mixture.FREE in
	 let (free_id',dangling',todos'),ports =
	   Tools.array_fold_left_mapi
	     (fun i (free_id,dangling,todos) _ ->
	      match (LargeArray.get graph.connect node).(i) with
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
	     Raw_mixture.a_ints = LargeArray.get graph.state node; } in
	 build (skel::acc) free_id' dangling' sorts' todos'
  in build [] 1 Int2Map.empty rsorts [ty,node]

let build_snapshot sigs graph =
  let () = assert (not graph.outdated) in
  let rec increment x = function
    | [] -> [1,x]
    | (n,y as h)::t ->
       if Raw_mixture.equal x y then (succ n,y)::t
       else h::increment x t in
  let rec aux ccs sorts =
    match IntMap.root sorts with
    | None -> ccs
    | Some (ty,nodes) ->
       match IntSet.choose nodes with
       | None -> aux ccs (IntMap.remove ty sorts)
       | Some node ->
	  let (out,_,sorts') =
	    one_connected_component sigs ty node sorts graph in
	  aux (increment out ccs) sorts' in
  aux [] graph.sort

let print sigs f graph =
  Pp.list Pp.space (fun f (i,mix) ->
		    Format.fprintf f "%%init: %i @[<h>%a@]" i
				   (Raw_mixture.print ~compact:false sigs) mix)
	  f (build_snapshot sigs graph)

let print_dot sigs f graph =
  Pp.listi
    Pp.cut
    (fun i f (nb,mix) ->
     Format.fprintf f "@[<v 2>subgraph cluster%d{@," i;
     Format.fprintf
       f "counter%d [label = \"%d instance(s)\", shape=none];@,%a}@]"
       i nb (Raw_mixture.print_dot sigs i) mix)
    f (build_snapshot sigs graph)

let debug_print f graph =
  let print_sites ag =
    (Pp.array Pp.comma
	      (fun s f l ->
	       Format.fprintf
		 f "%i%t%t" s
		 (match (LargeArray.get graph.state ag).(s) with
		  | Some int -> fun f -> Format.fprintf f "~%i" int
		  | None -> fun _ -> ())
		 (fun f -> match l with
			   | None ->
			      if Int2Set.mem (ag,s) graph.missings
			      then Format.pp_print_string f "?"
			   | Some ((ag',ty'),s') ->
			      Format.fprintf f "->%i:%i.%i" ag' ty' s'))) in
  LargeArray.print
    Pp.empty
    (fun ag f a ->
     match IntMap.fold
	     (fun ty nodes -> function
			   | None ->
			      if IntSet.mem ag nodes then Some ty else None
			   | Some _ as x ->
			      if IntSet.mem ag nodes then Some (-42) else x)
	     graph.sort None with
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
let breadth_first_traversal dist stop_on_find is_interesting sigs links =
  let rec look_each_site (id,ty,path as x) site (stop,don,out,next as acc) =
    if site = 0 then acc else
    match (LargeArray.get links id).(pred site) with
    | None -> look_each_site x (pred site) acc
    | Some ((id',ty'),site') ->
       if (stop&&stop_on_find) then acc
       else if IntSet.mem id' don then look_each_site x (pred site) acc
       else
	 let don' = IntSet.add id' don in
	 let path' = (((id',ty'),site'),((id,ty),pred site))::path in
	 let out',store =
	   match is_interesting id' with
	   | Some x -> ((x,id'),path')::out,true
	   | None -> out,false in
	 let next' = (id',ty',path')::next in
	 look_each_site x (pred site) (stop||store,don',out',next') in
  let rec aux depth don out next = function
    | (_,ty,_ as x)::todos ->
       let stop,don',out',next' =
	 look_each_site x (Signature.arity sigs ty) (false,don,out,next) in
       if stop&&stop_on_find then out' else aux depth don' out' next' todos 
    | [] -> match next with
	    | [] -> out 
	    (* end when all graph traversed and return the list of paths *)
	    | _ -> match dist with 
		   | Some d when d <= depth -> [] 
		   (* stop when the max distance is reached *)
		   | Some _ -> aux (depth+1) don out [] next
		   | None -> aux depth don out [] next
  in aux 1 

let paths_of_interest
      is_interesting sigs graph start_ty start_point done_path =
  let don = List.fold_left (fun s (_,((x,_),_)) -> IntSet.add x s)
			   (IntSet.singleton start_point) done_path in
  let () = assert (not graph.outdated) in
  let acc = match is_interesting start_point with
    | None -> []
    | Some x -> [(x,start_point),done_path] in
  breadth_first_traversal None false is_interesting sigs graph.connect don acc
			 [] [start_point,start_ty,done_path]

(* nodes_x: agent_id list = int * int list
   nodes_y: adent_id list = int list *)
let are_connected ?candidate sigs graph ty_x x y nodes_x nodes_y dist =
  let () = assert (not graph.outdated) in
  (* look for the closest node in nodes_y *)
  let rec is_in_nodes_y nodes_y z = match nodes_y with
    | [] -> None
    | y::nds -> if z = y then Some () else is_in_nodes_y nds z in
  let rec prepare_node x ty site acc =
    if site = 0 then acc else
      match (LargeArray.get graph.connect x).(pred site) with
      | None -> prepare_node x ty (pred site) acc
      | Some ((id',ty'),_) ->
	 if (List.mem (id',ty') nodes_x) then prepare_node x ty (pred site) acc
	 else prepare_node x ty (pred site) ((x,ty,[])::acc) in
  (* breadth first search is called on a list of sites;
     start the breadth first search with the boundaries of nodes_x,
     that is all sites that are connected to other nodes in x
     and with all nodes in nodes_x marked as done *)
  let rec prepare nodes_x = match nodes_x with
    | (x,ty)::nds -> prepare_node x ty (Signature.arity sigs ty) (prepare nds)
    | [] -> [] in
  let rec done_nodes nodes_x = match nodes_x with
    | [] -> IntSet.empty
    | (x,_)::nds -> IntSet.add x (done_nodes nds) in
  match dist with
  | None ->
     (match candidate with
      | Some p when is_valid_path graph p -> Some p
      | (Some _ | None) ->
	 (match
	     breadth_first_traversal dist true
				     (fun z -> if z = y then Some () else None)
				     sigs graph.connect (IntSet.singleton x)
				     [] [] [x,ty_x,[]] with
	   | [] -> None
	   | [ _,p ] -> Some p
	   | _ :: _ -> failwith "Edges.are_they_connected completely broken"))
  | Some _ ->
     match breadth_first_traversal dist true (is_in_nodes_y nodes_y) sigs
				   graph.connect (done_nodes nodes_x) [] [] (prepare nodes_x)
     with [] -> None
	| [ _,p ] -> Some p
	| _ :: _ -> failwith "Edges.are_they_connected completely broken"
