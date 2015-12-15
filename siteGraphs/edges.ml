open Mods

let reuse_id = false

module Edge = struct
  type t = ToFree
	 | Link of (int * int * int) (** sort * id * site *)

  let _compare x y = match x,y with
    | ToFree, Link _ -> -2
    | Link _, ToFree -> 2
    | ToFree, ToFree -> 0
    | Link (_,n,s), Link (_,n',s') ->
       let c = int_compare n n' in
       if c <> 0 then c else int_compare s s'

  let dummy_link = Link (-1,-1,-1)
end

type t = Edge.t Int2Map.t * int Int2Map.t * IntSet.t IntMap.t * (int * int list)
(** agent,site -> binding_state; agent,site -> internal_state; sort -> agents;
free_id *)

let empty = (Int2Map.empty, Int2Map.empty, IntMap.empty,(1,[]))

let add_agent ty (connect,state,sort,free_id) =
  match free_id with
  | new_id,h :: t when reuse_id ->
     h,
     (connect,state,
      IntMap.add ty (IntSet.add
		       h (IntMap.find_default IntSet.empty ty sort)) sort,
      (new_id,t))
  | new_id,l ->
     new_id,
     (connect,state,
      IntMap.add ty (IntSet.add
		       new_id (IntMap.find_default IntSet.empty ty sort)) sort,
      (succ new_id,l))

let add_free ag s (connect,state,sort,free_id) =
  (Int2Map.add (ag,s) Edge.ToFree connect,state,sort,free_id)
let add_internal ag s i (connect,state,sort,free_id) =
  (connect,Int2Map.add (ag,s) i state,sort,free_id)

let add_link ty ag s ty' ag' s' (connect,state,sort,free_id) =
  (Int2Map.add (ag,s) (Edge.Link (ty',ag',s'))
	       (Int2Map.add (ag',s') (Edge.Link (ty,ag,s)) connect),
   state,sort,free_id)

let remove ag s (connect,state,sort,free_id) = function
  | Edge.ToFree -> (Int2Map.remove (ag,s) connect,state,sort,free_id)
  | Edge.Link (_,ag',s') ->
     (Int2Map.remove (ag,s) (Int2Map.remove (ag',s') connect),state,sort,free_id)
let remove_agent ty ag (connect,state,sort,(new_id,ids)) =
  (connect,state,
   IntMap.add ty (IntSet.remove ag (IntMap.find_default IntSet.empty ty sort)) sort,
  (new_id,ag::ids))
let remove_free ag s t = remove ag s t Edge.ToFree
let remove_internal ag s (connect,state,sort,free_id) =
  match Int2Map.pop (ag,s) state with
  | Some i, state' -> (connect,state',sort,free_id),i
  | None, _ ->
     failwith ("Site "^string_of_int s^ " of agent "^string_of_int ag^
		 " has no internal state to remove in the current graph.")
let remove_link ag s ag' s' t = remove ag s t (Edge.Link (-1,ag',s'))

let is_agent ty ag (_,_,s,_) =
  IntSet.mem ag (IntMap.find_default IntSet.empty ty s)
let is_free ag s (t,_,_,_) =
  match Int2Map.find_default Edge.dummy_link (ag,s) t with
  | Edge.ToFree -> true
  | Edge.Link _ -> false
let is_internal i ag s (_,t,_,_) =
  match Int2Map.find_option (ag,s) t with
  | Some j -> j = i
  | None -> false
let link_exists ag s ag' s' (t,_,_,_) =
  match Int2Map.find_default Edge.ToFree (ag,s) t with
  | Edge.Link (_,ag'',s'') -> ag'=ag'' && s'=s''
  | Edge.ToFree -> false

let exists_fresh ag s ty s' (t,_,_,_) =
  match Int2Map.find_option (ag,s) t with
  | Some (Edge.Link (ty',ag',s'')) ->
    if ty'=ty && s'=s'' then Some ag' else None
  | Some Edge.ToFree -> None
  | None -> None

(** The snapshot machinery *)
let one_connected_component sigs ty node graph =
  let rec build acc free_id dangling (links,internals,sorts,free_ag as graph) = function
    | [] -> acc,free_id,graph
    | (ty,node) :: todos ->
       let nodes_of_type = IntMap.find_default IntSet.empty ty sorts in
       let nodes_of_type' = IntSet.remove node nodes_of_type in
       if nodes_of_type == nodes_of_type' then
	 build acc free_id dangling graph todos
       else
	 let sorts' = IntMap.add ty nodes_of_type' sorts in
	 let arity = Signature.arity sigs ty in
	 let ports = Array.make arity Raw_mixture.FREE in
	 let ints = Array.make arity None in
	 let (free_id',links',dangling',todos'),ports =
	   Tools.array_fold_left_mapi
	     (fun i (free_id,links,dangling,todos) _ ->
	      match Int2Map.pop (node,i) links with
	      | None, _ ->
		 failwith ("missing edge in graph for node "^string_of_int node
			   ^" edge "^string_of_int i)
	      | Some Edge.ToFree, links' ->
		 (free_id,links',dangling,todos),Raw_mixture.FREE
	      | Some (Edge.Link (ty',n',s')), links' ->
		 match Int2Map.pop (n',s') dangling with
		 | None, dangling ->
		    (succ free_id,links',
		     Int2Map.add (node,i) free_id dangling,
		     if n' = node || List.mem (ty',n') todos
		     then todos
		     else (ty',n')::todos),
		    Raw_mixture.VAL free_id
		 | Some id, dangling' ->
		    (free_id,links',dangling',todos), Raw_mixture.VAL id)
	     (free_id,links,dangling,todos) ports in
	 let internals',ints =
	   Tools.array_fold_left_mapi
	     (fun i internals _ ->
	      let (a,b) = Int2Map.pop (node,i) internals in (b,a))
	     internals ints in
	 let skel =
	   { Raw_mixture.a_id = node; Raw_mixture.a_type = ty;
	     Raw_mixture.a_ports = ports; Raw_mixture.a_ints = ints; } in
	 build (skel::acc) free_id' dangling'
	       (links',internals',sorts',free_ag) todos'
  in build [] 1 Int2Map.empty graph [ty,node]

let build_snapshot sigs graph =
  let rec increment x = function
    | [] -> [1,x]
    | (n,y as h)::t ->
       if Raw_mixture.equal x y then (succ n,y)::t
       else h::increment x t in
  let rec aux ccs (e,i,sorts,free_id as graph) =
    match IntMap.root sorts with
    | None -> ccs
    | Some (ty,nodes) ->
       match IntSet.choose nodes with
       | None -> aux ccs (e,i,IntMap.remove ty sorts,free_id)
       | Some node ->
	  let (out,_,graph') =
	    one_connected_component sigs ty node graph in
	  aux (increment out ccs) graph' in
  aux [] graph

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

let debug_print f (links,ints,sorts,_) =
  Pp.set
    ~trailing:(fun f -> Format.fprintf f "@])")
    Int2Map.bindings (fun f -> Format.fprintf f ",")
    (fun f ((ag,s),l) ->
     let () =
       if s=0 then
	 let () = if ag <> 1 then Format.fprintf f "@])%t" Pp.space in
	 match IntMap.fold
		 (fun ty nodes -> function
			       | None ->
				  if IntSet.mem ag nodes then Some ty else None
			       | Some _ as x ->
				  if IntSet.mem ag nodes then Some (-42) else x)
		 sorts None with
	 | Some ty ->
	    Format.fprintf f "%i:%i(@[" ag ty
	 | None ->
	    Format.fprintf f "%i:notype(@[" ag in
     Format.fprintf
       f "%i%t%t" s
       (match Int2Map.find_option (ag,s) ints with
       | Some int -> fun f -> Format.fprintf f "~%i" int
       | None -> fun _ -> ())
       (match l with
	| Edge.ToFree -> fun _ -> ()
	| Edge.Link (ty',ag',s') ->
	   fun f -> Format.fprintf f "->%i:%i.%i" ag' ty' s'))
    f links

type path = (int * int * int * int) list
let rec print_path ?sigs ?graph f = function
  | [] -> Pp.empty_set f
  | [p,s,s',p'] -> Format.fprintf f "%i.%i@,-%i.%i" p s s' p'
  | (p,s,s',p')::((p'',_,_,_)::_ as l) ->
     Format.fprintf f "%i.%i@,-%i.%t%a" p s s'
		    (fun f -> if p' <> p'' then Format.fprintf f "%i##" p')
		    (print_path ?sigs ?graph) l
let empty_path = []
let rev_path l = List.rev_map (fun (a,s,s',a') -> (a',s',s,a)) l
let is_valid_path graph l =
  List.for_all (fun (a,s,s',a') -> link_exists a s a' s' graph) l

let breath_first_traversal stop_on_find is_interresting links =
  let rec look_each_site (id,path as x) site (stop,don,out,next as acc) =
    match Int2Map.pop (id,site) links with
    | None,_ -> acc
    | Some Edge.ToFree,_ -> look_each_site x (succ site) acc
    | Some (Edge.Link (_,id',site')),_ ->
       if (stop&&stop_on_find) then acc
       else if IntSet.mem id' don then look_each_site x (succ site) acc
       else
	 let don' = IntSet.add id' don in
	 let path' = (id',site',site,id)::path in
	 let out',store =
	   match is_interresting id' with
	   | Some x -> ((x,id'),path')::out,true
	   | None -> out,false in
	 let next' = (id',path')::next in
	 look_each_site x (succ site) (stop||store,don',out',next') in
  let rec aux don out next = function
    | x::todos ->
       let stop,don',out',next' = look_each_site x 0 (false,don,out,next) in
       if stop&&stop_on_find then out' else aux don' out' next' todos
    | [] -> match next with [] -> out | _ -> aux don out [] next in
  aux

let pathes_of_interrest is_interresting (links,_,_,_) start_point done_path =
  let don = List.fold_left (fun s (_,_,_,x) -> IntSet.add x s)
			   (IntSet.singleton start_point) done_path in
  let acc = match is_interresting start_point with
    | None -> []
    | Some x -> [(x,start_point),done_path] in
  breath_first_traversal
    false is_interresting links don acc [] [start_point,done_path]

let are_connected ?candidate (links,_,_,_ as graph) x y =
  match candidate with
  | Some p when is_valid_path graph p -> Some p
  | (Some _ | None) ->
     match breath_first_traversal
	     true (fun z -> if z = y then Some () else None)
	     links (IntSet.singleton x) [] [] [x,[]] with
     | [] -> None
     | [ _,p ] -> Some p
     | _ :: _ -> failwith "Edges.are_they_connected completely broken"
