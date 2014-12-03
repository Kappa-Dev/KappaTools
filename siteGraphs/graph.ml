open ExceptionDefn
open Mods

module type NodeMemoryModel =
  sig
    type t
    exception Not_allocated
    val allocated : t -> int
    val dimension : t -> int
    val create : int -> t
    val free : t -> int -> unit
    val alloc : t -> Node.t -> t
    val get : t -> int -> Node.t
    val set : t -> int -> Node.t -> unit
    val iteri : (int -> Node.t -> unit) -> t -> unit
  end

module type SG =
  sig
    type t
    val fold : (int -> Node.t -> 'a -> 'a) -> t -> 'a -> 'a
    val init : int -> t
    val node_of_id : t -> int -> Node.t
    val add : t -> Node.t -> t
    val add_nodes : t -> Node.t Mods.IntMap.t -> t
    val dump : ?with_lift: bool -> t -> Environment.t -> unit
    val remove : t -> int -> unit
    val ( & ) : Node.t -> int
    val neighborhood :
      ?interrupt_with: Mods.IntSet.t ->
      ?check_connex: Mods.IntSet.t * Mods.IntSet.t ->
      ?complete_construction:bool ->
      ?d_map:int IntMap.t ->
      ?filter_elements: (Node.t -> bool) ->
      t -> int -> int -> (bool * int Mods.IntMap.t * IntSet.t * IntSet.t)

    val add_lift :
      t -> Injection.t -> ((int * int) list) Mods.IntMap.t -> Environment.t -> t
    val marshalize : t -> Node.t Mods.IntMap.t
    val size : t -> int
  end

module Make (A : NodeMemoryModel) : SG =
  struct
    type t = A.t

    let size sg = A.allocated sg

    (* A is a fragmented heap so iteri might not be very efficient *)
    let fold f sg cont =
      let cont = ref cont
      in (A.iteri (fun i node -> cont := f i node !cont) sg; !cont)

    let init size = A.create size

    let node_of_id sg i = try A.get sg i with _ -> raise Not_found

    let add sg node =
      try
	let sg = A.alloc sg node in sg
      with Invalid_argument msg -> invalid_arg ("Graph.add: "^msg)

    let add_nodes sg nodes =
      IntMap.fold (fun _ node sg -> add sg node) nodes sg

    let remove sg i = A.free sg i

    let ( & ) node = Node.get_address node

    exception Is_connex

    (*Should re-implement this function*)
    let neighborhood
	  ?(interrupt_with = IntSet.empty)
	  (** [interrrupt_with set] Interrupts function call if neighborhood *)
	  (** of node [id] contains an identifier in the specified set*)
	  ?check_connex
	  (** [check_connex=set option] if [Some set] then will additionally *)
	  (** check whether the cc of node [id] is connected to a node id in *)
	  (** [set] *)
	  ?(complete_construction = false)
	  (** if [complete=false] then the construction of the cc will stop *)
	  (** whenever [check_connex] has been verified -if asked for*)
	  ?(d_map = IntMap.empty)
	  ?filter_elements
	  sg id radius =
      let address node =
	try ( & ) node
	with Not_found -> invalid_arg "Graph.neighborhood: not allocated" in
      let add_min id d map =
	let d = try min (d+1) (IntMap.find id map)
		with Not_found ->
		     match check_connex with
		     | None -> d +1
		     | Some (_,codomain) ->
			if IntSet.mem id codomain then 0 else d+1 in
	IntMap.add id d map in
      let mark_next root depth_of_root d_map
		    to_do complete interrupt_with =
	let node = node_of_id sg root in
	let d_map,todo,stop =
	  Node.fold_status
	    (fun _ (_, lnk) (d_map, to_do, stop) ->
	     match lnk with
	     | Node.FPtr _ -> invalid_arg "Graph.neighborhood"
	     | Node.Null -> (d_map, to_do, stop)
	     | Node.Ptr (node', _) ->
		let id' = address node' in
		let stop = stop || IntSet.mem id' interrupt_with in
		let d_map = add_min id' depth_of_root d_map in
		if IntSet.mem id' complete then (d_map, to_do, stop)
		else (d_map, (id' :: to_do),stop))
	    node (d_map, to_do, false)
	in
	(d_map,todo,stop,IntSet.add root complete)
      in

      let rec iter remaining_roots to_do
		   (complete:Mods.IntSet.t) dist_map component is_connex =
	match to_do with
	| [] -> (is_connex,dist_map,component,remaining_roots)
	| root :: to_do ->
	   (*adding addr to cc -only if addr is the root of a
 nl injection if filtering is enabled*)
	   let component =
	     match filter_elements with
	     (*if root is not a non-local root then skip*)
	     | None -> IntSet.add root component
	     | Some predicate_over_node ->
		let n = node_of_id sg root in
		if predicate_over_node n
		then IntSet.add root component
		else component
	   in
	   (*checking connectivity of remaining_roots*)
	   let remaining_roots,is_connex =
	     match check_connex with
	     | None -> (remaining_roots,is_connex)
	     | Some _ ->
		let set = IntSet.remove root remaining_roots in
		let is_connex = IntSet.is_empty set in
		if complete_construction || (not is_connex) then
		  (set,is_connex)
		else raise Is_connex
	   in
	   let depth =
	     try IntMap.find root dist_map
	     with Not_found ->
	       invalid_arg "Graph.neighborhood: invariant violation" in
	   if (radius >= 0) && ((depth + 1) > radius)
				 (*do not try to mark next if depth is too big*)
	   then
	     iter remaining_roots to_do complete dist_map component is_connex
	   else
	     let (dist_map', to_do',stop, complete') =
	       mark_next root depth dist_map to_do complete interrupt_with in
	     if stop then (is_connex,dist_map',component,remaining_roots)
	     else
	       iter remaining_roots to_do' complete'
		    dist_map' component is_connex in

      let roots =
	match check_connex with None -> IntSet.empty |Some (roots,_) -> roots in
      try
	iter roots [id] IntSet.empty (IntMap.add id 0 d_map) IntSet.empty false
      with Is_connex -> (true,IntMap.empty,IntSet.empty,IntSet.empty)

    let add_lift sg phi port_map env =
      (IntMap.iter
	 (fun u_i port_list ->
	  let node_i =
	    try node_of_id sg u_i
	    with Not_found -> invalid_arg "Graph.add_lift"
	  in A.set sg u_i (Node.add_dep phi port_list node_i env))
	 port_map;
       sg)

    let marshalize sg =
      fold
	(fun id node map ->
	 IntMap.add id (Node.marshalize node) map
	) sg IntMap.empty

    let dump ?(with_lift = false) sg env =
      let hsh_lnk = Hashtbl.create (A.dimension sg) in
      let fresh = ref 0 in
      A.iteri
	(fun i node ->
	 if Node.is_empty node
	 then (Debug.tag "!")
	 else
	   (let (str, c) =
	      Node.to_string with_lift (hsh_lnk, (!fresh)) node env in
	    (fresh := c;
	     print_string ("#"^(string_of_int i)^":"^str);
	     print_newline ())))
	sg
end

module SiteGraph : SG = Make(Node.NodeHeap)
