open Mods

type lnk_t = WLD | BND | FREE | TYPE of (int*int) (*(site_id,nme)*)
type agent = {name:int ; interface : (int option * lnk_t) IntMap.t}

let dummy_agent = {name = 0 ; interface = IntMap.empty}
let name ag = ag.name
let interface ag = ag.interface
let fold_interface f ag = IntMap.fold f ag.interface
let create_agent name intf = {name=name ; interface = intf}


(** oriented graph to explore a fragment from a given root agent *)
type covering = { span: (int*int) Int2Map.t; (** covering tree *)
		  internal: (int*int) Int2Map.t; (** extra internal node *)
		}

type t = {
  agents:agent IntMap.t ;
  site_number : int ;
  graph : (int*int) Int2Map.t ;
  enum_cov : (int,covering) Hashtbl.t option ;
  ids_of_name : IntSet.t Int2Map.t ;
  (*(nm,cc_id) -> id if agent(id) has name nm in con. comp. cc_id*)
  component_of_id : int array option ; (*id -> cc_id starting at 0*)
  arity : int option (*number of explicitly connected components*) ;
  mix_id : int option ;
  size_of_cc : int array ;
  root_of_cc : int array ;
  unary : bool
}

let graph m = m.graph

let is_empty m = (IntMap.size m.agents) = 0

let get_id mix =
  match mix.mix_id with
  | None -> invalid_arg "State.get_id: Not found"
  | Some id -> id


let span root mix =
  match mix.enum_cov with
  | Some hsh ->
     (try (fun cov -> cov.span) (Hashtbl.find hsh root)
      with Not_found -> Int2Map.empty)
  | None -> invalid_arg "Mixture.span: covering not computed"

let internal_edges root mix =
  match mix.enum_cov with
  | Some hsh ->
     (try (fun cov -> cov.internal) (Hashtbl.find hsh root)
      with Not_found -> Int2Map.empty)
  | None -> invalid_arg "Mixture.span: covering not computed"

let arity mix =
  if is_empty mix then 0
  else
    match mix.arity with
    | Some arity -> arity
    | None -> invalid_arg "Mixture.arity: arity not computed"

let component_of_id id mix =
  match mix.component_of_id  with
  | Some ar ->
     begin
       try ar.(id)
       with Invalid_argument msg ->
	 invalid_arg ("Mixture.component_of_id: "^msg)
     end
  | None -> invalid_arg "Mixture.component_of_id: component_of_id not computed"

let set_root_of_cc mix =
  match mix.component_of_id with
  | None -> invalid_arg "Mixture.set_root_of_cc"
  | Some ar ->
     let root_of_cc = Array.make (arity mix) (-1) in
     Array.iteri
       (fun a_i cc_i ->
	if root_of_cc.(cc_i) < a_i then root_of_cc.(cc_i) <- a_i)
       ar;
     {mix with root_of_cc = root_of_cc}

let root_of_cc mix cc_id =
  try  Some (mix.root_of_cc.(cc_id))
  with _ -> None

let agent_of_id i mix = IntMap.find i mix.agents
let agents mix = mix.agents
let ids_of_name nm cc_id mix =
  try Int2Map.find (nm,cc_id) mix.ids_of_name with Not_found -> IntSet.empty

let is_bound a_i s_i mix =
  let ag_i = agent_of_id a_i mix in
  let (_,lnk) = IntMap.find s_i ag_i.interface in
  match lnk with
  | BND | TYPE _ -> true
  | WLD | FREE -> false

let empty id_opt = {
	agents = IntMap.empty ;
	graph = Int2Map.empty ;

	site_number = 0 ;
	enum_cov = None ;
	ids_of_name = Int2Map.empty ;
	component_of_id = None ;
	arity = None ;
	mix_id = id_opt ;
	size_of_cc = Array.make 0 0 ;
	root_of_cc = Array.make 0 0 ;
	unary = false
	}

let unary mix = mix.unary
let set_unary mix = {mix with unary = true}

let size_of_cc cc_id mix =
  try mix.size_of_cc.(cc_id)
  with Invalid_argument msg -> invalid_arg ("Mixture.size_of_cc "^msg)

let compose id agent mixture new_edges =
  let graph,site_num =
    Int2Map.fold
      (fun (a,i) (b,j) (graph,site_num) ->
       if not (id=a) then invalid_arg "Mixture.compose: invariant violation 1"
       else if a<b then
	 invalid_arg "Mixture.compose: invariant violation 2"
       (*spanning edge a->b*)
       else
	 (Int2Map.add (b,j) (a,i) (Int2Map.add (a,i) (b,j) graph),site_num+2)
      )
      new_edges (mixture.graph,mixture.site_number)
  in
  {mixture with
    graph = graph ;
    site_number = site_num ;
    agents = IntMap.add id agent mixture.agents ;
  }

let follow_in_spanning_tree root_ag (i,site_id) mix =
  let span =
    try (span root_ag mix)
    with Not_found ->
      invalid_arg
	("Mixture.follow_in_spanning_tree: span not precompiled for root "^
	   string_of_int root_ag)
  in
  try Some (Int2Map.find (i,site_id) span)
  with Not_found -> None

let follow (i,site_id) mix =
  try Some (Int2Map.find (i,site_id) mix.graph) with Not_found -> None

let create_sptr_from id mix =
  let rec depth_first queue viewed m2_span m2_internal component =
    match queue with
    | a_i::tl ->
	 let component' = if a_i < component then a_i else component in
	 let (queue',viewed',m2_span',m2_internal') =
	   fold_interface
	     (fun s_i _ (queue,viewed,m2_span,m2_internal) ->
	      let opt = follow (a_i,s_i) mix in
	      match opt with
	      | Some (b_j,s_j) ->
		 if (IntSet.mem b_j viewed) then
		   if (a_i,s_i) < (b_j,s_j) then
		     let m2_internal' =
		       Int2Map.add (a_i,s_i) (b_j,s_j) m2_internal in
		     (queue,viewed,m2_span,m2_internal')
		   else (queue,viewed,m2_span,m2_internal)
		 else
		   let m2_span' = Int2Map.add (a_i,s_i) (b_j,s_j) m2_span in
		   (b_j::queue,IntSet.add b_j viewed,m2_span',m2_internal)
	      | None -> (queue,viewed,m2_span,m2_internal)
	     ) (agent_of_id a_i mix) (tl,viewed,m2_span,m2_internal)
	 in
	 depth_first queue' viewed' m2_span' m2_internal' component'
    | [] -> ({span=m2_span; internal=m2_internal},component)
  in
  depth_first [id] (IntSet.singleton id) Int2Map.empty Int2Map.empty id

let enum_alternate_anchors mix =
  let sptrs = Hashtbl.create mix.site_number in
  let comp_map,comp_num =
    IntMap.fold
      (fun id ag (component_map,comp_num) ->
       let span,component = create_sptr_from id mix in
       (*component: smallest agent id belonging to CC(id)*)
       Hashtbl.replace sptrs id span;
       let comp_num' =
	 if component = id then comp_num+1 else comp_num in
       (IntMap.add id component component_map,comp_num')
      ) mix.agents (IntMap.empty,0)
  in
  let ar,_,_,ids_of_name,size_of_cc =
    IntMap.fold
      (fun id id_min (ar,m,fresh,ids_of_name,size_of_cc) ->
       let (cc_id,fresh) =
	 try (IntMap.find id_min m,fresh) with Not_found -> (fresh,fresh+1)
       in
       let ag = agent_of_id id mix in
       ar.(id) <- cc_id ;
       let n = try IntMap.find cc_id size_of_cc with Not_found -> 0 in
       let size_of_cc = IntMap.add cc_id (n+1) size_of_cc in
       let ids_of_name =
	 let set =
	   try Int2Map.find (ag.name,cc_id) ids_of_name
	   with Not_found -> IntSet.empty
	 in
	 Int2Map.add (ag.name,cc_id) (IntSet.add id set) ids_of_name
       in
       (ar,IntMap.add id_min cc_id m,fresh,ids_of_name,size_of_cc)
      ) comp_map (Array.make (IntMap.size comp_map) 0,
		  IntMap.empty,0,Int2Map.empty,IntMap.empty)
  in
  let size_of_cc =
    Array.init (comp_num) (fun cc_id -> IntMap.find cc_id size_of_cc) in
  {mix with enum_cov = Some sptrs ;
	    component_of_id = Some ar ;
	    arity = Some comp_num ;
	    ids_of_name = ids_of_name ;
	    size_of_cc = size_of_cc}

let dump_span f mix =
  Format.fprintf f "Arity: %d@." (arity mix) ;
  let ar = match mix.component_of_id with
      Some ar -> ar
    | None -> invalid_arg "Mixture.dump_span: component_of_id not computed"
  in
  Format.fprintf
    f "component map: %a@." (Pp.plain_array Format.pp_print_int) ar;
  let hsh = match mix.enum_cov with
      Some hsh -> hsh
    | None -> invalid_arg "Mixture.dump_span: hsh not computed"
  in
  Hashtbl.iter
    (fun root_id cov ->
     Format.fprintf f "SPTR[%d]: [%a]@." root_id
		    (Pp.set Int2Map.bindings Pp.comma
			    (fun f ((i,j),(i',j')) ->
			     Format.fprintf f "(%d,%d) -> (%d,%d)" i j i' j'))
		    cov.span;
    ) hsh
