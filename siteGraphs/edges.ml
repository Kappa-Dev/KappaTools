open Mods

module Edge = struct
  type t = ToFree
	 | Link of (int * int * int) (** type * id * site *)

  let compare x y = match x,y with
    | ToFree, Link _ -> -2
    | Link _, ToFree -> 2
    | ToFree, ToFree -> 0
    | Link (_,n,s), Link (_,n',s') ->
       let c = int_compare n n' in
       if c <> 0 then c else int_compare s s'
end

type t = Edge.t Int2Map.t * int Int2Map.t * int IntMap.t
(** agent,site -> binding_state; agent,site -> internal_state; agent -> sort *)

let empty = (Int2Map.empty, Int2Map.empty, IntMap.empty)

let add_free ty ag s (connect,state,sort) =
  (Int2Map.add (ag,s) Edge.ToFree connect,state,
   (* !HACK! *) if s = 0 then IntMap.add ag ty sort else sort)
let add_internal ag s i (connect,state,sort) =
  (connect,Int2Map.add (ag,s) i state,sort)

let add_link ty ag s ty' ag' s' (connect,state,sort) =
  (Int2Map.add (ag,s) (Edge.Link (ty',ag',s'))
	       (Int2Map.add (ag',s') (Edge.Link (ty,ag,s)) connect),
   state,sort)

let remove ag s (connect,state,sort) = function
  | Edge.ToFree -> (Int2Map.remove (ag,s) connect,state,
		    (* !HACK! *) if s = 0 then IntMap.remove ag sort else sort)
  | Edge.Link (_,ag',s') ->
     (Int2Map.remove (ag,s) (Int2Map.remove (ag',s') connect),state,sort)
let remove_free ag s t = remove ag s t Edge.ToFree
let remove_internal ag s i (connect,state,sort) =
  (connect,Int2Map.remove (ag,s) state,sort)
let remove_link ag s ag' s' t = remove ag s t (Edge.Link (-1,ag',s'))

let is_free ag s (t,_,_) =
  try Int2Map.find (ag,s) t = Edge.ToFree with Not_found -> false
let is_internal i ag s (_,t,_) =
  try Int2Map.find (ag,s) t = i with Not_found -> false
let link_exists ag s ag' s' (t,_,_) =
  try match Int2Map.find (ag,s) t with
      | Edge.Link (_,ag'',s'') -> ag'=ag'' && s'=s''
      | Edge. ToFree -> false
  with Not_found -> false
let exists_fresh ag s ty s' (t,_,_) =
  try match Int2Map.find (ag,s) t with
      | Edge.Link (ty',ag',s'') ->
	 if ty'=ty && s'=s'' then Some ag' else None
      | Edge. ToFree -> None
  with Not_found -> None

(** The snapshot machinery *)
(*let equal max_id cc1 cc2 =
  let always_equal_min_but_not_null _ p l1 l2 =
    match p with
    | None -> None
    | Some (l,_,_) ->
       let l' = List.length l1 in
       if l' <> List.length l2 then None
       else if l = 0 || (l' > 0 && l' < l) then Some (l',l1,l2) else p in
  let internals_are_ok iso =
    IntMap.fold
      (fun k a out->
       out &&
	 let a' = IntMap.find (Dipping.apply iso k) cc2.internals in
	 Tools.array_fold_left2i (fun _ b x y -> b && x = y) true a a')
      cc1.internals true in
  let rec admissible_mapping iso = function
    | [] -> if internals_are_ok iso then [iso] else []
    | (x,y) :: t ->
       try
	 let cand = Dipping.apply iso x in
	 if cand = y then admissible_mapping iso t else []
       with Dipping.Undefined ->
	 let iso' = Dipping.add x y iso in
	 let n_x = IntMap.find x cc1.links in
	 let n_y = IntMap.find y cc2.links in
	 try
	   let remains =
	     Tools.array_fold_left2i
	       (fun _ out a b -> match a,b with
				 | ((UnSpec, UnSpec) | (Free, Free)) -> out
				 | Link (a,i), Link (b,j)
				      when i = j -> (a,b)::out
				 | (UnSpec | Free | Link _), _ ->
				    raise Not_found) t n_x n_y in
	   admissible_mapping iso' remains
	 with Not_found -> []
  in
  if cc1 == cc2 then
    [identity_injection cc1]
  else
    match Tools.array_fold_left2i always_equal_min_but_not_null (Some (0,[],[]))
				  cc1.nodes_by_type cc2.nodes_by_type with
    | None -> []
    | Some (_,l1,l2) ->
       match l1 with
       | [] -> [Dipping.empty]
       | h :: _ ->
	  let rec find_admissible = function
	    | [] -> []
	    | x ::  t ->
	       match admissible_mapping Dipping.empty [(h,x)] with
	       | [] -> find_admissible t
	       | _ :: _ as l -> l in
	  find_admissible l2
	  *)
let dump = ()
