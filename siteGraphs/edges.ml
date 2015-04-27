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

type t = Edge.t Int2Map.t * int Int2Map.t

let add_free ag s (connect,state) =
  (Int2Map.add (ag,s) Edge.ToFree connect,state)
let add_internal ag s i (connect,state) =
  (connect,Int2Map.add (ag,s) i state)

let add_link ty ag s ty' ag' s' (connect,state) =
  (Int2Map.add (ag,s) (Edge.Link (ty',ag',s'))
	       (Int2Map.add (ag',s') (Edge.Link (ty,ag,s)) connect),
   state)

let remove ag s (connect,state) = function
  | Edge.ToFree -> (Int2Map.remove (ag,s) connect,state)
  | Edge.Link (_,ag',s') ->
     (Int2Map.remove (ag,s) (Int2Map.remove (ag',s') connect),state)
let remove_free ag s t = remove ag s t Edge.ToFree
let remove_internal ag s i (connect,state) =
  (connect,Int2Map.remove (ag,s) state)
let remove_link ag s ag' s' t = remove ag s t (Edge.Link (-1,ag',s'))

let is_free ag s (t,_) =
  try Int2Map.find (ag,s) t = Edge.ToFree with Not_found -> false
let is_internal i ag s (_,t) =
  try Int2Map.find (ag,s) t = i with Not_found -> false
let link_exists ag s ag' s' (t,_) =
  try match Int2Map.find (ag,s) t with
      | Edge.Link (_,ag'',s'') -> ag'=ag'' && s'=s''
      | Edge. ToFree -> false
  with Not_found -> false
let exists_fresh ag s ty s' (t,_) =
  try match Int2Map.find (ag,s) t with
      | Edge.Link (ty',ag',s'') ->
	 if ty'=ty && s'=s'' then Some ag' else None
      | Edge. ToFree -> None
  with Not_found -> None
