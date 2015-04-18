open Mods

module Edge = struct
  type t = ToFree
	 | Link of (int * int * int) (** type * id * site *)
	 | Internal of int

  let compare x y = match x,y with
    | ToFree _, Internal _ -> -1
    | Internal _, Link _ -> -1
    | Link _, Internal _ -> 1
    | Internal _, ToFree _ -> 1
    | ToFree _, Link _ -> -2
    | Link _, ToFree _ -> 2
    | ToFree, ToFree -> 0
    | Link (_,n,s), Link (_,n',s') ->
       let c = int_compare n n' in
       if c <> 0 then c else int_compare s s'
    | Internal i, Internal i' -> int_compare i i'
end

module EdgeMap = MapExt.Make(Edge)
module EdgeSet = Set.Make(Edge)
type plan = int EdgeMap.t

type t = Edge.t Int2Map.t

let build_plan sigs = EdgeMap.empty

let add_free ag s t = Int2Map.add (ag,s) Edge.ToFree t
let add_internal ag s i t = Int2Map.add (ag,s) (Edge.Internal i) t

let add_link ty ag s ty' ag' s' t =
  Int2Map.add (ag,s) (Edge.Link (ty',ag',s'))
	      (Int2Map.add (ag',s') (Edge.Link (ty,ag,s)) t)

let remove ag s t = function
  | (Edge.ToFree | Edge.Internal _) -> Int2Map.remove (ag,s) t
  | Edge.Link (_,ag',s') -> Int2Map.remove (ag,s) (Int2Map.remove (ag',s') t)

let is_free ag s t =
  try Int2Map.find (ag,s) t = Edge.ToFree with Not_found -> false
let is_internal i ag s t =
  try Int2Map.find (ag,s) t = Edge.Internal i with Not_found -> false
let link_exists ag s ag' s' t =
  try match Int2Map.find (ag,s) t with
      | Edge.Link (_,ag'',s'') -> ag'=ag'' && s'=s''
      | Edge. ToFree | Edge.Internal _ -> false
  with Not_found -> false
let exists_fresh ag s ty s' t =
  try match Int2Map.find (ag,s) t with
      | Edge.Link (ty',ag',s'') ->
	 if ty'=ty && s'=s'' then Some ag' else None
      | Edge. ToFree | Edge.Internal _ -> None
  with Not_found -> None
