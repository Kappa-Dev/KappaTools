open Mods

exception Undefined
exception NotBijective
type t = int IntMap.t

let empty = IntMap.empty
let is_identity i = IntMap.fold (fun x y b -> b && x = y) i true
let to_list = IntMap.bindings
let add = IntMap.add
let mem = IntMap.mem
let fold = IntMap.fold
let identity l =
  List.fold_left (fun out x -> IntMap.add x x out) IntMap.empty l
let apply i x = try IntMap.find x i with Not_found -> raise Undefined
let compose  i i' =
  IntMap.fold (fun x y out ->
	       if mem y i' then IntMap.add x (apply i' y) out else out) i i

let inverse i =
  IntMap.fold (fun x y out ->
	       if IntMap.mem y out then raise NotBijective
	       else IntMap.add y x out) i IntMap.empty

let compare i i' = IntMap.compare int_compare i i'
let equal i i' = (compare i i') = 0

let print f i =
  ignore
    (IntMap.fold
       (fun src dst b ->
	if src <> dst then
	  let () =
	    Format.fprintf f "%t%i->%i" (if b then Pp.comma else Pp.empty)
			   src dst in
	  true
	else b
       ) i false)

let print_full f i =
  Format.fprintf
    f "@[(%a)@]"
    (Pp.set IntMap.bindings Pp.comma
	    (fun f (src,dst) -> if src<>dst then Format.fprintf f "%i->%i" src dst
				else Format.pp_print_int f src)) i
