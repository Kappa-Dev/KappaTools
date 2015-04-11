open Mods

exception Undefined
type t = int IntMap.t

let empty = IntMap.empty
let is_identity i = IntMap.fold (fun x y b -> b && x = y) i true
let to_list = IntMap.bindings
let add = IntMap.add
let mem = IntMap.mem
let identity l =
  List.fold_left (fun out x -> IntMap.add x x out) IntMap.empty l
let apply i x = try IntMap.find x i with Not_found -> raise Undefined
let compose  i i' =
  IntMap.fold (fun x y out -> IntMap.add x (apply i' y) out) i i'

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
