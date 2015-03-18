open Mods

exception Undefined
type t = int IntMap.t

let empty = IntMap.empty
let add = IntMap.add
let identity l =
  List.fold_left (fun out x -> IntMap.add x x out) IntMap.empty l
let apply i x = try IntMap.find x i with Not_found -> raise Undefined
let compose  i i' =
  IntMap.fold (fun x y out -> IntMap.add x (apply i' y) out) IntMap.empty i
