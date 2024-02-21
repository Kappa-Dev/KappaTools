type 'a bot_or_not = Bot | Not_bot of 'a
type maybe_bool = Sure_value of bool | Maybe
type 'a top_or_not = Top | Not_top of 'a
type 'a flat_lattice = Val of 'a | Any | Undefined

let lub a b =
  match a, b with
  | Undefined, _ -> b
  | _, Undefined -> a
  | Any, _ | _, Any -> Any
  | Val x, Val y when x = y -> a
  | Val _, Val _ -> Any

let glb_list a b =
  match a, b with
  | Undefined, _ | _, Undefined -> Undefined
  | Any, Val l | Val l, Any -> Val l
  | Any, Any -> Any
  | Val l, Val l' ->
    (*get the intersection of list*)
    let l = Misc_sa.inter_list (fun a b -> compare a b) l l' in
    Val l
