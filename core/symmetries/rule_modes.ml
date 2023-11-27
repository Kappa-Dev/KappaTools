type arity = Usual | Unary | Unary_refinement
type direction = Direct | Op

module RuleModeIdS : SetMap.S with type elt = int * arity * direction =
SetMap.Make (struct
  type t = int * arity * direction

  let compare = compare
  let print _ _ = ()
end)

module RuleModeIdSet = RuleModeIdS.Set

module RuleModeS : SetMap.S with type elt = arity * direction =
SetMap.Make (struct
  type t = arity * direction

  let compare = compare
  let print _ _ = ()
end)

module RuleModeMap = RuleModeS.Map

let sum_map add map1 map2 =
  snd
    (RuleModeMap.monadic_fold2 () ()
       (fun () () key a1 a2 map -> (), RuleModeMap.add key (add a1 a2) map)
       (fun () () key a1 map -> (), RuleModeMap.add key a1 map)
       (fun () () _ _ map -> (), map)
       map1 map2 map2)
