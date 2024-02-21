type arity = Usual | Unary | Unary_refinement
type direction = Direct | Op

module RuleModeIdSet : SetMap.Set with type elt = int * arity * direction
module RuleModeMap : SetMap.Map with type elt = arity * direction

val sum_map :
  ('a -> 'a -> 'a) -> 'a RuleModeMap.t -> 'a RuleModeMap.t -> 'a RuleModeMap.t
