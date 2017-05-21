type arity = Usual | Unary | Unary_refinement
type direction = Direct | Op

module RuleModeIdSet:
  SetMap.Set with type elt = int * arity * direction 

module RuleModeMap:
  SetMap.Map with type elt = arity * direction

val add_map:
  ('a, 'b) Alg_expr.e Locality.annot RuleModeMap.t ->
  ('a, 'b) Alg_expr.e Locality.annot RuleModeMap.t ->
  ('a, 'b) Alg_expr.e Locality.annot RuleModeMap.t
