type arity = Usual | Unary
type direction = Direct | Op

module RuleModeMap:
  SetMap.Map with type elt = arity * direction

val add_map:
  ('a, 'b) Alg_expr.e Locality.annot RuleModeMap.t ->
  ('a, 'b) Alg_expr.e Locality.annot RuleModeMap.t ->
  ('a, 'b) Alg_expr.e Locality.annot RuleModeMap.t
