type arity = Usual | Unary
type direction = Direct | Op

module RuleModeMap:
  SetMap.Map with type elt = arity * direction
