type arity = Usual | Unary
type direction = Direct | Op

module RuleModeS:
  SetMap.S with type elt = arity * direction
  =
  SetMap.Make
    (struct
      type t = arity * direction
      let compare = compare
      let print _ _ = ()
    end)

module RuleModeMap = RuleModeS.Map
