type 'a variable =
    CONST of 'a
  | VAR of
      ((int -> Nbr.t) -> (int -> Nbr.t) -> float ->
       int -> int -> float -> (int -> Nbr.t) -> 'a)

type id = FRESH of int | KEPT of int
type port = id * int
type action =
    BND of (port * port)
  | FREE of (port * bool)
  | MOD of (port * int)
  | DEL of int
  | ADD of (int * int)

module IdMap = MapExt.Make (struct type t = id let compare = compare end)
module PortMap = MapExt.Make (struct type t = port let compare = compare end)
module ActionSet = Set.Make(struct type t=action let compare = compare end)


