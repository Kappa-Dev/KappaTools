type 'a variable =
    CONST of 'a
  | VAR of
      ((int -> Nbr.t) -> (int -> Nbr.t) -> float ->
       int -> int -> float -> (int -> Nbr.t) -> 'a)

(** binding or modifying a port that has been added or kept from the lhs *)
type id = FRESH of int | KEPT of int
type port = id * int
type action =
    BND of (port * port)
  | FREE of (port * bool) (** FREE(p,is_side_effect_free) *)
  | MOD of (port * int)
  | DEL of int
  | ADD of (int * int) (**(id in mixture, name_id)*)

module IdMap : MapExt.S with type key = id
module PortMap : MapExt.S with type key = port
module ActionSet : Set.S with type elt = action
