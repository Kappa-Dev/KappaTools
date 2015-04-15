type place = Connected_component.node Connected_component.place
type t =
    Freed of place * int
  | Linked of (place * int) * (place * int)
  | Internalized of place * int * int

val rename :
  Connected_component.work -> Connected_component.cc -> Dipping.t -> t -> t

val print : Signature.s -> Format.formatter -> t -> unit
