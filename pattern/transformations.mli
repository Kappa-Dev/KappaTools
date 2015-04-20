type place =
    Existing of Connected_component.Node.t
  | Fresh of int * int (* type, id *)

type t =
    Freed of place * int
  | Linked of (place * int) * (place * int)
  | Internalized of place * int * int

val rename_place :
  Connected_component.work -> Connected_component.cc ->
  Dipping.t -> place -> place

val rename :
  Connected_component.work -> Connected_component.cc -> Dipping.t -> t -> t

val print : Signature.s -> Format.formatter -> t -> unit
