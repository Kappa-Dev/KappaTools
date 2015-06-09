type place =
    Existing of Connected_component.ContentAgent.t * int (* node, id *)
  | Fresh of int * int (* type, id *)

type t =
    Freed of place * int
  | Linked of (place * int) * (place * int)
  | Internalized of place * int * int

val rename_place :
  Connected_component.work -> int -> Connected_component.cc ->
  Renaming.t -> place -> place

val rename :
  Connected_component.work -> int ->
  Connected_component.cc -> Renaming.t -> t -> t

val print : ?sigs:Signature.s -> Format.formatter -> t -> unit
